open! Core
open! Prigh
open Tool_test_helpers

(* The [issue] and [repo] tools and [pull_request]'s actions beyond reading
   and opening, on GitHub (github.com/o/r) and Forgejo (codeberg.org/o/r):
   each prints the requests made and the tool's text. The fake answers from
   a route table; anything else is a 404. *)

type route =
  { meth : Forge_http.Meth.t
  ; path : string
  ; query : string option (* a substring the query must contain *)
  ; status : int
  ; headers : (string * string) list
  ; body : string
  }

let route ?query ?(status = 200) ?(headers = []) meth path body =
  { meth; path; query; status; headers; body }
;;

let handler routes (r : Forge_http.Request.t) : Forge_http.Response.t =
  let uri = Uri.of_string r.url in
  let path = Uri.path uri in
  let query = Option.value (Uri.verbatim_query uri) ~default:"" in
  match
    List.find routes ~f:(fun route ->
      Forge_http.Meth.equal route.meth r.meth
      && String.equal route.path path
      && Option.for_all route.query ~f:(fun q ->
        String.is_substring query ~substring:q))
  with
  | Some { status; headers; body; _ } -> { status; headers; body }
  | None -> { status = 404; headers = []; body = {|{"message":"Not Found"}|} }
;;

let sign_in store ~host ~(kind : Forge_kind.t) =
  Or_error.ok_exn
    (Auth_store.modify_entry store (Forge_credential.key ~host) ~f:(fun _ ->
       Ok
         (Some
            (Forge_credential.to_json
               { kind
               ; method_ = Token
               ; token = "tok"
               ; refresh = None
               ; expires_ms = None
               ; client_id = None
               ; user = Some "octocat"
               }))))
  |> (ignore : Jsonaf.t option -> unit)
;;

type host =
  | Github
  | Forgejo

(* A repository whose origin is on [host], the tools on it, and [run TOOL
   ARGS], which prints the requests then the result. *)
let with_tools host routes f =
  with_sandbox
  @@ fun t ->
  let remote, cred_host, kind =
    match host with
    | Github -> "https://github.com/o/r.git", "github.com", Forge_kind.Github
    | Forgejo ->
      "https://codeberg.org/o/r.git", "codeberg.org", Forge_kind.Forgejo
  in
  let repo = Filename.concat t.dir "r" in
  Core_unix.mkdir_p repo;
  Fake_forge.sh
    repo
    (sprintf "git init -q -b main && git remote add origin %s" remote);
  let fake = Fake_forge.create_full (handler routes) in
  let store = Auth_store.create ~path:(Filename.concat t.dir "auth.json") in
  sign_in store ~host:cred_host ~kind;
  let forges =
    Forges.create
      ~http:(Fake_forge.http fake)
      ~sleep:(fun _ -> ())
      ~getenv:(fun _ -> None)
      ~home:t.dir
      ~store
      ()
  in
  let tools =
    [ Tool_issue.create ~forges ()
    ; Tool_repo.create ~forges ()
    ; Tool_pull_request.create
        ~forges
        ~worktrees_dir:(fun () -> Filename.concat t.dir "worktrees")
        ()
    ]
  in
  let run name args =
    let tool =
      List.find_exn tools ~f:(fun (tool : Tool.t) ->
        String.equal tool.spec.name name)
    in
    let context = Tool.Context.create ~env:t.env ~cwd:repo ~emit:ignore () in
    let result = Tool.execute tool context (Jsonaf.of_string args) in
    Fake_forge.print_requests fake;
    printf
      "%s%s\n"
      (if result.is_error then "ERROR: " else "")
      (mask t result.text)
  in
  f ~run
;;

let gh_issue
      ?(number = 12)
      ?(state = "open")
      ?(labels = "[]")
      ?(pull = false)
      ()
  =
  sprintf
    {|{"number":%d,"title":"Crash on start","state":%S,"state_reason":null,"user":{"login":"alice"},"labels":%s,"assignees":[],"comments":1,"html_url":"https://github.com/o/r/issues/%d","body":"It crashes.","created_at":"2026-10-20T10:00:00Z","updated_at":"2026-10-21T10:00:00Z"%s}|}
    number
    state
    labels
    number
    (if pull then {|,"pull_request":{"url":"x"}|} else "")
;;

let comment_json ~host =
  sprintf
    {|{"id":99,"user":{"login":"octocat"},"body":"Looking.","created_at":"2026-10-22T10:00:00Z","html_url":"https://%s/o/r/issues/12#issuecomment-99"}|}
    host
;;

(* ---- issues ------------------------------------------------------------ *)

let%expect_test "issue list and view on GitHub: pull requests are left out" =
  with_tools
    Github
    [ route
        Get
        "/repos/o/r/issues"
        (sprintf "[%s,%s]" (gh_issue ()) (gh_issue ~number:13 ~pull:true ()))
    ; route
        Get
        "/repos/o/r/issues/12"
        (gh_issue ~labels:{|[{"name":"bug"}]|} ())
    ; route
        Get
        "/repos/o/r/issues/12/comments"
        (sprintf "[%s]" (comment_json ~host:"github.com"))
    ]
  @@ fun ~run ->
  run "issue" {|{"action":"list","labels":["bug"],"assignee":"bob"}|};
  [%expect
    {|
    GET https://api.github.com/repos/o/r/issues?per_page=100&state=open&sort=updated&direction=desc&labels=bug&assignee=bob
      Authorization: Bearer tok
    1 open issue in o/r on github.com:
    #12 Crash on start (open, by alice, 1 comment, updated 2026-10-21)
    |}];
  run "issue" {|{"action":"view","number":12}|};
  [%expect
    {|
    GET https://api.github.com/repos/o/r/issues/12
      Authorization: Bearer tok
    GET https://api.github.com/repos/o/r/issues/12/comments?per_page=100
      Authorization: Bearer tok
    #12 Crash on start
    open, opened by alice on 2026-10-20
    labels: bug
    https://github.com/o/r/issues/12

    It crashes.

    --- octocat, 2026-10-22T10:00:00Z (comment 99)
    Looking.
    |}]
;;

let%expect_test "issue search on GitHub goes through /search/issues" =
  with_tools
    Github
    [ route Get "/search/issues" (sprintf {|{"items":[%s]}|} (gh_issue ())) ]
  @@ fun ~run ->
  run "issue" {|{"action":"list","search":"crash","state":"all"}|};
  [%expect
    {|
    GET https://api.github.com/search/issues?per_page=100&q=repo:o/r%20is:issue%20crash
      Authorization: Bearer tok
    1 issue in o/r on github.com:
    #12 Crash on start (open, by alice, 1 comment, updated 2026-10-21)
    |}]
;;

let fj_issue ?(state = "open") ?(labels = "[]") () =
  sprintf
    {|{"number":12,"title":"Crash on start","state":%S,"user":{"login":"alice"},"labels":%s,"assignees":null,"comments":0,"html_url":"https://codeberg.org/o/r/issues/12","body":"It crashes.","created_at":"2026-10-20T10:00:00Z","updated_at":"2026-10-21T10:00:00Z","pull_request":null}|}
    state
    labels
;;

let%expect_test "list pages through Link: rel=next" =
  let page n =
    String.concat
      ~sep:","
      (List.init 2 ~f:(fun i -> gh_issue ~number:((n * 10) + i) ()))
  in
  with_tools
    Github
    [ route Get "/repos/o/r/issues" ~query:"page=2" (sprintf "[%s]" (page 2))
    ; route
        Get
        "/repos/o/r/issues"
        ~headers:
          [ ( "Link"
            , {|<https://api.github.com/repos/o/r/issues?state=open&per_page=100&page=2>; rel="next"|}
            )
          ]
        (sprintf "[%s]" (page 1))
    ]
  @@ fun ~run ->
  run "issue" {|{"action":"list","limit":3}|};
  [%expect
    {|
    GET https://api.github.com/repos/o/r/issues?per_page=100&state=open&sort=updated&direction=desc
      Authorization: Bearer tok
    GET https://api.github.com/repos/o/r/issues?state=open&per_page=100&page=2
      Authorization: Bearer tok
    3 open issues in o/r on github.com (the first 3: limit shows more):
    #10 Crash on start (open, by alice, 1 comment, updated 2026-10-21)
    #11 Crash on start (open, by alice, 1 comment, updated 2026-10-21)
    #20 Crash on start (open, by alice, 1 comment, updated 2026-10-21)
    |}]
;;

let%expect_test "a 403 names the scope to sign in with" =
  with_tools
    Github
    [ route
        Post
        "/repos/o/r/issues/12/comments"
        ~status:403
        ~headers:[ "X-Accepted-OAuth-Scopes", "repo" ]
        {|{"message":"Resource not accessible by integration"}|}
    ]
  @@ fun ~run ->
  run "issue" {|{"action":"comment","number":12,"body":"x"}|};
  [%expect
    {|
    POST https://api.github.com/repos/o/r/issues/12/comments
      Authorization: Bearer tok
      {"body":"x"}
    ERROR: issue comment: github.com: a comment on #12: not allowed (HTTP 403: Resource not accessible by integration): the account may lack access to the repository, or the sign-in may lack a scope (it needs repo): ask the user to sign in again with /login github
    |}]
;;

(* ---- pull requests ----------------------------------------------------- *)

let gh_pr ?(draft = false) ?(state = "open") () =
  let json =
    Fake_forge.pr_json
      ~host:"github.com"
      ~number:7
      ~title:"Add x"
      ~draft
      ~state
      ()
  in
  String.chop_suffix_exn json ~suffix:"}" ^ {|,"node_id":"PR_7"}|}
;;

let%expect_test "pull request comments on GitHub" =
  with_tools
    Github
    [ route
        Get
        "/repos/o/r/issues/7/comments"
        (sprintf "[%s]" (comment_json ~host:"github.com"))
    ; route
        Get
        "/repos/o/r/pulls/7/reviews"
        {|[{"id":5,"user":{"login":"bob"},"state":"CHANGES_REQUESTED","body":"Fix a.","submitted_at":"2026-10-22T10:00:00Z","html_url":"u"}]|}
    ; route
        Get
        "/repos/o/r/pulls/7/comments"
        {|[{"id":40,"pull_request_review_id":5,"user":{"login":"bob"},"path":"a","line":3,"body":"Why?","created_at":"2026-10-22T10:00:00Z","html_url":"u40"}]|}
    ]
  @@ fun ~run ->
  run "pull_request" {|{"action":"comments","number":7}|};
  [%expect
    {|
    GET https://api.github.com/repos/o/r/issues/7/comments?per_page=100
      Authorization: Bearer tok
    GET https://api.github.com/repos/o/r/pulls/7/reviews?per_page=100
      Authorization: Bearer tok
    GET https://api.github.com/repos/o/r/pulls/7/comments?per_page=100
      Authorization: Bearer tok
    comments on #7

    conversation:
    --- octocat, 2026-10-22T10:00:00Z (comment 99)
    Looking.

    reviews:
    --- bob CHANGES_REQUESTED, 2026-10-22T10:00:00Z (review 5)
    Fix a.

    line comments:
    --- bob on a:3, 2026-10-22T10:00:00Z (comment_id 40)
    Why?
    |}]
;;

let fj_pr ?(title = "WIP: Add x") () =
  Fake_forge.pr_json ~host:"codeberg.org" ~number:7 ~title ()
;;

let%expect_test "label names and branches are encoded in URLs" =
  with_tools
    Github
    [ route Delete "/repos/o/r/issues/12/labels/area%2Fbackend" ~status:200 "[]"
    ; route Get "/repos/o/r/issues/12" (gh_issue ())
    ; route Get "/repos/o/r/actions/runs" {|{"workflow_runs":[]}|}
    ; route Get "/search/issues" {|{"items":[]}|}
    ]
  @@ fun ~run ->
  run "issue" {|{"action":"unlabel","number":12,"labels":["area/backend"]}|};
  run "repo" {|{"action":"runs","branch":"a+b&c"}|};
  run "issue" {|{"action":"list","search":"crash","labels":["prioritet: høy"]}|};
  [%expect
    {|
    DELETE https://api.github.com/repos/o/r/issues/12/labels/area%2Fbackend
      Authorization: Bearer tok
    GET https://api.github.com/repos/o/r/issues/12
      Authorization: Bearer tok
    #12 Crash on start: labels none
    GET https://api.github.com/repos/o/r/actions/runs?per_page=100&branch=a%2Bb%26c
      Authorization: Bearer tok
    no CI runs in o/r on github.com
    GET https://api.github.com/search/issues?per_page=100&q=repo:o/r%20is:issue%20state:open%20label:%22prioritet:%20h%C3%B8y%22%20crash
      Authorization: Bearer tok
    no open issues in o/r on github.com
    |}]
;;

let%expect_test "GitHub issue lists fill up to the limit past pull requests" =
  with_tools
    Github
    [ route
        Get
        "/repos/o/r/issues"
        ~query:"page=2"
        (sprintf "[%s,%s]" (gh_issue ~number:20 ()) (gh_issue ~number:21 ()))
    ; route
        Get
        "/repos/o/r/issues"
        ~headers:
          [ ( "Link"
            , {|<https://api.github.com/repos/o/r/issues?q=a,b&page=2>; rel="next", <https://api.github.com/repos/o/r/issues?page=9>; rel="last"|}
            )
          ]
        (sprintf
           "[%s,%s,%s]"
           (gh_issue ~number:10 ())
           (gh_issue ~number:11 ~pull:true ())
           (gh_issue ~number:12 ~pull:true ()))
    ]
  @@ fun ~run ->
  run "issue" {|{"action":"list","limit":2}|};
  [%expect
    {|
    GET https://api.github.com/repos/o/r/issues?per_page=100&state=open&sort=updated&direction=desc
      Authorization: Bearer tok
    GET https://api.github.com/repos/o/r/issues?q=a,b&page=2
      Authorization: Bearer tok
    2 open issues in o/r on github.com (the first 2: limit shows more):
    #10 Crash on start (open, by alice, 1 comment, updated 2026-10-21)
    #20 Crash on start (open, by alice, 1 comment, updated 2026-10-21)
    |}]
;;

let%expect_test "a next page on another host is not fetched" =
  with_tools
    Github
    [ route
        Get
        "/repos/o/r/issues"
        ~headers:
          [ ( "Link"
            , {|<https://evil.example/repos/o/r/issues?page=2>; rel="next"|} )
          ]
        (sprintf "[%s]" (gh_issue ()))
    ]
  @@ fun ~run ->
  run "issue" {|{"action":"list","limit":5}|};
  [%expect
    {|
    GET https://api.github.com/repos/o/r/issues?per_page=100&state=open&sort=updated&direction=desc
      Authorization: Bearer tok
    1 open issue in o/r on github.com:
    #12 Crash on start (open, by alice, 1 comment, updated 2026-10-21)
    |}]
;;

let%expect_test "403 for fine-grained tokens, and 429" =
  with_tools
    Github
    [ route
        Post
        "/repos/o/r/issues/12/comments"
        ~status:403
        ~headers:[ "X-Accepted-GitHub-Permissions", "issues=write" ]
        {|{"message":"Resource not accessible by personal access token"}|}
    ; route
        Patch
        "/repos/o/r/issues/12"
        ~status:429
        {|{"message":"You have exceeded a secondary rate limit."}|}
    ]
  @@ fun ~run ->
  run "issue" {|{"action":"comment","number":12,"body":"x"}|};
  run "issue" {|{"action":"close","number":12}|};
  [%expect
    {|
    POST https://api.github.com/repos/o/r/issues/12/comments
      Authorization: Bearer tok
      {"body":"x"}
    ERROR: issue comment: github.com: a comment on #12: not allowed (HTTP 403: Resource not accessible by personal access token): the account may lack access to the repository, or the sign-in may lack a scope (it needs the permission issues=write): ask the user to sign in again with /login github
    PATCH https://api.github.com/repos/o/r/issues/12
      Authorization: Bearer tok
      {"state":"closed"}
    ERROR: issue close: github.com: #12 of o/r: rate limited (HTTP 429: You have exceeded a secondary rate limit.)
    |}]
;;

(* ---- the repository and CI --------------------------------------------- *)

let%expect_test "repo on Forgejo: runs from the tasks API, no logs" =
  with_tools
    Forgejo
    [ route
        Get
        "/api/v1/repos/o/r"
        {|{"full_name":"o/r","description":"","default_branch":"main","private":true,"fork":false,"archived":false,"stars_count":0,"forks_count":0,"open_issues_count":0,"html_url":"https://codeberg.org/o/r","website":""}|}
    ; route
        Get
        "/api/v1/repos/o/r/actions/tasks"
        {|{"workflow_runs":[{"id":3,"name":"test","display_title":"Add x","status":"failure","head_branch":"feat/x","head_sha":"abc123","event":"push","url":"https://codeberg.org/o/r/actions/runs/1","run_number":1,"created_at":"2026-10-22T10:00:00Z"}],"total_count":1}|}
    ]
  @@ fun ~run ->
  run "repo" {|{"action":"view"}|};
  [%expect
    {|
    GET https://codeberg.org/api/v1/repos/o/r
      Authorization: token tok
    GET https://codeberg.org/api/v1/repos/o/r/raw/README.md
      Authorization: token tok
      Accept: text/plain
    o/r
    private, default branch main, 0 stars, 0 forks, 0 open issues
    https://codeberg.org/o/r
    |}];
  run "repo" {|{"action":"runs"}|};
  [%expect
    {|
    GET https://codeberg.org/api/v1/repos/o/r/actions/tasks?limit=50
      Authorization: token tok
    CI runs in o/r on codeberg.org, newest first:
    3 test: Add x [failure] on feat/x (push, 2026-10-22T10:00) https://codeberg.org/o/r/actions/runs/1
    |}];
  run "repo" {|{"action":"logs","job":3}|};
  [%expect
    {| ERROR: repo logs: codeberg.org (Forgejo) has no API for job logs: pull_request checks shows a pull request's commit statuses, whose links lead to the CI's pages |}]
;;

(* ---- approvals --------------------------------------------------------- *)

let%expect_test "what changes the host" =
  List.iter [ "list"; "view"; "comment"; "close"; "label" ] ~f:(fun a ->
    printf "issue %s: %b\n" a (Tool_issue.changes a));
  List.iter [ "list"; "comments"; "merge"; "review"; "ready" ] ~f:(fun a ->
    printf "pull_request %s: %b\n" a (Tool_pull_request.changes a));
  List.iter [ "view"; "runs"; "logs"; "rerun" ] ~f:(fun a ->
    printf "repo %s: %b\n" a (Tool_repo.changes a));
  [%expect
    {|
    issue list: false
    issue view: false
    issue comment: true
    issue close: true
    issue label: true
    pull_request list: false
    pull_request comments: false
    pull_request merge: true
    pull_request review: true
    pull_request ready: true
    repo view: false
    repo runs: false
    repo logs: false
    repo rerun: true
    |}]
;;
