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

let%expect_test "issue changes on GitHub" =
  with_tools
    Github
    [ route Post "/repos/o/r/issues" ~status:201 (gh_issue ())
    ; route
        Post
        "/repos/o/r/issues/12/comments"
        ~status:201
        (comment_json ~host:"github.com")
    ; route Patch "/repos/o/r/issues/12" (gh_issue ~state:"closed" ())
    ; route
        Post
        "/repos/o/r/issues/12/labels"
        {|[{"name":"bug"},{"name":"ui"}]|}
    ; route Delete "/repos/o/r/issues/12/labels/ui" {|[{"name":"bug"}]|}
    ; route
        Get
        "/repos/o/r/issues/12"
        (gh_issue ~labels:{|[{"name":"bug"}]|} ())
    ; route Post "/repos/o/r/issues/12/assignees" ~status:201 (gh_issue ())
    ]
  @@ fun ~run ->
  run
    "issue"
    {|{"action":"create","title":"Crash on start","body":"It crashes.","labels":["bug"],"assignees":["bob"]}|};
  [%expect
    {|
    POST https://api.github.com/repos/o/r/issues
      Authorization: Bearer tok
      {"title":"Crash on start","body":"It crashes.","labels":["bug"],"assignees":["bob"]}
    opened #12 Crash on start (open)
    https://github.com/o/r/issues/12
    |}];
  run "issue" {|{"action":"comment","number":12,"body":"Looking."}|};
  [%expect
    {|
    POST https://api.github.com/repos/o/r/issues/12/comments
      Authorization: Bearer tok
      {"body":"Looking."}
    commented on #12 (comment 99)
    https://github.com/o/r/issues/12#issuecomment-99
    |}];
  run
    "issue"
    {|{"action":"close","number":12,"reason":"not_planned","body":"Won't fix."}|};
  [%expect
    {|
    POST https://api.github.com/repos/o/r/issues/12/comments
      Authorization: Bearer tok
      {"body":"Won't fix."}
    PATCH https://api.github.com/repos/o/r/issues/12
      Authorization: Bearer tok
      {"state":"closed","state_reason":"not_planned"}
    closed #12 Crash on start (closed)
    https://github.com/o/r/issues/12
    |}];
  run "issue" {|{"action":"label","number":12,"labels":["bug","ui"]}|};
  [%expect
    {|
    POST https://api.github.com/repos/o/r/issues/12/labels
      Authorization: Bearer tok
      {"labels":["bug","ui"]}
    GET https://api.github.com/repos/o/r/issues/12
      Authorization: Bearer tok
    #12 Crash on start: labels bug
    |}];
  run "issue" {|{"action":"unlabel","number":12,"labels":["ui"]}|};
  [%expect
    {|
    DELETE https://api.github.com/repos/o/r/issues/12/labels/ui
      Authorization: Bearer tok
    GET https://api.github.com/repos/o/r/issues/12
      Authorization: Bearer tok
    #12 Crash on start: labels bug
    |}];
  run "issue" {|{"action":"assign","number":12,"assignees":["bob"]}|};
  [%expect
    {|
    POST https://api.github.com/repos/o/r/issues/12/assignees
      Authorization: Bearer tok
      {"assignees":["bob"]}
    GET https://api.github.com/repos/o/r/issues/12
      Authorization: Bearer tok
    #12 Crash on start: assignees none
    |}];
  run "issue" {|{"action":"frobnicate","number":12}|};
  [%expect
    {| ERROR: invalid arguments: unknown action "frobnicate": use list, view, create, comment, edit, close, reopen, label, unlabel, assign or unassign |}]
;;

let fj_issue ?(state = "open") ?(labels = "[]") () =
  sprintf
    {|{"number":12,"title":"Crash on start","state":%S,"user":{"login":"alice"},"labels":%s,"assignees":null,"comments":0,"html_url":"https://codeberg.org/o/r/issues/12","body":"It crashes.","created_at":"2026-10-20T10:00:00Z","updated_at":"2026-10-21T10:00:00Z","pull_request":null}|}
    state
    labels
;;

let%expect_test "issues on Forgejo: labels by id, assignees by edit" =
  with_tools
    Forgejo
    [ route Get "/api/v1/repos/o/r/issues" (sprintf "[%s]" (fj_issue ()))
    ; route
        Get
        "/api/v1/repos/o/r/labels"
        {|[{"id":3,"name":"bug"},{"id":4,"name":"ui"}]|}
    ; route Post "/api/v1/repos/o/r/issues" ~status:201 (fj_issue ())
    ; route
        Post
        "/api/v1/repos/o/r/issues/12/labels"
        {|[{"id":3,"name":"bug"},{"id":4,"name":"ui"}]|}
    ; route Delete "/api/v1/repos/o/r/issues/12/labels/4" ~status:204 ""
    ; route
        Get
        "/api/v1/repos/o/r/issues/12"
        (fj_issue ~labels:{|[{"name":"bug"}]|} ())
    ; route Patch "/api/v1/repos/o/r/issues/12" (fj_issue ~state:"closed" ())
    ]
  @@ fun ~run ->
  run "issue" {|{"action":"list","labels":["bug"],"search":"crash"}|};
  [%expect
    {|
    GET https://codeberg.org/api/v1/repos/o/r/issues?limit=50&state=open&type=issues&labels=bug&q=crash
      Authorization: token tok
    1 open issue in o/r on codeberg.org:
    #12 Crash on start (open, by alice, 0 comments, updated 2026-10-21)
    |}];
  run
    "issue"
    {|{"action":"create","title":"Crash on start","labels":["bug","nope"]}|};
  [%expect
    {|
    GET https://codeberg.org/api/v1/repos/o/r/labels?limit=50
      Authorization: token tok
    GET https://codeberg.org/api/v1/orgs/o/labels?limit=50
      Authorization: token tok
    ERROR: issue create: o/r has no label nope (it has bug, ui)
    |}];
  run "issue" {|{"action":"label","number":12,"labels":["bug","ui"]}|};
  [%expect
    {|
    GET https://codeberg.org/api/v1/repos/o/r/labels?limit=50
      Authorization: token tok
    POST https://codeberg.org/api/v1/repos/o/r/issues/12/labels
      Authorization: token tok
      {"labels":[3,4]}
    GET https://codeberg.org/api/v1/repos/o/r/issues/12
      Authorization: token tok
    #12 Crash on start: labels bug
    |}];
  run "issue" {|{"action":"unlabel","number":12,"labels":["ui"]}|};
  [%expect
    {|
    GET https://codeberg.org/api/v1/repos/o/r/labels?limit=50
      Authorization: token tok
    DELETE https://codeberg.org/api/v1/repos/o/r/issues/12/labels/4
      Authorization: token tok
    GET https://codeberg.org/api/v1/repos/o/r/issues/12
      Authorization: token tok
    #12 Crash on start: labels bug
    |}];
  run "issue" {|{"action":"assign","number":12,"assignees":["bob"]}|};
  [%expect
    {|
    GET https://codeberg.org/api/v1/repos/o/r/issues/12
      Authorization: token tok
    PATCH https://codeberg.org/api/v1/repos/o/r/issues/12
      Authorization: token tok
      {"assignees":["bob"]}
    #12 Crash on start: assignees none
    |}];
  run "issue" {|{"action":"close","number":12,"reason":"not_planned"}|};
  [%expect
    {|
    PATCH https://codeberg.org/api/v1/repos/o/r/issues/12
      Authorization: token tok
      {"state":"closed"}
    closed #12 Crash on start (closed)
    https://codeberg.org/o/r/issues/12
    |}]
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

let%expect_test "pull request changes on GitHub" =
  with_tools
    Github
    [ route Get "/repos/o/r/pulls/7" (gh_pr ~draft:true ())
    ; route Patch "/repos/o/r/pulls/7" (gh_pr ())
    ; route
        Post
        "/repos/o/r/issues/7/comments"
        ~status:201
        (comment_json ~host:"github.com")
    ; route
        Post
        "/graphql"
        {|{"data":{"markPullRequestReadyForReview":{"pullRequest":{"isDraft":false}}}}|}
    ; route Put "/repos/o/r/pulls/7/merge" {|{"merged":true,"sha":"def"}|}
    ; route Get "/repos/o/r" {|{"full_name":"o/r","default_branch":"main"}|}
    ; route Delete "/repos/o/r/git/refs/heads/feat/x" ~status:204 ""
    ; route
        Post
        "/repos/o/r/pulls/7/reviews"
        {|{"id":5,"user":{"login":"octocat"},"state":"APPROVED","body":"LGTM","submitted_at":"2026-10-22T10:00:00Z","html_url":"u"}|}
    ; route
        Post
        "/repos/o/r/pulls/7/comments/40/replies"
        ~status:201
        {|{"id":41,"user":{"login":"octocat"},"path":"a","line":3,"body":"Done.","in_reply_to_id":40,"created_at":"2026-10-22T10:00:00Z","html_url":"u41"}|}
    ; route Post "/repos/o/r/pulls/7/requested_reviewers" ~status:201 (gh_pr ())
    ]
  @@ fun ~run ->
  run "pull_request" {|{"action":"edit","number":7,"closes":[12,13]}|};
  [%expect
    {|
    GET https://api.github.com/repos/o/r/pulls/7
      Authorization: Bearer tok
    PATCH https://api.github.com/repos/o/r/pulls/7
      Authorization: Bearer tok
      {"body":"Does x.\n\nCloses #12\nCloses #13"}
    edited #7 Add x (open)
    https://github.com/o/r/pull/7
    |}];
  run "pull_request" {|{"action":"comment","number":7,"body":"Looking."}|};
  [%expect
    {|
    POST https://api.github.com/repos/o/r/issues/7/comments
      Authorization: Bearer tok
      {"body":"Looking."}
    commented on #7 (comment 99)
    https://github.com/o/r/issues/12#issuecomment-99
    |}];
  run "pull_request" {|{"action":"ready","number":7}|};
  [%expect
    {|
    GET https://api.github.com/repos/o/r/pulls/7
      Authorization: Bearer tok
    POST https://api.github.com/graphql
      Authorization: Bearer tok
      {"query":"mutation($id: ID!) { markPullRequestReadyForReview(input: {pullRequestId: $id}) { pullRequest { isDraft } } }","variables":{"id":"PR_7"}}
    ready for review: #7 Add x (open)
    https://github.com/o/r/pull/7
    |}];
  run
    "pull_request"
    {|{"action":"merge","number":7,"method":"squash","delete_branch":true}|};
  [%expect
    {|
    GET https://api.github.com/repos/o/r/pulls/7
      Authorization: Bearer tok
    GET https://api.github.com/repos/o/r
      Authorization: Bearer tok
    PUT https://api.github.com/repos/o/r/pulls/7/merge
      Authorization: Bearer tok
      {"merge_method":"squash","sha":"abc123"}
    DELETE https://api.github.com/repos/o/r/git/refs/heads/feat/x
      Authorization: Bearer tok
    merged #7 Add x (squash) into main, deleted feat/x
    https://github.com/o/r/pull/7
    |}];
  run
    "pull_request"
    {|{"action":"review","number":7,"event":"approve","body":"LGTM","comments":[{"path":"a","line":3,"body":"nit"}]}|};
  [%expect
    {|
    GET https://api.github.com/repos/o/r/pulls/7
      Authorization: Bearer tok
    POST https://api.github.com/repos/o/r/pulls/7/reviews
      Authorization: Bearer tok
      {"event":"APPROVE","body":"LGTM","commit_id":"abc123","comments":[{"path":"a","line":3,"side":"RIGHT","body":"nit"}]}
    reviewed #7 Add x: APPROVED, 1 line comment (review 5)
    https://github.com/o/r/pull/7
    |}];
  run
    "pull_request"
    {|{"action":"reply","number":7,"comment_id":40,"body":"Done."}|};
  [%expect
    {|
    POST https://api.github.com/repos/o/r/pulls/7/comments/40/replies
      Authorization: Bearer tok
      {"body":"Done."}
    replied to comment 40 on #7 (comment 41)
    u41
    |}];
  run
    "pull_request"
    {|{"action":"request_reviewers","number":7,"reviewers":["bob"]}|};
  [%expect
    {|
    POST https://api.github.com/repos/o/r/pulls/7/requested_reviewers
      Authorization: Bearer tok
      {"reviewers":["bob"]}
    asked bob to review #7
    |}]
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

let%expect_test "pull request changes on Forgejo" =
  with_tools
    Forgejo
    [ route Get "/api/v1/repos/o/r/pulls/7" (fj_pr ())
    ; route Patch "/api/v1/repos/o/r/pulls/7" (fj_pr ~title:"Add x" ())
    ; route Post "/api/v1/repos/o/r/pulls/7/merge" ""
    ; route
        Get
        "/api/v1/repos/o/r"
        {|{"full_name":"o/r","default_branch":"main"}|}
    ; route
        Post
        "/api/v1/repos/o/r/pulls/7/reviews"
        {|{"id":5,"user":{"login":"octocat"},"state":"REQUEST_CHANGES","body":"Fix a.","submitted_at":"2026-10-22T10:00:00Z","html_url":"u"}|}
    ; route
        Get
        "/api/v1/repos/o/r/pulls/7/reviews"
        {|[{"id":5,"user":{"login":"bob"},"state":"COMMENT","body":"","submitted_at":"2026-10-22T10:00:00Z","html_url":"u"}]|}
    ; route
        Get
        "/api/v1/repos/o/r/pulls/7/reviews/5/comments"
        {|[{"id":40,"pull_request_review_id":5,"user":{"login":"bob"},"path":"a","position":3,"body":"Why?","created_at":"2026-10-22T10:00:00Z","html_url":"u40"}]|}
    ; route Get "/api/v1/repos/o/r/issues/7/comments" "[]"
    ]
  @@ fun ~run ->
  run "pull_request" {|{"action":"ready","number":7}|};
  [%expect
    {|
    GET https://codeberg.org/api/v1/repos/o/r/pulls/7
      Authorization: token tok
    PATCH https://codeberg.org/api/v1/repos/o/r/pulls/7
      Authorization: token tok
      {"title":"Add x"}
    ready for review: #7 Add x (open)
    https://codeberg.org/o/r/pull/7
    |}];
  run
    "pull_request"
    {|{"action":"merge","number":7,"method":"rebase","delete_branch":true,"title":"Add x (#7)"}|};
  [%expect
    {|
    GET https://codeberg.org/api/v1/repos/o/r/pulls/7
      Authorization: token tok
    GET https://codeberg.org/api/v1/repos/o/r
      Authorization: token tok
    POST https://codeberg.org/api/v1/repos/o/r/pulls/7/merge
      Authorization: token tok
      {"Do":"rebase","delete_branch_after_merge":true,"MergeTitleField":"Add x (#7)","head_commit_id":"abc123"}
    merged #7 WIP: Add x (rebase) into main, deleted feat/x
    https://codeberg.org/o/r/pull/7
    |}];
  run
    "pull_request"
    {|{"action":"review","number":7,"event":"request_changes","body":"Fix a.","comments":[{"path":"a","line":3,"body":"nit"}]}|};
  [%expect
    {|
    GET https://codeberg.org/api/v1/repos/o/r/pulls/7
      Authorization: token tok
    POST https://codeberg.org/api/v1/repos/o/r/pulls/7/reviews
      Authorization: token tok
      {"event":"REQUEST_CHANGES","body":"Fix a.","commit_id":"abc123","comments":[{"path":"a","new_position":3,"body":"nit"}]}
    reviewed #7 WIP: Add x: CHANGES_REQUESTED, 1 line comment (review 5)
    https://codeberg.org/o/r/pull/7
    |}];
  run "pull_request" {|{"action":"comments","number":7}|};
  [%expect
    {|
    GET https://codeberg.org/api/v1/repos/o/r/issues/7/comments?limit=50
      Authorization: token tok
    GET https://codeberg.org/api/v1/repos/o/r/pulls/7/reviews?limit=50
      Authorization: token tok
    GET https://codeberg.org/api/v1/repos/o/r/pulls/7/reviews/5/comments?limit=50
      Authorization: token tok
    comments on #7

    reviews:
    --- bob COMMENTED, 2026-10-22T10:00:00Z (review 5)

    line comments:
    --- bob on a:3, 2026-10-22T10:00:00Z (comment_id 40)
    Why?
    |}];
  run
    "pull_request"
    {|{"action":"reply","number":7,"comment_id":40,"body":"Done."}|};
  [%expect
    {| ERROR: pull_request reply: codeberg.org (Forgejo) cannot reply in a review thread: comment on the pull request instead, quoting the comment |}]
;;

let%expect_test
    "merge never deletes a branch outside the repository, nor its default or \
     base branch"
  =
  let pr ~number ?head_owner ?head_name ?head_deleted ?(head = "feat/x") () =
    Fake_forge.pr_json
      ~host:"github.com"
      ~number
      ~title:"Add x"
      ?head_owner
      ?head_name
      ?head_deleted
      ~head
      ()
  in
  with_tools
    Github
    [ route
        Get
        "/repos/o/r/pulls/7"
        (pr ~number:7 ~head_deleted:true ~head:"main" ())
    ; route
        Get
        "/repos/o/r/pulls/8"
        (pr ~number:8 ~head_name:"r-fork" ~head:"main" ())
    ; route Get "/repos/o/r/pulls/9" (pr ~number:9 ~head:"dev" ())
    ; route Get "/repos/o/r/pulls/10" (pr ~number:10 ~head:"main" ())
    ; route Get "/repos/o/r/pulls/11" (pr ~number:11 ~head:"fix/a#b" ())
    ; route Put "/repos/o/r/pulls/7/merge" {|{"merged":true}|}
    ; route Put "/repos/o/r/pulls/8/merge" {|{"merged":true}|}
    ; route Put "/repos/o/r/pulls/11/merge" {|{"merged":true}|}
    ; route Delete "/repos/o/r/git/refs/heads/fix/a%23b" ~status:204 ""
    ; route Get "/repos/o/r" {|{"full_name":"o/r","default_branch":"dev"}|}
    ]
  @@ fun ~run ->
  let merge n =
    run
      "pull_request"
      (sprintf {|{"action":"merge","number":%d,"delete_branch":true}|} n)
  in
  (* a deleted fork's head: [head.repo] is null *)
  merge 7;
  [%expect
    {|
    GET https://api.github.com/repos/o/r/pulls/7
      Authorization: Bearer tok
    PUT https://api.github.com/repos/o/r/pulls/7/merge
      Authorization: Bearer tok
      {"merge_method":"merge","sha":"abc123"}
    merged #7 Add x (merge) into main, kept main: it is in another repository (a fork, or one that was deleted), not this one
    https://github.com/o/r/pull/7
    |}];
  (* a fork with the same owner *)
  merge 8;
  [%expect
    {|
    GET https://api.github.com/repos/o/r/pulls/8
      Authorization: Bearer tok
    PUT https://api.github.com/repos/o/r/pulls/8/merge
      Authorization: Bearer tok
      {"merge_method":"merge","sha":"abc123"}
    merged #8 Add x (merge) into main, kept main: it is in another repository (a fork, or one that was deleted), not this one
    https://github.com/o/r/pull/8
    |}];
  (* the default branch, then the base: refused before merging *)
  merge 9;
  [%expect
    {|
    GET https://api.github.com/repos/o/r/pulls/9
      Authorization: Bearer tok
    GET https://api.github.com/repos/o/r
      Authorization: Bearer tok
    ERROR: pull_request merge: refusing to delete dev: it is the repository's default branch; merge without delete_branch
    |}];
  merge 10;
  [%expect
    {|
    GET https://api.github.com/repos/o/r/pulls/10
      Authorization: Bearer tok
    GET https://api.github.com/repos/o/r
      Authorization: Bearer tok
    ERROR: pull_request merge: refusing to delete main: it is the pull request's base branch; merge without delete_branch
    |}];
  (* a branch name with [#]: encoded, its [/] kept *)
  merge 11;
  [%expect
    {|
    GET https://api.github.com/repos/o/r/pulls/11
      Authorization: Bearer tok
    GET https://api.github.com/repos/o/r
      Authorization: Bearer tok
    PUT https://api.github.com/repos/o/r/pulls/11/merge
      Authorization: Bearer tok
      {"merge_method":"merge","sha":"abc123"}
    DELETE https://api.github.com/repos/o/r/git/refs/heads/fix/a%23b
      Authorization: Bearer tok
    merged #11 Add x (merge) into main, deleted fix/a#b
    https://github.com/o/r/pull/11
    |}]
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

let%expect_test "repo view, runs, run, logs and rerun on GitHub" =
  with_tools
    Github
    [ route
        Get
        "/repos/o/r"
        {|{"full_name":"o/r","description":"Does r.","default_branch":"main","private":false,"fork":false,"archived":false,"stargazers_count":5,"forks_count":1,"open_issues_count":2,"html_url":"https://github.com/o/r","homepage":""}|}
    ; route Get "/repos/o/r/readme" "# r\n\nDoes r.\n"
    ; route
        Get
        "/repos/o/r/actions/runs"
        {|{"workflow_runs":[{"id":100,"name":"CI","display_title":"Add x","status":"completed","conclusion":"failure","head_branch":"feat/x","head_sha":"abc123","event":"pull_request","html_url":"https://github.com/o/r/actions/runs/100","created_at":"2026-10-22T10:00:00Z"}]}|}
    ; route
        Get
        "/repos/o/r/actions/runs/100"
        {|{"id":100,"name":"CI","display_title":"Add x","status":"completed","conclusion":"failure","head_branch":"feat/x","head_sha":"abc123","event":"pull_request","html_url":"https://github.com/o/r/actions/runs/100","created_at":"2026-10-22T10:00:00Z"}|}
    ; route
        Get
        "/repos/o/r/actions/runs/100/jobs"
        {|{"jobs":[{"id":200,"name":"build","status":"completed","conclusion":"failure","html_url":"u","steps":[{"name":"Checkout","status":"completed","conclusion":"success"},{"name":"Test","status":"completed","conclusion":"failure"}]}]}|}
    ; route
        Get
        "/repos/o/r/actions/jobs/200/logs"
        ~status:302
        ~headers:[ "Location", "https://blob.example/logs/200?sig=x" ]
        ""
    ; route Get "/logs/200" "line 1\nFAIL: test_x\n"
    ; route
        Post
        "/repos/o/r/actions/runs/100/rerun-failed-jobs"
        ~status:201
        "{}"
    ]
  @@ fun ~run ->
  run "repo" {|{"action":"view"}|};
  [%expect
    {|
    GET https://api.github.com/repos/o/r
      Authorization: Bearer tok
    GET https://api.github.com/repos/o/r/readme
      Authorization: Bearer tok
      Accept: application/vnd.github.raw
    o/r: Does r.
    public, default branch main, 5 stars, 1 fork, 2 open issues
    https://github.com/o/r

    README:
    # r

    Does r.
    |}];
  run "repo" {|{"action":"runs","branch":"feat/x"}|};
  [%expect
    {|
    GET https://api.github.com/repos/o/r/actions/runs?per_page=100&branch=feat/x
      Authorization: Bearer tok
    CI runs in o/r on github.com, newest first:
    100 CI: Add x [failure] on feat/x (pull_request, 2026-10-22T10:00) https://github.com/o/r/actions/runs/100
    |}];
  run "repo" {|{"action":"run","run":100}|};
  [%expect
    {|
    GET https://api.github.com/repos/o/r/actions/runs/100
      Authorization: Bearer tok
    GET https://api.github.com/repos/o/r/actions/runs/100/jobs?per_page=100
      Authorization: Bearer tok
    100 CI: Add x [failure] on feat/x (pull_request, 2026-10-22T10:00) https://github.com/o/r/actions/runs/100
      job 200 build [failure]
        Checkout [success]
        Test [failure]
    |}];
  run "repo" {|{"action":"logs","job":200}|};
  [%expect
    {|
    GET https://api.github.com/repos/o/r/actions/jobs/200/logs
      Authorization: Bearer tok
      Accept: */*
    GET https://blob.example/logs/200?sig=x
    line 1
    FAIL: test_x
    |}];
  run "repo" {|{"action":"rerun","run":100}|};
  [%expect
    {|
    POST https://api.github.com/repos/o/r/actions/runs/100/rerun-failed-jobs
      Authorization: Bearer tok
    re-running the failed jobs of run 100 (action=run shows its progress)
    |}]
;;

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
