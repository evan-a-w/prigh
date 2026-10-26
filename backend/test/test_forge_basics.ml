open! Core
open! Prigh
open Tool_test_helpers

let%expect_test "remote URLs" =
  List.iter
    [ "https://github.com/evan-a-w/prigh.git"
    ; "https://github.com/evan-a-w/prigh"
    ; "https://user:secret@GitHub.com/o/r/"
    ; "git@github.com:o/r.git"
    ; "ssh://git@codeberg.org:2222/forgejo/forgejo.git"
    ; "http://git.lan:3000/team/app.git"
    ; "https://git.corp/sub/path/team/app"
    ; "github.com:o/r"
    ; "/srv/git/r.git"
    ; "https://github.com/onlyowner"
    ; "file:///srv/git/r.git"
    ]
    ~f:(fun url ->
      print_s [%message url ~_:(Forge_remote.parse url : Forge_remote.t option)]);
  [%expect
    {|
    (https://github.com/evan-a-w/prigh.git
     (((host github.com) (owner evan-a-w) (name prigh))))
    (https://github.com/evan-a-w/prigh
     (((host github.com) (owner evan-a-w) (name prigh))))
    (https://user:secret@GitHub.com/o/r/
     (((host github.com) (owner o) (name r))))
    (git@github.com:o/r.git (((host github.com) (owner o) (name r))))
    (ssh://git@codeberg.org:2222/forgejo/forgejo.git
     (((host codeberg.org) (owner forgejo) (name forgejo))))
    (http://git.lan:3000/team/app.git
     (((host git.lan:3000) (owner team) (name app))))
    (https://git.corp/sub/path/team/app
     (((host git.corp) (owner team) (name app))))
    (github.com:o/r (((host github.com) (owner o) (name r))))
    (/srv/git/r.git ())
    (https://github.com/onlyowner ())
    (file:///srv/git/r.git ())
    |}]
;;

let%expect_test "host names as typed" =
  List.iter
    [ "codeberg.org"
    ; " https://Codeberg.org/forgejo/forgejo "
    ; "git@github.com"
    ; "git.lan:3000"
    ; ""
    ; "not a host"
    ]
    ~f:(fun h ->
      print_s
        [%message h ~_:(Forge_instance.normalize_host h : string Or_error.t)]);
  [%expect
    {|
    (codeberg.org (Ok codeberg.org))
    (" https://Codeberg.org/forgejo/forgejo " (Ok codeberg.org))
    (git@github.com (Ok github.com))
    (git.lan:3000 (Ok git.lan:3000))
    ("" (Error "not a host name: \"\" (e.g. codeberg.org)"))
    ("not a host" (Error "not a host name: \"not a host\" (e.g. codeberg.org)"))
    |}]
;;

let%expect_test
    "instances: kinds, URLs and OAuth clients from the settings and the \
     environment"
  =
  with_sandbox
  @@ fun t ->
  write
    t
    ".prigh/config.json"
    {|{"forges": {"github.com": {"client_id": "Ov23-configured"},
                  "ghe.corp": {"kind": "github", "client_id": "Ov23-corp"},
                  "git.lan:3000": {"url": "http://git.lan:3000/"},
                  "mine.example": {"kind": "forgejo", "client_id": "my-app"}}}|};
  let settings = Forge_instance.Settings.load ~home:t.dir in
  let show ?getenv ?kind ?stored host =
    let i = Forge_instance.resolve ?getenv ?kind ?stored settings ~host in
    print_s
      [%message
        ""
          ~_:(i : Forge_instance.t)
          ~api:(Forge_instance.api_url i)
          ~login:(Forge_instance.login_arg i)
          ~note:(Forge_login.oauth_note i : string option)]
  in
  let env = function
    | "PRIGH_GITHUB_CLIENT_ID" -> Some "Ov23-env"
    | _ -> None
  in
  show "github.com";
  show ~getenv:env "github.com";
  show "ghe.corp";
  show "git.lan:3000";
  show "mine.example";
  show "codeberg.org";
  show ~stored:Github "elsewhere.example";
  show ~kind:Forgejo ~stored:Github "elsewhere.example";
  print_s
    [%sexp
      (Forge_instance.Settings.hosts settings : string list)
    , (Forge_instance.Settings.of_config_fields
         [ "forges", `Object [ "x", `Object [ "kind", `String "gitlab" ] ] ]
       : Forge_instance.Settings.t Or_error.t)];
  [%expect
    {|
    (((host github.com) (kind Github) (web_url https://github.com)
      (client_id (Ov23-configured)) (default_client false))
     (api https://api.github.com) (login github) (note ()))
    (((host github.com) (kind Github) (web_url https://github.com)
      (client_id (Ov23-configured)) (default_client false))
     (api https://api.github.com) (login github) (note ()))
    (((host ghe.corp) (kind Github) (web_url https://ghe.corp)
      (client_id (Ov23-corp)) (default_client false))
     (api https://ghe.corp/api/v3) (login "github ghe.corp") (note ()))
    (((host git.lan:3000) (kind Forgejo) (web_url http://git.lan:3000)
      (client_id (a4792ccc-144e-407e-86c9-5e7d8d9c3269)) (default_client true))
     (api http://git.lan:3000/api/v1) (login "forgejo git.lan:3000")
     (note
      ("signs in through git.lan:3000's pre-registered git-credential-oauth app (its consent page names it); forges.\"git.lan:3000\".client_id sets your own")))
    (((host mine.example) (kind Forgejo) (web_url https://mine.example)
      (client_id (my-app)) (default_client false))
     (api https://mine.example/api/v1) (login "forgejo mine.example") (note ()))
    (((host codeberg.org) (kind Forgejo) (web_url https://codeberg.org)
      (client_id (a4792ccc-144e-407e-86c9-5e7d8d9c3269)) (default_client true))
     (api https://codeberg.org/api/v1) (login "forgejo codeberg.org")
     (note
      ("signs in through codeberg.org's pre-registered git-credential-oauth app (its consent page names it); forges.\"codeberg.org\".client_id sets your own")))
    (((host elsewhere.example) (kind Github) (web_url https://elsewhere.example)
      (client_id ()) (default_client false))
     (api https://elsewhere.example/api/v3) (login "github elsewhere.example")
     (note
      ("no OAuth app for elsewhere.example: set forges.\"elsewhere.example\".client_id in ~/.prigh/config.json (README: Git hosting), or sign in with a token")))
    (((host elsewhere.example) (kind Forgejo) (web_url https://elsewhere.example)
      (client_id (a4792ccc-144e-407e-86c9-5e7d8d9c3269)) (default_client true))
     (api https://elsewhere.example/api/v1) (login "forgejo elsewhere.example")
     (note
      ("signs in through elsewhere.example's pre-registered git-credential-oauth app (its consent page names it); forges.\"elsewhere.example\".client_id sets your own")))
    ((github.com ghe.corp git.lan:3000 mine.example)
     (Error
      (forges.x.kind "unknown git host kind \"gitlab\" (github or forgejo)")))
    |}];
  let show ?getenv host =
    let i =
      Forge_instance.resolve ?getenv Forge_instance.Settings.empty ~host
    in
    print_s
      [%message
        ""
          ~_:(i : Forge_instance.t)
          ~note:(Forge_login.oauth_note i : string option)]
  in
  show ~getenv:(fun _ -> None) "github.com";
  show
    ~getenv:(function
      | "PRIGH_GITHUB_CLIENT_ID" -> Some " Ov23-env "
      | _ -> None)
    "github.com";
  [%expect
    {|
    (((host github.com) (kind Github) (web_url https://github.com) (client_id ())
      (default_client false))
     (note
      ("no OAuth app for github.com: set forges.\"github.com\".client_id in ~/.prigh/config.json or $PRIGH_GITHUB_CLIENT_ID (README: Git hosting), or sign in with a token")))
    (((host github.com) (kind Github) (web_url https://github.com)
      (client_id (Ov23-env)) (default_client false))
     (note ()))
    |}]
;;

let%expect_test "pull requests and checks from GitHub's and Forgejo's JSON" =
  let pr kind json =
    print_s
      [%sexp
        (Pull_request.of_json kind (Jsonaf.of_string json)
         : Pull_request.t Or_error.t)]
  in
  pr
    Github
    (Fake_forge.pr_json
       ~host:"github.com"
       ~number:3
       ~title:"Add x"
       ~draft:true
       ());
  pr
    Forgejo
    (Fake_forge.pr_json
       ~host:"codeberg.org"
       ~number:4
       ~title:"WIP: half done"
       ~state:"closed"
       ~merged:true
       ~head_owner:"fork"
       ());
  pr Github {|{"title":"no number"}|};
  print_endline (Pull_request.forgejo_draft_title "Add x");
  print_endline (Pull_request.forgejo_draft_title "[WIP] Add x");
  let checks =
    Pr_checks.of_items
      (Pr_checks.of_github_check_runs
         (Jsonaf.of_string
            {|{"check_runs":[
               {"name":"build","status":"completed","conclusion":"success","html_url":"https://ci/1"},
               {"name":"lint","status":"completed","conclusion":"failure"},
               {"name":"docs","status":"completed","conclusion":"skipped"},
               {"name":"e2e","status":"in_progress","conclusion":null}]}|})
       @ Pr_checks.of_statuses
           (Jsonaf.of_string
              {|{"state":"pending","statuses":[
                 {"context":"ci/woodpecker","status":"pending","target_url":"https://w/1"},
                 {"context":"legacy","state":"error"}]}|})
      )
  in
  print_endline (Jsonaf.to_string (Pr_checks.to_json checks));
  List.iter
    [ []
    ; [ { Pr_checks.Item.name = "a"; state = "success"; url = None } ]
    ; [ { name = "a"; state = "success"; url = None }
      ; { name = "b"; state = "pending"; url = None }
      ]
    ]
    ~f:(fun items -> print_endline (Pr_checks.of_items items).state);
  [%expect
    {|
    (Ok
     ((number 3) (title "Add x") (state Open) (draft true) (author alice)
      (head feat/x) (head_sha abc123) (base main)
      (url https://github.com/o/r/pull/3) (body "Does x.")
      (created_at 2026-10-20T10:00:00Z) (updated_at 2026-10-21T10:00:00Z)))
    (Ok
     ((number 4) (title "WIP: half done") (state Merged) (draft true)
      (author alice) (head fork:feat/x) (head_sha abc123) (base main)
      (url https://codeberg.org/o/r/pull/4) (body "Does x.")
      (created_at 2026-10-20T10:00:00Z) (updated_at 2026-10-21T10:00:00Z)))
    (Error "pull request: missing number")
    WIP: Add x
    [WIP] Add x
    {"state":"failure","items":[{"name":"build","state":"success","url":"https://ci/1"},{"name":"lint","state":"failure","url":null},{"name":"docs","state":"skipped","url":null},{"name":"e2e","state":"pending","url":null},{"name":"ci/woodpecker","state":"pending","url":"https://w/1"},{"name":"legacy","state":"failure","url":null}]}
    none
    success
    pending
    |}]
;;

let%expect_test
    "Forgejo's API: pull requests, their diff and statuses, drafts as WIP \
     titles, errors"
  =
  let fake =
    Fake_forge.create (fun r ->
      match r.meth, Fake_forge.path_of r.url with
      | Get, "/api/v1/repos/team/app/pulls" ->
        ( 200
        , sprintf
            "[%s]"
            (Fake_forge.pr_json ~host:"git.lan" ~number:2 ~title:"[WIP] x" ()) )
      | Get, "/api/v1/repos/team/app/pulls/2" ->
        200, Fake_forge.pr_json ~host:"git.lan" ~number:2 ~title:"[WIP] x" ()
      | Get, "/api/v1/repos/team/app/pulls/2.diff" ->
        200, "diff --git a/x b/x\n"
      | Get, "/api/v1/repos/team/app/commits/abc123/status" ->
        ( 200
        , {|{"state":"failure","statuses":[{"context":"ci","status":"failure","target_url":"https://ci/9"}]}|}
        )
      | Post, "/api/v1/repos/team/app/pulls" ->
        201, Fake_forge.pr_json ~host:"git.lan" ~number:3 ~title:"WIP: y" ()
      | Get, ("/api/v1/repos/team/gone" | "/api/v1/user") ->
        404, {|{"message":"not found"}|}
      | _ -> 500, "oops")
  in
  let instance =
    Forge_instance.resolve
      Forge_instance.Settings.empty
      ~kind:Forgejo
      ~host:"git.lan"
  in
  let api =
    Forge_api.create
      ~http:(Fake_forge.http fake)
      instance
      { kind = Forgejo
      ; method_ = Oauth
      ; token = "jwt"
      ; refresh = None
      ; expires_ms = None
      ; client_id = None
      ; user = None
      }
  in
  let repo name =
    { Forge_repo.instance; owner = "team"; name; remote = "origin" }
  in
  let show sexp_of r =
    match r with
    | Ok v -> print_s (sexp_of v)
    | Error e -> printf "error: %s\n" (Error.to_string_hum e)
  in
  show
    [%sexp_of: Pull_request.t list]
    (Forge_api.list_pulls api (repo "app") ~state:"all");
  show [%sexp_of: string * bool] (Forge_api.pull_diff api (repo "app") 2);
  show [%sexp_of: Pr_checks.t] (Forge_api.checks api (repo "app") ~sha:"abc123");
  show
    [%sexp_of: Pull_request.t]
    (Forge_api.create_pull
       api
       (repo "app")
       ~title:"y"
       ~body:""
       ~head:"y"
       ~base:"main"
       ~draft:true);
  show [%sexp_of: string] (Forge_api.default_branch api (repo "gone"));
  show [%sexp_of: string] (Forge_api.current_user api);
  show [%sexp_of: string * bool] (Forge_api.pull_diff api (repo "app") 9);
  Fake_forge.print_requests fake;
  [%expect
    {|
    (((number 2) (title "[WIP] x") (state Open) (draft true) (author alice)
      (head feat/x) (head_sha abc123) (base main)
      (url https://git.lan/o/r/pull/2) (body "Does x.")
      (created_at 2026-10-20T10:00:00Z) (updated_at 2026-10-21T10:00:00Z)))
    ("diff --git a/x b/x\n" false)
    ((state failure) (items (((name ci) (state failure) (url (https://ci/9))))))
    ((number 3) (title "WIP: y") (state Open) (draft true) (author alice)
     (head feat/x) (head_sha abc123) (base main) (url https://git.lan/o/r/pull/3)
     (body "Does x.") (created_at 2026-10-20T10:00:00Z)
     (updated_at 2026-10-21T10:00:00Z))
    error: git.lan: team/gone not found (HTTP 404): check the URL of the git remote origin, or sign in as an account that can see team/gone (/login forgejo git.lan)
    error: git.lan has no Forgejo API at https://git.lan/api/v1: if it is a GitHub, set forges."git.lan".kind to github in ~/.prigh/config.json
    error: git.lan: the diff of #9: HTTP 500: oops
    GET https://git.lan/api/v1/repos/team/app/pulls?state=all&limit=50&sort=recentupdate
      Authorization: Bearer jwt
    GET https://git.lan/api/v1/repos/team/app/pulls/2.diff
      Authorization: Bearer jwt
      Accept: text/plain
    GET https://git.lan/api/v1/repos/team/app/commits/abc123/status
      Authorization: Bearer jwt
    POST https://git.lan/api/v1/repos/team/app/pulls
      Authorization: Bearer jwt
      {"title":"WIP: y","body":"","head":"y","base":"main"}
    GET https://git.lan/api/v1/repos/team/gone
      Authorization: Bearer jwt
    GET https://git.lan/api/v1/user
      Authorization: Bearer jwt
    GET https://git.lan/api/v1/repos/team/app/pulls/9.diff
      Authorization: Bearer jwt
      Accept: text/plain
    |}]
;;
