open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

(* The git hosting methods end to end, on {!Fake_forge.set_up_repos}'s
   repository and {!Fake_forge.github}. *)

let with_server f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let repo = Fake_forge.set_up_repos t in
  write
    t
    ".prigh/config.json"
    (sprintf {|{"worktrees_dir": %S}|} (Filename.concat t.dir "worktrees"));
  let store = Auth_store.create ~path:(Filename.concat t.dir "auth.json") in
  let login =
    Login_manager.create ~env:t.env ~sw ~getenv:(fun _ -> None) ~store ()
  in
  let fake = Fake_forge.create Fake_forge.github in
  let forges =
    Forges.create
      ~http:(Fake_forge.http fake)
      ~sleep:(fun _ -> ())
      ~getenv:(fun _ -> None)
      ~home:t.dir
      ~store
      ()
  in
  let provider = Faux_provider.create [] in
  let sessions_dir = Filename.concat t.dir "sessions" in
  let new_agent ?session ~cwd () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:Tools.all
      ~sessions_dir
      ~home:t.dir
      ?session
      ~cwd
      ()
  in
  let server =
    Rpc_server.create
      ~env:t.env
      ~sw
      ~login
      ~forges
      ~sessions_dir
      ~cwd:repo
      ~new_agent
      ()
  in
  let sent = Queue.create () in
  let client = Rpc_server.connect server ~send:(Queue.enqueue sent) in
  let call ?(params = "{}") meth =
    let response =
      Rpc_server.handle
        server
        client
        (Json.of_string
           (sprintf {|{"id": "r", "method": "%s", "params": %s}|} meth params))
    in
    (match Json.member "result" response with
     | Some result -> print_endline (mask t (Json.to_string result))
     | None ->
       (match Json.member "error" response with
        | Some (`String e) -> printf "error: %s\n" (mask t e)
        | _ -> print_endline (Json.to_string response)));
    response
  in
  let call ?params meth = ignore (call ?params meth : Json.t) in
  f t ~repo ~call ~sent ~fake ~login
;;

let auth_events sent =
  Queue.to_list sent
  |> List.filter ~f:(fun j ->
    match Json.member "event" j with
    | Some (`String "auth") -> true
    | _ -> false)
;;

let rec wait_for ?(n = 2000) sent ~kind =
  match
    List.find (auth_events sent) ~f:(fun j ->
      match Json.member "kind" j with
      | Some (`String k) -> String.equal k kind
      | _ -> false)
  with
  | Some j -> j
  | None ->
    if n = 0 then failwithf "no %s event" kind ();
    Eio_unix.sleep 0.005;
    wait_for ~n:(n - 1) sent ~kind
;;

let%expect_test
    "sign in, list, view, prefill, create (pushing first) and check out pull \
     requests"
  =
  with_server
  @@ fun t ~repo ~call ~sent ~fake ~login ->
  call "pr_list";
  call "forge_status";
  call "forge_login" ~params:{|{"host": "github.com", "method": "token"}|};
  let prompt = wait_for sent ~kind:"prompt" in
  print_endline (Json.to_string prompt);
  call
    "auth_respond"
    ~params:
      (sprintf
         {|{"id": %s, "value": "ghp_pasted"}|}
         (Json.to_string (Option.value_exn (Json.member "id" prompt))));
  print_endline (Json.to_string (wait_for sent ~kind:"forge_done"));
  Login_manager.wait login;
  call "forge_status";
  Fake_forge.print_requests fake;
  [%expect
    {|
    error: not signed in to github.com: sign in under Settings → Git hosting in prigh-web, or with /login github
    {"accounts":[{"host":"github.com","kind":"github","label":"GitHub","user":null,"method":null,"expires_ms":null,"oauth":false,"oauth_note":"no OAuth app for github.com: set forges.\"github.com\".client_id in ~/.prigh/config.json or $PRIGH_GITHUB_CLIENT_ID (README: Git hosting), or sign in with a token"},{"host":"codeberg.org","kind":"forgejo","label":"Forgejo","user":null,"method":null,"expires_ms":null,"oauth":true,"oauth_note":"signs in through codeberg.org's pre-registered git-credential-oauth app (its consent page names it); forges.\"codeberg.org\".client_id sets your own"}]}
    {}
    {"type":"event","event":"auth","kind":"prompt","id":"p1","prompt":"secret","message":"Paste a GitHub personal access token for github.com: create one at https://github.com/settings/tokens/new?scopes=repo&description=prigh (classic, scope repo; or a fine-grained token with Contents and Pull requests: read and write)"}
    {}
    {"type":"event","event":"auth","kind":"forge_done","host":"github.com","user":"octocat","method":"token"}
    {"accounts":[{"host":"github.com","kind":"github","label":"GitHub","user":"octocat","method":"token","expires_ms":null,"oauth":false,"oauth_note":"no OAuth app for github.com: set forges.\"github.com\".client_id in ~/.prigh/config.json or $PRIGH_GITHUB_CLIENT_ID (README: Git hosting), or sign in with a token"},{"host":"codeberg.org","kind":"forgejo","label":"Forgejo","user":null,"method":null,"expires_ms":null,"oauth":true,"oauth_note":"signs in through codeberg.org's pre-registered git-credential-oauth app (its consent page names it); forges.\"codeberg.org\".client_id sets your own"}]}
    GET https://api.github.com/user
      Authorization: Bearer ghp_pasted
    |}];
  call "pr_list";
  call "pr_list" ~params:{|{"state": "merged"}|};
  call "pr_view" ~params:{|{"number": 7}|};
  call "pr_view" ~params:{|{"number": 99}|};
  call "pr_view" ~params:{|{"number": "7"}|};
  Fake_forge.print_requests fake;
  [%expect
    {|
    {"repo":{"host":"github.com","kind":"github","owner":"o","name":"r","remote":"origin","url":"https://github.com/o/r"},"prs":[{"number":7,"title":"From a contributor","state":"open","draft":false,"author":"alice","head":"fork:feat/x","base":"main","url":"https://github.com/o/r/pull/7","body":"Does x.","created_at":"2026-10-20T10:00:00Z","updated_at":"2026-10-21T10:00:00Z"},{"number":5,"title":"Old one","state":"open","draft":true,"author":"alice","head":"feat/x","base":"main","url":"https://github.com/o/r/pull/5","body":"Does x.","created_at":"2026-10-20T10:00:00Z","updated_at":"2026-10-21T10:00:00Z"}]}
    error: unknown state "merged": open, closed or all
    {"repo":{"host":"github.com","kind":"github","owner":"o","name":"r","remote":"origin","url":"https://github.com/o/r"},"pr":{"number":7,"title":"From a contributor","state":"open","draft":false,"author":"alice","head":"fork:feat/x","base":"main","url":"https://github.com/o/r/pull/7","body":"Does x.","created_at":"2026-10-20T10:00:00Z","updated_at":"2026-10-21T10:00:00Z"},"checks":{"state":"success","items":[{"name":"build","state":"success","url":"https://ci/1"}]},"diff":"diff --git a/b b/b\nnew file mode 100644\n--- /dev/null\n+++ b/b\n@@ -0,0 +1 @@\n+b\n","truncated":false}
    error: github.com: pull request #99 of o/r not found (HTTP 404): /pr lists the open ones
    error: param "number" must be a number
    GET https://api.github.com/repos/o/r/pulls?state=open&per_page=100&sort=updated&direction=desc
      Authorization: Bearer ghp_pasted
    GET https://api.github.com/repos/o/r/pulls/7
      Authorization: Bearer ghp_pasted
    GET https://api.github.com/repos/o/r/pulls/7
      Authorization: Bearer ghp_pasted
      Accept: application/vnd.github.diff
    GET https://api.github.com/repos/o/r/commits/abc123/check-runs?per_page=100
      Authorization: Bearer ghp_pasted
    GET https://api.github.com/repos/o/r/commits/abc123/status
      Authorization: Bearer ghp_pasted
    GET https://api.github.com/repos/o/r/pulls/99
      Authorization: Bearer ghp_pasted
    |}];
  call "pr_defaults";
  Fake_forge.create_reply
  := 201, Fake_forge.pr_json ~host:"github.com" ~number:8 ~title:"Add things" ();
  call
    "pr_create"
    ~params:
      {|{"title": " Add things ", "body": "- Add c\n- Add d", "draft": true, "push": true}|};
  print_endline
    (Fake_forge.git_out
       (Filename.concat t.dir "bare.git")
       "log --format=%s feat/add-things");
  call "pr_defaults";
  Fake_forge.create_reply
  := ( 422
     , {|{"message":"Validation Failed","errors":[{"resource":"PullRequest","code":"custom","message":"A pull request already exists for o:feat/add-things."}]}|}
     );
  call "pr_create" ~params:{|{"title": "Again"}|};
  Fake_forge.create_reply
  := ( 422
     , {|{"message":"Validation Failed","errors":[{"resource":"PullRequest","field":"head","code":"invalid"}]}|}
     );
  call
    "pr_create"
    ~params:{|{"title": "Elsewhere", "head": "not-pushed", "base": "dev"}|};
  call "pr_create" ~params:{|{"title": "  "}|};
  Fake_forge.print_requests fake;
  [%expect
    {|
    {"repo":{"host":"github.com","kind":"github","owner":"o","name":"r","remote":"origin","url":"https://github.com/o/r"},"head":"feat/add-things","base":"main","title":"Add things","body":"- Add c\n- Add d","commits":["Add c","Add d"],"pushed":false}
    {"repo":{"host":"github.com","kind":"github","owner":"o","name":"r","remote":"origin","url":"https://github.com/o/r"},"pr":{"number":8,"title":"Add things","state":"open","draft":false,"author":"alice","head":"feat/x","base":"main","url":"https://github.com/o/r/pull/8","body":"Does x.","created_at":"2026-10-20T10:00:00Z","updated_at":"2026-10-21T10:00:00Z"},"diff":"","truncated":false}
    Add d
    Add c
    Initial
    {"repo":{"host":"github.com","kind":"github","owner":"o","name":"r","remote":"origin","url":"https://github.com/o/r"},"head":"feat/add-things","base":"main","title":"Add things","body":"- Add c\n- Add d","commits":["Add c","Add d"],"pushed":true}
    error: github.com refused the pull request feat/add-things → main: Validation Failed: A pull request already exists for o:feat/add-things.
    error: github.com: the branch not-pushed is not on github.com: push it first (the push option, or git push -u origin not-pushed)
    error: a pull request needs a title
    GET https://api.github.com/repos/o/r
      Authorization: Bearer ghp_pasted
    GET https://api.github.com/repos/o/r
      Authorization: Bearer ghp_pasted
    POST https://api.github.com/repos/o/r/pulls
      Authorization: Bearer ghp_pasted
      {"title":"Add things","body":"- Add c\n- Add d","head":"feat/add-things","base":"main","draft":true}
    GET https://api.github.com/repos/o/r
      Authorization: Bearer ghp_pasted
    GET https://api.github.com/repos/o/r
      Authorization: Bearer ghp_pasted
    POST https://api.github.com/repos/o/r/pulls
      Authorization: Bearer ghp_pasted
      {"title":"Again","body":"","head":"feat/add-things","base":"main","draft":false}
    POST https://api.github.com/repos/o/r/pulls
      Authorization: Bearer ghp_pasted
      {"title":"Elsewhere","body":"","head":"not-pushed","base":"dev","draft":false}
    |}];
  call "pr_checkout" ~params:{|{"number": 7}|};
  let worktree = Filename.concat t.dir "worktrees/r/pr-7" in
  print_endline (Fake_forge.git_out worktree "log --format=%s -1");
  call "pr_checkout" ~params:{|{"number": 7}|};
  call "pr_checkout" ~params:{|{"number": 3}|};
  [%expect
    {|
    {"path":"$DIR/worktrees/r/pr-7","branch":"pr-7","created":true}
    From a contributor
    {"path":"$DIR/worktrees/r/pr-7","branch":"pr-7","created":false}
    error: git fetch origin refs/pull/3/head (is #3 a pull request of origin's repository? /pr lists them): fatal: couldn't find remote ref refs/pull/3/head
    |}];
  (* The session records the checkout among its worktrees, once. *)
  call "set_session_name" ~params:{|{"name": "reviewing"}|};
  call "list_sessions";
  [%expect
    {|
    {}
    [{"id":"<id>","path":"$DIR/sessions/<stamp>_<id>.jsonl","name":"reviewing","description":null,"cwd":"$DIR/r","created_at":"<time>","updated_at":"<time>","first_prompt":null,"message_count":0,"parent":null,"host":null,"repo":{"root":"$DIR/r","worktree":"$DIR/r","branch":"feat/add-things","main":true},"worktrees":[{"path":"$DIR/worktrees/r/pr-7","branch":"pr-7","root":"$DIR/r","source":"pull_request","pr":7}],"bytes":<n>,"live":true,"clients":1,"running":false,"background":0,"archived":false}]
    |}];
  Fake_forge.sh repo "git checkout -q --detach";
  call "pr_defaults";
  Fake_forge.sh
    repo
    "git remote remove origin && git remote add upstream /srv/x.git";
  call "pr_list";
  call "set_cwd" ~params:(sprintf {|{"path": %S}|} t.dir);
  call "pr_list";
  call "forge_logout" ~params:{|{"host": "github.com"}|};
  print_endline (Json.to_string (wait_for sent ~kind:"forge_logged_out"));
  call "forge_logout" ~params:{|{"host": "github.com"}|};
  [%expect
    {|
    error: $DIR/r has no current branch (a detached HEAD): git switch -c NAME makes one
    error: no GitHub or Forgejo remote in $DIR/r (git remote -v lists upstream /srv/x.git): add one with git remote add origin URL
    {"cwd":"$DIR","created":false}
    error: not a git repository: $DIR (/cd into one)
    {}
    {"type":"event","event":"auth","kind":"forge_logged_out","host":"github.com"}
    error: not signed in to github.com
    |}]
;;
