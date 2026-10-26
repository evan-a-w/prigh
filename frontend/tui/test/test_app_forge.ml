open! Core
open! Expect_test_helpers_core
open Prigh_ui
open Fixtures
module H = Test_app.H
module P = Prigh_protocol

let run = Test_app.run

let forge_json =
  {|{"accounts":[
  {"host":"github.com","kind":"github","label":"GitHub","user":"octo","method":"token","expires_ms":null,"oauth":false,"oauth_note":"no OAuth client id for github.com: set forges.github.com.client_id in ~/.prigh/config.json or $PRIGH_GITHUB_CLIENT_ID; or sign in with a token"},
  {"host":"codeberg.org","kind":"forgejo","label":"Forgejo","user":null,"method":null,"expires_ms":null,"oauth":true,"oauth_note":null},
  {"host":"git.example.com","kind":"forgejo","label":"Forgejo","user":null,"method":null,"expires_ms":null,"oauth":false,"oauth_note":"no OAuth client id for git.example.com"}]}|}
;;

let connected ?(width = 80) ?(height = 20) () =
  let h = Test_app.connected ~width ~height () in
  H.reply ~quiet:true h Forge_refresh forge_json;
  h
;;

let repo_json =
  {|{"host":"github.com","kind":"github","owner":"evan-a-w","name":"prigh","remote":"origin","url":"https://github.com/evan-a-w/prigh"}|}
;;

let pr_json ?(state = "open") ?(draft = false) ?(body = "") number title head =
  sprintf
    {|{"number":%d,"title":%S,"state":%S,"draft":%b,"author":"lloyd","head":%S,"base":"main","url":"https://github.com/evan-a-w/prigh/pull/%d","body":%S,"created_at":"2026-10-25T10:00:00Z","updated_at":"2026-10-25T11:00:00Z"}|}
    number
    title
    state
    draft
    head
    number
    body
;;

let pr_list_json =
  sprintf
    {|{"repo":%s,"prs":[%s,%s,%s]}|}
    repo_json
    (pr_json ~draft:true 24 "Git hosting" "feat/git-hosting")
    (pr_json 23 "Faster startup" "fix/startup")
    (pr_json ~state:"merged" 20 "Web topbar" "fix/web-topbar")
;;

let pr_detail_json =
  sprintf
    {|{"repo":%s,"pr":%s,"checks":{"state":"failure","items":[{"name":"build","state":"success","url":"https://ci/1"},{"name":"lint","state":"failure","url":null},{"name":"e2e","state":"pending","url":null}]},"diff":"diff --git a/x.ml b/x.ml\nindex 1..2 100644\n--- a/x.ml\n+++ b/x.ml\n@@ -1 +1 @@\n-let x = 1\n+let x = 2\n","truncated":true}|}
    repo_json
    (pr_json
       ~draft:true
       ~body:"Signs in to **GitHub** and Forgejo.\n\n- `/pr` lists them"
       24
       "Git hosting"
       "feat/git-hosting")
;;

let%expect_test "/pr's arguments" =
  List.iter
    [ ""
    ; "list"
    ; "list closed"
    ; "all"
    ; "list mine"
    ; "24"
    ; "#24"
    ; "view 24"
    ; "view #7"
    ; "view"
    ; "view x"
    ; "view 1 2"
    ; "new"
    ; "new  Add the thing "
    ; "checkout 7"
    ; "checkout"
    ; "merge 3"
    ]
    ~f:(fun rest ->
      print_s
        [%sexp
          (rest : string)
        , (Pr_command.parse rest : (Pr_command.t, string) Result.t)]);
  [%expect
    {|
    ("" (Ok (List (state ()))))
    (list (Ok (List (state ()))))
    ("list closed" (Ok (List (state (closed)))))
    (all (Ok (List (state (all)))))
    ("list mine" (Error "usage: /pr list [open|closed|all], not /pr list mine"))
    (24 (Ok (View 24)))
    (#24 (Ok (View 24)))
    ("view 24" (Ok (View 24)))
    ("view #7" (Ok (View 7)))
    (view (Error "/pr view needs a number: /pr view 24 (/pr lists them)"))
    ("view x" (
      Error
      "not a pull request number: x; /pr view needs a number: /pr view 24 (/pr lists them)"))
    ("view 1 2" (Error "usage: /pr view N, e.g. /pr view 24"))
    (new (Ok (New (title ()) (generate false))))
    ("new  Add the thing " (Ok (New (title ("Add the thing")) (generate false))))
    ("checkout 7" (Ok (Checkout 7)))
    (checkout (
      Error "/pr checkout needs a number: /pr checkout 24 (/pr lists them)"))
    ("merge 3" (
      Error
      "unknown /pr subcommand \"merge\": use list [open|closed|all], view N, new [--generate] [title] or checkout N"))
    |}]
;;

let%expect_test "/login's git-host arguments" =
  let accounts =
    Or_error.ok_exn
      (Or_error.bind (P.Json.parse forge_json) ~f:P.Forge_account.list_of_json)
  in
  List.iter
    [ [ "github" ]
    ; [ "github"; "token" ]
    ; [ "github"; "ghe.example.com" ]
    ; [ "github"; "ghe.example.com"; "token" ]
    ; [ "github"; "a"; "b" ]
    ; [ "forgejo" ]
    ; [ "forgejo"; "token" ]
    ; [ "gitea"; "gitea.example.com" ]
    ; [ "forgejo"; "codeberg.org"; "oauth" ]
    ; [ "codeberg.org" ]
    ; [ "git.example.com"; "token" ]
    ; [ "new.example.org" ]
    ; [ "codeberg.org"; "please" ]
    ; [ "anthropic"; "api_key" ]
    ]
    ~f:(fun args ->
      print_s
        [%sexp
          (String.concat ~sep:" " args : string)
        , (Forge_accounts.Login.parse accounts args
           : (Forge_accounts.Login.t, string) Result.t option)]);
  print_s
    [%sexp
      (List.map [ "github"; "codeberg.org"; "x.org"; "anthropic" ] ~f:(fun w ->
         w, Forge_accounts.logout_host accounts w)
       : (string * string option) list)];
  [%expect
    {|
    (github ((Ok (Host (host github.com) (kind (github)) (method_ ())))))
    ("github token" ((
      Ok (
        Host
        (host github.com)
        (kind    (github))
        (method_ (token))))))
    ("github ghe.example.com" ((
      Ok (Host (host ghe.example.com) (kind (github)) (method_ ())))))
    ("github ghe.example.com token" ((
      Ok (
        Host
        (host ghe.example.com)
        (kind    (github))
        (method_ (token))))))
    ("github a b" ((
      Error "usage: /login github [HOST] [token], e.g. /login github token")))
    (forgejo ((Ok (Ask_host (kind forgejo) (method_ ())))))
    ("forgejo token" ((Ok (Ask_host (kind forgejo) (method_ (token))))))
    ("gitea gitea.example.com" ((
      Ok (Host (host gitea.example.com) (kind (forgejo)) (method_ ())))))
    ("forgejo codeberg.org oauth" ((
      Ok (
        Host
        (host codeberg.org)
        (kind    (forgejo))
        (method_ (oauth))))))
    (codeberg.org ((Ok (Host (host codeberg.org) (kind (forgejo)) (method_ ())))))
    ("git.example.com token" ((
      Ok (
        Host
        (host git.example.com)
        (kind    (forgejo))
        (method_ (token))))))
    (new.example.org ((
      Ok (
        Host
        (host new.example.org)
        (kind    ())
        (method_ ())))))
    ("codeberg.org please" ((Error "usage: /login codeberg.org [token]")))
    ("anthropic api_key" ())
    ((github       (github.com))
     (codeberg.org (codeberg.org))
     (x.org        (x.org))
     (anthropic ()))
    |}]
;;

let%expect_test "the git hosts: fetched at start, listed by /auth" =
  let h = H.create () in
  H.step h Start;
  [%expect
    {|
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ auth_status) (params ()) (tag Auth_refresh))
    (Rpc (method_ forge_status) (params ()) (tag Forge_refresh))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ list_models) (params ()) (tag Models_catalog))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    Load_history
    (Rpc (method_ get_prefs) (params ((keys (history)))) (tag Prefs_history))
    |}];
  let h = connected ~height:24 () in
  run h "/auth";
  [%expect
    {|
    (Rpc (method_ auth_status) (params ()) (tag Auth_show))
    (Rpc (method_ forge_status) (params ()) (tag Forge_refresh))
    |}];
  H.reply h Auth_show auth_json;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Logins ─────────────────────────────────────────────────────────────────────┐
    │ anthropic  not configured  [oauth (Claude Pro/Max), api_key (API key)]       │
    │ openai     not configured  [api_key (API key)]                               │
    │ deepseek   logged in via auth.json  [api_key (API key)]                      │
    │ /login custom adds an OpenAI-compatible endpoint                             │
    │                                                                              │
    │ Git hosting                                                                  │
    │ github.com       GitHub   signed in as octo (token)                          │
    │   no OAuth client id for github.com: set forges.github.com.client_id in      │
    │ ~/.prigh/config.json or $PRIGH_GITHUB_CLIENT_ID; or sign in with a token     │
    │ codeberg.org     Forgejo  not signed in                                      │
    │ git.example.com  Forgejo  not signed in                                      │
    │   no OAuth client id for git.example.com                                     │
    │ /login github or /login forgejo HOST signs in; /logout HOST signs out        │
    └───────────────────────────────────────────────────────────────── Esc closes ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  ↑↓ scroll · Esc closes · typing goes to the editor
    |}];
  (* A backend without git hosting answers with an error: nothing is shown. *)
  H.reply_error h Forge_refresh "unknown method forge_status";
  H.mode h;
  [%expect {| output |}]
;;

let%expect_test
    "/login picker: the git hosts with model providers; a Forgejo OAuth flow; \
     done refreshes the hosts"
  =
  let h = connected ~height:24 () in
  run h "/login";
  [%expect
    {|
    (Rpc (method_ auth_status) (params ()) (tag Auth_login_picker))
    (Rpc (method_ forge_status) (params ()) (tag Forge_refresh))
    |}];
  H.reply h Auth_login_picker auth_json;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Log in to  (10)
    / ▏
       Anthropic (oauth)                         Claude Pro/Max
       Anthropic (api_key)                       API key
       OpenAI (api_key)                          API key
    ▸* DeepSeek (api_key)                        API key  logged in via auth.json
       custom (add an OpenAI-compatible endpoint)  aiproxy, LiteLLM, OpenRouter, vL…
      ── Git hosting ──
     * GitHub (github.com)                       signed in as octo (token)
       Forgejo (codeberg.org)                    OAuth in the browser
       Forgejo (codeberg.org) with a token       paste a token
       Forgejo (git.example.com)                 paste a token
       Forgejo: another host…                    Forgejo or Gitea on any host: asks…
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  H.keys h "codeberg";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Log in to  (2)
    / codeberg▏
      ── Git hosting ──
    ▸  Forgejo (codeberg.org)               OAuth in the browser
       Forgejo (codeberg.org) with a token  paste a token
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  H.enter h;
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ forge_login)
      (params (
        (host codeberg.org)
        (kind forgejo)))
      (tag Show_error))

















      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────────────── signing in to codeberg.org… ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  H.event
    h
    (Auth
       (Auth_url
          { url = "https://codeberg.org/login/oauth/authorize?client_id=x"
          ; instructions = "Approve prigh in the browser."
          }));
  H.event
    h
    (Auth
       (Prompt
          { id = "p1"
          ; prompt =
              Manual_code
                { message =
                    "If the browser cannot reach this machine, paste the URL \
                     it was sent to"
                ; placeholder = "http://127.0.0.1:1455/callback"
                }
          }));
  H.show h;
  [%expect
    {|
    (Open_browser https://codeberg.org/login/oauth/authorize?client_id=x)










      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Log in ─────────────────────────────────────────────────────────────────────┐
    │ Open this URL to log in:                                                     │
    │   https://codeberg.org/login/oauth/authorize?client_id=x                     │
    │ Approve prigh in the browser.                                                │
    │ If the browser cannot reach this machine, paste the URL it was sent to       │
    │ e.g. http://127.0.0.1:1455/callback?code=...                                 │
    └──────────────────────────────────────────────────────────────────────────────┘
    ──────────────────────────────────────────────── signing in to codeberg.org… ───
    ? ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  login: Enter answers · Esc cancels
    |}];
  H.event
    h
    (Auth
       (Forge_done
          { host = "codeberg.org"; user = Some "lloyd"; method_ = "oauth" }));
  H.show h;
  [%expect
    {|
    (Rpc (method_ forge_status) (params ()) (tag Forge_refresh))

















      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ───────────────────────────────── signed in to codeberg.org as lloyd (oauth) ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test
    "/login picker: the hosts arriving after the providers update it; a token \
     login; another Forgejo host"
  =
  let h = Test_app.connected ~width:80 ~height:24 () in
  run h "/login";
  H.reply h Auth_login_picker auth_json;
  H.reply h Forge_refresh forge_json;
  H.keys h "git.ex";
  H.show h;
  [%expect
    {|
    (Rpc (method_ auth_status) (params ()) (tag Auth_login_picker))
    (Rpc (method_ forge_status) (params ()) (tag Forge_refresh))













      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Log in to  (1)
    / git.ex▏
      ── Git hosting ──
    ▸  Forgejo (git.example.com)  paste a token
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ forge_login)
      (params (
        (host git.example.com)
        (kind forgejo)))
      (tag Show_error))
    |}];
  H.event
    h
    (Auth
       (Prompt
          { id = "p2"
          ; prompt =
              Secret
                { message = "Paste a token for git.example.com (scopes: repo)"
                ; allow_empty = false
                }
          }));
  H.keys h "tok";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Log in ─────────────────────────────────────────────────────────────────────┐
    │ Paste a token for git.example.com (scopes: repo)                             │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    ? ***▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  login: Enter answers · Esc cancels
    |}];
  H.enter h;
  H.event
    h
    (Auth
       (Failed { provider = "git.example.com"; error = "bad token (HTTP 401)" }));
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ auth_respond)
      (params (
        (id    p2)
        (value tok)))
      (tag Show_error))

















      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────── login to git.example.com failed: bad token (HTTP 401) ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  run h "/login";
  H.reply ~quiet:true h Auth_login_picker auth_json;
  H.keys h "other host";
  H.enter h;
  H.show h;
  [%expect
    {|
    (Rpc (method_ auth_status) (params ()) (tag Auth_login_picker))
    (Rpc (method_ forge_status) (params ()) (tag Forge_refresh))















      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Forgejo or Gitea host (e.g. codeberg.org, git.example.com) ─────────────────┐
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  Enter submits · Esc cancels
    |}];
  H.esc h;
  H.mode h;
  [%expect {| editing |}];
  run h "/login forgejo";
  H.keys h "git.corp.example";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ forge_login)
      (params (
        (host git.corp.example)
        (kind forgejo)))
      (tag Show_error))
    |}]
;;

let%expect_test "/login with git-host arguments: completed, then sent as \
                 forge_login; GitHub's device code"
  =
  let h = connected () in
  H.keys h "/login g";
  H.show h;
  [%expect {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ git.example.com  Forgejo · not signed in
      github           GitHub (github.com); github HOST for GitHub Enterprise
      codeberg.org     Forgejo · not signed in
      forgejo          forgejo HOST: Forgejo or Gitea, e.g. codeberg.org
    ────────────────────────────────────────────────────────────────────────────────
    > /login g▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  Tab/Enter accept · Alt+S as typed
    |}];
  H.step ~quiet:true h (Intent Kill_to_start);
  H.step ~quiet:true h (Intent Cancel);
  run h "/login github";
  run h "/login github token";
  run h "/login github ghe.example.com";
  run h "/login forgejo codeberg.org token";
  run h "/login git.example.com";
  run h "/login anthropic api_key";
  [%expect
    {|
    (Rpc
      (method_ forge_login)
      (params (
        (host github.com)
        (kind github)))
      (tag Show_error))
    (Rpc
      (method_ forge_login)
      (params (
        (host   github.com)
        (kind   github)
        (method token)))
      (tag Show_error))
    (Rpc
      (method_ forge_login)
      (params (
        (host ghe.example.com)
        (kind github)))
      (tag Show_error))
    (Rpc
      (method_ forge_login)
      (params (
        (host   codeberg.org)
        (kind   forgejo)
        (method token)))
      (tag Show_error))
    (Rpc
      (method_ forge_login)
      (params (
        (host git.example.com)
        (kind forgejo)))
      (tag Show_error))
    (Rpc
      (method_ login)
      (params (
        (provider anthropic)
        (method   api_key)))
      (tag Show_error))
    |}];
  run h "/login github a b";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────── usage: /login github [HOST] [token], e.g. /login github token ───
    > /login github a b▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  H.step ~quiet:true h (Intent Kill_to_start);
  run h "/login github";
  H.event
    h
    (Auth
       (Auth_url
          { url = "https://github.com/login/device"
          ; instructions =
              "Enter the code ABCD-1234 at https://github.com/login/device"
          }));
  H.event h (Auth (Progress "waiting for GitHub…"));
  H.event
    h
    (Auth
       (Prompt
          { id = "p3"
          ; prompt =
              Secret
                { message = "Or paste a token instead"; allow_empty = false }
          }));
  H.show h;
  [%expect {|
    (Rpc
      (method_ forge_login)
      (params (
        (host github.com)
        (kind github)))
      (tag Show_error))
    (Open_browser https://github.com/login/device)






      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Log in ─────────────────────────────────────────────────────────────────────┐
    │ Open this URL to log in:                                                     │
    │   https://github.com/login/device                                            │
    │ Enter the code ABCD-1234 at https://github.com/login/device                  │
    │ waiting for GitHub…                                                          │
    │ Or paste a token instead                                                     │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────── signing in to github.com… ───
    ? ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  login: Enter answers · Esc cancels
    |}]
;;

let%expect_test
    "/logout: signed-in hosts in the picker, a confirmation, then the hosts \
     are refreshed"
  =
  let h = connected ~height:16 () in
  run h "/logout";
  [%expect
    {|
    (Rpc (method_ auth_status) (params ()) (tag Auth_logout_picker))
    (Rpc (method_ forge_status) (params ()) (tag Forge_refresh))
    |}];
  H.reply h Auth_logout_picker auth_json;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Log out of  (2)
    / ▏
    ▸  DeepSeek             api_key via auth.json
      ── Git hosting ──
       GitHub (github.com)  signed in as octo (token)
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  H.keys h "github";
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────────────────────────────────────────────┐
    │ Sign out of github.com (octo) and delete its token? (y/n)                    │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  confirm: y / n
    |}];
  H.esc h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────────── cancelled ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  run h "/logout github";
  H.keys h "y";
  [%expect
    {| (Rpc (method_ forge_logout) (params ((host github.com))) (tag Show_error)) |}];
  H.event h (Auth (Forge_logged_out "github.com"));
  H.show h;
  [%expect
    {|
    (Rpc (method_ forge_status) (params ()) (tag Forge_refresh))









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ─────────────────────────────────────────────────── signed out of github.com ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  run h "/logout codeberg.org";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────── not signed in to codeberg.org; /login forgejo codeberg.org signs in ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test
    "/pr: a picker of pull requests; Enter shows one with its checks and diff"
  =
  let h = connected ~height:30 () in
  run h "/pr";
  H.show h;
  [%expect
    {|
    (Rpc (method_ pr_list) (params ()) (tag (Prs_picker ())))























      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ───────────────────────────────────────────────────── reading pull requests… ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  H.reply h (Prs_picker None) pr_list_json;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Pull requests in evan-a-w/prigh (open)  (3)
    / ▏
    ▸  #24 Git hosting     lloyd · feat/git-hosting → main · draft
       #23 Faster startup  lloyd · fix/startup → main
       #20 Web topbar      lloyd · fix/web-topbar → main · merged
    ───────────────────────────────────────────────────── reading pull requests… ───
    >
    deepseek-flash  think:off  Enter shows it, its checks and its diff · Esc closes
    |}];
  H.keys h "start";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Pull requests in evan-a-w/prigh (open)  (1)
    / start▏
    ▸  #23 Faster startup  lloyd · fix/startup → main
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  Enter shows it, its checks and its diff · Esc closes
    |}];
  H.key h (Key.plain Backspace);
  H.key h (Key.plain Backspace);
  H.key h (Key.plain Backspace);
  H.key h (Key.plain Backspace);
  H.key h (Key.plain Backspace);
  H.enter h;
  [%expect {| (Rpc (method_ pr_view) (params ((number 24))) (tag Pr_shown)) |}];
  H.reply h Pr_shown pr_detail_json;
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer
    ┌─ Pull request #24 ───────────────────────────────────────────────────────────┐
    │ #24 Git hosting                                                              │
    │ open · lloyd · feat/git-hosting → main · draft                               │
    │ https://github.com/evan-a-w/prigh/pull/24                                    │
    │                                                                              │
    │ Signs in to GitHub and Forgejo.                                              │
    │                                                                              │
    │ • /pr lists them                                                             │
    │                                                                              │
    │ checks ✗ failure                                                             │
    │   ✓ build  success                                                           │
    │   ✗ lint   failure                                                           │
    │   ● e2e    pending                                                           │
    │                                                                              │
    │ diff evan-a-w/prigh against main  1 file changed, +1 −1                      │
    │ x.ml  +1 −1                                                                  │
    │   @@ -1 +1 @@                                                                │
    │   -let x = 1                                                                 │
    │   +let x = 2                                                                 │
    │ the diff is cut short: the rest is at                                        │
    │ https://github.com/evan-a-w/prigh/pull/24                                    │
    │                                                                              │
    │ /pr checkout 24 checks it out · /pr lists the others                         │
    └───────────────────────────────────────────────────────────────── Esc closes ─┘
    ────────────────────────────────────────────────── reading pull request #24… ───
    > ▏
    deepseek-flash  think:off  ↑↓ scroll · Esc closes · typing goes to the editor
    |}];
  print_endline
    (Content.to_styled
       (Pr_view.render
          (Or_error.ok_exn
             (Or_error.bind
                (P.Json.parse pr_detail_json)
                ~f:P.Pr_detail.of_json))));
  [%expect
    {|
    [bold]#24 Git hosting[/]
    [green]open[/] · lloyd · feat/git-hosting → main · draft
    [dim][link=https://github.com/evan-a-w/prigh/pull/24]https://github.com/evan-a-w/prigh/pull/24[/]

    Signs in to [bold]GitHub[/] and Forgejo.

    • [cyan]/pr[/] lists them

    [bold]checks [/][red]✗ failure[/]
    [green]  ✓ [/]build[dim]  success[/]
    [red]  ✗ [/]lint [dim]  failure[/]
    [yellow]  ● [/]e2e  [dim]  pending[/]

    [bold]diff [/]evan-a-w/prigh[dim] against main[/][gray]  1 file changed, +1 −1[/]
    [bold]x.ml[/][dim]  +1 −1[/]
    [cyan]  @@ -1 +1 @@[/]
    [red]  -let x = 1[/]
    [green]  +let x = 2[/]
    [yellow]the diff is cut short: the rest is at https://github.com/evan-a-w/prigh/pull/24[/]

    [dim]/pr checkout 24 checks it out · /pr lists the others[/]
    |}]
;;

let%expect_test "/pr list states, view, an empty list, errors" =
  let h = connected () in
  run h "/pr list closed";
  run h "/pr all";
  run h "/pr #24";
  run h "/pr view 7";
  [%expect
    {|
    (Rpc (method_ pr_list) (params ((state closed))) (tag (Prs_picker (closed))))
    (Rpc (method_ pr_list) (params ((state all))) (tag (Prs_picker (all))))
    (Rpc (method_ pr_view) (params ((number 24))) (tag Pr_shown))
    (Rpc (method_ pr_view) (params ((number 7))) (tag Pr_shown))
    |}];
  H.reply
    h
    (Prs_picker (Some "closed"))
    (sprintf {|{"repo":%s,"prs":[]}|} repo_json);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────────────────────────┐
    │ no closed pull requests in evan-a-w/prigh; /pr new creates one, /pr list all │
    │ lists every one                                                              │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  run h "/pr view";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ view  N: show a pull request, its checks and its diff
    ────────────────────── /pr view needs a number: /pr view 24 (/pr lists them) ───
    > /pr view▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  Tab/Enter accept · Alt+S as typed
    |}];
  H.step ~quiet:true h (Intent Kill_to_start);
  run h "/pr";
  H.reply_error
    h
    (Prs_picker None)
    "not signed in to github.com: /login github signs in";
  H.show h;
  [%expect
    {|
    (Rpc (method_ pr_list) (params ()) (tag (Prs_picker ())))













      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────── not signed in to github.com: /login github signs in ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let defaults_json ~pushed =
  sprintf
    {|{"repo":%s,"head":"feat/x","base":"main","title":"Add x","body":"- Add x\n- Test x","commits":["Add x","Test x"],"pushed":%b}|}
    repo_json
    pushed
;;

let%expect_test
    "/pr new: the form, prefilled; editing, toggles, a refusal, then created"
  =
  let h = connected ~height:20 () in
  run h "/pr new";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ pr_defaults)
      (params ())
      (tag (Pr_form_defaults (title ()) (generate false))))













      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────────────────────── reading the branch… ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  H.reply
    h
    (Pr_form_defaults { title = None; generate = false })
    (defaults_json ~pushed:false);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ New pull request in evan-a-w/prigh ─────────────────────────────────────────┐
    │ title  Add x▏                                                                │
    │ body   - Add x                                                               │
    │        - Test x                                                              │
    │ base   main                                                                  │
    │ [ ] draft                                                                    │
    │ [x] push feat/x first  (not on the remote yet)                               │
    │ feat/x → main · 2 commits                                                    │
    │ Tab/↑↓ fields · Space toggles · Ctrl+J new line in the body · Ctrl+G drafts  │
    │ it with the model · Enter creates · Esc cancels                              │
    └──────────────────────────────────────────────────────────────────────────────┘
    ──────────────────────────────────────────────────────── reading the branch… ───
    >
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  Enter creates · Esc cancels
    |}];
  H.step ~quiet:true h (Intent Kill_word);
  H.keys h "the x";
  H.key h (Key.plain Tab);
  H.step ~quiet:true h (Intent Newline);
  H.keys h "Closes #3";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ New pull request in evan-a-w/prigh ─────────────────────────────────────────┐
    │ title  Add the x                                                             │
    │ body   - Add x                                                               │
    │        - Test x                                                              │
    │        Closes #3▏                                                            │
    │ base   main                                                                  │
    │ [ ] draft                                                                    │
    │ [x] push feat/x first  (not on the remote yet)                               │
    │ feat/x → main · 2 commits                                                    │
    │ Tab/↑↓ fields · Space toggles · Ctrl+J new line in the body · Ctrl+G drafts  │
    │ it with the model · Enter creates · Esc cancels                              │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  Enter creates · Esc cancels
    |}];
  H.key h (Key.plain Tab);
  H.key h (Key.plain Tab);
  H.keys h " ";
  H.key h (Key.plain Down);
  H.keys h " ";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ New pull request in evan-a-w/prigh ─────────────────────────────────────────┐
    │ title  Add the x                                                             │
    │ body   - Add x                                                               │
    │        - Test x                                                              │
    │        Closes #3                                                             │
    │ base   main                                                                  │
    │ [x] draft                                                                    │
    │ [ ]▏push feat/x first  (not on the remote yet)                               │
    │ feat/x → main · 2 commits                                                    │
    │ Tab/↑↓ fields · Space toggles · Ctrl+J new line in the body · Ctrl+G drafts  │
    │ it with the model · Enter creates · Esc cancels                              │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  Enter creates · Esc cancels
    |}];
  H.enter h;
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ pr_create)
      (params (
        (title "Add the x")
        (body  "- Add x\n- Test x\nCloses #3")
        (base  main)
        (head  feat/x)
        (draft true)
        (push  false)))
      (tag Pr_created))
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ New pull request in evan-a-w/prigh ─────────────────────────────────────────┐
    │ title  Add the x                                                             │
    │ body   - Add x                                                               │
    │        - Test x                                                              │
    │        Closes #3                                                             │
    │ base   main                                                                  │
    │ [x] draft                                                                    │
    │ [ ]▏push feat/x first  (not on the remote yet)                               │
    │ feat/x → main · 2 commits                                                    │
    │ creating…                                                                    │
    │ Tab/↑↓ fields · Space toggles · Ctrl+J new line in the body · Ctrl+G drafts  │
    │ it with the model · Enter creates · Esc cancels                              │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  Enter creates · Esc cancels
    |}];
  H.reply_error
    h
    Pr_created
    "a pull request for feat/x already exists: #24 (/pr view 24)";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ New pull request in evan-a-w/prigh ─────────────────────────────────────────┐
    │ title  Add the x                                                             │
    │ body   - Add x                                                               │
    │        - Test x                                                              │
    │        Closes #3                                                             │
    │ base   main                                                                  │
    │ [x] draft                                                                    │
    │ [ ]▏push feat/x first  (not on the remote yet)                               │
    │ feat/x → main · 2 commits                                                    │
    │ a pull request for feat/x already exists: #24 (/pr view 24)                  │
    │ Tab/↑↓ fields · Space toggles · Ctrl+J new line in the body · Ctrl+G drafts  │
    │ it with the model · Enter creates · Esc cancels                              │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  Enter creates · Esc cancels
    |}];
  H.key h { (Key.plain Tab) with shift = true };
  H.key h { (Key.plain Tab) with shift = true };
  H.step ~quiet:true h (Intent Kill_to_start);
  H.keys h "dev";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ pr_create)
      (params (
        (title "Add the x")
        (body  "- Add x\n- Test x\nCloses #3")
        (base  dev)
        (head  feat/x)
        (draft true)
        (push  false)))
      (tag Pr_created))
    |}];
  H.reply
    h
    Pr_created
    (sprintf
       {|{"repo":%s,"pr":%s}|}
       repo_json
       (pr_json 25 "Add the x" "feat/x"));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ───────────────────── #25 created: https://github.com/evan-a-w/prigh/pull/25 ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test
    "/pr new [title]: the title typed; an empty title is refused; Esc closes \
     without creating"
  =
  let h = connected ~width:60 ~height:18 () in
  run h "/pr new Make it faster";
  H.reply
    h
    (Pr_form_defaults { title = Some "Make it faster"; generate = false })
    (defaults_json ~pushed:true);
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ pr_defaults)
      (params ())
      (tag (Pr_form_defaults (title ("Make it faster")) (generate false))))

    ▌ earlier question
      earlier answer
    ┌─ New pull request in evan-a-w/prigh ─────────────────────┐
    │ title  Make it faster▏                                   │
    │ body   - Add x                                           │
    │        - Test x                                          │
    │ base   main                                              │
    │ [ ] draft                                                │
    │ [ ] push feat/x first  (it is on the remote)             │
    │ feat/x → main · 2 commits                                │
    │ Tab/↑↓ fields · Space toggles · Ctrl+J new line in the   │
    │ body · Ctrl+G drafts it with the model · Enter creates · │
    │ Esc cancels                                              │
    └──────────────────────────────────────────────────────────┘
    ──────────────────────────────────── reading the branch… ───
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter creates · Esc cancels
    |}];
  H.step ~quiet:true h (Intent Kill_to_start);
  H.enter h;
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer
    ┌─ New pull request in evan-a-w/prigh ─────────────────────┐
    │ title  ▏                                                 │
    │ body   - Add x                                           │
    │        - Test x                                          │
    │ base   main                                              │
    │ [ ] draft                                                │
    │ [ ] push feat/x first  (it is on the remote)             │
    │ feat/x → main · 2 commits                                │
    │ type a title                                             │
    │ Tab/↑↓ fields · Space toggles · Ctrl+J new line in the   │
    │ body · Ctrl+G drafts it with the model · Enter creates · │
    │ Esc cancels                                              │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter creates · Esc cancels
    |}];
  H.esc h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────── no pull request created ───
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/pr checkout: offers to change the directory there" =
  let h = connected () in
  run h "/pr checkout 24";
  [%expect
    {| (Rpc (method_ pr_checkout) (params ((number 24))) (tag (Pr_checked_out 24))) |}];
  let checkout =
    {|{"path":"/src/prigh-wt/pr-24","branch":"pr-24","created":true}|}
  in
  H.reply h (Pr_checked_out 24) checkout;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────────────────────────────────────────────┐
    │ Change the session's directory to /src/prigh-wt/pr-24? (y/n)                 │
    └──────────────────────────────────────────────────────────────────────────────┘
    ──────────────── checked out pull request #24 (pr-24) at /src/prigh-wt/pr-24 ───
    ? ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  confirm: y / n
    |}];
  H.keys h "y";
  [%expect
    {|
    (Rpc
      (method_ set_cwd)
      (params (
        (path     /src/prigh-wt/pr-24)
        (worktree true)))
      (tag (Dir_changed Cwd)))
    |}];
  H.reply h (Pr_checked_out 24) checkout;
  H.keys h "n";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ─────────────────────────── staying here; /cd /src/prigh-wt/pr-24 goes there ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  run h "/pr checkout";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ checkout  N: check a pull request out in a git worktree
    ────────────── /pr checkout needs a number: /pr checkout 24 (/pr lists them) ───
    > /pr checkout▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  Tab/Enter accept · Alt+S as typed
    |}]
;;

let%expect_test "switching sessions closes the pull request views" =
  let h = connected () in
  run h "/pr";
  H.reply ~quiet:true h (Prs_picker None) pr_list_json;
  H.mode h;
  H.reply ~quiet:true h Reload_messages {|null|};
  H.mode h;
  run h "/pr new";
  H.reply
    ~quiet:true
    h
    (Pr_form_defaults { title = None; generate = false })
    (defaults_json ~pushed:true);
  H.mode h;
  H.reply ~quiet:true h Reload_messages {|null|};
  H.mode h;
  [%expect
    {|
    (Rpc (method_ pr_list) (params ()) (tag (Prs_picker ())))
    picker
    editing
    (Rpc
      (method_ pr_defaults)
      (params ())
      (tag (Pr_form_defaults (title ()) (generate false))))
    pr_form
    editing
    |}]
;;
