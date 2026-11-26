open! Core
open! Expect_test_helpers_core
open Prigh_ui
open Fixtures
module H = Test_app.H

(* /adapters: the composer's agent CLIs on each tool host. *)

let connected () =
  let h = Test_app.connected ~width:90 ~height:20 () in
  H.event h (State (state ()));
  h
;;

let run h text =
  String.iter text ~f:(fun c -> H.step ~quiet:true h (Key (Key.char c)));
  if Option.is_some h.H.model.autocomplete then H.esc h;
  H.enter h
;;

let statuses =
  {|{"adapters":[
     {"adapter":"codex","label":"Codex","host":"h1","host_name":"laptop",
      "installed":true,"version":"0.166.0","path":"/usr/bin/codex",
      "auth":"logged_in","account":"ChatGPT","checked_at":0},
     {"adapter":"claude","label":"Claude Code","host":"h1","host_name":"laptop",
      "installed":true,"version":"2.1.330","path":"/usr/bin/claude",
      "auth":"logged_out",
      "problem":"Claude Code on laptop is not logged in: run `claude auth login` there",
      "checked_at":0},
     {"adapter":"codex","label":"Codex","host":"h2","host_name":"server",
      "installed":false,"auth":"unknown",
      "problem":"Codex is not installed on server, please install it",
      "checked_at":0}],
   "adapter_logins":[{"adapter":"codex","provider":"openai-codex","login":"work","label":"Work","choices":[]}]}|}
;;

let%expect_test "/adapters lists each host's CLIs and what to run" =
  let h = connected () in
  run h "/adapters";
  H.reply h Adapters_shown statuses;
  H.show h;
  run h "/adapters refresh";
  run h "/adapters now";
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_adapters) (params ()) (tag Adapters_shown))
    ▌ earlier question
      earlier answer
    ┌─ Agent CLIs ───────────────────────────────────────────────────────────────────────────┐
    │ laptop:                                                                                │
    │   ✓ Codex  0.166.0 · logged in (ChatGPT)                                               │
    │   ✗ Claude Code  2.1.330 · not logged in                                               │
    │       Claude Code on laptop is not logged in: run `claude auth login` there            │
    │ server:                                                                                │
    │   ✗ Codex  not installed                                                               │
    │       Codex is not installed on server, please install it                              │
    │                                                                                        │
    │ prigh never installs them or logs in: run that on the host                             │
    │                                                                                        │
    │ Logins:                                                                                │
    │   Codex runs on ChatGPT (prigh) · Work  (/adapters login codex [LOGIN|own], /adapters  │
    │ import codex)                                                                          │
    └─────────────────────────────────────────────────────────────────────────── Esc closes ─┘
    ──────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  $0.01  ↑↓ scroll · Esc closes · typing goes to the editor
    (Rpc (method_ list_adapters) (params ((refresh true))) (tag Adapters_shown))









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ────────────────────────────────────────────────────────────────────────────────┐
    │ unknown argument "now": /adapters [refresh] | /adapters login [ADAPTER] [LOGIN|own] |  │
    │ /adapters import ADAPTER [HOST]                                                        │
    └──────────────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ──────────────────────────────────────────────────────────────────────────────────────────
    > /adapters now▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/adapters on a backend without the composer" =
  let h = connected () in
  run h "/adapters";
  H.reply_error
    h
    Adapters_shown
    "this backend does not run the composer engine, so it drives no agent \
     CLIs: start one with prigh serve -engines composer";
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_adapters) (params ()) (tag Adapters_shown))









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ────────────────────────────────────────────────────────────────────────────────┐
    │ this backend does not run the composer engine, so it drives no agent CLIs: start one   │
    │ with prigh serve -engines composer                                                     │
    └──────────────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ──────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let logins ~login =
  sprintf
    {|{"adapter_logins":[
       {"adapter":"codex","provider":"openai-codex","login":%s,"label":%s,
        "choices":[{"ref":"work","label":"Work"},
                   {"ref":"~alice/main","label":"Alice's ChatGPT"}]}]}|}
    (match login with
     | Some _ -> {|"work"|}
     | None -> "null")
    (match login with
     | Some l -> sprintf "%S" l
     | None -> "null")
;;

let%expect_test "/adapters login: Codex on one of prigh's ChatGPT logins" =
  let h = connected () in
  run h "/adapters login";
  H.reply h (Adapter_login_picker None) (logins ~login:None);
  H.show h;
  H.key h (Key.plain Down);
  H.enter h;
  H.reply h (Adapter_login_set "codex") (logins ~login:(Some "Work"));
  H.show h;
  run h "/adapters login codex own";
  run h "/adapters login claude";
  H.reply h (Adapter_login_picker (Some "claude")) (logins ~login:None);
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_adapters) (params ()) (tag (Adapter_login_picker ())))








      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Codex runs on  (3)
    / ▏
    ▸* Codex's own login  on the session's tool host (log in there)
       Work               work, prigh hands it short-lived tokens
       Alice's ChatGPT    ~alice/main, prigh hands it short-lived tokens
    ──────────────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  Enter selects · Esc closes
    (Rpc
      (method_ set_adapter_login)
      (params (
        (adapter codex)
        (login   work)))
      (tag (Adapter_login_set codex)))













      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────── Codex runs on ChatGPT (prigh) · Work from its next turn ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    (Rpc
      (method_ set_adapter_login)
      (params (
        (adapter codex)
        (login   null)))
      (tag (Adapter_login_set codex)))
    (Rpc
      (method_ list_adapters)
      (params ())
      (tag (Adapter_login_picker (claude))))













      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────── claude runs only on its own login; prigh can hand one of its logins to: codex ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "a Codex session on prigh's login says so" =
  let h = connected () in
  H.reply
    ~quiet:true
    h
    Initial_state
    (state_json
       ~model:(model_json ~provider:"openai-codex" "gpt-5.5" "GPT-5.5")
       ~extra:
         [ "engine", {|"composer"|}
         ; "adapter", {|"codex"|}
         ; "adapter_login", {|"Work"|}
         ]
       ());
  run h "/account";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    ─────────────────── Codex runs on prigh's login Work: /adapters login codex changes it ───
    > ▏
    /work  Codex · gpt-5.5@Work  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let claude_logins ~login =
  sprintf
    {|{"adapter_logins":[
       {"adapter":"claude","provider":"anthropic","login":%s,"label":%s,
        "choices":[{"ref":"max","label":"Max"}]}]}|}
    (if login then {|"max"|} else "null")
    (if login then {|"Max"|} else "null")
;;

let%expect_test
    "/adapters login claude: prigh's Claude logins, with Anthropic's terms"
  =
  let h = connected () in
  run h "/adapters login claude";
  H.reply h (Adapter_login_picker (Some "claude")) (claude_logins ~login:false);
  H.show h;
  H.key h (Key.plain Escape);
  run h "/adapters login claude max";
  H.reply h (Adapter_login_set "claude") (claude_logins ~login:true);
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ list_adapters)
      (params ())
      (tag (Adapter_login_picker (claude))))








      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Claude Code runs on  (2)
    / ▏
    ▸* Claude Code's own login  on the session's tool host (log in there)
      ── Anthropic's terms keep Claude Pro/Max logins to Claude's own apps: prigh holding one…
       Max                      max, prigh hands it short-lived tokens
    ──────────────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  Enter selects · Esc closes
    (Rpc
      (method_ set_adapter_login)
      (params (
        (adapter claude)
        (login   max)))
      (tag (Adapter_login_set claude)))








      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ───────────────────────────────────────────────────────────────────────────────┐
    │ Claude Code runs on Claude Pro/Max (prigh) · Max from its next turn (Anthropic's terms │
    │ keep Claude Pro/Max logins to Claude's own apps: prigh holding one for Claude Code may │
    │ breach them)                                                                           │
    └──────────────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ──────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let import_reply ~confirmed =
  sprintf
    {|{"confirmed":%b,"adapter":"claude","provider":"anthropic","host":"laptop","path":"/home/u/.claude/.credentials.json","label":"imported from laptop","warning":"Claude Code on laptop will stop being logged in.","note":null,"adapter_logins":[]}|}
    confirmed
;;

let%expect_test "/adapters import claude laptop: asks first, then moves it" =
  let h = connected () in
  run h "/adapters import claude laptop";
  H.reply h Adapter_import_found (import_reply ~confirmed:false);
  H.show h;
  H.enter h;
  H.reply h Adapter_imported (import_reply ~confirmed:true);
  H.show h;
  run h "/adapters import";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ import_adapter_login)
      (params (
        (adapter claude)
        (host    laptop)))
      (tag Adapter_import_found))









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ──────────────────────────────────────────────────────────────────────────────┐
    │ Import Claude Code's login from laptop? Claude Code on laptop will stop being logged   │
    │ in. Anthropic's terms keep Claude Pro/Max logins to Claude's own apps.                 │
    └────────────────────────────────────────────────────────────────────────────────────────┘
    ──────────────────────────────────────────────────────────────────────────────────────────
    ? ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  confirm: y / n
    (Rpc
      (method_ import_adapter_login)
      (params (
        (adapter claude)
        (host    laptop)
        (confirm true)))
      (tag Adapter_imported))









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ───────────────────────────────────────────────────────────────────────────────┐
    │ Claude Code runs on "imported from laptop" from its next turn: laptop's file is now    │
    │ /home/u/.claude/.credentials.json.moved-to-prigh                                       │
    └──────────────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ──────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01













      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ───── which CLI? /adapters import codex|claude|antigravity [HOST] moves its login here ───
    > /adapters import▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/adapters import antigravity: from the keyring, then cleared" =
  let reply ~confirmed =
    sprintf
      {|{"confirmed":%b,"adapter":"acp:antigravity","provider":"google-antigravity","host":"laptop","path":"keyring","label":"imported from laptop","warning":"Antigravity on laptop will stop being logged in.","note":null,"adapter_logins":[]}|}
      confirmed
  in
  let h = connected () in
  run h "/adapters import antigravity";
  H.reply h Adapter_import_found (reply ~confirmed:false);
  H.enter h;
  H.reply h Adapter_imported (reply ~confirmed:true);
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ import_adapter_login)
      (params ((adapter acp:antigravity)))
      (tag Adapter_import_found))
    (Rpc
      (method_ import_adapter_login)
      (params (
        (adapter acp:antigravity)
        (host    laptop)
        (confirm true)))
      (tag Adapter_imported))









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ───────────────────────────────────────────────────────────────────────────────┐
    │ Antigravity runs on "imported from laptop" from its next turn: its login is cleared    │
    │ from laptop's keyring                                                                  │
    └──────────────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ──────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;
