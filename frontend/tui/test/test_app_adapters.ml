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
      "checked_at":0}]}|}
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


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

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
    └─────────────────────────────────────────────────────────────────────────── Esc closes ─┘
    ──────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  $0.01  ↑↓ scroll · Esc closes · typing goes to the editor
    (Rpc (method_ list_adapters) (params ((refresh true))) (tag Adapters_shown))













      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────── unknown argument "now": /adapters [refresh] ───
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
