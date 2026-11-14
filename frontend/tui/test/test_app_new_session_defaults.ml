open! Core
open! Expect_test_helpers_core
open Prigh_ui
module H = Test_app.H
module P = Prigh_protocol

(* [/default-host] and [/default-budget]: the defaults new sessions take
   that had no command. *)

let config fields = sprintf {|{"scoped_models":[]%s}|} fields

let type_quietly h text =
  String.iter text ~f:(fun c -> H.step ~quiet:true h (Key (Key.char c)))
;;

let run h text =
  type_quietly h text;
  H.enter h;
  H.enter h
;;

let with_hosts h =
  H.event
    h
    (State
       (Or_error.ok_exn
          (P.State.of_json
             (Or_error.ok_exn
                (P.Json.parse
                   {|{"session_id":"abc123","session_path":"/s","session_name":null,"cwd":"/work","git_branch":null,"model":{"id":"m","provider":"deepseek","key":"deepseek/m","name":"M","context_window":1000,"max_output":10,"supports_thinking":false,"cost":{"input":1,"output":1,"cache_read":1}},"thinking":"off","running":false,"message_count":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0,"active_host":"backend","hosts":[{"id":"backend","name":"srv","cwd":"/work"},{"id":"host-1","name":"laptop","cwd":"/home/me"}]}|})))))
;;

let%expect_test "parsing the arguments" =
  List.iter [ ""; "laptop"; "off"; "two words" ] ~f:(fun rest ->
    print_s
      [%message
        rest
          ~_:(Default_host_command.parse rest : Default_host_command.Action.t)]);
  List.iter [ ""; "5"; "$2.50"; "off"; "0"; "-1"; "lots" ] ~f:(fun rest ->
    print_s
      [%message
        rest
          ~_:
            (Default_budget_command.parse rest
             : Default_budget_command.Action.t)]);
  [%expect
    {|
    ("" Show)
    (laptop (Set (laptop)))
    (off (Set ()))
    ("two words" (
      Error
      "one host, by its name or id; usage: /default-host or /default-host NAME|off"))
    ("" Show)
    (5 (Set (5)))
    ($2.50 (Set (2.5)))
    (off (Set ()))
    (0 (
      Error
      "\"0\" is not a positive number of dollars (off: no budget); usage: /default-budget or /default-budget DOLLARS|off"))
    (-1 (
      Error
      "\"-1\" is not a positive number of dollars (off: no budget); usage: /default-budget or /default-budget DOLLARS|off"))
    (lots (
      Error
      "\"lots\" is not a positive number of dollars (off: no budget); usage: /default-budget or /default-budget DOLLARS|off"))
    |}]
;;

let%expect_test "/default-host shows, sets by name, and clears" =
  let h = Test_app.connected ~height:14 () in
  with_hosts h;
  H.reply ~quiet:true h Config (config "");
  run h "/default-host";
  H.reply h Default_host_shown (config "");
  H.show h;
  run h "/default-host laptop";
  H.reply
    h
    (Default_host_saved "/default-host laptop")
    (config {|,"default_host":"host-1"|});
  H.show h;
  run h "/default-host off";
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ no default host: new sessions from prigh-web start where │
    │ your session is, a terminal's on its machine;            │
    │ /default-host NAME picks one                             │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    /work  m  think:n/a  mode:auto  ctx:0.0%/1.0k  $0.00
    (Rpc
      (method_ set_config)
      (params ((config ((default_host laptop)))))
      (tag (Default_host_saved "/default-host laptop")))


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ new sessions from prigh-web start on laptop              │
    │ (/default-host off: where your session is; a terminal's  │
    │ start on its machine)                                    │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    /work  m  think:n/a  mode:auto  ctx:0.0%/1.0k  $0.00
    (Rpc
      (method_ set_config)
      (params ((config ((default_host null)))))
      (tag (Default_host_saved "/default-host off")))
    |}]
;;

let%expect_test "/default-budget sets, and a refusal goes back to the editor" =
  let h = Test_app.connected ~height:14 () in
  H.reply ~quiet:true h Config (config "");
  run h "/default-budget 5";
  H.reply
    h
    (Default_budget_saved "/default-budget 5")
    (config {|,"max_budget_usd":5|});
  H.show h;
  run h "/default-budget lots";
  H.show h;
  run h "/default-budget off";
  [%expect
    {|
    (Rpc
      (method_ set_config)
      (params ((config ((max_budget_usd 5)))))
      (tag (Default_budget_saved "/default-budget 5")))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ sessions without their own /budget stop once they cost   │
    │ $5.00 (/default-budget off removes it)                   │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────┐
    │ "lots" is not a positive number of dollars (off: no      │
    │ budget); usage: /default-budget or /default-budget       │
    │ DOLLARS|off                                              │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > /default-budget lots▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;
