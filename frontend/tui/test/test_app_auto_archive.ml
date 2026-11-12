open! Core
open! Expect_test_helpers_core
open Prigh_ui
module H = Test_app.H

(* [/auto-archive]: the config's [archive_after_days] from the terminal. *)

let config days = sprintf {|{"scoped_models":[],"archive_after_days":%s}|} days

let type_quietly h text =
  String.iter text ~f:(fun c -> H.step ~quiet:true h (Key (Key.char c)))
;;

let%expect_test "parsing /auto-archive's argument" =
  List.iter
    [ ""
    ; "7"
    ; "2.5"
    ; "14d"
    ; "3 days"
    ; "1 day"
    ; "off"
    ; "never"
    ; "0"
    ; "366"
    ; "-1"
    ; "soon"
    ]
    ~f:(fun rest ->
      print_s
        [%message
          rest
            ~_:(Auto_archive_command.parse rest : Auto_archive_command.Action.t)]);
  [%expect
    {|
    ("" Show)
    (7 (Set 7))
    (2.5 (Set 2.5))
    (14d (Set 14))
    ("3 days" (Set 3))
    ("1 day" (Set 1))
    (off (Set 0))
    (never (Set 0))
    (0 (Set 0))
    (366 (
      Error
      "\"366\" is not a number of days from 0 to 365 (off: never archive by age); usage: /auto-archive or /auto-archive DAYS|off"))
    (-1 (
      Error
      "\"-1\" is not a number of days from 0 to 365 (off: never archive by age); usage: /auto-archive or /auto-archive DAYS|off"))
    (soon (
      Error
      "\"soon\" is not a number of days from 0 to 365 (off: never archive by age); usage: /auto-archive or /auto-archive DAYS|off"))
    |}]
;;

let%expect_test
    "/auto-archive shows, sets, and a refusal goes back to the editor"
  =
  let h = Test_app.connected ~height:14 () in
  H.reply ~quiet:true h Config (config "7");
  type_quietly h "/auto-archive";
  H.enter h;
  H.enter h;
  H.reply h Auto_archive_shown (config "7");
  H.show h;
  type_quietly h "/auto-archive 3";
  H.enter h;
  H.enter h;
  H.reply h (Auto_archive_saved "/auto-archive 3") (config "3");
  H.show h;
  type_quietly h "/auto-archive soon";
  H.enter h;
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ sessions are archived after 7 days without activity      │
    │ (/auto-archive DAYS|off)                                 │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    (Rpc
      (method_ set_config)
      (params ((config ((archive_after_days 3)))))
      (tag (Auto_archive_saved "/auto-archive 3")))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ sessions are archived after 3 days without activity      │
    │ (/auto-archive DAYS|off)                                 │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────┐
    │ "soon" is not a number of days from 0 to 365 (off: never │
    │ archive by age); usage: /auto-archive or /auto-archive   │
    │ DAYS|off                                                 │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > /auto-archive soon▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;
