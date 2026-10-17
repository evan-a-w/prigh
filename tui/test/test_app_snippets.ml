open! Core
open! Expect_test_helpers_core
open Prigh_ui
module H = Test_app.H

let config_json =
  {|{"snippets":[{"name":"review","text":"Review the diff for bugs.\nList them by severity."},{"name":"tests","text":"Run the tests and fix failures."},{"name":"plan","text":"Plan first; change nothing yet."}]}|}
;;

let connected ?(config = config_json) () =
  let h = Test_app.connected ~height:14 () in
  H.reply ~quiet:true h Config config;
  h
;;

(* Alt+S: as typed, whatever the completion popup offers. *)
let command h text =
  H.keys h text;
  H.key h (Key.alt (Char "s"))
;;

let%expect_test
    "/snippet: a fuzzy picker; Esc closes, Enter inserts without sending"
  =
  let h = connected () in
  H.keys h "/snippet";
  H.enter h;
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Snippets  (3)
    / ▏
    ▸  review  Review the diff for bugs.
       tests   Run the tests and fix failures.
       plan    Plan first; change nothing yet.
    ────────────────────────────────────────────────────────────
    …deepseek-flash  ctx:0.1%/1.0M  Enter inserts the text at t…
    |}];
  H.keys h "tst";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Snippets  (2)
    / tst▏
    ▸  tests   Run the tests and fix failures.
       review  Review the diff for bugs.
    ────────────────────────────────────────────────────────────
    …deepseek-flash  ctx:0.1%/1.0M  Enter inserts the text at t…
    |}];
  H.esc h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.keys h "/snippet";
  H.enter h;
  H.enter h;
  H.keys h "bugs";
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > Review the diff for bugs.
      List them by severity.▏
    …deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/snippet NAME inserts; an unknown name says the closest" =
  let h = connected () in
  command h "/snippet tests";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > Run the tests and fix failures.▏
    …deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Key.ctrl 'u');
  command h "/snippet revew";
  command h "/snippet zzz";
  command h "/snippet review extra words";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      unknown snippet "revew"; did you mean review? (/snippet
      lists them)
      unknown snippet "zzz"; /snippet lists the 3 saved
      usage: /snippet [NAME] · /snippet add NAME [TEXT] ·
      /snippet rm NAME
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/snippet with none saved, or before the config loads" =
  let h = Test_app.connected () in
  command h "/snippet";
  command h "/snippet add x y";
  H.reply ~quiet:true h Config {|{}|};
  command h "/snippet";
  command h "/snippet foo";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      the config has not loaded yet; try again in a moment
      the config has not loaded yet; try again in a moment
      no snippets yet: /snippet add NAME TEXT saves one
      unknown snippet "foo"; no snippets yet: /snippet add NAME
      TEXT saves one
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/snippet add and rm save through set_config, acknowledged" =
  let h = connected () in
  command h "/snippet add fix Fix the failing test.";
  [%expect
    {|
    (Rpc
      (method_ set_config)
      (params ((
        config ((
          snippets (
            ((name review)
             (text "Review the diff for bugs.\nList them by severity."))
            ((name tests) (text "Run the tests and fix failures."))
            ((name plan)  (text "Plan first; change nothing yet."))
            ((name fix)   (text "Fix the failing test."))))))))
      (tag (
        Snippets_saved
        "snippet fix saved: Fix the failing test. (/snippet fix inserts it)")))
    |}];
  H.reply
    h
    (Snippets_saved
       "snippet fix saved: Fix the failing test. (/snippet fix inserts it)")
    {|{"snippets":[{"name":"fix","text":"Fix the failing test."}]}|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      snippet fix saved: Fix the failing test. (/snippet fix
      inserts it)
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  (* Without TEXT: the last prompt sent. *)
  command h "/snippet add ask";
  [%expect
    {|
    (Rpc
      (method_ set_config)
      (params ((
        config ((
          snippets (
            ((name fix) (text "Fix the failing test."))
            ((name ask) (text "earlier question"))))))))
      (tag (
        Snippets_saved
        "snippet ask saved: earlier question (/snippet ask inserts it)")))
    |}];
  command h "/snippet add fix Fix it.";
  [%expect
    {|
    (Rpc
      (method_ set_config)
      (params ((
        config ((
          snippets ((
            (name fix)
            (text "Fix it."))))))))
      (tag (
        Snippets_saved "snippet fix replaced: Fix it. (/snippet fix inserts it)")))
    |}];
  (* TEXT may span lines (Ctrl+J). *)
  H.keys h "/snippet add two first";
  H.key h (Key.ctrl 'j');
  command h "second";
  [%expect
    {|
    (Rpc
      (method_ set_config)
      (params ((
        config ((
          snippets (
            ((name fix) (text "Fix the failing test."))
            ((name two) (text "first\nsecond"))))))))
      (tag (Snippets_saved "snippet two saved: first (/snippet two inserts it)")))
    |}];
  command h "/snippet rm fix";
  [%expect
    {|
    (Rpc
      (method_ set_config)
      (params ((config ((snippets ())))))
      (tag (Snippets_saved "snippet fix removed")))
    |}];
  H.reply_error
    h
    (Snippets_saved "snippet fix removed")
    {|config.snippets[0] ("fix") has an empty text|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      snippet fix saved: Fix the failing test. (/snippet fix
      inserts it)
      config.snippets[0] ("fix") has an empty text
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/snippet add and rm: errors say what to do" =
  let h = connected () in
  List.iter
    [ "/snippet add"
    ; "/snippet add rm text"
    ; "/snippet rm"
    ; "/snippet rm revie"
    ; "/snippet rm review plan"
    ]
    ~f:(command h);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      usage: /snippet add NAME [TEXT] (without TEXT: your last
      prompt)
      "rm" is a /snippet subcommand; pick another name
      usage: /snippet rm NAME
      unknown snippet "revie"; did you mean review? (/snippet
      lists them)
      usage: /snippet rm NAME
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/snippet completes names, add and rm" =
  let h = connected () in
  H.keys h "/snippet ";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > /snippet ▏
    ▸ review  Review the diff for bugs.
      tests   Run the tests and fix failures.
      plan    Plan first; change nothing yet.
      add     add NAME [TEXT]: save a snippet
      rm      rm NAME: delete a snippet
    …deepseek-flash  Tab accepts · Enter as typed · Esc hides
    |}];
  H.keys h "re";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > /snippet re▏
    ▸ review  Review the diff for bugs.
      tests   Run the tests and fix failures.
      rm      rm NAME: delete a snippet
      plan    Plan first; change nothing yet.
    …deepseek-flash  $0.01  Tab/Enter accept · Alt+S as typed
    |}];
  H.keys h "view more";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > /snippet review more▏
    …deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;
