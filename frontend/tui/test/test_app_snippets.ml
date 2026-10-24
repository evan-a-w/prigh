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
    prigh  new session                                     /work




    ╭─ Snippets (3) ───────────────────────────────────────────╮
    │ / ▏                                                      │
    │ ▸  review  Review the diff for bugs.                     │
    │    tests   Run the tests and fix failures.               │
    │    plan    Plan first; change nothing yet.               │
    ╰──────────────────────────────────────────────────────────╯
    ── Enter inserts the text at the cursor · Esc closes ───────
    >
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.keys h "tst";
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work





    ╭─ Snippets (2) ───────────────────────────────────────────╮
    │ / tst▏                                                   │
    │ ▸  tests   Run the tests and fix failures.               │
    │    review  Review the diff for bugs.                     │
    ╰──────────────────────────────────────────────────────────╯
    ── Enter inserts the text at the cursor · Esc closes ───────
    >
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.esc h;
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work








    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up ──────────────
    > ▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.keys h "/snippet";
  H.enter h;
  H.enter h;
  H.keys h "bugs";
  H.enter h;
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work







    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up ──────────────
    > Review the diff for bugs.
      List them by severity.▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/snippet NAME inserts; an unknown name says the closest" =
  let h = connected () in
  command h "/snippet tests";
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work








    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up ──────────────
    > Run the tests and fix failures.▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.key h (Key.ctrl 'u');
  command h "/snippet revew";
  command h "/snippet zzz";
  command h "/snippet review extra words";
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work






    ╭─ Error ──────────────────────────────────────────────────╮
    │ usage: /snippet [NAME] · /snippet add NAME [TEXT] ·      │
    │ /snippet rm NAME                                         │
    ╰────────────────────────────────────────── Esc dismisses ─╯
    ── /help · Tab commands · Alt+Enter follow-up ──────────────
    > ▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                     /work




    ╭─ Error ──────────────────────────────────────────────────╮
    │ unknown snippet "foo"; no snippets yet: /snippet add     │
    │ NAME TEXT saves one                                      │
    ╰────────────────────────────────────────── Esc dismisses ─╯
    ── /help · Tab commands · Alt+Enter follow-up ──────────────
    > ▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                     /work






    ╭─ Notice ─────────────────────────────────────────────────╮
    │ snippet fix saved: Fix the failing test. (/snippet fix   │
    │ inserts it)                                              │
    ╰────────────────────────────────────────── Esc dismisses ─╯
    ── /help · Tab commands · Alt+Enter follow-up ──────────────
    > ▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                     /work








    ▌ earlier question
      earlier answer
    ──────────── config.snippets[0] ("fix") has an empty text ──
    > ▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                     /work








    ▌ earlier question
      earlier answer
    ── /help · Tab commands ───────── usage: /snippet rm NAME ──
    > ▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/snippet completes names, add and rm" =
  let h = connected () in
  H.keys h "/snippet ";
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work



    ╭─ /snippet ───────────────────────────────────────────────╮
    │ ▸ review  Review the diff for bugs.                      │
    │   tests   Run the tests and fix failures.                │
    │   plan    Plan first; change nothing yet.                │
    │   add     add NAME [TEXT]: save a snippet                │
    │   rm      rm NAME: delete a snippet                      │
    ╰──────────────────────────────────────────────────────────╯
    ── Tab accepts · Enter as typed · Esc hides ────────────────
    > /snippet ▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.keys h "re";
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work




    ╭─ /snippet ───────────────────────────────────────────────╮
    │ ▸ review  Review the diff for bugs.                      │
    │   tests   Run the tests and fix failures.                │
    │   rm      rm NAME: delete a snippet                      │
    │   plan    Plan first; change nothing yet.                │
    ╰──────────────────────────────────────────────────────────╯
    ── Tab/Enter accept · Alt+S as typed ───────────────────────
    > /snippet re▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.keys h "view more";
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work








    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up ──────────────
    > /snippet review more▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "#name: completes snippets fuzzily; Tab inserts their text" =
  let h = connected () in
  H.keys h "please #e";
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work






    ╭─ Snippets ───────────────────────────────────────────────╮
    │ ▸ #tests   Run the tests and fix failures.               │
    │   #review  Review the diff for bugs.                     │
    ╰──────────────────────────────────────────────────────────╯
    ── Tab/Enter accept · Alt+S as typed ───────────────────────
    > please #e▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.keys h "s";
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work







    ╭─ Snippets ───────────────────────────────────────────────╮
    │ ▸ #tests  Run the tests and fix failures.                │
    ╰──────────────────────────────────────────────────────────╯
    ── Tab/Enter accept · Alt+S as typed ───────────────────────
    > please #es▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.key h (Key.plain Tab);
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work








    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up ──────────────
    > please Run the tests and fix failures.▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "#name: Enter inserts multi-line text mid-line, cursor after it"
  =
  let h = connected () in
  H.keys h "a #rev b";
  H.key h (Key.plain Left);
  H.key h (Key.plain Left);
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work







    ╭─ Snippets ───────────────────────────────────────────────╮
    │ ▸ #review  Review the diff for bugs.                     │
    ╰──────────────────────────────────────────────────────────╯
    ── Tab/Enter accept · Alt+S as typed ───────────────────────
    > a #rev▏b
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.enter h;
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work







    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up ──────────────
    > a Review the diff for bugs.
      List them by severity.▏b
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.keys h "!";
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work







    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up ──────────────
    > a Review the diff for bugs.
      List them by severity.!▏b
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  (* On a later line too. *)
  H.key h (Key.ctrl 'j');
  H.keys h "#pl";
  H.key h (Key.plain Tab);
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work






    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up ──────────────
    > a Review the diff for bugs.
      List them by severity.!
      Plan first; change nothing yet.▏b
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  (* One undo step takes the insertion back. *)
  H.key h (Key.ctrl '_');
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work





    ╭─ Snippets ───────────────────────────────────────────────╮
    │ ▸ #plan  Plan first; change nothing yet.                 │
    ╰──────────────────────────────────────────────────────────╯
    ── Tab/Enter accept · Alt+S as typed ───────────────────────
    > a Review the diff for bugs.
      List them by severity.!
      #pl▏b
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "#: headings, issue refs and words with # type and send as is" =
  let h = connected () in
  H.keys h "#";
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work








    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up ──────────────
    > #▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.keys h " Title #123 x#rev ##rev";
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work








    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up ──────────────
    > # Title #123 x#rev ##rev▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ prompt)
      (params ((text "# Title #123 x#rev ##rev")))
      (tag Show_error))
    (Append_history "# Title #123 x#rev ##rev")
    |}];
  let h = connected ~config:{|{"snippets":[]}|} () in
  H.keys h "#rev";
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work








    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up ──────────────
    > #rev▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.enter h;
  [%expect
    {|
    (Rpc (method_ prompt) (params ((text #rev))) (tag Show_error))
    (Append_history #rev)
    |}]
;;
