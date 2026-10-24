open! Core
module H = Harness
module App = Prigh_web.App

(* [/snippet], its picker (Alt+N), completion, and saving through
   [set_config]. *)

let run h command =
  H.type_ h command;
  H.act h Send
;;

let toasts h = H.text h ~selector:".toast"

let draft h =
  let m = H.model h in
  print_s [%message (m.draft : string) (m.cursor : int)]
;;

let snippets_json =
  {|{"snippets":[{"name":"review","text":"Review the diff for bugs.\nList them by severity."},{"name":"tests","text":"Run the tests and fix failures."}]}|}
;;

let with_snippets () =
  let h = H.create () in
  H.act h (Reply (Config, Ok (Jsonaf.of_string snippets_json)));
  h
;;

let%expect_test "/snippet: a fuzzy picker; Enter inserts, Esc changes nothing" =
  let h = with_snippets () in
  run h "/snippet";
  H.text h ~selector:".modal";
  [%expect
    {|
    (Save_history (/snippet))
    (Focus picker-input)
    Snippets
    (Close (Esc))
    []
    review Review the diff for bugs.
    tests Run the tests and fix failures.
    ↑↓ to move, Enter to choose, Esc to close
    |}];
  H.act h (Picker_query "severity");
  H.text h ~selector:".picker-items";
  [%expect {| review Review the diff for bugs. |}];
  H.key h "Escape" ~target:Field;
  draft h;
  [%expect
    {|
    Close_dialog
    (Focus editor)
    ((m.draft "") (m.cursor 0))
    |}];
  (* Alt+N inserts at the editor's caret, without sending. *)
  H.type_ h "Please  now";
  H.key h ~alt:true ~code:"KeyN" "n" ~target:(Editor { cursor = 7 });
  H.act h (Picker_query "tst");
  H.key h "Enter" ~target:Field;
  draft h;
  [%expect
    {|
    (Open_snippets (7))
    (Focus picker-input)
    Dialog_accept
    (Focus editor)
    (Set_caret 38)
    ((m.draft "Please Run the tests and fix failures. now") (m.cursor 38))
    |}]
;;

let%expect_test "/snippet NAME inserts; unknown names say the closest" =
  let h = with_snippets () in
  run h "/snippet tests";
  draft h;
  [%expect
    {|
    (Save_history ("/snippet tests"))
    (Set_caret 31)
    ((m.draft "Run the tests and fix failures.") (m.cursor 31))
    |}];
  H.type_ h "";
  run h "/snippet revew";
  toasts h;
  draft h;
  [%expect
    {|
    (Save_history ("/snippet revew" "/snippet tests"))
    unknown snippet "revew"; did you mean review? (/snippet lists them) (Dismiss)
    ((m.draft "/snippet revew") (m.cursor 14))
    |}]
;;

let%expect_test "/snippet add and rm save only snippets, acknowledged" =
  let h = with_snippets () in
  run h "/snippet add fix Fix the failing test.\nThen rerun.";
  H.reply
    h
    "set_config"
    {|{"snippets":[{"name":"fix","text":"Fix the failing test.\nThen rerun."}]}|};
  toasts h;
  [%expect
    {|
    (Save_history ( "/snippet add fix Fix the failing test.\
                   \nThen rerun."))
    (Rpc (method_ set_config)
     (params
      ((config
        ((snippets
          (((name review)
            (text  "Review the diff for bugs.\
                  \nList them by severity."))
           ((name tests) (text "Run the tests and fix failures."))
           ((name fix) (text  "Fix the failing test.\
                             \nThen rerun."))))))))
     (tag
      (Config_saved
       "snippet fix saved: Fix the failing test. (/snippet fix inserts it)")))
    (Expire_toast (id 0) (after_ms 4140))
    snippet fix saved: Fix the failing test. (/snippet fix inserts it)
    |}];
  run h "/snippet rm fix";
  H.fail h "set_config" "cannot write ~/.prigh/config.json";
  H.reply h "get_config" snippets_json;
  toasts h;
  [%expect
    {|
    (Save_history
     ("/snippet rm fix"  "/snippet add fix Fix the failing test.\
                        \nThen rerun."))
    (Rpc (method_ set_config) (params ((config ((snippets ())))))
     (tag (Config_saved "snippet fix removed")))
    (Rpc (method_ get_config) (params ()) (tag Config))
    snippet fix saved: Fix the failing test. (/snippet fix inserts it)
    Couldn't save the settings: cannot write ~/.prigh/config.json (Dismiss)
    |}];
  run h "/snippet rm nope";
  toasts h;
  draft h;
  [%expect
    {|
    (Save_history
     ("/snippet rm nope" "/snippet rm fix"
       "/snippet add fix Fix the failing test.\
      \nThen rerun."))
    snippet fix saved: Fix the failing test. (/snippet fix inserts it)
    unknown snippet "nope"; /snippet lists the 2 saved (Dismiss)
    ((m.draft "/snippet rm nope") (m.cursor 16))
    |}]
;;

let%expect_test "/snippet add without TEXT saves the last prompt" =
  let h = H.create () in
  run h "/snippet add ask";
  toasts h;
  [%expect
    {|
    (Save_history ("/snippet add ask"))
    no TEXT and no prompt sent yet: /snippet add ask TEXT (Dismiss)
    |}];
  run h "what changed?";
  H.event
    h
    {|{"event":"message_start","message":{"role":"user","text":"what changed?"}}|};
  run h "/snippet add ask";
  [%expect
    {|
    (Save_history ("what changed?" "/snippet add ask"))
    Scroll_to_bottom
    (Rpc (method_ prompt) (params ((text "what changed?")))
     (tag (Sent (text "what changed?") (images ()))))
    (Save_history ("/snippet add ask" "what changed?" "/snippet add ask"))
    (Rpc (method_ set_config)
     (params ((config ((snippets (((name ask) (text "what changed?"))))))))
     (tag
      (Config_saved "snippet ask saved: what changed? (/snippet ask inserts it)")))
    |}]
;;

let%expect_test "/snippet completes names, add and rm" =
  let h = with_snippets () in
  H.type_ h "/snippet ";
  H.text h ~selector:".popup";
  [%expect
    {|
    Snippets ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    review Review the diff for bugs.
    tests Run the tests and fix failures.
    add add NAME [TEXT]: save a snippet
    rm rm NAME: delete a snippet
    |}];
  H.type_ h "/snippet re";
  H.text h ~selector:".popup";
  [%expect
    {|
    Snippets ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    review Review the diff for bugs.
    tests Run the tests and fix failures.
    rm rm NAME: delete a snippet
    |}]
;;

let%expect_test "#name: completes snippets fuzzily; Tab inserts their text" =
  let h = with_snippets () in
  H.type_ h "please #e";
  H.text h ~selector:".popup";
  [%expect
    {|
    Snippets ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    #tests Run the tests and fix failures.
    #review Review the diff for bugs.
    |}];
  H.type_ h "please #es";
  H.text h ~selector:".popup";
  H.key h "Tab";
  draft h;
  H.text h ~selector:".popup";
  [%expect
    {|
    Snippets ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    #tests Run the tests and fix failures.
    (Complete_accept (run false))
    (Set_caret 38)
    ((m.draft "please Run the tests and fix failures.") (m.cursor 38))
    |}]
;;

let%expect_test "#name: Enter inserts multi-line text mid-text, caret after it" =
  let h = with_snippets () in
  H.act h (Edit { text = "a #rev b\nmore"; cursor = 6 });
  H.text h ~selector:".popup";
  H.key h "Enter" ~target:(Editor { cursor = 6 });
  draft h;
  [%expect
    {|
    Snippets ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    #review Review the diff for bugs.
    (Complete_accept (run true))
    (Set_caret 50)
    ((m.draft  "a Review the diff for bugs.\
              \nList them by severity. b\
              \nmore")
     (m.cursor 50))
    |}];
  (* On a later line, at its start. *)
  H.act h (Edit { text = "first\n#tes"; cursor = 10 });
  H.key h "Tab" ~target:(Editor { cursor = 10 });
  draft h;
  [%expect
    {|
    (Complete_accept (run false))
    (Set_caret 37)
    ((m.draft  "first\
              \nRun the tests and fix failures.") (m.cursor 37))
    |}]
;;

let%expect_test "#: headings, issue refs and words with # type and send as is" =
  let h = with_snippets () in
  H.type_ h "#";
  H.text h ~selector:".popup";
  H.type_ h "# Title";
  H.text h ~selector:".popup";
  H.type_ h "# Title #123";
  H.text h ~selector:".popup";
  H.type_ h "# Title #123 x#rev";
  H.text h ~selector:".popup";
  H.type_ h "# Title #123 x#rev ##rev";
  H.text h ~selector:".popup";
  H.key h "Enter";
  [%expect
    {|
    Send
    (Save_history ("# Title #123 x#rev ##rev"))
    Scroll_to_bottom
    (Rpc (method_ prompt) (params ((text "# Title #123 x#rev ##rev")))
     (tag (Sent (text "# Title #123 x#rev ##rev") (images ()))))
    |}];
  let h = H.create () in
  H.type_ h "#rev";
  H.text h ~selector:".popup";
  H.key h "Enter";
  [%expect
    {|
    Send
    (Save_history (#rev))
    Scroll_to_bottom
    (Rpc (method_ prompt) (params ((text #rev)))
     (tag (Sent (text #rev) (images ()))))
    |}]
;;
