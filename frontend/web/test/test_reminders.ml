open! Core
open Prigh_web
module H = Harness

(* [/remind]: the session's reminders, from its state; the picker, its
   completion and the status line item. *)

let run h command =
  H.type_ h command;
  H.act h Send
;;

let toasts h = H.text h ~selector:".toast"

let reminders_json =
  {|[{"name":"r1","text":"Don't stop until the tests pass.\nThen commit.","on":"stop","max_repeats":3,"repeats":1},
     {"name":"r3","text":"Check the disk space.","every_turns":20}]|}
;;

let state ?(session = "s1") ?(reminders = reminders_json) () =
  Jsonaf.of_string
    (H.state_json
       ~fields:
         [ "session_id", `String session
         ; "session_path", `String (sprintf "/sessions/%s.jsonl" session)
         ; "reminders", Jsonaf.of_string reminders
         ]
       ())
;;

let with_reminders () =
  let h = H.create () in
  H.quiet h (fun () -> H.act h (Reply (State, Ok (state ()))));
  h
;;

let%expect_test "the status line counts the reminders; clicked, it lists them" =
  let h = H.create () in
  H.text h ~selector:".status-extras .reminders";
  H.quiet h (fun () -> H.act h (Reply (State, Ok (state ()))));
  H.text h ~selector:".status-extras .reminders";
  H.names h ~selector:".status-extras .reminders";
  H.click h ~selector:".status-extras .reminders";
  H.text h ~selector:".modal";
  [%expect
    {|
    (Remind 2)
    button "Remind 2" -- Reminders (/remind lists them, /remind rm NAME removes one):
    r1 · on stop 1/3 · Don't stop until the tests pass.
    r3 · every 20 turns · Check the disk space.
    (Run /remind)
    (Focus picker-input)
    Reminders
    (Close (Esc))
    []
    r1 on stop 1/3 · Don't stop until the tests pass. (Remove)
    r3 every 20 turns · Check the disk space. (Remove)
    ↑↓ to move, Enter to show, Delete to remove, Esc to close
    |}]
;;

let%expect_test "/remind stop and every add one, acknowledged by name" =
  let h = with_reminders () in
  run h "/remind stop Don't stop until the tests pass.";
  run h "/remind stop 5 Keep going.";
  run h "/remind every 20 Check the disk space.\nAnd the logs.";
  H.reply
    h
    "add_reminder"
    {|{"name":"r4","text":"Don't stop until the tests pass.","on":"stop","max_repeats":3,"repeats":0}|};
  H.reply
    h
    "add_reminder"
    {|{"name":"r5","text":"Keep going.","on":"stop","max_repeats":5,"repeats":0}|};
  toasts h;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params
      ((key history) (value "/remind stop Don't stop until the tests pass.")
       (max 500)))
     (tag Ignore))
    (Rpc (method_ add_reminder)
     (params ((text "Don't stop until the tests pass.") (on stop)))
     (tag
      (Reminder (command "/remind stop Don't stop until the tests pass.")
       (notice ()))))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/remind stop 5 Keep going.") (max 500)))
     (tag Ignore))
    (Rpc (method_ add_reminder)
     (params ((text "Keep going.") (on stop) (max_repeats 5)))
     (tag (Reminder (command "/remind stop 5 Keep going.") (notice ()))))
    (Rpc (method_ push_pref)
     (params
      ((key history)
       (value  "/remind every 20 Check the disk space.\
              \nAnd the logs.")
       (max 500)))
     (tag Ignore))
    (Rpc (method_ add_reminder)
     (params ((text  "Check the disk space.\
                    \nAnd the logs.") (every_turns 20)))
     (tag
      (Reminder
       (command  "/remind every 20 Check the disk space.\
                \nAnd the logs.")
       (notice ()))))
    (Expire_toast (id 0) (after_ms 6000))
    (Expire_toast (id 1) (after_ms 5780))
    Reminder r4 added (when the agent ends its turn, up to 3 in a row): Don't stop until the tests pass. · /remind rm r4 removes it
    Reminder r5 added (when the agent ends its turn, up to 5 in a row): Keep going. · /remind rm r5 removes it
    |}]
;;

let%expect_test "/remind's mistakes say what to type; a refusal goes back" =
  let h = with_reminders () in
  List.iter
    [ "/remind stop"
    ; "/remind every 0 x"
    ; "/remind stp x"
    ; "/remind rm r2"
    ; "/remind rm zz"
    ]
    ~f:(fun command ->
      run h command;
      toasts h;
      print_s [%sexp ((H.model h).draft : string)];
      H.type_ h "");
  run h "/remind every 2 x";
  H.fail h "add_reminder" "the session is gone";
  print_s [%sexp ((H.model h).draft : string)];
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/remind stop") (max 500))) (tag Ignore))
    Usage: /remind stop [N] TEXT: reminded when it ends its turn, at most N times in a row (default 3) (Dismiss)
    "/remind stop"
    (Rpc (method_ push_pref)
     (params ((key history) (value "/remind every 0 x") (max 500))) (tag Ignore))
    N must be 1 or more: /remind every N TEXT (Dismiss)
    "/remind every 0 x"
    (Rpc (method_ push_pref)
     (params ((key history) (value "/remind stp x") (max 500))) (tag Ignore))
    Unknown /remind "stp"; did you mean stop? usage: /remind [stop [N] TEXT|every N TEXT|rm NAME|off] (Dismiss)
    "/remind stp x"
    (Rpc (method_ push_pref)
     (params ((key history) (value "/remind rm r2") (max 500))) (tag Ignore))
    No reminder "r2"; did you mean r1, r3? (/remind lists them) (Dismiss)
    "/remind rm r2"
    (Rpc (method_ push_pref)
     (params ((key history) (value "/remind rm zz") (max 500))) (tag Ignore))
    No reminder "zz"; the session's are r1, r3 (/remind lists them) (Dismiss)
    "/remind rm zz"
    (Rpc (method_ push_pref)
     (params ((key history) (value "/remind every 2 x") (max 500))) (tag Ignore))
    (Rpc (method_ add_reminder) (params ((text x) (every_turns 2)))
     (tag (Reminder (command "/remind every 2 x") (notice ()))))
    "/remind every 2 x"
    |}]
;;

let%expect_test "/remind rm and /remind off; with none, it says how to add one" =
  let h = with_reminders () in
  run h "/remind rm r3";
  H.reply h "remove_reminder" "{}";
  run h "/remind off";
  H.reply h "clear_reminders" "{}";
  H.quiet h (fun () -> H.act h (Reply (State, Ok (state ~reminders:"[]" ()))));
  run h "/remind";
  run h "/remind off";
  toasts h;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/remind rm r3") (max 500))) (tag Ignore))
    (Rpc (method_ remove_reminder) (params ((name r3)))
     (tag (Reminder (command "/remind rm r3") (notice ("reminder r3 removed")))))
    (Expire_toast (id 0) (after_ms 2500))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/remind off") (max 500))) (tag Ignore))
    (Rpc (method_ clear_reminders) (params ())
     (tag
      (Reminder (command "/remind off") (notice ("all 2 reminders removed")))))
    (Expire_toast (id 1) (after_ms 2500))
    (Rpc (method_ push_pref) (params ((key history) (value /remind) (max 500)))
     (tag Ignore))
    (Expire_toast (id 2) (after_ms 4220))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/remind off") (max 500))) (tag Ignore))
    (Expire_toast (id 3) (after_ms 2500))
    No reminders yet: /remind stop TEXT or /remind every N TEXT adds one
    No reminders to clear
    |}]
;;

let%expect_test "/remind: a fuzzy picker; Enter shows one, Delete removes it" =
  let h = with_reminders () in
  run h "/remind";
  H.text h ~selector:".modal";
  H.act h (Picker_query "disk");
  H.text h ~selector:".picker-items";
  H.key h "Enter" ~target:Field;
  toasts h;
  run h "/remind";
  H.key h "Delete" ~target:Field;
  print_s [%sexp ((H.model h).dialog : Dialog.t option)];
  run h "/remind";
  H.click h ~selector:".picker-remove" ~nth:1;
  H.reply h "remove_reminder" "{}";
  toasts h;
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /remind) (max 500)))
     (tag Ignore))
    (Focus picker-input)
    Reminders
    (Close (Esc))
    []
    r1 on stop 1/3 · Don't stop until the tests pass. (Remove)
    r3 every 20 turns · Check the disk space. (Remove)
    ↑↓ to move, Enter to show, Delete to remove, Esc to close
    r3 every 20 turns · Check the disk space. (Remove)
    Dialog_accept
    (Focus editor)
    (Expire_toast (id 0) (after_ms 4300))
    r3 (every 20 turns): Check the disk space. · /remind rm r3 removes it
    (Rpc (method_ push_pref) (params ((key history) (value /remind) (max 500)))
     (tag Ignore))
    (Focus picker-input)
    (Remove_reminder r1)
    (Focus editor)
    (Rpc (method_ remove_reminder) (params ((name r1)))
     (tag (Reminder (command "/remind rm r1") (notice ("reminder r1 removed")))))
    ()
    (Rpc (method_ push_pref) (params ((key history) (value /remind) (max 500)))
     (tag Ignore))
    (Focus picker-input)
    (Remove_reminder r3)
    (Focus editor)
    (Rpc (method_ remove_reminder) (params ((name r3)))
     (tag (Reminder (command "/remind rm r3") (notice ("reminder r3 removed")))))
    (Expire_toast (id 1) (after_ms 2500))
    r3 (every 20 turns): Check the disk space. · /remind rm r3 removes it
    Reminder r1 removed
    |}]
;;

let%expect_test "/remind completes subcommands, and names after rm" =
  let h = with_reminders () in
  H.type_ h "/remind ";
  H.text h ~selector:".popup";
  H.type_ h "/remind rm ";
  H.text h ~selector:".popup";
  H.type_ h "/remind rm 3";
  H.text h ~selector:".popup";
  H.key h "Tab";
  print_s [%sexp ((H.model h).draft : string)];
  H.type_ h "/remind stop keep ";
  H.text h ~selector:".popup";
  [%expect
    {|
    Reminders ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    stop stop [N] TEXT: remind the agent when it ends its turn
    every every N TEXT: remind the agent every N turns
    rm rm NAME: remove a reminder
    off remove every reminder
    Reminders ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    r1 on stop 1/3 · Don't stop until the tests pass.
    r3 every 20 turns · Check the disk space.
    Reminders ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    r3 every 20 turns · Check the disk space.
    (Complete_accept (run false))
    (Set_caret 13)
    "/remind rm r3"
    |}]
;;

let%expect_test
    "switching sessions closes the picker; the new state brings its reminders"
  =
  let h = with_reminders () in
  run h "/remind";
  H.quiet h (fun () ->
    H.act h (Reply (State, Ok (state ~session:"s2" ~reminders:"[]" ()))));
  print_s [%sexp ((H.model h).dialog : Dialog.t option)];
  H.text h ~selector:".status-extras .reminders";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /remind) (max 500)))
     (tag Ignore))
    (Focus picker-input)
    ()
    |}]
;;
