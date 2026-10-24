open! Core
open! Expect_test_helpers_core
open Prigh_ui
open Fixtures
module H = Test_app.H
module P = Prigh_protocol

(* /remind: the session's reminders, from its state. *)

let reminders_json =
  {|[{"name":"r1","text":"Don't stop until the tests pass.\nThen commit.","on":"stop","max_repeats":3,"repeats":1},
     {"name":"r3","text":"Check the disk space.","every_turns":20}]|}
;;

let connected ?(height = 14) ?(reminders = reminders_json) () =
  let h = Test_app.connected ~width:80 ~height () in
  H.event h (State (state ~extra:[ "reminders", reminders ] ()));
  h
;;

(* As typed, closing the completion first so that Enter runs it. *)
let run h text =
  String.iter text ~f:(fun c -> H.step ~quiet:true h (Key (Key.char c)));
  if Option.is_some h.H.model.autocomplete then H.esc h;
  H.enter h
;;

let status h =
  print_endline
    (List.last_exn
       (String.split_lines
          (Screen.to_plain ~show_cursor:false (Render.screen h.H.model))))
;;

let%expect_test "the status line counts the reminders" =
  let h = connected ~reminders:"[]" () in
  status h;
  H.event h (State (state ~extra:[ "reminders", reminders_json ] ()));
  status h;
  [%expect
    {|
    deepseek-flash  think:off  mode:auto  view:normal     ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    deepseek-flash  think:off  mode:auto  remind:2        ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/remind stop and /remind every add one, acknowledged by name" =
  let h = connected () in
  run h "/remind stop Don't stop until the tests pass.";
  run h "/remind stop 5 Keep going.";
  run h "/remind every 20 Check the disk space.";
  H.reply
    h
    (Retry_in_editor
       { command = "/remind every 20 Check the disk space."
       ; hint = ""
       ; then_ = Reminder_added
       })
    {|{"name":"r4","text":"Check the disk space.","every_turns":20}|};
  H.reply
    h
    (Retry_in_editor
       { command = "/remind stop 5 Keep going."
       ; hint = ""
       ; then_ = Reminder_added
       })
    {|{"name":"r5","text":"Keep going.","on":"stop","max_repeats":5,"repeats":0}|};
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ add_reminder)
      (params (
        (text "Don't stop until the tests pass.")
        (on   stop)))
      (tag (
        Retry_in_editor
        (command "/remind stop Don't stop until the tests pass.")
        (hint  "")
        (then_ Reminder_added))))
    (Rpc
      (method_ add_reminder)
      (params (
        (text        "Keep going.")
        (on          stop)
        (max_repeats 5)))
      (tag (
        Retry_in_editor
        (command "/remind stop 5 Keep going.")
        (hint    "")
        (then_   Reminder_added))))
    (Rpc
      (method_ add_reminder)
      (params (
        (text        "Check the disk space.")
        (every_turns 20)))
      (tag (
        Retry_in_editor
        (command "/remind every 20 Check the disk space.")
        (hint    "")
        (then_   Reminder_added))))
    prigh  new session                                                         /work






    ╭─ Notice ─────────────────────────────────────────────────────────────────────╮
    │ reminder r5 added (when the agent ends its turn, up to 5 in a row): Keep     │
    │ going. · /remind rm r5 removes it                                            │
    ╰────────────────────────────────────────────────────────────── Esc dismisses ─╯
    ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────
    > ▏
    deepseek-flash  think:off  mode:auto  remind:2        ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/remind's mistakes say what to type, the command kept to fix" =
  let h = connected ~height:30 () in
  List.iter
    [ "/remind stop"
    ; "/remind stop 4"
    ; "/remind stop 0 x"
    ; "/remind every x"
    ; "/remind every 0 x"
    ; "/remind every 3"
    ; "/remind stp x"
    ; "/remind rm"
    ; "/remind rm r2"
    ; "/remind rm zz"
    ; "/remind off now"
    ]
    ~f:(fun text ->
      run h text;
      print_endline (Editor.text h.model.editor);
      H.key h (Key.ctrl 'u'));
  H.show h;
  (* A refusal from the backend puts it back too. *)
  run h "/remind every 2 x";
  H.reply_error
    h
    (Retry_in_editor
       { command = "/remind every 2 x"; hint = ""; then_ = Reminder_added })
    "the session is gone";
  H.show h;
  [%expect
    {|
    /remind stop
    /remind stop 4
    /remind stop 0 x
    /remind every x
    /remind every 0 x
    /remind every 3
    /remind stp x
    /remind rm
    /remind rm r2
    /remind rm zz
    /remind off now
    prigh  new session                                                         /work
























    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────
    > ▏
    deepseek-flash  think:off  mode:auto  remind:2        ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    (Rpc
      (method_ add_reminder)
      (params (
        (text        x)
        (every_turns 2)))
      (tag (
        Retry_in_editor
        (command "/remind every 2 x")
        (hint    "")
        (then_   Reminder_added))))
    prigh  new session                                                         /work
























    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up ─────────── the session is gone ──
    > /remind every 2 x▏
    deepseek-flash  think:off  mode:auto  remind:2        ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/remind rm and /remind off, with none, and before the state" =
  let h = connected () in
  run h "/remind rm r3";
  H.reply
    h
    (Retry_in_editor
       { command = "/remind rm r3"
       ; hint = ""
       ; then_ = Notice_on_success "reminder r3 removed"
       })
    "{}";
  run h "/remind off";
  H.reply
    h
    (Retry_in_editor
       { command = "/remind off"
       ; hint = ""
       ; then_ = Notice_on_success "all 2 reminders removed"
       })
    "{}";
  H.event h (State (state ~extra:[ "reminders", "[]" ] ()));
  run h "/remind";
  run h "/remind off";
  run h "/remind rm r1";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ remove_reminder)
      (params ((name r3)))
      (tag (
        Retry_in_editor
        (command "/remind rm r3")
        (hint    "")
        (then_ (Notice_on_success "reminder r3 removed")))))
    (Rpc
      (method_ clear_reminders)
      (params ())
      (tag (
        Retry_in_editor
        (command "/remind off")
        (hint    "")
        (then_ (Notice_on_success "all 2 reminders removed")))))
    prigh  new session                                                         /work






    ╭─ Error ──────────────────────────────────────────────────────────────────────╮
    │ no reminder "r1"; no reminders yet: /remind stop TEXT or /remind every N     │
    │ TEXT adds one                                                                │
    ╰────────────────────────────────────────────────────────────── Esc dismisses ─╯
    ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────
    > /remind rm r1▏
    deepseek-flash  think:off  mode:auto  view:normal     ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  let h = H.create () in
  H.step ~quiet:true h Start;
  run h "/remind";
  H.key h (Key.ctrl 'u');
  run h "/remind rm r1";
  H.show h;
  [%expect
    {|
    prigh  connecting…








    ─── the session has not loaded yet; try again in a moment ──
    > /remind rm r1▏
    connecting…
    |}]
;;

let%expect_test "/remind: a fuzzy picker; Enter shows one, Ctrl+D removes it" =
  let h = connected () in
  run h "/remind";
  H.show h;
  [%expect
    {|
    prigh  new session                                                         /work





    ╭─ Reminders (2) ──────────────────────────────────────────────────────────────╮
    │ / ▏                                                                          │
    │ ▸  r1  on stop 1/3 · Don't stop until the tests pass.                        │
    │    r3  every 20 turns · Check the disk space.                                │
    ╰──────────────────────────────────────────────────────────────────────────────╯
    ── Enter shows it · Esc closes · Ctrl+D removes ────────────────────────────────
    >
    deepseek-flash  think:off  mode:auto  remind:2        ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.keys h "disk";
  H.show h;
  [%expect
    {|
    prigh  new session                                                         /work






    ╭─ Reminders (1) ──────────────────────────────────────────────────────────────╮
    │ / disk▏                                                                      │
    │ ▸  r3  every 20 turns · Check the disk space.                                │
    ╰──────────────────────────────────────────────────────────────────────────────╯
    ── Enter shows it · Esc closes · Ctrl+D removes ────────────────────────────────
    >
    deepseek-flash  think:off  mode:auto  remind:2        ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.enter h;
  H.show h;
  [%expect
    {|
    prigh  new session                                                         /work








    ▌ earlier question
      earlier answer
    ─────── r3 (every 20 turns): Check the disk space. · /remind rm r3 removes it ──
    > ▏
    deepseek-flash  think:off  mode:auto  remind:2        ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  run h "/remind";
  H.key h (Key.ctrl 'd');
  H.mode h;
  [%expect
    {|
    (Rpc
      (method_ remove_reminder)
      (params ((name r1)))
      (tag (
        Retry_in_editor
        (command "/remind rm r1")
        (hint    "")
        (then_ (Notice_on_success "reminder r1 removed")))))
    editing
    |}];
  run h "/remind";
  H.esc h;
  H.mode h;
  [%expect {| editing |}]
;;

let%expect_test "/remind completes subcommands, and names after rm" =
  let h = connected () in
  let popup () =
    match h.model.autocomplete with
    | None -> print_endline "(none)"
    | Some ac ->
      List.iter (Autocomplete.items ac) ~f:(fun (i : Picker.Item.t) ->
        printf "%s  %s\n" i.label i.detail)
  in
  H.keys h "/remind ";
  popup ();
  H.keys h "ev";
  popup ();
  H.key h (Key.ctrl 'u');
  H.keys h "/remind rm ";
  popup ();
  H.keys h "3";
  popup ();
  H.key h (Key.plain Tab);
  print_endline (Editor.text h.model.editor);
  H.key h (Key.ctrl 'u');
  H.keys h "/remind stop keep ";
  popup ();
  [%expect
    {|
    stop  stop [N] TEXT: remind the agent when it ends its turn
    every  every N TEXT: remind the agent every N turns
    rm  rm NAME: remove a reminder
    off  remove every reminder
    every  every N TEXT: remind the agent every N turns
    r1  on stop 1/3 · Don't stop until the tests pass.
    r3  every 20 turns · Check the disk space.
    r3  every 20 turns · Check the disk space.
    /remind rm r3
    (none)
    |}]
;;

let%expect_test
    "switching sessions closes the picker; the new session's state brings its \
     reminders"
  =
  let h = connected () in
  run h "/remind";
  H.mode h;
  H.reply ~quiet:true h (Reload_messages_notice "cloned session") {|{}|};
  H.mode h;
  H.reply
    ~quiet:true
    h
    Initial_state
    (state_json ~extra:[ "reminders", "[]" ] ());
  status h;
  run h "/remind";
  H.show h;
  [%expect
    {|
    picker
    editing
    deepseek-flash  think:off  mode:auto  view:normal     ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    prigh  new session                                                         /work










    ──────── no reminders yet: /remind stop TEXT or /remind every N TEXT adds one ──
    > ▏
    deepseek-flash  think:off  mode:auto  view:normal     ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}]
;;
