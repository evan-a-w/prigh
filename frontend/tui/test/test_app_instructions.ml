open! Core
open! Expect_test_helpers_core
open Prigh_ui
module H = Test_app.H
module P = Prigh_protocol

(* /instructions: the user's custom instructions, from
   [get_custom_instructions]; add, remove (Ctrl+D), on and off. *)

let json =
  {|{"enabled":true,"limits":{"text":1000,"title":80,"count":50},"notes":[
     {"id":"ci1","text":"Use Australian spelling.","saved":"2026-11-01 09:00:00.000000Z"},
     {"id":"ci2","title":"Maths","text":"Keep one memory note on my maths up to date.","saved":"2026-11-02 09:00:00.000000Z"}]}|}
;;

let of_json json =
  Or_error.ok_exn
    (Or_error.bind (P.Json.parse json) ~f:P.Custom_instructions.of_json)
;;

(* As typed, closing the completion first so that Enter runs it. *)
let run h text =
  String.iter text ~f:(fun c -> H.step ~quiet:true h (Key (Key.char c)));
  if Option.is_some h.H.model.autocomplete then H.esc h;
  H.enter h
;;

let%expect_test "/instructions: a picker in the model's order, with ids" =
  let h = Test_app.connected ~width:80 ~height:14 () in
  run h "/instructions";
  H.reply h Instructions_picker json;
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_custom_instructions) (params ()) (tag Instructions_picker))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Custom instructions · 2 · ~40 tokens per session · model edits on  (2)
    / ▏
    ▸  Use Australian spelling.  ci1
       Maths                     ci2 · Keep one memory note on my maths up to date.
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter shows it · Ctrl+D removes it · Esc closes
    |}];
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ───────── ci1: Use Australian spelling. · Ctrl+D in /instructions removes it ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test
    "Ctrl+D removes one after a confirmation; changes reach the picker"
  =
  let h = Test_app.connected ~width:80 ~height:14 () in
  run h "/instructions";
  H.reply ~quiet:true h Instructions_picker json;
  H.key h (Key.ctrl 'd');
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_custom_instructions) (params ()) (tag Instructions_picker))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────────────────────────────────────────────┐
    │ remove ci1 "Use Australian spelling."? It leaves every session's context     │
    │ (y/n)                                                                        │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  confirm: y / n
    |}];
  H.key h (Key.char 'n');
  H.mode h;
  H.key h (Key.ctrl 'd');
  H.key h (Key.char 'y');
  H.mode h;
  [%expect
    {|
    picker
    (Rpc
      (method_ remove_custom_instruction)
      (params ((id ci1)))
      (tag (Notice_on_success "removed ci1")))
    picker
    |}];
  H.event
    h
    (Custom_instructions_changed
       (of_json
          {|{"enabled":false,"notes":[
             {"id":"ci2","text":"Answer briefly.","saved":"2026-11-03 09:00:00.000000Z"}]}|}));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Custom instructions · 1 · ~23 tokens per session · model edits off  (1)
    / ▏
    ▸  Answer briefly.  ci2
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter shows it · Ctrl+D removes it · Esc closes
    |}]
;;

let%expect_test "/instructions add, on, off, a bad argument, and none" =
  let h = Test_app.connected ~width:80 ~height:14 () in
  run h "/instructions add Prefer OCaml examples.";
  H.reply
    h
    (Instruction_saved "/instructions add Prefer OCaml examples.")
    {|{"note":{"id":"ci3","text":"Prefer OCaml examples.","saved":"2026-11-12 09:00:00.000000Z"}}|};
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ save_custom_instruction)
      (params ((text "Prefer OCaml examples.")))
      (tag (Instruction_saved "/instructions add Prefer OCaml examples.")))







      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ─────── added ci3, in the context of every session: /instructions lists them ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  H.esc h;
  run h "/instructions off";
  run h "/instructions on";
  run h "/instructions maybe";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ set_custom_instructions_enabled)
      (params ((enabled false)))
      (tag (
        Notice_on_success
        "custom instructions: the model may not change them; they stay in every session's context")))
    (Rpc
      (method_ set_custom_instructions_enabled)
      (params ((enabled true)))
      (tag (
        Notice_on_success
        "custom instructions: the model may add, edit and remove them")))


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ unknown argument "maybe": /instructions lists them (Ctrl+D removes one), add │
    │ TEXT adds one, on or off lets the model change them or not; prigh-web's      │
    │ Settings edits them                                                          │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > /instructions maybe▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Key.ctrl 'u');
  run h "/instructions";
  H.reply h Instructions_picker {|{"enabled":true,"notes":[]}|};
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_custom_instructions) (params ()) (tag Instructions_picker))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────────────────────────┐
    │ no custom instructions: /instructions add TEXT adds one (in the context of   │
    │ every session), or tell the model a lasting preference                       │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;
