open! Core
open! Expect_test_helpers_core
open Prigh_ui
module H = Test_app.H
module P = Prigh_protocol

(* /memory: the user's notes, from [get_memory]; on and off. *)

let memory_json =
  {|{"enabled":true,"notes":[
     {"id":"m1","title":"editor","text":"helix, with the default keymap","saved":"2026-10-01 09:00:00.000000Z"},
     {"id":"m2","text":"Deploys go through staging first.\nThen production.","saved":"2026-10-02 09:00:00.000000Z"},
     {"id":"m3","title":"shell","text":"fish","saved":"2026-10-03 09:00:00.000000Z"}]}|}
;;

let memory_of json =
  Or_error.ok_exn (Or_error.bind (P.Json.parse json) ~f:P.Memory.of_json)
;;

(* As typed, closing the completion first so that Enter runs it. *)
let run h text =
  String.iter text ~f:(fun c -> H.step ~quiet:true h (Key (Key.char c)));
  if Option.is_some h.H.model.autocomplete then H.esc h;
  H.enter h
;;

let%expect_test "/memory: a fuzzy picker, newest first; Enter shows one" =
  let h = Test_app.connected ~width:80 ~height:14 () in
  run h "/memory";
  H.reply h Memory_picker memory_json;
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_memory) (params ()) (tag Memory_picker))


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Memory · on · 3 notes  (3)
    / ▏
    ▸  shell                                     2026-10-03 · fish
       Deploys go through staging first. Then production.  2026-10-02
       editor                                    2026-10-01 · helix, with the defau…
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter shows it · Ctrl+D forgets it · Esc closes
    |}];
  H.keys h "deploy";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Memory · on · 3 notes  (1)
    / deploy▏
    ▸  Deploys go through staging first. Then production.  2026-10-02
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter shows it · Ctrl+D forgets it · Esc closes
    |}];
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────────────────────────┐
    │ m2 (saved 2026-10-02): Deploys go through staging first.                     │
    │ Then production. · Ctrl+D in /memory forgets it                              │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test
    "Ctrl+D forgets one after a confirmation; changes reach the picker"
  =
  let h = Test_app.connected ~width:80 ~height:14 () in
  run h "/memory";
  H.reply ~quiet:true h Memory_picker memory_json;
  H.key h (Key.ctrl 'd');
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_memory) (params ()) (tag Memory_picker))




      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────────────────────────────────────────────┐
    │ forget "shell"? The model can no longer recall it (y/n)                      │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  confirm: y / n
    |}];
  H.key h (Key.char 'n');
  H.mode h;
  [%expect {| picker |}];
  H.key h (Key.ctrl 'd');
  H.key h (Key.char 'y');
  H.mode h;
  [%expect
    {|
    (Rpc
      (method_ forget_memory)
      (params ((id m3)))
      (tag (Notice_on_success "forgot \"shell\"")))
    picker
    |}];
  H.event
    h
    (Memory_changed
       (memory_of
          {|{"enabled":false,"notes":[
             {"id":"m1","title":"editor","text":"helix","saved":"2026-10-01 09:00:00.000000Z"}]}|}));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Memory · off: the model is not given it · 1 note  (1)
    / ▏
    ▸  editor  2026-10-01 · helix
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter shows it · Ctrl+D forgets it · Esc closes
    |}];
  H.esc h;
  H.mode h;
  [%expect {| editing |}]
;;

let%expect_test "/memory on, off, a bad argument, and an empty memory" =
  let h = Test_app.connected ~width:80 ~height:14 () in
  run h "/memory off";
  run h "/memory on";
  run h "/memory maybe";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ set_memory_enabled)
      (params ((enabled false)))
      (tag (
        Notice_on_success
        "memory off: the model is not given the memory tool; your notes are kept (/memory lists them)")))
    (Rpc
      (method_ set_memory_enabled)
      (params ((enabled true)))
      (tag (
        Notice_on_success
        "memory on: the model can save and recall notes when you ask")))







      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────── unknown argument "maybe"; use on or off, or nothing to list the notes ───
    > /memory maybe▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Key.ctrl 'u');
  run h "/memory";
  H.reply h Memory_picker {|{"enabled":false,"notes":[]}|};
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_memory) (params ()) (tag Memory_picker))


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────────────────────────┐
    │ memory is empty (off; /memory on gives the model the memory tool): the model │
    │ saves a note when you ask it to remember something, and recalls it when you  │
    │ refer back                                                                   │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;
