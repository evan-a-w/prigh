open! Core
open! Expect_test_helpers_core
open Prigh_ui
module H = Test_app.H
module P = Prigh_protocol

(* /user-info: what the user wrote about themselves for every session. *)

(* As typed, closing the completion first so that Enter runs it. *)
let run h text =
  String.iter text ~f:(fun c -> H.step ~quiet:true h (Key (Key.char c)));
  if Option.is_some h.H.model.autocomplete then H.esc h;
  H.enter h
;;

let%expect_test "/user-info shows it, or says how to write it" =
  let h = Test_app.connected ~width:80 ~height:14 () in
  run h "/user-info";
  H.reply h User_info_shown {|{"text":"","max":4000}|};
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_user_info) (params ()) (tag User_info_shown))


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────────────────────────┐
    │ nothing about you yet: /user-info TEXT tells the model who you are and how   │
    │ you like to work, in every session (~/.prigh/user_info.md on the backend;    │
    │ prigh-web: Settings, About you)                                              │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  run h "/user-info";
  H.reply
    ~quiet:true
    h
    User_info_shown
    {|{"text":"I'm Lloyd.\nI prefer short answers.","max":4000}|};
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_user_info) (params ()) (tag User_info_shown))
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ About you · given to the model in every session · 34 of 4000 bytes ─────────┐
    │ I'm Lloyd.                                                                   │
    │ I prefer short answers.                                                      │
    │                                                                              │
    │ /user-info TEXT replaces it, /user-info clear removes it                     │
    │ (~/.prigh/user_info.md on the backend; prigh-web: Settings, About you)       │
    └───────────────────────────────────────────────────────────────── Esc closes ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  ↑↓ scroll · Esc closes · typing goes to the editor
    |}]
;;

let%expect_test "/user-info TEXT saves it; a refusal puts the command back" =
  let h = Test_app.connected ~width:80 ~height:14 () in
  run h "/user-info I'm Lloyd. I prefer short answers.";
  H.reply
    h
    (User_info_saved "/user-info I'm Lloyd. I prefer short answers.")
    {|{"text":"I'm Lloyd. I prefer short answers.","max":4000}|};
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ set_user_info)
      (params ((text "I'm Lloyd. I prefer short answers.")))
      (tag (User_info_saved "/user-info I'm Lloyd. I prefer short answers.")))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────────────────────────┐
    │ saved (34 of 4000 bytes): the model has it in new sessions, and in open ones │
    │ from your next message                                                       │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  run h "/user-info far too long";
  H.reply_error
    h
    (User_info_saved "/user-info far too long")
    "the text about you is limited to 4000 bytes (this one has 4001): shorten \
     it, and keep what is only sometimes needed in memory notes";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ set_user_info)
      (params ((text "far too long")))
      (tag (User_info_saved "/user-info far too long")))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ the text about you is limited to 4000 bytes (this one has 4001): shorten it, │
    │ and keep what is only sometimes needed in memory notes                       │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > /user-info far too long▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/user-info clear asks first" =
  let h = Test_app.connected ~width:80 ~height:14 () in
  run h "/user-info clear";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────────────────────────────────────────────┐
    │ remove what you wrote about yourself? The model is no longer given it (y/n)  │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  confirm: y / n
    |}];
  H.key h (Key.char 'n');
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ───────────────────────────────────────── kept what you wrote about yourself ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  run h "/user-info clear";
  H.key h (Key.char 'y');
  H.reply h (User_info_saved "") {|{"text":"","max":4000}|};
  H.show h;
  [%expect
    {|
    (Rpc (method_ set_user_info) (params ((text ""))) (tag (User_info_saved "")))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────────────────────────┐
    │ removed what you wrote about yourself: open sessions are told with your next │
    │ message                                                                      │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "the note a session is told with is a chip in the transcript" =
  let origin =
    Or_error.ok_exn
      (Or_error.bind
         (P.Json.parse
            {|{"body":"hello","notes":[{"kind":"user_info","title":"About you: the model was given what you wrote","text":"[User info: ...]"}]}|})
         ~f:P.Origin.of_json)
  in
  print_s [%sexp (origin : P.Origin.t)];
  List.iter origin.notes ~f:(fun n ->
    printf "%s %s\n" (Injected_note.glyph n.kind) n.title);
  [%expect
    {|
    ((body hello)
     (notes ((
       (kind User_info)
       (title "About you: the model was given what you wrote")
       (text "[User info: ...]")))))
    ☺ About you: the model was given what you wrote
    |}]
;;
