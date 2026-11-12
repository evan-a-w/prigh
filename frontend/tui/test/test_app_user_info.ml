open! Core
open! Expect_test_helpers_core
open Prigh_ui
module H = Test_app.H
module P = Prigh_protocol

(* /user-info: what the user set about themselves for every session. *)

(* As typed, closing the completion first so that Enter runs it. *)
let run h text =
  String.iter text ~f:(fun c -> H.step ~quiet:true h (Key (Key.char c)));
  if Option.is_some h.H.model.autocomplete then H.esc h;
  H.enter h
;;

let limits =
  {|"limits":{"name":100,"background":1000,"anything_else":4000,"total":5000}|}
;;

let empty = sprintf {|{"form":{},"text":"",%s}|} limits

let lloyd =
  sprintf
    {|{"form":{"name":"Lloyd","country":"Australia","units":"metric"},"text":"- Name: Lloyd\n- Country/region: Australia\n- Units: metric (°C, km, kg, L)",%s}|}
    limits
;;

let%expect_test "/user-info shows the block the model is given" =
  let h = Test_app.connected ~width:80 ~height:16 () in
  run h "/user-info";
  H.reply h User_info_shown empty;
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_user_info) (params ()) (tag User_info_shown))
    ┌─ About you · nothing yet ────────────────────────────────────────────────────┐
    │ Nothing about you yet: tell the model who you are, in every session.         │
    │                                                                              │
    │ /user-info FIELD VALUE sets a field (an empty VALUE clears it), /user-info   │
    │ clear clears them all (~/.prigh/about_you.json on the backend; prigh-web:    │
    │ Settings, About you). The fields:                                            │
    │   name           what to call you                                            │
    │   country        country or region                                           │
    │   time_zone      IANA time zone, e.g. Australia/Sydney                       │
    │   language       language and spelling, e.g. English, Australian spelling    │
    │   units          metric, imperial or us_customary                            │
    │   date_format    dmy, mdy or iso                                             │
    └────────────────────────────── 1–11 of 14 · ↑↓ PgUp PgDn scroll · Esc closes ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  ↑↓ scroll · Esc closes · typing goes to the editor
    |}];
  run h "/user-info";
  H.reply ~quiet:true h User_info_shown lloyd;
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_user_info) (params ()) (tag User_info_shown))
    ┌─ About you · given to the model in every session · 74 bytes ─────────────────┐
    │ - Name: Lloyd                                                                │
    │ - Country/region: Australia                                                  │
    │ - Units: metric (°C, km, kg, L)                                              │
    │                                                                              │
    │ /user-info FIELD VALUE sets a field (an empty VALUE clears it), /user-info   │
    │ clear clears them all (~/.prigh/about_you.json on the backend; prigh-web:    │
    │ Settings, About you). The fields:                                            │
    │   name           what to call you                                            │
    │   country        country or region                                           │
    │   time_zone      IANA time zone, e.g. Australia/Sydney                       │
    │   language       language and spelling, e.g. English, Australian spelling    │
    └────────────────────────────── 1–11 of 16 · ↑↓ PgUp PgDn scroll · Esc closes ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  ↑↓ scroll · Esc closes · typing goes to the editor
    |}]
;;

let%expect_test
    "/user-info FIELD VALUE sets one; a refusal puts the command back"
  =
  let h = Test_app.connected ~width:80 ~height:14 () in
  run h "/user-info timezone Australia/Sydney";
  H.reply h (User_info_saved "/user-info timezone Australia/Sydney") lloyd;
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ set_user_info)
      (params (
        (field time_zone)
        (value Australia/Sydney)))
      (tag (User_info_saved "/user-info timezone Australia/Sydney")))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────────────────────────┐
    │ saved (74 bytes): the model has it in new sessions, and in open ones from    │
    │ your next message                                                            │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  run h "/user-info units";
  H.reply h (User_info_saved "/user-info units") lloyd;
  [%expect
    {|
    (Rpc
      (method_ set_user_info)
      (params (
        (field units)
        (value "")))
      (tag (User_info_saved "/user-info units")))
    |}];
  run h "/user-info units furlongs";
  H.reply_error
    h
    (User_info_saved "/user-info units furlongs")
    "units is one of metric, imperial, us_customary (or empty, to clear it), \
     not \"furlongs\"";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ set_user_info)
      (params (
        (field units)
        (value furlongs)))
      (tag (User_info_saved "/user-info units furlongs")))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ units is one of metric, imperial, us_customary (or empty, to clear it), not  │
    │ "furlongs"                                                                   │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > /user-info units furlongs▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  let h = Test_app.connected ~width:80 ~height:14 () in
  run h "/user-info shoe-size 9";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ no field "shoe_size": /user-info FIELD VALUE, where FIELD is one of name,    │
    │ country, time_zone, language, units, date_format, occupation, background,    │
    │ anything_else                                                                │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > /user-info shoe-size 9▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/user-info completes the fields" =
  let h = Test_app.connected ~width:80 ~height:24 () in
  String.iter "/user-info " ~f:(fun c ->
    H.step ~quiet:true h (Key (Key.char c)));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ name         what to call you
      country      country or region
      time_zone    IANA time zone, e.g. Australia/Sydney
      language     language and spelling, e.g. English, Australian spelling
      units        metric, imperial or us_customary
      date_format  dmy, mdy or iso
      occupation   occupation or role
      background   background and expertise
      ↕ 1–8 of 10
    ────────────────────────────────────────────────────────────────────────────────
    > /user-info ▏
    deepseek-flash  ctx:0.1%/1.0M  $0.01  Tab accepts · Enter as typed · Esc hides
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
    │ clear everything you set about yourself? The model is no longer given it     │
    │ (y/n)                                                                        │
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
    ─────────────────────────────────────────── kept what you set about yourself ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  run h "/user-info clear";
  H.key h (Key.char 'y');
  H.reply h (User_info_saved "") empty;
  H.show h;
  [%expect
    {|
    (Rpc (method_ set_user_info) (params ((form ()))) (tag (User_info_saved "")))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────────────────────────┐
    │ cleared what you set about yourself: open sessions are told with your next   │
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
            {|{"body":"hello","notes":[{"kind":"user_info","title":"About you: the model was given your details","text":"[User info: ...]"}]}|})
         ~f:P.Origin.of_json)
  in
  print_s [%sexp (origin : P.Origin.t)];
  List.iter origin.notes ~f:(fun n ->
    printf "%s %s\n" (Injected_note.glyph n.kind) n.title);
  [%expect
    {|
    ((body hello)
     (notes ((
       (kind  User_info)
       (title "About you: the model was given your details")
       (text  "[User info: ...]")))))
    ☺ About you: the model was given your details
    |}]
;;
