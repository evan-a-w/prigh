open! Core
open! Expect_test_helpers_core
open Fixtures
module H = Test_app.H
module P = Prigh_protocol

let numbered ?partial ?from ~seq ~block delta : P.Event.t =
  Message_update
    { stream = 7
    ; from = Option.value from ~default:seq
    ; seq
    ; block
    ; delta
    ; partial
    }
;;

let assistant_with content : P.Message.Assistant.t = { partial with content }

let start h =
  H.event h (State (state ~running:true ()));
  H.event h (Message_start (Assistant partial))
;;

let pieces =
  [ P.Delta.Thinking_delta "let me "
  ; Thinking_delta "look"
  ; Text_delta "Sure, here"
  ; Text_delta " they are:\nfirst"
  ; Text_delta " line"
  ]
;;

let block_of = function
  | P.Delta.Thinking_delta _ -> 0
  | _ -> 1
;;

let%expect_test "numbered deltas show what unnumbered updates showed" =
  let screen events =
    let h = Test_app.connected () in
    start h;
    List.iter events ~f:(H.event h);
    H.show h
  in
  screen (List.map pieces ~f:(fun d -> P.Event.Message_update (unnumbered d)));
  let unnumbered = Expect_test_helpers_core.expect_test_output () in
  screen
    (List.mapi pieces ~f:(fun i d -> numbered ~seq:(i + 1) ~block:(block_of d) d));
  let numbered = Expect_test_helpers_core.expect_test_output () in
  print_s [%message (String.equal unnumbered numbered : bool)];
  print_endline numbered;
  [%expect {|
    ("String.equal unnumbered numbered" true)


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┆ let me look
      Sure, here they are:
      first line
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  $0.01  ⠋ writing · Esc aborts · Enter steers
    |}]
;;

let%expect_test "a gap asks for a resync once; the whole message repairs it" =
  let h = Test_app.connected () in
  start h;
  H.event h (numbered ~seq:1 ~block:0 (Text_delta "Hello "));
  print_endline "-- seq 3 after 1: a gap";
  H.event h (numbered ~seq:3 ~block:0 (Text_delta "lost "));
  print_endline "-- still missing: not asked again, nothing applied";
  H.event h (numbered ~seq:4 ~block:0 (Text_delta "more "));
  H.show h;
  print_endline "-- the resync";
  H.event
    h
    (numbered
       ~seq:5
       ~block:0
       ~partial:(assistant_with [ Text "Hello world, again " ])
       (Text_delta "again "));
  print_endline "-- merged deltas after it";
  H.event h (numbered ~from:6 ~seq:7 ~block:0 (Text_delta "and on"));
  print_endline "-- one already had";
  H.event h (numbered ~seq:7 ~block:0 (Text_delta "and on"));
  H.show h;
  [%expect {|
    -- seq 3 after 1: a gap
    (Rpc (method_ resync) (params ()) (tag Ignore))
    -- still missing: not asked again, nothing applied




      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      Hello
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  $0.01  ⠋ writing · Esc aborts · Enter steers
    -- the resync
    -- merged deltas after it
    -- one already had




      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      Hello world, again and on
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  $0.01  ⠋ writing · Esc aborts · Enter steers
    |}]
;;

let%expect_test "joining mid-stream: the first update has the message so far" =
  let h = Test_app.connected () in
  H.event h (State (state ~running:true ()));
  H.event
    h
    (numbered
       ~seq:40
       ~block:1
       ~partial:(assistant_with [ Thinking "hmm"; Text "Half way" ])
       (Text_delta "way"));
  H.event h (numbered ~seq:41 ~block:1 (Text_delta " there"));
  H.show h;
  [%expect {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┆ hmm
      Half way there
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  $0.01  ⠋ writing · Esc aborts · Enter steers
    |}]
;;
