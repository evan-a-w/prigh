open! Core
open! Expect_test_helpers_core
open Prigh_ui
open Fixtures
module H = Test_app.H

let transcript_sexp (h : H.t) = [%sexp (h.model.transcript : Transcript.t)]

let%expect_test "/btw while running: streamed box, Esc dismisses without \
                 aborting, transcript untouched"
  =
  let h = Test_app.connected ~height:16 () in
  H.event h (State (state ~running:true ()));
  H.event h (Message_update { partial; delta = Text_delta "Working on it" });
  let before = transcript_sexp h in
  H.keys h "/btw which file are you editing?";
  H.enter h;
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ btw)
      (params (
        (question "which file are you editing?")
        (btw_id   btw-1)))
      (tag (Btw btw-1)))





    prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    > earlier question
    earlier answer
    Working on it
    ┌─ btw ────────────────────────────────────────────────────┐
    │ ? which file are you editing?                            │
    │ answering… · Esc to dismiss                              │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  ⠋ working · Esc closes btw · Enter steers
    |}];
  H.event h (Btw_delta { btw_id = "btw-1"; delta = "It is `app.ml`, " });
  H.event h (Btw_delta { btw_id = "btw-1"; delta = "around **line 40**." });
  H.event h (Btw_delta { btw_id = "other"; delta = "ignored" });
  H.show h;
  [%expect
    {|
    prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    > earlier question
    earlier answer
    Working on it
    ┌─ btw ────────────────────────────────────────────────────┐
    │ ? which file are you editing?                            │
    │ It is app.ml, around line 40.                            │
    │ answering… · Esc to dismiss                              │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  ⠋ working · Esc closes btw · Enter steers
    |}];
  (* Esc cancels the in-flight call and closes the box; no abort. *)
  H.esc h;
  H.show h;
  [%expect
    {|
    (Rpc (method_ btw_cancel) (params ((btw_id btw-1))) (tag Ignore))









    prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    > earlier question
    earlier answer
    Working on it
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  ⠋ working · Esc aborts · Enter steers
    |}];
  H.reply_error h (Btw "btw-1") "cancelled";
  printf "transcript unchanged: %b\n" (Sexp.equal before (transcript_sexp h));
  [%expect {| transcript unchanged: true |}];
  (* The next Esc has its usual meaning. *)
  H.esc h;
  [%expect {| (Rpc (method_ abort) (params ()) (tag Abort_done)) |}]
;;

let%expect_test "/btw: a newer question replaces the box; final reply; errors" =
  let h = Test_app.connected ~height:16 () in
  H.keys h "/btw first?";
  H.enter h;
  H.keys h "/btw second?";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ btw)
      (params (
        (question first?)
        (btw_id   btw-1)))
      (tag (Btw btw-1)))
    (Rpc (method_ btw_cancel) (params ((btw_id btw-1))) (tag Ignore))
    (Rpc
      (method_ btw)
      (params (
        (question second?)
        (btw_id   btw-2)))
      (tag (Btw btw-2)))
    |}];
  H.reply_error h (Btw "btw-1") "cancelled";
  H.event h (Btw_delta { btw_id = "btw-2"; delta = "partial" });
  H.reply
    h
    (Btw "btw-2")
    {|{"btw_id":"btw-2","text":"Second answer.\n\n- one\n- two","usage":{"input":1,"output":2,"cache_read":0},"cost_usd":0}|};
  H.show h;
  [%expect
    {|
    prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    > earlier question
    earlier answer
    ┌─ btw ────────────────────────────────────────────────────┐
    │ ? second?                                                │
    │ Second answer.                                           │
    │                                                          │
    │ • one                                                    │
    │ • two                                                    │
    │ Esc to dismiss                                           │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  ctx:0.1%/1.0M  $0.01  Esc dismisses btw
    |}];
  (* A finished answer needs no cancel. *)
  H.esc h;
  H.mode h;
  [%expect {| editing |}];
  H.keys h "/btw broken?";
  H.enter h;
  H.reply_error h (Btw "btw-3") "no credentials for deepseek; /login deepseek";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ btw)
      (params (
        (question broken?)
        (btw_id   btw-3)))
      (tag (Btw btw-3)))





    prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    > earlier question
    earlier answer
    ┌─ btw ────────────────────────────────────────────────────┐
    │ ? broken?                                                │
    │ error: no credentials for deepseek; /login deepseek      │
    │ Esc to dismiss                                           │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  ctx:0.1%/1.0M  $0.01  Esc dismisses btw
    |}];
  H.esc h;
  H.keys h "/btw";
  H.enter h;
  H.show h;
  [%expect
    {|
    prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    > earlier question
    earlier answer
    usage: /btw <question>
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/btw: a long answer shows its tail within half the screen" =
  let h = Test_app.connected ~height:16 () in
  H.keys h "/btw count";
  H.enter h;
  H.event
    h
    (Btw_delta
       { btw_id = "btw-1"
       ; delta =
           String.concat
             ~sep:"\n\n"
             (List.init 12 ~f:(fun i -> sprintf "line %d" (i + 1)))
       });
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ btw)
      (params (
        (question count)
        (btw_id   btw-1)))
      (tag (Btw btw-1)))


    prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    > earlier question
    earlier answer
    ┌─ btw ────────────────────────────────────────────────────┐
    │ ? count                                                  │
    │ … 20 lines above                                         │
    │ line 11                                                  │
    │                                                          │
    │ line 12                                                  │
    │ answering… · Esc to dismiss                              │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  ctx:0.1%/1.0M  $0.01  Esc dismisses btw
    |}]
;;

let%expect_test "/bt autocompletes to /btw" =
  let h = Test_app.connected ~height:16 () in
  H.keys h "/bt";
  H.show h;
  [%expect
    {|
    prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > /bt▏
    ▸ /btw <question>                 ask a side question witho…
      /abort                          abort the current run
      /verbosity [quiet|normal|verbose]  set the transcript ver…
      /retry-backend-connection       reconnect to the backend …
    …deepseek-flash  ctx:0.1%/1.0M  Tab/Enter accept · Esc close
    |}]
;;
