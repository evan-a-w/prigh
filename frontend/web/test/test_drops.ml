open! Core
open Prigh_web

(* A connection that keeps going while the session loads (a backend that
   cannot serve it in time, a network too slow to carry it) must not be
   retried at once forever, with a "Reconnected" each time: each loss waits
   longer, then the page stops and says what to do. *)

let user text = sprintf {|{"role":"user","text":"%s"}|} text

let session () =
  let h = Harness.create () in
  Harness.quiet h (fun () ->
    Harness.act
      h
      (Reply
         ( Harness.newest "s1"
         , Ok (Jsonaf.of_string (Harness.page ("[" ^ user "hi" ^ "]"))) )));
  h
;;

let show h =
  let m = Harness.model h in
  Harness.text h ~selector:".banner";
  Harness.text h ~selector:".status";
  Harness.text h ~selector:".toasts";
  print_s
    [%message
      ""
        ~connection:(m.connection : App.Connection.t)
        ~lost:(m.drops.lost : int)]
;;

(* Connected again; the state comes, the connection goes before the
   session's messages do. *)
let lost_while_loading h ~generation =
  Harness.quiet h (fun () ->
    Harness.act
      h
      (Reply (Reconnect generation, Ok (Jsonaf.of_string {|{"client_id":"c2"}|})));
    Harness.reply h "get_state" (Harness.state_json ()));
  Harness.act h Backend_closed;
  Harness.quiet h (fun () ->
    List.iter
      [ "get_messages_page"
      ; "get_pending"
      ; "list_sessions"
      ; "list_subagents"
      ; "list_jobs"
      ; "list_models"
      ; "auth_status"
      ; "get_config"
      ]
      ~f:(fun method_ -> Harness.fail h method_ "backend closed"))
;;

let%expect_test
    "a connection lost while the session loads backs off, then stops"
  =
  let h = session () in
  (* Lost once it had loaded: back at once. *)
  Harness.act h Backend_closed;
  [%expect {| (Reconnect (generation 1) (delay_ms 0) (session (s1))) |}];
  for generation = 2 to App.Drops.give_up_after do
    lost_while_loading h ~generation:(generation - 1)
  done;
  [%expect
    {|
    (Reconnect (generation 2) (delay_ms 250) (session (s1)))
    (Reconnect (generation 3) (delay_ms 500) (session (s1)))
    (Reconnect (generation 4) (delay_ms 1000) (session (s1)))
    (Reconnect (generation 5) (delay_ms 2000) (session (s1)))
    (Reconnect (generation 6) (delay_ms 4000) (session (s1)))
    |}];
  show h;
  [%expect
    {|
    Connection lost: reconnecting (attempt 6, next in 4.0s)… (Retry now)
    Reconnecting…
    ((connection (Reconnecting (attempt 5) (generation 6))) (lost 5))
    |}];
  (* Once more: it stops, and says what to do. *)
  lost_while_loading h ~generation:6;
  show h;
  [%expect
    {|
    The backend keeps dropping the connection while loading this session: open another session, or reload. (Retry now) (Another session) (Reload)
    Stopped reconnecting
    ((connection (Reconnecting (attempt 6) (generation 7))) (lost 6))
    |}];
  (* It does not start again by itself. *)
  Harness.act h (Network true);
  Harness.act h (Page_hidden true);
  Harness.act h (Page_hidden false);
  [%expect {| |}];
  Harness.click h ~nth:1 ~selector:".banner button";
  Harness.click h ~nth:2 ~selector:".banner button";
  [%expect
    {|
    (Reload_page (keep_session false))
    (Reload_page (keep_session false))
    (Reload_page (keep_session true))
    (Reload_page (keep_session true))
    |}];
  (* Retrying is one more attempt: this time the session comes back. *)
  Harness.click h ~selector:".banner button";
  [%expect
    {|
    Retry_connection
    (Expire_toast (id 0) (after_ms 2500))
    (Reconnect (generation 8) (delay_ms 0) (session (s1)))
    |}];
  Harness.quiet h (fun () ->
    Harness.act
      h
      (Reply (Reconnect 8, Ok (Jsonaf.of_string {|{"client_id":"c3"}|})));
    Harness.reply h "get_state" (Harness.state_json ()));
  show h;
  [%expect
    {|
    (0%) ↑0 ↓0 $0.0000 Ready
    Reconnecting now…
    ((connection Connected) (lost 5))
    |}];
  Harness.reply h "get_messages_page" (Harness.page ("[" ^ user "hi" ^ "]"));
  show h;
  [%expect
    {|
    (Expire_toast (id 1) (after_ms 2500))
    (0%) ↑0 ↓0 $0.0000 Ready
    Reconnecting now…
    Reconnected
    ((connection Connected) (lost 0))
    |}];
  (* Back to normal: a loss now retries at once. *)
  Harness.act h Backend_closed;
  [%expect {| (Reconnect (generation 9) (delay_ms 0) (session (s1))) |}]
;;

(* Failing to connect at all (the backend restarting) is the backoff of one
   outage: it never gives up. *)
let%expect_test "failing to connect keeps trying" =
  let h = session () in
  Harness.act h Backend_closed;
  Harness.quiet h (fun () ->
    for _ = 1 to 20 do
      Harness.act h (Reply (Reconnect 1, Error "cannot connect"))
    done);
  Harness.act h (Reply (Reconnect 1, Error "cannot connect"));
  show h;
  [%expect
    {|
    (Reconnect (generation 1) (delay_ms 0) (session (s1)))
    (Reconnect (generation 1) (delay_ms 30000) (session (s1)))
    Connection lost: reconnecting (attempt 22, next in 30.0s)… (Retry now)
    Reconnecting…
    ((connection (Reconnecting (attempt 21) (generation 1))) (lost 0))
    |}]
;;
