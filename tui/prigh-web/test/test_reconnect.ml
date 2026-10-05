open! Core
open Prigh_web

(* The backend goes away: the page retries with backoff behind a banner
   (no toast per attempt), then rejoins the session and reloads it. *)
let%expect_test "reconnecting after the backend goes away" =
  let h = Harness.create () in
  Harness.event
    h
    {|{"event":"message_start","message":{"role":"user","text":"hello"}}|};
  [%expect {| |}];
  Harness.act h Backend_closed;
  Harness.text h ~selector:".banner";
  [%expect
    {|
    (Reconnect (generation 1) (delay_ms 0) (session (s1)))
    Connection lost: reconnecting…
    |}];
  Harness.act h Backend_closed;
  for _ = 1 to 3 do
    Harness.act h (Reply (Reconnect 1, Error "cannot connect"))
  done;
  Harness.text h ~selector:".toasts";
  [%expect
    {|
    (Reconnect (generation 1) (delay_ms 250) (session (s1)))
    (Reconnect (generation 1) (delay_ms 500) (session (s1)))
    (Reconnect (generation 1) (delay_ms 1000) (session (s1)))
    |}];
  Harness.act h (Reply (Reconnect 0, Error "stale"));
  Harness.act h (Reply (Reconnect 1, Ok (Jsonaf.of_string "{}")));
  Harness.reply h "get_state" (Harness.state_json ());
  Harness.reply h "get_messages" {|[{"role":"user","text":"hello"}]|};
  Harness.text h ~selector:".chat";
  let m = Harness.model h in
  print_s
    [%message
      ""
        ~connection:(m.connection : App.Connection.t)
        ~toasts:(List.length m.toasts : int)];
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 4000))
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Set_url_session s1)
    (Rpc (method_ get_messages) (params ()) (tag Messages))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    hello
    ((connection Connected) (toasts 1))
    |}]
;;
