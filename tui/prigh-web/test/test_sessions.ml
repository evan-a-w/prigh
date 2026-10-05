open! Core

(* The backend moves the client to the other session without telling it:
   the page asks for the new state, which resets the transcript. *)
let%expect_test "new session and switching session reload the state" =
  let h = Harness.create () in
  [%expect
    {|
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Set_url_session s1)
    (Rpc (method_ get_messages) (params ()) (tag Messages))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    |}];
  Harness.event
    h
    {|{"event":"message_start","message":{"role":"user","text":"hello"}}|};
  Harness.act h New_session;
  Harness.reply h "new_session" "{}";
  Harness.reply
    h
    "get_state"
    (Harness.state_json ~fields:[ "session_id", `String "s2" ] ());
  Harness.text h ~selector:".chat";
  [%expect
    {|
    (Rpc (method_ new_session) (params ()) (tag Reload_state))
    (Rpc (method_ get_state) (params ()) (tag State))
    (Set_url_session s2)
    (Rpc (method_ get_messages) (params ()) (tag Messages))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    What are we building? Ask a question, paste a screenshot, or point prigh at a file with @path.
    |}];
  Harness.act h (Switch_session "/sessions/s1.jsonl");
  Harness.reply h "switch_session" "{}";
  Harness.reply h "get_state" (Harness.state_json ());
  Harness.reply h "get_messages" {|[{"role":"user","text":"hello"}]|};
  Harness.text h ~selector:".chat";
  [%expect
    {|
    (Rpc (method_ switch_session) (params ((path /sessions/s1.jsonl)))
     (tag Reload_state))
    (Rpc (method_ get_state) (params ()) (tag State))
    (Set_url_session s1)
    (Rpc (method_ get_messages) (params ()) (tag Messages))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    hello
    |}]
;;
