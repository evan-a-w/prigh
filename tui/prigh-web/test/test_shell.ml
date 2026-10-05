open! Core

let shell_events =
  [ {|{"event":"tool_start","call":{"id":"shell-0","name":"shell","arguments":"{\"command\":\"echo hi\"}"}}|}
  ; {|{"event":"tool_output","call_id":"shell-0","chunk":"hi\n"}|}
  ]
;;

let%expect_test "!command, !!command and !&command" =
  let h = Harness.create () in
  let send text =
    Harness.act h (Set_draft text);
    Harness.act h Send
  in
  send "!echo hi";
  send "!!ls";
  send "!&make test";
  send "!";
  [%expect
    {|
    (Save_history ("!echo hi"))
    Scroll_to_bottom
    (Rpc (method_ shell) (params ((command "echo hi") (add_to_context true)))
     (tag Show_error))
    (Save_history (!!ls "!echo hi"))
    Scroll_to_bottom
    (Rpc (method_ shell) (params ((command ls) (add_to_context false)))
     (tag Show_error))
    (Save_history ("!&make test" !!ls "!echo hi"))
    Scroll_to_bottom
    (Rpc (method_ shell) (params ((command "make test") (background true)))
     (tag Job_started))
    (Save_history (! "!&make test" !!ls "!echo hi"))
    Scroll_to_bottom
    |}];
  List.iter shell_events ~f:(Harness.event h);
  Harness.text h ~selector:".msg.shell";
  [%expect
    {|
    bash echo hi
    hi
    |}];
  Harness.event
    h
    {|{"event":"tool_end","call":{"id":"shell-0","name":"shell","arguments":"{\"command\":\"echo hi\"}"},"result":{"role":"tool_result","tool_call_id":"shell-0","tool_name":"shell","text":"hi\n","is_error":false}}|};
  (* Added to the context: the same output as a user message, not shown
     twice. *)
  Harness.event
    h
    {|{"event":"message_start","message":{"role":"user","text":"$ echo hi\nhi\n"}}|};
  Harness.text h ~selector:".entries";
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    ✓ bash echo hi
    hi
    |}];
  Harness.act h (Reply (Job_started, Ok (Jsonaf.of_string {|{"job_id":"j1"}|})));
  Harness.text h ~selector:".toast";
  [%expect
    {|
    (Expire_toast (id 1) (after_ms 4000))
    Type a command after ! to run it.
    Started job j1: its result reaches the agent when it exits.
    |}];
  (* While the agent works, only a background job can start. *)
  Harness.event
    h
    (sprintf
       {|{"event":"state","state":%s}|}
       (Harness.state_json ~fields:[ "running", `True ] ()));
  send "!ls";
  Harness.text h ~selector:".toast";
  [%expect
    {|
    (Save_history (!ls ! "!&make test" !!ls "!echo hi"))
    Scroll_to_bottom
    Type a command after ! to run it.
    Started job j1: its result reaches the agent when it exits.
    Wait for the agent to finish (or Esc to stop it) before running !commands; !&command starts a background job now.
    |}]
;;
