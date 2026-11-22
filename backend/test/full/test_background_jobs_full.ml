open! Core
open! Prigh
open Tool_test_helpers
open! Test_background_jobs

let%expect_test "job_output pages back; the buffer keeps the end" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let background =
    Background_tasks.create
      ~env:t.env
      ~sw
      ~first_subagent_id:1
      ~first_job_id:1
      ()
  in
  let context = Tool.Context.create ~background ~env:t.env ~cwd:t.dir () in
  let call tool args =
    let result = Tool.execute tool context (Json.of_string args) in
    print_endline (clean t result.text)
  in
  let tool name =
    List.find_exn Tool_jobs.tools ~f:(fun tool ->
      String.equal (Tool.name tool) name)
  in
  (* What the agent is sent once they have all finished. *)
  let reports () =
    Background_tasks.wait_all background;
    List.iter (Background_tasks.take_undelivered background) ~f:(fun task ->
      print_endline (clean t (Background_tasks.Task.report task)))
  in
  call Tool_bash.tool {|{"command": "seq 1 100", "background": true}|};
  reports ();
  call (tool "job_output") {|{"id": "j1", "lines": 3}|};
  call (tool "job_output") {|{"id": "j1", "lines": 3, "offset": 10}|};
  call (tool "job_output") {|{"id": "j1", "lines": 3, "offset": 200}|};
  call (tool "job_output") {|{"id": "j7"}|};
  call Tool_bash.tool {|{"command": "exit 1", "background": true}|};
  call
    Tool_bash.tool
    {|{"command": "sleep 5", "background": true, "timeout": 1}|};
  call Tool_bash.tool {|{"command": "kill -9 $$", "background": true}|};
  reports ();
  [%expect
    {|
    started job j1: seq 1 100; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_kill to stop it.
    [job j1 exited 0] seq 1 100
    [last 40 of 100 lines; job_output shows more]
    61
    62
    63
    64
    65
    66
    67
    68
    69
    70
    71
    72
    73
    74
    75
    76
    77
    78
    79
    80
    81
    82
    83
    84
    85
    86
    87
    88
    89
    90
    91
    92
    93
    94
    95
    96
    97
    98
    99
    100

    [job j1 exited 0 after Ns, 292 bytes; lines 98-100 of 100] seq 1 100
    98
    99
    100
    [job j1 exited 0 after Ns, 292 bytes; lines 88-90 of 100] seq 1 100
    88
    89
    90
    [job j1 exited 0 after Ns, 292 bytes; no lines in range; 100 retained] seq 1 100
    invalid arguments: unknown job "j7"; known: j1
    started job j2: exit 1; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_kill to stop it.
    started job j3: sleep 5; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_kill to stop it.
    started job j4: kill -9 $$; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_kill to stop it.
    [job j2 exited 1] exit 1
    [job j3 timed out after Ns] sleep 5
    [job j4 killed by sigkill] kill -9 $$
    |}];
  let tail = Output_tail.create ~capacity:10 () in
  List.iter
    [ "one\ntwo\n"; "three\nfour\n"; "five\nsix\nseven" ]
    ~f:(fun chunk ->
      Output_tail.add tail chunk;
      print_s
        [%sexp
          { total_bytes = (Output_tail.total_bytes tail : int)
          ; lines = (Output_tail.lines tail : string list)
          ; last = (Output_tail.last_line tail : string option)
          }]);
  [%expect
    {|
    ((total_bytes 8) (lines (one two)) (last (two)))
    ((total_bytes 19) (lines (four)) (last (four)))
    ((total_bytes 33) (lines (six seven)) (last (seven)))
    |}]
;;

let%expect_test "jobs on a client tool host: output, list, kill, disconnect" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let host_dir = Filename.concat t.dir "host" in
  Core_unix.mkdir_p host_dir;
  let agent, h =
    Test_rpc.make_server
      ~token:"sekrit"
      t
      ~sw
      ~provider:
        (Faux_provider.create
           [ background
               ~id:"c1"
               "basename $PWD; echo from-host; while [ ! -f gate ]; do sleep \
                0.02; done; echo after-gate"
           ; Reply.text "started"
           ; Reply.text "j1 noted"
           ; Reply.text "j2 noted"
           ; Reply.text "j3 noted"
           ])
  in
  rpc t h "hello" {|{"token": "sekrit"}|};
  let listener = Test_tool_host.Listener.start t ~sw h.server in
  let host =
    Test_tool_host.Host.start
      t
      ~sw
      ~port:listener.port
      ~host_id:"host-box"
      ~token:(Some "sekrit")
      ~cwd:host_dir
  in
  Test_tool_host.Host.wait_logs t host 1;
  rpc t h "set_active_host" {|{"host": "host-box"}|};
  rpc t h "prompt" {|{"text": "build"}|};
  Test_background_subagents.wait_turn agent;
  ignore
    (eventually t (fun () ->
       match Agent.job_output agent ~job_id:"j1" ~lines:10 with
       | Ok text -> String.is_substring text ~substring:"\nfrom-host"
       | Error _ -> false)
     : bool);
  rpc t h "job_output" {|{"job_id": "j1", "lines": 5}|};
  rpc t h "list_jobs" "{}";
  print_endline
    (Json.to_string
       (Option.value_exn
          (Json.member "jobs" (Rpc_json.state (Agent.state agent)))));
  write t "host/gate" "";
  wait_idle t agent;
  print_tail t agent 2;
  [%expect
    {|
    {"type":"response","id":"r","ok":true,"result":{"client_id":"client-1","host_id":"client-1","namespace":null,"user":null,"superuser":false,"features":["pasted_text"],"state":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":null,"session_description":null,"cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"thinking_options":["off","default","low","high","max"],"thinking_default":"high","supports_images":true,"legacy":false,"image_limit":{"max_bytes":3538944,"max_dimension":8000},"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"default","running":false,"message_count":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0,"active_host":"backend","active_host_name":"<host>","hosts":[{"id":"backend","name":"<host>","cwd":"$DIR","session_id":null,"session_name":null,"home":"$HOME"}],"subagents":[],"jobs":[],"budget_usd":null,"goal":null,"reminders":[],"resume_at_ms":null,"approval_mode":"auto","plan_mode":false,"chat_mode":false,"no_host":null,"computer_control":null,"plan":null,"session_rules":[]}}}
    connected to 127.0.0.1:PORT as client-2
    {"type":"response","id":"r","ok":true,"result":{"cwd":"$DIR/host","created":false}}
    {"type":"response","id":"r","ok":true,"result":{}}
    {"type":"response","id":"r","ok":true,"result":{"text":"[job j1 running after Ns, 15 bytes; lines 1-2 of 2] basename $PWD; echo from-host; while [ ! -f gate ]; do slee…\nhost\nfrom-host"}}
    {"type":"response","id":"r","ok":true,"result":[{"id":"j1","command":"basename $PWD; echo from-host; while [ ! -f gate ]; do sleep 0.02; done; echo after-gate","running":true,"exit":null,"delivered":false,"elapsed":<t>,"bytes":15,"last_line":"from-host"}]}
    [{"id":"j1","command":"basename $PWD; echo from-host; while [ ! -f gate ]; do sleep 0.02; done; echo after-gate","running":true,"exit":null}]
    user: [job j1 exited 0] basename $PWD; echo from-host; while [ ! -f gate ]; do slee… | host | from-host | after-gate
    assistant: j1 noted
    |}];
  (* [!&cmd]: a job started by the user, then killed. *)
  rpc
    t
    h
    "shell"
    {|{"command": "sleep 30 & echo $! > sleep.pid; echo sleeping; wait", "background": true}|};
  (* Its output comes from the host: killed before it arrives, the report
     would not have it. *)
  ignore
    (eventually t (fun () ->
       match Agent.job_output agent ~job_id:"j2" ~lines:10 with
       | Ok text -> String.is_substring text ~substring:"\nsleeping"
       | Error _ -> false)
     : bool);
  rpc t h "kill_job" {|{"job_id": "j2"}|};
  rpc t h "kill_job" {|{"job_id": "j9"}|};
  wait_idle t agent;
  let pid = Int.of_string (String.strip (read t "host/sleep.pid")) in
  ignore (eventually t (fun () -> not (pid_alive pid)) : bool);
  printf "sleep alive: %b\n" (pid_alive pid);
  print_tail t agent 2;
  [%expect
    {|
    {"type":"response","id":"r","ok":true,"result":{"job_id":"j2"}}
    {"type":"response","id":"r","ok":true,"result":{}}
    {"type":"response","id":"r","ok":false,"error":"unknown job \"j9\"; known: j1, j2"}
    sleep alive: false
    user: [job j2 killed] sleep 30 & echo $! > sleep.pid; echo sleeping; wait | sleeping
    assistant: j2 noted
    |}];
  rpc
    t
    h
    "shell"
    {|{"command": "echo waiting; while [ ! -f gate2 ]; do sleep 0.02; done", "background": true}|};
  ignore
    (eventually t (fun () ->
       match Agent.job_output agent ~job_id:"j3" ~lines:10 with
       | Ok text -> String.is_substring text ~substring:"\nwaiting"
       | Error _ -> false)
     : bool);
  (* The connection drops: the host reconnects with the job still running,
     and it carries on there. *)
  Test_tool_host.Listener.drop listener;
  Test_tool_host.Host.wait_logs t host 4;
  write t "host/gate2" "";
  wait_idle t agent;
  print_tail t agent 2;
  [%expect
    {|
    {"type":"response","id":"r","ok":true,"result":{"job_id":"j3"}}
    tool_exec_result failed: no tool execution "<id>/j2-3"
    disconnected from 127.0.0.1:PORT
    retrying in 50ms
    connected to 127.0.0.1:PORT as client-3
    user: [job j3 exited 0] echo waiting; while [ ! -f gate2 ]; do sleep 0.02; done | waiting
    assistant: j3 noted
    |}]
;;
