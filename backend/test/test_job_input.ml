open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf

let%expect_test "Process.run with an input feed" =
  with_sandbox
  @@ fun t ->
  let feed = Stdin_feed.create () in
  (* Written before the process starts: kept in order. *)
  print_s [%sexp (Stdin_feed.write feed "one\n" ~close:false : unit Or_error.t)];
  let output = Buffer.create 16 in
  Eio.Fiber.both
    (fun () ->
       let exit =
         Process.run
           ~env:t.env
           ~input:feed
           ~on_stdout:(Buffer.add_string output)
           ~prog:"cat"
           ~args:[]
           ()
       in
       print_s [%sexp (exit : Process.Exit.t)])
    (fun () ->
       print_s
         [%sexp (Stdin_feed.write feed "two\n" ~close:true : unit Or_error.t)];
       print_s
         [%sexp
           (Stdin_feed.write feed "three\n" ~close:false : unit Or_error.t)]);
  print_string (Buffer.contents output);
  (* A process that exits without reading its input. *)
  let feed = Stdin_feed.create () in
  ignore
    (Stdin_feed.write feed (String.make 200_000 'x') ~close:false
     : unit Or_error.t);
  print_s
    [%sexp
      (Process.run ~env:t.env ~input:feed ~prog:"true" ~args:[] ()
       : Process.Exit.t)];
  print_s [%sexp (Stdin_feed.write feed "late" ~close:false : unit Or_error.t)];
  let feed = Stdin_feed.create () in
  Stdin_feed.refuse feed "the host cannot";
  print_s [%sexp (Stdin_feed.write feed "x" ~close:false : unit Or_error.t)];
  [%expect
    {|
    (Ok ())
    (Ok ())
    (Error "its stdin is already closed")
    (Exited 0)
    one
    two
    (Exited 0)
    (Error "it has already exited")
    (Error "the host cannot")
    |}]
;;

let call ~id name args =
  Reply.tool_call ~id ~name ~arguments:(Json.to_string (`Object args)) ()
;;

let input ?close ?wait_ms ~id job text =
  call
    ~id
    "job_input"
    ([ "id", `String job; "input", `String text ]
     @ Option.value_map close ~default:[] ~f:(fun c ->
       [ ("close", if c then `True else `False) ])
     @ Option.value_map wait_ms ~default:[] ~f:(fun ms ->
       [ "wait_ms", `Number (Int.to_string ms) ]))
;;

let%expect_test "job_input drives a job on this machine" =
  Test_background_jobs.with_agent
    ~main:
      [ Test_background_jobs.background
          ~id:"c1"
          ~stdin:true
          {|while read line; do echo "got $line"; done; echo bye|}
      ; input ~id:"i1" ~wait_ms:10_000 "j1" "one\n"
      ; input ~id:"i2" ~wait_ms:10_000 ~close:true "j1" "two\n"
      ; input ~id:"i3" "j1" "three\n"
      ; input ~id:"i4" "j9" "x"
      ; Reply.text "done"
      ; Reply.text "noted"
      ]
    []
  @@ fun t agent ->
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  ignore t;
  [%expect
    {|
    state: running=true subagents=[] jobs=[]
    agent_start
    user: go
    assistant:  bash{"command":"while read line; do echo \"got $line\"; done; echo bye","background":true,"stdin":true}
    state: running=true subagents=[] jobs=[j1 running]
    tool_result bash: started job j1: while read line; do echo "got $line"; done; echo bye; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_kill to stop it.
    assistant:  job_input{"id":"j1","input":"one\n","wait_ms":10000}
    tool_result job_input: sent 4 bytes to job j1 | got one
    assistant:  job_input{"id":"j1","input":"two\n","close":true,"wait_ms":10000}
    state: running=true subagents=[] jobs=[j1 exited 0]
    tool_result job_input: sent 4 bytes to job j1 and closed its stdin; it then exited 0 | got two | bye
    state: running=true subagents=[] jobs=[]
    user: [job j1 exited 0] while read line; do echo "got $line"; done; echo bye | got one | got two | bye
    assistant:  job_input{"id":"j1","input":"three\n"}
    tool_result job_input: cannot write to job j1: it has already exited (exited 0); start it again with bash and background
    assistant:  job_input{"id":"j9","input":"x"}
    tool_result job_input: invalid arguments: unknown job "j9"; known: j1
    assistant: done
    agent_end
    state: running=false subagents=[] jobs=[]
    |}]
;;

let%expect_test "without stdin: true a job reads end of input at once" =
  Test_background_jobs.with_agent
    ~main:
      [ Test_background_jobs.background
          ~id:"c1"
          (Test_background_jobs.gated "cat; echo read to the end")
      ; input ~id:"i1" "j1" "x"
      ; Reply.text "noted"
      ; Reply.text "done"
      ]
    []
  @@ fun t agent ->
  Or_error.ok_exn (Agent.prompt agent "go");
  Test_background_subagents.wait_turn agent;
  Test_background_jobs.open_gate t;
  Agent.wait_idle agent;
  [%expect
    {|
    state: running=true subagents=[] jobs=[]
    agent_start
    user: go
    assistant:  bash{"command":"while [ ! -f gate ]; do sleep 0.02; done; cat; echo read to the end","background":true}
    state: running=true subagents=[] jobs=[j1 running]
    tool_result bash: started job j1: while [ ! -f gate ]; do sleep 0.02; done; cat; echo read to…; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_kill to stop it.
    assistant:  job_input{"id":"j1","input":"x"}
    tool_result job_input: cannot write to job j1: it was started without stdin: true, so its stdin is closed; start it again with bash, background: true and stdin: true to send it input
    assistant: noted
    agent_end
    state: running=false subagents=[] jobs=[j1 running]
    state: running=true subagents=[] jobs=[]
    agent_start
    user: [job j1 exited 0] while [ ! -f gate ]; do sleep 0.02; done; cat; echo read to… | read to the end
    assistant: done
    agent_end
    state: running=false subagents=[] jobs=[]
    |}]
;;

let%expect_test "a model without jobs does not get job_input; subagents do" =
  let names tools = List.map tools ~f:Tool.name in
  let all = Tools.all @ Tool_jobs.tools in
  print_s
    [%sexp
      (List.filter (names all) ~f:(String.is_prefix ~prefix:"job_")
       : string list)
    , (Result.map (Tools.for_context ~parent:all ~depth:1 ()) ~f:(fun tools ->
         List.filter (names tools) ~f:(String.is_prefix ~prefix:"job_"))
       : string list Or_error.t)];
  [%expect
    {|
    ((job_status job_output job_input job_kill)
     (Ok (job_status job_output job_input job_kill)))
    |}]
;;

(* The host protocol, played by a test client: a host that announced
   [exec_input] gets the job's exec with [stdin] and its input as
   [tool_exec_input] events; one that did not (an older prigh) makes
   job_input fail with what to do. *)
let host_scenario ~features =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let agent, h =
    Test_rpc.make_server
      t
      ~sw
      ~tools:(Tools.all @ Tool_jobs.tools)
      ~provider:
        (Faux_provider.create
           [ Test_background_jobs.background ~id:"c1" ~stdin:true "cat"
           ; input ~id:"i1" ~wait_ms:10_000 "j1" "one\n"
           ; input ~id:"i2" ~wait_ms:10_000 ~close:true "j1" ""
           ; Reply.text "done"
           ; Reply.text "noted"
           ])
  in
  let sent = Queue.create () in
  let laptop = Rpc_server.connect h.server ~send:(Queue.enqueue sent) in
  let call_as ?(quiet = false) meth params =
    let response =
      Rpc_server.handle
        h.server
        laptop
        (Json.of_string
           (sprintf {|{"id": "r2", "method": "%s", "params": %s}|} meth params))
    in
    if not quiet then print_endline (mask t (Json.to_string response))
  in
  call_as
    ~quiet:true
    "hello"
    (Test_rpc.hello_params
       ~session:agent
       (sprintf
          {|"name": "laptop", "tools": true, "cwd": "%s"%s|}
          t.dir
          (if features then {|, "features": ["exec_input"]|} else "")));
  Test_rpc.call t h ~params:{|{"text": "go"}|} "prompt";
  let exec = Test_rpc.wait_for_exec h laptop sent in
  let exec_id =
    Json.string_exn (Option.value_exn (Json.member "exec_id" exec))
  in
  print_endline (mask t (Json.to_string exec));
  Queue.clear sent;
  let clock = Eio.Stdenv.clock t.env in
  let rec next_input n =
    match
      Queue.find sent ~f:(fun json ->
        match Json.member "event" json with
        | Some (`String "tool_exec_input") -> true
        | _ -> false)
    with
    | Some json ->
      Queue.filter_inplace sent ~f:(fun j -> not (phys_equal j json));
      Some json
    | None when n = 0 -> None
    | None ->
      Eio.Time.sleep clock 0.01;
      next_input (n - 1)
  in
  (match next_input 300 with
   | None -> print_endline "(no input reached the host)"
   | Some json ->
     print_endline (mask t (Json.to_string json));
     call_as
       "tool_exec_output"
       (sprintf {|{"exec_id": "%s", "chunk": "one\n"}|} exec_id);
     Option.iter (next_input 300) ~f:(fun json ->
       print_endline (mask t (Json.to_string json))));
  call_as
    "tool_exec_result"
    (sprintf {|{"exec_id": "%s", "text": "one\n", "is_error": false}|} exec_id);
  Agent.wait_idle agent;
  List.iter (Agent.messages agent) ~f:(fun m ->
    print_endline (mask t (Test_background_subagents.message_line m)))
;;

let%expect_test "job_input on a client tool host" =
  host_scenario ~features:true;
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{}}
    (host answered $instructions)
    {"type":"event","event":"tool_exec","host":"client-2","session_id":"<id>","exec_id":"<id>/j1-1","call_id":"j1","name":"bash","arguments":{"command":"cat","timeout":2592000},"cwd":"$DIR","stdin":true}
    {"type":"event","event":"tool_exec_input","host":"client-2","exec_id":"<id>/j1-1","data":"one\n","close":false}
    {"type":"response","id":"r2","ok":true,"result":{}}
    {"type":"event","event":"tool_exec_input","host":"client-2","exec_id":"<id>/j1-1","data":"","close":true}
    {"type":"response","id":"r2","ok":true,"result":{}}
    user: go
    assistant:  bash{"command":"cat","background":true,"stdin":true}
    tool_result bash: started job j1: cat; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_kill to stop it.
    assistant:  job_input{"id":"j1","input":"one\n","wait_ms":10000}
    tool_result job_input: sent 4 bytes to job j1 | one
    assistant:  job_input{"id":"j1","input":"","close":true,"wait_ms":10000}
    tool_result job_input: sent 0 bytes to job j1 and closed its stdin; it then exited 0 | (no output yet; job_output shows what comes later)
    user: [job j1 exited 0] cat | one
    assistant: done
    |}]
;;

let%expect_test "job_input on a tool host that predates it" =
  host_scenario ~features:false;
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{}}
    (host answered $instructions)
    {"type":"event","event":"tool_exec","host":"client-2","session_id":"<id>","exec_id":"<id>/j1-1","call_id":"j1","name":"bash","arguments":{"command":"cat","timeout":2592000},"cwd":"$DIR"}
    (no input reached the host)
    {"type":"response","id":"r2","ok":true,"result":{}}
    user: go
    assistant:  bash{"command":"cat","background":true,"stdin":true}
    tool_result bash: started job j1: cat; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_kill to stop it.
    assistant:  job_input{"id":"j1","input":"one\n","wait_ms":10000}
    tool_result job_input: cannot write to job j1: tool host "laptop" runs an older prigh that cannot pass input to jobs: update prigh there and restart its TUI or tool-host, or start the job again with its input in the command (e.g. a here-document)
    assistant:  job_input{"id":"j1","input":"","close":true,"wait_ms":10000}
    tool_result job_input: cannot write to job j1: tool host "laptop" runs an older prigh that cannot pass input to jobs: update prigh there and restart its TUI or tool-host, or start the job again with its input in the command (e.g. a here-document)
    assistant: done
    user: [job j1 exited 0] cat | one
    assistant: noted
    |}]
;;

(* The worker side: an exec with [stdin] keeps its stdin open for [input]
   lines. *)
let%expect_test "stdio worker: input for an exec's stdin" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let w = Test_tool_host.Worker.start t ~sw in
  Test_tool_host.Worker.send
    w
    (sprintf
       {|{"type":"exec","exec_id":"e1","name":"bash","arguments":{"command":"while read l; do echo \"got $l\"; done"},"cwd":"%s","stdin":true}|}
       t.dir);
  Test_tool_host.Worker.send
    w
    {|{"type":"input","exec_id":"e1","data":"a\n","close":false}|};
  Test_tool_host.Worker.send
    w
    {|{"type":"input","exec_id":"nope","data":"x","close":true}|};
  Test_tool_host.Worker.send
    w
    {|{"type":"input","exec_id":"e1","data":"b\n","close":true}|};
  let rec until_result () =
    match Test_tool_host.Worker.read_line w with
    | None -> ()
    | Some line ->
      (* Output chunks split arbitrarily: the result has it all. *)
      if String.is_substring line ~substring:{|"type":"result"|}
      then print_endline (mask t line)
      else until_result ()
  in
  until_result ();
  Test_tool_host.Worker.close w;
  ignore (Test_tool_host.Worker.read_line w : string option);
  [%expect
    {|
    {"type":"result","exec_id":"e1","text":"got a\ngot b\n","is_error":false}
    (worker finished)
    |}]
;;
