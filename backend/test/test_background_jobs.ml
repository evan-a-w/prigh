open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Gates = Routed_provider.Gates
module Json = Jsonaf

let message_line = Test_background_subagents.message_line
let print_messages = Test_background_subagents.print_messages
let eventually = Test_tool_host.eventually
let seconds_re = Re.compile (Re.Perl.re {|\b[0-9]+s\b|})
let clean t s = Re.replace_string seconds_re ~by:"Ns" (mask t s)

let summaries (l : Background_tasks.Summary.t list) =
  String.concat
    ~sep:"; "
    (List.map l ~f:(fun s ->
       sprintf
         "%s %s"
         s.id
         (match s.status with
          | None -> "running"
          | Some status -> status)))
;;

let log_events t agent =
  let last_state = ref "" in
  Agent.subscribe agent ~f:(fun (event : Agent.Event.t) ->
    match event with
    | Loop Agent_start -> print_endline "agent_start"
    | Loop (Agent_end _) -> print_endline "agent_end"
    | Loop (Message_end m) -> print_endline (clean t (message_line m))
    | State_changed s ->
      let line =
        sprintf
          "state: running=%b subagents=[%s] jobs=[%s]"
          s.running
          (summaries s.subagents)
          (summaries s.jobs)
      in
      if not (String.equal line !last_state)
      then (
        last_state := line;
        print_endline line)
    | _ -> ())
;;

(* A [hold] tool that blocks until [promise] resolves or the run is
   aborted. *)
let hold_tool promise ~started =
  { Tool.spec =
      { Tool_spec.name = "hold"
      ; description = "wait"
      ; parameters = Tool_args.schema []
      ; parallel_safe = false
      ; destructive = false
      ; on_host = false
      }
  ; run =
      (fun context _ ->
        started := true;
        match
          Cancellation.protect context.cancel ~f:(fun () ->
            Eio.Promise.await promise)
        with
        | Some () -> Tool.Result.ok "held"
        | None -> Tool.Result.error "[cancelled]")
  }
;;

let tools ~provider =
  Tools.all
  @ Test_background_subagents.subagent_tools ~provider
  @ Tool_jobs.tools
;;

let with_agent ?(extra_tools = []) ?before ?session ~main children f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let provider = Routed_provider.create ?before ~main children in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:(tools ~provider @ extra_tools)
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ?session
      ~cwd:t.dir
      ()
  in
  log_events t agent;
  f t agent
;;

let background ?timeout ~id command =
  Reply.tool_call
    ~id
    ~name:"bash"
    ~arguments:
      (Json.to_string
         (`Object
             ([ "command", `String command; "background", `True ]
              @ Option.value_map timeout ~default:[] ~f:(fun s ->
                [ "timeout", `Number (Int.to_string s) ]))))
    ()
;;

let gated ?(gate = "gate") rest =
  sprintf "while [ ! -f %s ]; do sleep 0.02; done; %s" gate rest
;;

let open_gate ?(gate = "gate") t = write t gate ""

let job_output t agent id =
  print_endline
    (clean t (Or_error.ok_exn (Agent.job_output agent ~job_id:id ~lines:10)))
;;

let job_finished agent id =
  List.exists (Agent.jobs agent) ~f:(fun task ->
    String.equal (Background_tasks.Task.id task) id
    && not (Background_tasks.Task.running task))
;;

let%expect_test "background bash returns at once; the exit report starts a turn"
  =
  with_agent
    ~main:
      [ background ~id:"c1" (gated "echo finished; exit 3")
      ; Reply.text "started it"
      ; Reply.text "it failed with 3"
      ]
    []
  @@ fun t agent ->
  Or_error.ok_exn (Agent.prompt agent "build it");
  Test_background_subagents.wait_turn agent;
  printf
    "--- turn over; job running: %b\n"
    (Background_tasks.Task.running (List.hd_exn (Agent.jobs agent)));
  open_gate t;
  Agent.wait_idle agent;
  job_output t agent "j1";
  print_messages (Agent.messages agent);
  [%expect
    {|
    state: running=true subagents=[] jobs=[]
    agent_start
    user: build it
    assistant:  bash{"command":"while [ ! -f gate ]; do sleep 0.02; done; echo finished; exit 3","background":true}
    state: running=true subagents=[] jobs=[j1 running]
    tool_result bash: started job j1: while [ ! -f gate ]; do sleep 0.02; done; echo finished; ex…; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_wait to block, job_kill to stop it.
    assistant: started it
    agent_end
    state: running=false subagents=[] jobs=[j1 running]
    --- turn over; job running: true
    state: running=true subagents=[] jobs=[]
    agent_start
    user: [job j1 exited 3] while [ ! -f gate ]; do sleep 0.02; done; echo finished; ex… | finished
    assistant: it failed with 3
    agent_end
    state: running=false subagents=[] jobs=[]
    [job j1 exited 3 after Ns, 9 bytes; lines 1-1 of 1] while [ ! -f gate ]; do sleep 0.02; done; echo finished; ex…
    finished
    --- messages
    user: build it
    assistant:  bash{"command":"while [ ! -f gate ]; do sleep 0.02; done; echo finished; exit 3","background":true}
    tool_result bash: started job j1: while [ ! -f gate ]; do sleep 0.02; done; echo finished; ex…; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_wait to block, job_kill to stop it.
    assistant: started it
    user: [job j1 exited 3] while [ ! -f gate ]; do sleep 0.02; done; echo finished; ex… | finished
    assistant: it failed with 3
    |}]
;;

let pid_alive pid =
  match In_channel.read_all (sprintf "/proc/%d/stat" pid) with
  | stat ->
    (* A zombie has exited; only its exit status is left to collect. *)
    (match String.rsplit2 stat ~on:')' with
     | Some (_, rest) -> not (String.is_prefix (String.lstrip rest) ~prefix:"Z")
     | None -> true)
  | exception _ -> false
;;

let%expect_test "job_wait, job_output, job_status and job_kill" =
  with_agent
    ~main:
      [ background
          ~id:"c1"
          "sleep 30 & echo $! > sleep.pid; echo started-sleeping; wait"
      ; background ~id:"c2" "sleep 0.3; echo hi"
      ; Reply.tool_call
          ~id:"w1"
          ~name:"job_wait"
          ~arguments:{|{"ids":["j2"]}|}
          ()
      ; Reply.tool_call
          ~id:"o1"
          ~name:"job_output"
          ~arguments:{|{"id":"j1"}|}
          ()
      ; Reply.tool_call ~id:"s1" ~name:"job_status" ~arguments:"{}" ()
      ; Reply.tool_call
          ~id:"w2"
          ~name:"job_wait"
          ~arguments:{|{"timeout":0}|}
          ()
      ; Reply.tool_call ~id:"k1" ~name:"job_kill" ~arguments:{|{"id":"j1"}|} ()
      ; Reply.tool_call ~id:"k2" ~name:"job_kill" ~arguments:{|{"id":"j9"}|} ()
      ; Reply.tool_call ~id:"s2" ~name:"job_status" ~arguments:"{}" ()
      ; Reply.tool_call ~id:"w3" ~name:"job_wait" ~arguments:"{}" ()
      ; Reply.text "all done"
      ]
    []
  @@ fun t agent ->
  Or_error.ok_exn (Agent.prompt agent "go");
  Test_background_subagents.wait_turn agent;
  Agent.wait_idle agent;
  let pid = Int.of_string (String.strip (read t "sleep.pid")) in
  printf
    "sleep still alive: %b\n"
    (not (eventually t (fun () -> not (pid_alive pid))));
  (* Everything was returned by the tools, so nothing is delivered again. *)
  print_endline "--- messages";
  List.iter (Agent.messages agent) ~f:(fun m ->
    print_endline (clean t (message_line m)));
  [%expect
    {|
    state: running=true subagents=[] jobs=[]
    agent_start
    user: go
    assistant:  bash{"command":"sleep 30 & echo $! > sleep.pid; echo started-sleeping; wait","background":true}
    state: running=true subagents=[] jobs=[j1 running]
    tool_result bash: started job j1: sleep 30 & echo $! > sleep.pid; echo started-sleeping; wait; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_wait to block, job_kill to stop it.
    assistant:  bash{"command":"sleep 0.3; echo hi","background":true}
    state: running=true subagents=[] jobs=[j1 running; j2 running]
    tool_result bash: started job j2: sleep 0.3; echo hi; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_wait to block, job_kill to stop it.
    assistant:  job_wait{"ids":["j2"]}
    state: running=true subagents=[] jobs=[j1 running; j2 exited 0]
    state: running=true subagents=[] jobs=[j1 running]
    tool_result job_wait: [job j2 exited 0] sleep 0.3; echo hi | hi
    assistant:  job_output{"id":"j1"}
    tool_result job_output: [job j1 running after Ns, 17 bytes; lines 1-1 of 1] sleep 30 & echo $! > sleep.pid; echo started-sleeping; wait | started-sleeping
    assistant:  job_status{}
    tool_result job_status: j1  running  Ns  sleep 30 & echo $! > sleep.pid; echo started-sleeping; wait  (last: 17 B, started-sleeping) | j2  exited 0, delivered  Ns  sleep 0.3; echo hi  (last: 3 B, hi)
    assistant:  job_wait{"timeout":0}
    tool_result job_wait: timed out; still running: j1 (sleep 30 & echo $! > sleep.pid; echo started-sleeping; wait)
    assistant:  job_kill{"id":"j1"}
    state: running=true subagents=[] jobs=[j1 killed]
    state: running=true subagents=[] jobs=[]
    tool_result job_kill: [job j1 killed] sleep 30 & echo $! > sleep.pid; echo started-sleeping; wait | started-sleeping
    assistant:  job_kill{"id":"j9"}
    tool_result job_kill: invalid arguments: unknown job "j9"; known: j1, j2
    assistant:  job_status{}
    tool_result job_status: j1  killed, delivered  Ns  sleep 30 & echo $! > sleep.pid; echo started-sleeping; wait  (last: 17 B, started-sleeping) | j2  exited 0, delivered  Ns  sleep 0.3; echo hi  (last: 3 B, hi)
    assistant:  job_wait{}
    tool_result job_wait: no jobs to wait for
    assistant: all done
    agent_end
    state: running=false subagents=[] jobs=[]
    sleep still alive: false
    --- messages
    user: go
    assistant:  bash{"command":"sleep 30 & echo $! > sleep.pid; echo started-sleeping; wait","background":true}
    tool_result bash: started job j1: sleep 30 & echo $! > sleep.pid; echo started-sleeping; wait; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_wait to block, job_kill to stop it.
    assistant:  bash{"command":"sleep 0.3; echo hi","background":true}
    tool_result bash: started job j2: sleep 0.3; echo hi; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_wait to block, job_kill to stop it.
    assistant:  job_wait{"ids":["j2"]}
    tool_result job_wait: [job j2 exited 0] sleep 0.3; echo hi | hi
    assistant:  job_output{"id":"j1"}
    tool_result job_output: [job j1 running after Ns, 17 bytes; lines 1-1 of 1] sleep 30 & echo $! > sleep.pid; echo started-sleeping; wait | started-sleeping
    assistant:  job_status{}
    tool_result job_status: j1  running  Ns  sleep 30 & echo $! > sleep.pid; echo started-sleeping; wait  (last: 17 B, started-sleeping) | j2  exited 0, delivered  Ns  sleep 0.3; echo hi  (last: 3 B, hi)
    assistant:  job_wait{"timeout":0}
    tool_result job_wait: timed out; still running: j1 (sleep 30 & echo $! > sleep.pid; echo started-sleeping; wait)
    assistant:  job_kill{"id":"j1"}
    tool_result job_kill: [job j1 killed] sleep 30 & echo $! > sleep.pid; echo started-sleeping; wait | started-sleeping
    assistant:  job_kill{"id":"j9"}
    tool_result job_kill: invalid arguments: unknown job "j9"; known: j1, j2
    assistant:  job_status{}
    tool_result job_status: j1  killed, delivered  Ns  sleep 30 & echo $! > sleep.pid; echo started-sleeping; wait  (last: 17 B, started-sleeping) | j2  exited 0, delivered  Ns  sleep 0.3; echo hi  (last: 3 B, hi)
    assistant:  job_wait{}
    tool_result job_wait: no jobs to wait for
    assistant: all done
    |}]
;;

let%expect_test
    "a job and a subagent finishing mid-turn arrive in one message at the next \
     boundary"
  =
  let gates = Gates.create () in
  Gates.hold gates "look";
  let release, resolve_release = Eio.Promise.create () in
  let started = ref false in
  with_agent
    ~before:(Gates.before gates)
    ~extra_tools:[ hold_tool release ~started ]
    ~main:
      [ Reply.tool_calls
          [ "c1", "subagent", {|{"task":"look"}|}
          ; ( "c2"
            , "bash"
            , Json.to_string
                (`Object
                    [ "command", `String (gated "echo built")
                    ; "background", `True
                    ]) )
          ]
      ; Reply.tool_call ~id:"h1" ~name:"hold" ~arguments:"{}" ()
      ; Reply.text "both in"
      ]
    [ "look", [ Reply.text "child report" ] ]
  @@ fun t agent ->
  Or_error.ok_exn (Agent.prompt agent "go");
  ignore (eventually t (fun () -> !started) : bool);
  Gates.release gates "look";
  ignore
    (eventually t (fun () -> not (Agent.has_running_subagents agent)) : bool);
  open_gate t;
  ignore
    (eventually t (fun () ->
       (not (Agent.has_running_background agent)) && job_finished agent "j1")
     : bool);
  print_endline "--- both finished; releasing the turn";
  Eio.Promise.resolve resolve_release ();
  Agent.wait_idle agent;
  print_messages (Agent.messages agent);
  [%expect
    {|
    state: running=true subagents=[] jobs=[]
    agent_start
    user: go
    assistant:  subagent{"task":"look"} bash{"command":"while [ ! -f gate ]; do sleep 0.02; done; echo built","background":true}
    state: running=true subagents=[a1 running] jobs=[]
    tool_result subagent: started agent a1 (look); its result will be delivered to you when it finishes; use subagent_wait to block on it
    state: running=true subagents=[a1 running] jobs=[j1 running]
    tool_result bash: started job j1: while [ ! -f gate ]; do sleep 0.02; done; echo built; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_wait to block, job_kill to stop it.
    assistant:  hold{}
    state: running=true subagents=[a1 finished] jobs=[j1 running]
    state: running=true subagents=[a1 finished] jobs=[j1 exited 0]
    --- both finished; releasing the turn
    tool_result hold: held
    state: running=true subagents=[] jobs=[]
    user: [subagent a1 finished] look | child report | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000] |  | [job j1 exited 0] while [ ! -f gate ]; do sleep 0.02; done; echo built | built
    assistant: both in
    agent_end
    state: running=false subagents=[] jobs=[]
    --- messages
    user: go
    assistant:  subagent{"task":"look"} bash{"command":"while [ ! -f gate ]; do sleep 0.02; done; echo built","background":true}
    tool_result subagent: started agent a1 (look); its result will be delivered to you when it finishes; use subagent_wait to block on it
    tool_result bash: started job j1: while [ ! -f gate ]; do sleep 0.02; done; echo built; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_wait to block, job_kill to stop it.
    assistant:  hold{}
    tool_result hold: held
    user: [subagent a1 finished] look | child report | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000] |  | [job j1 exited 0] while [ ! -f gate ]; do sleep 0.02; done; echo built | built
    assistant: both in
    |}]
;;

let%expect_test
    "abort keeps jobs; a report ready at the abort waits for the next prompt"
  =
  let never, _ = Eio.Promise.create () in
  let started = ref false in
  with_agent
    ~extra_tools:[ hold_tool never ~started ]
    ~main:
      [ Reply.tool_calls
          [ ( "c1"
            , "bash"
            , Json.to_string
                (`Object
                    [ "command", `String (gated ~gate:"gate1" "echo one")
                    ; "background", `True
                    ]) )
          ; ( "c2"
            , "bash"
            , Json.to_string
                (`Object
                    [ "command", `String (gated ~gate:"gate2" "echo two")
                    ; "background", `True
                    ]) )
          ]
      ; Reply.tool_call ~id:"h1" ~name:"hold" ~arguments:"{}" ()
      ; Reply.text "saw j1"
      ; Reply.text "saw j2"
      ]
    []
  @@ fun t agent ->
  Or_error.ok_exn (Agent.prompt agent "go");
  ignore (eventually t (fun () -> !started) : bool);
  open_gate ~gate:"gate1" t;
  ignore (eventually t (fun () -> job_finished agent "j1") : bool);
  print_endline "--- abort";
  ignore (Agent.abort agent : string list);
  Test_background_subagents.wait_turn agent;
  for _ = 1 to 10 do
    Eio.Fiber.yield ()
  done;
  printf "--- idle after abort: running=%b\n" (Agent.is_running agent);
  Or_error.ok_exn (Agent.prompt agent "next");
  Test_background_subagents.wait_turn agent;
  print_endline "--- j2 exits while idle";
  open_gate ~gate:"gate2" t;
  Agent.wait_idle agent;
  print_messages (Agent.messages agent);
  [%expect
    {|
    state: running=true subagents=[] jobs=[]
    agent_start
    user: go
    assistant:  bash{"command":"while [ ! -f gate1 ]; do sleep 0.02; done; echo one","background":true} bash{"command":"while [ ! -f gate2 ]; do sleep 0.02; done; echo two","background":true}
    state: running=true subagents=[] jobs=[j1 running]
    tool_result bash: started job j1: while [ ! -f gate1 ]; do sleep 0.02; done; echo one; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_wait to block, job_kill to stop it.
    state: running=true subagents=[] jobs=[j1 running; j2 running]
    tool_result bash: started job j2: while [ ! -f gate2 ]; do sleep 0.02; done; echo two; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_wait to block, job_kill to stop it.
    assistant:  hold{}
    state: running=true subagents=[] jobs=[j1 exited 0; j2 running]
    --- abort
    tool_result hold: [cancelled]
    agent_end
    state: running=false subagents=[] jobs=[j1 exited 0; j2 running]
    --- idle after abort: running=false
    state: running=true subagents=[] jobs=[j2 running]
    agent_start
    user: [job j1 exited 0] while [ ! -f gate1 ]; do sleep 0.02; done; echo one | one
    user: next
    assistant: saw j1
    agent_end
    state: running=false subagents=[] jobs=[j2 running]
    --- j2 exits while idle
    state: running=true subagents=[] jobs=[]
    agent_start
    user: [job j2 exited 0] while [ ! -f gate2 ]; do sleep 0.02; done; echo two | two
    assistant: saw j2
    agent_end
    state: running=false subagents=[] jobs=[]
    --- messages
    user: go
    assistant:  bash{"command":"while [ ! -f gate1 ]; do sleep 0.02; done; echo one","background":true} bash{"command":"while [ ! -f gate2 ]; do sleep 0.02; done; echo two","background":true}
    tool_result bash: started job j1: while [ ! -f gate1 ]; do sleep 0.02; done; echo one; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_wait to block, job_kill to stop it.
    tool_result bash: started job j2: while [ ! -f gate2 ]; do sleep 0.02; done; echo two; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_wait to block, job_kill to stop it.
    assistant:  hold{}
    tool_result hold: [cancelled]
    user: [job j1 exited 0] while [ ! -f gate1 ]; do sleep 0.02; done; echo one | one
    user: next
    assistant: saw j1
    user: [job j2 exited 0] while [ ! -f gate2 ]; do sleep 0.02; done; echo two | two
    assistant: saw j2
    |}]
;;

let%expect_test "job ids continue after a reload; subagents cannot start jobs" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let sessions_dir = Filename.concat t.dir "sessions" in
  let make ?session main =
    let provider = Routed_provider.create ~main [] in
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:(tools ~provider)
      ~sessions_dir
      ~home:t.dir
      ?session
      ~cwd:t.dir
      ()
  in
  let first =
    make
      [ background ~id:"c1" (gated "echo one")
      ; background ~id:"c2" (gated "echo two")
      ; Reply.text "ok"
      ; Reply.text "reports in"
      ]
  in
  Or_error.ok_exn (Agent.prompt first "go");
  Test_background_subagents.wait_turn first;
  open_gate t;
  Agent.wait_idle first;
  Core_unix.unlink (Filename.concat t.dir "gate");
  let path = Session.path (Agent.session first) in
  let second =
    make
      ~session:(Or_error.ok_exn (Session.load path))
      [ background ~id:"c3" (gated "echo three")
      ; Reply.text "ok"
      ; Reply.text "in"
      ]
  in
  Or_error.ok_exn (Agent.prompt second "again");
  Test_background_subagents.wait_turn second;
  open_gate t;
  Agent.wait_idle second;
  print_messages
    (List.drop_while (Agent.messages second) ~f:(function
       | User { text } -> not (String.equal text "again")
       | _ -> true));
  [%expect
    {|
    --- messages
    user: again
    assistant:  bash{"command":"while [ ! -f gate ]; do sleep 0.02; done; echo three","background":true}
    tool_result bash: started job j3: while [ ! -f gate ]; do sleep 0.02; done; echo three; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_wait to block, job_kill to stop it.
    assistant: ok
    user: [job j3 exited 0] while [ ! -f gate ]; do sleep 0.02; done; echo three | three
    assistant: in
    |}];
  (* Below depth 0, bash has no [background] and the job tools are gone. *)
  let sub =
    Or_error.ok_exn
      (Tools.for_context
         ~parent:(tools ~provider:(Faux_provider.create []))
         ~depth:1
         ())
  in
  print_s [%sexp (List.map sub ~f:Tool.name : string list)];
  print_endline
    (Json.to_string
       (List.find_exn sub ~f:(fun tool -> String.equal (Tool.name tool) "bash"))
         .spec
         .parameters);
  [%expect
    {|
    (bash read write edit ls grep find subagent)
    {"type":"object","properties":{"command":{"type":"string","description":"The command to run"},"timeout":{"type":"integer","description":"Timeout in seconds (default 600, none in the background). The command is killed when it expires."}},"required":["command"],"additionalProperties":false}
    |}]
;;

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
  call Tool_bash.tool {|{"command": "seq 1 100", "background": true}|};
  call (tool "job_wait") "{}";
  call (tool "job_output") {|{"id": "j1", "lines": 3}|};
  call (tool "job_output") {|{"id": "j1", "lines": 3, "offset": 10}|};
  call (tool "job_output") {|{"id": "j1", "lines": 3, "offset": 200}|};
  call (tool "job_output") {|{"id": "j7"}|};
  call Tool_bash.tool {|{"command": "exit 1", "background": true}|};
  call
    Tool_bash.tool
    {|{"command": "sleep 5", "background": true, "timeout": 1}|};
  call Tool_bash.tool {|{"command": "kill -9 $$", "background": true}|};
  call (tool "job_wait") "{}";
  [%expect
    {|
    started job j1: seq 1 100; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_wait to block, job_kill to stop it.
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
    started job j2: exit 1; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_wait to block, job_kill to stop it.
    started job j3: sleep 5; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_wait to block, job_kill to stop it.
    started job j4: kill -9 $$; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_wait to block, job_kill to stop it.
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

let elapsed_re = Re.compile (Re.Perl.re {|"elapsed":[0-9.e+-]+|})

let rpc t (h : Test_rpc.H.t) meth params =
  print_endline
    (clean
       t
       (Re.replace_string
          elapsed_re
          ~by:{|"elapsed":<t>|}
          (Json.to_string
             (Rpc_server.handle
                h.server
                h.client
                (Json.of_string
                   (sprintf
                      {|{"id": "r", "method": "%s", "params": %s}|}
                      meth
                      params))))))
;;

let print_tail t agent n =
  let messages = Agent.messages agent in
  List.iter
    (List.drop messages (List.length messages - n))
    ~f:(fun m -> print_endline (clean t (message_line m)))
;;

let wait_idle t agent =
  Eio.Fiber.first
    (fun () -> Agent.wait_idle agent)
    (fun () ->
       Eio.Time.sleep (Eio.Stdenv.clock t.env) 20.;
       print_endline "TIMEOUT waiting for the agent")
;;

(* The job runs where the session's tools run: a [prigh tool-host] connected
   over TCP. Its output streams into the job; killing the job cancels the
   exec on the host; the host disconnecting fails the job. *)
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
               "pwd; echo from-host; while [ ! -f gate ]; do sleep 0.02; done; \
                echo after-gate"
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
      ~token:(Some "sekrit")
      ~cwd:host_dir
  in
  Test_tool_host.Host.wait_logs t host 1;
  rpc t h "set_active_host" {|{"host": "client-2"}|};
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
    {"type":"response","id":"r","ok":true,"result":{"client_id":"client-1","namespace":null,"user":null,"superuser":false,"state":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":null,"session_description":null,"cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"off","running":false,"message_count":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0,"active_host":"backend","hosts":[{"id":"backend","name":"<host>","cwd":"$DIR","session_id":null,"session_name":null}],"subagents":[],"jobs":[]}}}
    connected to 127.0.0.1:PORT as client-2
    {"type":"response","id":"r","ok":true,"result":{}}
    {"type":"response","id":"r","ok":true,"result":{}}
    {"type":"response","id":"r","ok":true,"result":{"text":"[job j1 running after Ns, 60 bytes; lines 1-2 of 2] pwd; echo from-host; while [ ! -f gate ]; do sleep 0.02; do…\n$DIR/host\nfrom-host"}}
    {"type":"response","id":"r","ok":true,"result":[{"id":"j1","command":"pwd; echo from-host; while [ ! -f gate ]; do sleep 0.02; done; echo after-gate","running":true,"exit":null,"delivered":false,"elapsed":<t>,"bytes":60,"last_line":"from-host"}]}
    [{"id":"j1","command":"pwd; echo from-host; while [ ! -f gate ]; do sleep 0.02; done; echo after-gate","running":true,"exit":null}]
    user: [job j1 exited 0] pwd; echo from-host; while [ ! -f gate ]; do sleep 0.02; do… | $DIR/host | from-host | after-gate
    assistant: j1 noted
    |}];
  (* [!&cmd]: a job started by the user, then killed. *)
  rpc
    t
    h
    "shell"
    {|{"command": "sleep 30 & echo $! > sleep.pid; echo sleeping; wait", "background": true}|};
  ignore
    (eventually t (fun () ->
       Sys_unix.file_exists_exn (Filename.concat host_dir "sleep.pid"))
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
  Test_tool_host.Listener.drop listener;
  wait_idle t agent;
  print_tail t agent 2;
  [%expect
    {|
    {"type":"response","id":"r","ok":true,"result":{"job_id":"j3"}}
    user: [job j3 failed: tool host disconnected] echo waiting; while [ ! -f gate2 ]; do sleep 0.02; done | waiting
    assistant: j3 noted
    |}]
;;
