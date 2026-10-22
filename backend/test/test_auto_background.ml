open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Gates = Routed_provider.Gates
module Json = Jsonaf

(* Foreground [bash] calls of the main agent that run past
   [auto_background_seconds] become jobs. *)

let message_line = Test_background_subagents.message_line
let print_messages = Test_background_subagents.print_messages
let eventually = Test_tool_host.eventually
let gated = Test_background_jobs.gated
let open_gate = Test_background_jobs.open_gate
let clean = Test_background_jobs.clean
let summaries = Test_background_jobs.summaries

let config ~seconds =
  sprintf {|{"auto_background_seconds": %s}|} (Float.to_string seconds)
;;

let log_events t agent =
  let last_state = ref "" in
  Agent.subscribe agent ~f:(fun (event : Agent.Event.t) ->
    match event with
    | Loop Agent_start -> print_endline "agent_start"
    | Loop (Agent_end _) -> print_endline "agent_end"
    | Loop (Tool_output { call_id; chunk }) ->
      printf "tool_output %s: %s\n" call_id (String.strip chunk)
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

let with_agent
      ?(seconds = 1.)
      ?(extra_tools = [])
      ?before
      ?session
      ?(log = true)
      ~main
      children
      f
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  write t ".prigh/config.json" (config ~seconds);
  let provider = Routed_provider.create ?before ~main children in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:(Test_background_jobs.tools ~provider @ extra_tools)
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ?session
      ~cwd:t.dir
      ()
  in
  if log then log_events t agent;
  f t agent
;;

let bash ~id command =
  Reply.tool_call
    ~id
    ~name:"bash"
    ~arguments:(Json.to_string (`Object [ "command", `String command ]))
    ()
;;

let call ~id name arguments = Reply.tool_call ~id ~name ~arguments ()

let%expect_test
    "a foreground command still running after the limit becomes a job; its \
     report comes once"
  =
  with_agent
    ~main:
      [ bash ~id:"c1" "echo quick"
      ; bash ~id:"c2" ("echo before; " ^ gated "echo after; seq 1 3; exit 2")
      ; Reply.text "moved on"
      ; Reply.text "it exited 2"
      ]
    []
  @@ fun t agent ->
  Or_error.ok_exn (Agent.prompt agent "build it");
  Test_background_subagents.wait_turn agent;
  print_endline "--- turn over";
  open_gate t;
  Agent.wait_idle agent;
  print_endline
    (clean t (Or_error.ok_exn (Agent.job_output agent ~job_id:"j1" ~lines:10)));
  print_messages (Agent.messages agent);
  [%expect
    {|
    state: running=true subagents=[] jobs=[]
    agent_start
    user: build it
    assistant:  bash{"command":"echo quick"}
    tool_output c1: quick
    tool_result bash: quick
    assistant:  bash{"command":"echo before; while [ ! -f gate ]; do sleep 0.02; done; echo after; seq 1 3; exit 2"}
    tool_output c2: before
    state: running=true subagents=[] jobs=[j1 running]
    tool_result bash: still running after Ns, so it was moved to the background as job j1 (not restarted: it keeps running). Output so far: | before | Its report ([job j1 exited ...] and its last lines) is delivered to you when it exits: keep working on something else or end your turn. job_output shows more output, job_kill stops it; do not job_wait on it unless you truly cannot do anything else.
    assistant: moved on
    agent_end
    state: running=false subagents=[] jobs=[j1 running]
    --- turn over
    state: running=true subagents=[] jobs=[]
    agent_start
    user: [job j1 exited 2] echo before; while [ ! -f gate ]; do sleep 0.02; done; echo… | before | after | 1 | 2 | 3
    assistant: it exited 2
    agent_end
    state: running=false subagents=[] jobs=[]
    [job j1 exited 2 after Ns, 19 bytes; lines 1-5 of 5] echo before; while [ ! -f gate ]; do sleep 0.02; done; echo…
    before
    after
    1
    2
    3
    --- messages
    user: build it
    assistant:  bash{"command":"echo quick"}
    tool_result bash: quick
    assistant:  bash{"command":"echo before; while [ ! -f gate ]; do sleep 0.02; done; echo after; seq 1 3; exit 2"}
    tool_result bash: still running after 1s, so it was moved to the background as job j1 (not restarted: it keeps running). Output so far: | before | Its report ([job j1 exited ...] and its last lines) is delivered to you when it exits: keep working on something else or end your turn. job_output shows more output, job_kill stops it; do not job_wait on it unless you truly cannot do anything else.
    assistant: moved on
    user: [job j1 exited 2] echo before; while [ ! -f gate ]; do sleep 0.02; done; echo… | before | after | 1 | 2 | 3
    assistant: it exited 2
    |}]
;;

let%expect_test
    "the main agent ends its turn with a subagent and a job running; each \
     report wakes it"
  =
  let gates = Gates.create () in
  Gates.hold gates "look";
  with_agent
    ~before:(Gates.before gates)
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
        (* A habit from sessions that had it: an unknown tool now. *)
      ; call ~id:"w1" "job_wait" "{}"
      ; Reply.text "ending my turn"
      ; Reply.text "a1 in"
      ; Reply.text "j1 in"
      ]
    [ "look", [ Reply.text "child report" ] ]
  @@ fun t agent ->
  Or_error.ok_exn (Agent.prompt agent "go");
  Test_background_subagents.wait_turn agent;
  print_endline "--- turn over";
  Gates.release gates "look";
  ignore
    (eventually t (fun () -> not (Agent.has_running_subagents agent)) : bool);
  open_gate t;
  Agent.wait_idle agent;
  [%expect {| |}];
  print_messages
    (List.drop_while (Agent.messages agent) ~f:(function
       | Assistant a ->
         not (String.equal (Message.Assistant.text a) "ending my turn")
       | _ -> true));
  [%expect {| |}]
;;

let%expect_test
    "an abort before the limit kills the command; after, the job keeps running"
  =
  with_agent ~seconds:30. ~main:[ bash ~id:"c1" (gated "echo never") ] []
  @@ fun t agent ->
  Or_error.ok_exn (Agent.prompt agent "go");
  Eio.Time.sleep (Eio.Stdenv.clock t.env) 0.3;
  ignore (Agent.abort agent : string list);
  Test_background_subagents.wait_turn agent;
  printf "jobs: %d\n" (List.length (Agent.jobs agent));
  [%expect
    {|
    state: running=true subagents=[] jobs=[]
    agent_start
    user: go
    assistant:  bash{"command":"while [ ! -f gate ]; do sleep 0.02; done; echo never"}
    tool_result bash: [cancelled]
    agent_end
    state: running=false subagents=[] jobs=[]
    jobs: 0
    |}];
  let never, _ = Eio.Promise.create () in
  let started = ref false in
  with_agent
    ~extra_tools:[ Test_background_jobs.hold_tool never ~started ]
    ~main:
      [ bash ~id:"c1" (gated "echo after the abort")
      ; call ~id:"h1" "hold" "{}"
      ; Reply.text "saw it"
      ]
    []
  @@ fun t agent ->
  Or_error.ok_exn (Agent.prompt agent "go");
  ignore (eventually t (fun () -> !started) : bool);
  print_endline "--- abort";
  ignore (Agent.abort agent : string list);
  Test_background_subagents.wait_turn agent;
  printf
    "--- job running after the abort: %b\n"
    (Background_tasks.Task.running (List.hd_exn (Agent.jobs agent)));
  open_gate t;
  Agent.wait_idle agent;
  [%expect
    {|
    state: running=true subagents=[] jobs=[]
    agent_start
    user: go
    assistant:  bash{"command":"while [ ! -f gate ]; do sleep 0.02; done; echo after the abort"}
    state: running=true subagents=[] jobs=[j1 running]
    tool_result bash: still running after Ns, so it was moved to the background as job j1 (not restarted: it keeps running). No output so far. | Its report ([job j1 exited ...] and its last lines) is delivered to you when it exits: keep working on something else or end your turn. job_output shows more output, job_kill stops it; do not job_wait on it unless you truly cannot do anything else.
    assistant:  hold{}
    --- abort
    tool_result hold: [cancelled]
    agent_end
    state: running=false subagents=[] jobs=[j1 running]
    --- job running after the abort: true
    state: running=true subagents=[] jobs=[]
    agent_start
    user: [job j1 exited 0] while [ ! -f gate ]; do sleep 0.02; done; echo after the ab… | after the abort
    assistant: saw it
    agent_end
    state: running=false subagents=[] jobs=[]
    |}]
;;

(* The limit counts from when the command starts, not while the user decides
   whether it may run. *)
let%expect_test "waiting for approval does not count" =
  with_agent
    ~log:false
    ~main:[ bash ~id:"c1" "sleep 0.1; echo ran"; Reply.text "done" ]
    []
  @@ fun t agent ->
  Agent.set_mode agent ~approval:Ask;
  Or_error.ok_exn (Agent.prompt agent "go");
  ignore
    (eventually t (fun () -> not (List.is_empty (Agent.pending_confirms agent)))
     : bool);
  Eio.Time.sleep (Eio.Stdenv.clock t.env) 1.5;
  print_endline "--- allowed after 1.5s";
  Or_error.ok_exn (Agent.respond_confirm agent ~call_id:"c1" Allow);
  Agent.wait_idle agent;
  print_messages (Agent.messages agent);
  printf "jobs: %d\n" (List.length (Agent.jobs agent));
  [%expect
    {|
    --- allowed after 1.5s
    --- messages
    user: go
    assistant:  bash{"command":"sleep 0.1; echo ran"}
    tool_result bash: ran
    assistant: done
    jobs: 0
    |}]
;;

(* On a client tool host the pending exec keeps going after the move, its
   streamed output feeding the job; the host disconnecting fails the job. *)
let%expect_test "a command on a client tool host moves to the background" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  write t ".prigh/config.json" (config ~seconds:1.);
  let host_dir = Filename.concat t.dir "host" in
  Core_unix.mkdir_p host_dir;
  let agent, h =
    Test_rpc.make_server
      ~token:"sekrit"
      t
      ~sw
      ~provider:
        (Faux_provider.create
           [ bash
               ~id:"c1"
               ("basename $PWD; echo from-host; " ^ gated "echo after-gate")
           ; Reply.text "moved"
           ; Reply.text "j1 noted"
           ; bash ~id:"c2" ("echo waiting; " ^ gated ~gate:"gate2" "true")
           ; Reply.text "moved again"
           ; Reply.text "j2 noted"
           ])
  in
  let rpc = Test_background_jobs.rpc t h in
  rpc "hello" {|{"token": "sekrit"}|};
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
  rpc "set_active_host" {|{"host": "host-box"}|};
  rpc "prompt" {|{"text": "build"}|};
  Test_background_subagents.wait_turn agent;
  Test_background_jobs.print_tail t agent 3;
  rpc "job_output" {|{"job_id": "j1", "lines": 5}|};
  write t "host/gate" "";
  Test_background_jobs.wait_idle t agent;
  Test_background_jobs.print_tail t agent 2;
  [%expect
    {|
    {"type":"response","id":"r","ok":true,"result":{"client_id":"client-1","host_id":"client-1","namespace":null,"user":null,"superuser":false,"state":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":null,"session_description":null,"cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"on","running":false,"message_count":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0,"active_host":"backend","hosts":[{"id":"backend","name":"<host>","cwd":"$DIR","session_id":null,"session_name":null}],"subagents":[],"jobs":[],"budget_usd":null,"goal":null,"reminders":[],"resume_at_ms":null,"approval_mode":"auto","plan_mode":false,"plan":null,"session_rules":[]}}}
    connected to 127.0.0.1:PORT as client-2
    {"type":"response","id":"r","ok":true,"result":{"cwd":"$DIR/host","created":false}}
    {"type":"response","id":"r","ok":true,"result":{}}
    assistant:  bash{"command":"basename $PWD; echo from-host; while [ ! -f gate ]; do sleep 0.02; done; echo after-gate"}
    tool_result bash: still running after Ns, so it was moved to the background as job j1 (not restarted: it keeps running). Output so far: | host | from-host | Its report ([job j1 exited ...] and its last lines) is delivered to you when it exits: keep working on something else or end your turn. job_output shows more output, job_kill stops it; do not job_wait on it unless you truly cannot do anything else.
    assistant: moved
    {"type":"response","id":"r","ok":true,"result":{"text":"[job j1 running after Ns, 15 bytes; lines 1-2 of 2] basename $PWD; echo from-host; while [ ! -f gate ]; do slee…\nhost\nfrom-host"}}
    user: [job j1 exited 0] basename $PWD; echo from-host; while [ ! -f gate ]; do slee… | host | from-host | after-gate
    assistant: j1 noted
    |}];
  rpc "prompt" {|{"text": "again"}|};
  Test_background_subagents.wait_turn agent;
  Test_background_jobs.print_tail t agent 2;
  Test_tool_host.Listener.drop listener;
  Test_background_jobs.wait_idle t agent;
  Test_background_jobs.print_tail t agent 2;
  [%expect
    {|
    {"type":"response","id":"r","ok":true,"result":{}}
    tool_result bash: still running after Ns, so it was moved to the background as job j2 (not restarted: it keeps running). Output so far: | waiting | Its report ([job j2 exited ...] and its last lines) is delivered to you when it exits: keep working on something else or end your turn. job_output shows more output, job_kill stops it; do not job_wait on it unless you truly cannot do anything else.
    assistant: moved again
    user: [job j2 failed: tool host disconnected] echo waiting; while [ ! -f gate2 ]; do sleep 0.02; done; tr… | waiting
    assistant: j2 noted
    |}]
;;

let%expect_test "job ids continue past moved calls after a reload" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  write t ".prigh/config.json" (config ~seconds:1.);
  let sessions_dir = Filename.concat t.dir "sessions" in
  let make ?session main =
    let provider = Routed_provider.create ~main [] in
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:(Test_background_jobs.tools ~provider)
      ~sessions_dir
      ~home:t.dir
      ?session
      ~cwd:t.dir
      ()
  in
  let first = make [ bash ~id:"c1" (gated "echo one"); Reply.text "ok" ] in
  Or_error.ok_exn (Agent.prompt first "go");
  Test_background_subagents.wait_turn first;
  (* Gone without a report: only the moved call's result names j1. *)
  Agent.cancel_background ~discard:true first;
  Agent.wait_idle first;
  let path = Session.path (Agent.session first) in
  let second =
    make
      ~session:(Or_error.ok_exn (Session.load path))
      [ Test_background_jobs.background ~id:"c2" "echo two"
      ; Reply.text "ok"
      ; Reply.text "in"
      ]
  in
  Or_error.ok_exn (Agent.prompt second "again");
  Agent.wait_idle second;
  List.iter (Agent.messages second) ~f:(fun m ->
    print_endline (clean t (message_line m)));
  [%expect
    {|
    user: go
    assistant:  bash{"command":"while [ ! -f gate ]; do sleep 0.02; done; echo one"}
    tool_result bash: still running after Ns, so it was moved to the background as job j1 (not restarted: it keeps running). No output so far. | Its report ([job j1 exited ...] and its last lines) is delivered to you when it exits: keep working on something else or end your turn. job_output shows more output, job_kill stops it; do not job_wait on it unless you truly cannot do anything else.
    assistant: ok
    user: again
    assistant:  bash{"command":"echo two","background":true}
    tool_result bash: started job j2: echo two; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_wait to block, job_kill to stop it.
    assistant: ok
    user: [job j2 exited 0] echo two | two
    assistant: in
    |}];
  print_s
    [%sexp
      (List.map
         [ "still running after 30s, so it was moved to the background as job \
            j12 (not restarted)"
         ; "started job j3: x"
         ; "moved to the background as job j4"
         ; "still running after 1s"
         ]
         ~f:Tool_bash.moved_job_id
       : string option list)];
  [%expect {| ((j12) () () ()) |}]
;;
