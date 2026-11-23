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
  [%expect
    {|
    state: running=true subagents=[] jobs=[]
    agent_start
    user: [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).] |  | go
    assistant:  subagent{"task":"look"} bash{"command":"while [ ! -f gate ]; do sleep 0.02; done; echo built","background":true}
    state: running=true subagents=[a1 running] jobs=[]
    tool_result subagent: started agent a1 (look); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    state: running=true subagents=[a1 running] jobs=[j1 running]
    tool_result bash: started job j1: while [ ! -f gate ]; do sleep 0.02; done; echo built; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_kill to stop it.
    assistant:  job_wait{}
    tool_result job_wait: unknown tool "job_wait"
    assistant: ending my turn
    agent_end
    state: running=false subagents=[a1 running] jobs=[j1 running]
    --- turn over
    state: running=true subagents=[] jobs=[j1 running]
    agent_start
    user: [subagent a1 finished] look | child report | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    assistant: a1 in
    agent_end
    state: running=false subagents=[] jobs=[j1 running]
    state: running=true subagents=[] jobs=[]
    agent_start
    user: [job j1 exited 0] while [ ! -f gate ]; do sleep 0.02; done; echo built | built
    assistant: j1 in
    agent_end
    state: running=false subagents=[] jobs=[]
    |}];
  print_messages
    (List.drop_while (Agent.messages agent) ~f:(function
       | Assistant a ->
         not (String.equal (Message.Assistant.text a) "ending my turn")
       | _ -> true));
  [%expect
    {|
    --- messages
    assistant: ending my turn
    user: [subagent a1 finished] look | child report | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    assistant: a1 in
    user: [job j1 exited 0] while [ ! -f gate ]; do sleep 0.02; done; echo built | built
    assistant: j1 in
    |}]
;;

(* 0 turns it off: a command runs in the foreground however long it takes
   (the web e2e scripts' gates rely on it). *)
let%expect_test "with auto_background_seconds 0 a command never moves" =
  with_agent
    ~seconds:0.
    ~main:
      [ bash ~id:"c1" ("echo before; " ^ gated "echo after")
      ; Reply.text "done"
      ]
    []
  @@ fun t agent ->
  let output = ref false in
  Agent.subscribe agent ~f:(function
    | Loop (Tool_output _) -> output := true
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "go");
  ignore (eventually t (fun () -> !output) : bool);
  Eio.Time.sleep (Eio.Stdenv.clock t.env) 0.3;
  print_endline "--- gate opened";
  open_gate t;
  Agent.wait_idle agent;
  printf "jobs: %d\n" (List.length (Agent.jobs agent));
  [%expect
    {|
    state: running=true subagents=[] jobs=[]
    agent_start
    user: [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).] |  | go
    assistant:  bash{"command":"echo before; while [ ! -f gate ]; do sleep 0.02; done; echo after"}
    tool_output c1: before
    --- gate opened
    tool_output c1: after
    tool_result bash: before | after
    assistant: done
    agent_end
    state: running=false subagents=[] jobs=[]
    jobs: 0
    |}]
;;

let%expect_test
    "a foreground command still running after the limit becomes a job; its \
     report comes once"
  =
  with_agent
    ~seconds:0.5
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
    user: [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).] |  | build it
    assistant:  bash{"command":"echo quick"}
    tool_output c1: quick
    tool_result bash: quick
    assistant:  bash{"command":"echo before; while [ ! -f gate ]; do sleep 0.02; done; echo after; seq 1 3; exit 2"}
    tool_output c2: before
    state: running=true subagents=[] jobs=[j1 running]
    tool_result bash: still running after 0.Ns, so it was moved to the background as job j1 (not restarted: it keeps running). Output so far: | before | Its report ([job j1 exited ...] and its last lines) is delivered to you when it exits: keep working on something else or end your turn. job_output shows more output, job_kill stops it.
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
    user: [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).] |  | build it
    assistant:  bash{"command":"echo quick"}
    tool_result bash: quick
    assistant:  bash{"command":"echo before; while [ ! -f gate ]; do sleep 0.02; done; echo after; seq 1 3; exit 2"}
    tool_result bash: still running after 0.5s, so it was moved to the background as job j1 (not restarted: it keeps running). Output so far: | before | Its report ([job j1 exited ...] and its last lines) is delivered to you when it exits: keep working on something else or end your turn. job_output shows more output, job_kill stops it.
    assistant: moved on
    user: [job j1 exited 2] echo before; while [ ! -f gate ]; do sleep 0.02; done; echo… | before | after | 1 | 2 | 3
    assistant: it exited 2
    |}]
;;
