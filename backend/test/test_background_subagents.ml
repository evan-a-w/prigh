open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Gates = Routed_provider.Gates

let one_line s =
  String.substr_replace_all (String.strip s) ~pattern:"\n" ~with_:" | "
;;

let message_line (m : Message.t) =
  match m with
  | User { text; _ } -> "user: " ^ one_line text
  | Assistant a ->
    let calls =
      List.map (Message.Assistant.tool_calls a) ~f:(fun c ->
        sprintf " %s%s" c.name c.arguments)
    in
    "assistant: " ^ one_line (Message.Assistant.text a) ^ String.concat calls
  | Tool_result r -> sprintf "tool_result %s: %s" r.tool_name (one_line r.text)
;;

let print_messages messages =
  print_endline "--- messages";
  List.iter messages ~f:(fun m -> print_endline (message_line m))
;;

let subagent_tools ~provider =
  Tool_subagent.create
    ~provider
    ~current_model:(fun () -> Model.default)
    ~current_thinking:(fun () -> Off)
    ~home:"/nonexistent"
    ()
  :: Tool_subagent.control_tools
;;

(* Prints the main agent's events as they happen. *)
let log_events agent =
  let last_state = ref "" in
  Agent.subscribe agent ~f:(fun (event : Agent.Event.t) ->
    match event with
    | Loop Agent_start -> print_endline "agent_start"
    | Loop (Agent_end _) -> print_endline "agent_end"
    | Loop (Message_end m) -> print_endline (message_line m)
    | Loop (Subagent_start { agent_id; task; _ }) ->
      printf "subagent_start %s: %s\n" agent_id task
    | Loop (Subagent_end { agent_id; result; _ }) ->
      printf
        "subagent_end %s: %s %s\n"
        agent_id
        (if result.is_error then "error" else "ok")
        (one_line result.text)
    | State_changed s ->
      let line =
        sprintf
          "state: running=%b subagents=[%s]"
          s.running
          (String.concat
             ~sep:"; "
             (List.map s.subagents ~f:(fun a ->
                sprintf "%s %s" a.id (if a.running then "running" else "done"))))
      in
      if not (String.equal line !last_state)
      then (
        last_state := line;
        print_endline line)
    | _ -> ())
;;

let with_agent ?(extra_tools = []) ?before ?on_request ~main children f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let provider = Routed_provider.create ?before ?on_request ~main children in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:(Tools.all @ subagent_tools ~provider @ extra_tools)
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:t.dir
      ()
  in
  log_events agent;
  f t agent
;;

let rec wait_turn agent =
  if Agent.is_running agent
  then (
    Eio.Fiber.yield ();
    wait_turn agent)
;;

let spawn ?(id = "c1") task =
  Reply.tool_call
    ~id
    ~name:"subagent"
    ~arguments:(sprintf {|{"task":%S}|} task)
    ()
;;

(* A tool that blocks until [promise] resolves, so a subagent can finish
   while the main turn is executing it. *)
let hold_tool promise =
  { Tool.spec =
      { Tool_spec.name = "hold"
      ; description = "wait"
      ; parameters = Tool_args.schema []
      ; parallel_safe = false
      ; destructive = false
      ; on_host = false
      }
  ; run =
      (fun _ _ ->
        Eio.Promise.await promise;
        Tool.Result.ok "held")
  }
;;

let%expect_test "spawning returns at once; a finished agent starts a turn" =
  let gates = Gates.create () in
  Gates.hold gates "look around";
  with_agent
    ~before:(Gates.before gates)
    ~main:
      [ spawn "look around"
      ; Reply.text "started it"
      ; Reply.text "thanks for the report"
      ]
    [ "look around", [ Reply.text "child report" ] ]
  @@ fun _t agent ->
  Or_error.ok_exn (Agent.prompt agent "go");
  wait_turn agent;
  printf
    "--- turn over; subagents running: %b\n"
    (Agent.has_running_subagents agent);
  print_endline
    (Jsonaf.to_string
       (Option.value_exn
          (Jsonaf.member "subagents" (Rpc_json.state (Agent.state agent)))));
  Gates.release gates "look around";
  Agent.wait_idle agent;
  print_messages (Agent.messages agent);
  [%expect
    {|
    state: running=true subagents=[]
    agent_start
    user: go
    assistant:  subagent{"task":"look around"}
    subagent_start a1: look around
    state: running=true subagents=[a1 running]
    tool_result subagent: started agent a1 (look around); its result will be delivered to you when it finishes; use subagent_wait to block on it
    assistant: started it
    agent_end
    state: running=false subagents=[a1 running]
    --- turn over; subagents running: true
    [{"id":"a1","task":"look around","running":true}]
    subagent_end a1: ok child report | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    state: running=true subagents=[]
    agent_start
    user: [subagent a1 finished] look around | child report | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    assistant: thanks for the report
    agent_end
    state: running=false subagents=[]
    --- messages
    user: go
    assistant:  subagent{"task":"look around"}
    tool_result subagent: started agent a1 (look around); its result will be delivered to you when it finishes; use subagent_wait to block on it
    assistant: started it
    user: [subagent a1 finished] look around | child report | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    assistant: thanks for the report
    |}]
;;

let%expect_test "a report arriving mid-run is injected after the tool results" =
  let gates = Gates.create () in
  Gates.hold gates "one";
  Gates.hold gates "two";
  let both_done, resolve_both = Eio.Promise.create () in
  let requests = ref [] in
  with_agent
    ~extra_tools:[ hold_tool both_done ]
    ~before:(fun ~key ->
      (* The agents run while the main turn is busy with [hold]. *)
      if String.equal key "main" && List.length !requests = 2
      then (
        Gates.release gates "one";
        Gates.release gates "two");
      Gates.before gates ~key)
    ~on_request:(fun ~key request ->
      if String.equal key "main" then requests := request :: !requests)
    ~main:
      [ Reply.tool_calls
          [ "c1", "subagent", {|{"task":"one"}|}
          ; "c2", "subagent", {|{"task":"two"}|}
          ]
      ; Reply.tool_call ~id:"h1" ~name:"hold" ~arguments:"{}" ()
      ; Reply.text "both reports in"
      ]
    [ "one", [ Reply.text "report one" ]; "two", [ Reply.text "report two" ] ]
  @@ fun _t agent ->
  let ended = ref 0 in
  Agent.subscribe agent ~f:(function
    | Loop (Subagent_end _) ->
      incr ended;
      if !ended = 2 then Eio.Promise.resolve resolve_both ()
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  print_endline "--- last request";
  List.iter (List.hd_exn !requests).messages ~f:(fun m ->
    print_endline (message_line m));
  [%expect
    {|
    state: running=true subagents=[]
    agent_start
    user: go
    assistant:  subagent{"task":"one"} subagent{"task":"two"}
    subagent_start a1: one
    state: running=true subagents=[a1 running]
    subagent_start a2: two
    state: running=true subagents=[a1 running; a2 running]
    tool_result subagent: started agent a1 (one); its result will be delivered to you when it finishes; use subagent_wait to block on it
    tool_result subagent: started agent a2 (two); its result will be delivered to you when it finishes; use subagent_wait to block on it
    subagent_end a1: ok report one | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    state: running=true subagents=[a1 done; a2 running]
    subagent_end a2: ok report two | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    state: running=true subagents=[a1 done; a2 done]
    assistant:  hold{}
    tool_result hold: held
    state: running=true subagents=[]
    user: [subagent a1 finished] one | report one | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000] |  | [subagent a2 finished] two | report two | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    assistant: both reports in
    agent_end
    state: running=false subagents=[]
    --- last request
    user: go
    assistant:  subagent{"task":"one"} subagent{"task":"two"}
    tool_result subagent: started agent a1 (one); its result will be delivered to you when it finishes; use subagent_wait to block on it
    tool_result subagent: started agent a2 (two); its result will be delivered to you when it finishes; use subagent_wait to block on it
    assistant:  hold{}
    tool_result hold: held
    user: [subagent a1 finished] one | report one | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000] |  | [subagent a2 finished] two | report two | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    |}]
;;

let%expect_test "subagent_status, subagent_wait and subagent_cancel" =
  let gates = Gates.create () in
  Gates.hold gates "slow";
  with_agent
    ~before:(Gates.before gates)
    ~main:
      [ Reply.tool_calls
          [ "c1", "subagent", {|{"task":"quick"}|}
          ; "c2", "subagent", {|{"task":"slow"}|}
          ]
      ; Reply.tool_call
          ~id:"w1"
          ~name:"subagent_wait"
          ~arguments:{|{"ids":["a1"]}|}
          ()
      ; Reply.tool_call ~id:"s1" ~name:"subagent_status" ~arguments:"{}" ()
      ; Reply.tool_call
          ~id:"w2"
          ~name:"subagent_wait"
          ~arguments:{|{"timeout":0}|}
          ()
      ; Reply.tool_call
          ~id:"w3"
          ~name:"subagent_wait"
          ~arguments:{|{"ids":["a9"]}|}
          ()
      ; Reply.tool_call
          ~id:"x1"
          ~name:"subagent_cancel"
          ~arguments:{|{"id":"a2"}|}
          ()
      ; Reply.tool_call ~id:"s2" ~name:"subagent_status" ~arguments:"{}" ()
      ; Reply.tool_call ~id:"w4" ~name:"subagent_wait" ~arguments:"{}" ()
      ; Reply.text "done"
      ]
    [ "quick", [ Reply.text "quick report" ]; "slow", [ Reply.text "never" ] ]
  @@ fun _t agent ->
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  (* Everything was returned by the tools, so nothing is delivered again. *)
  print_messages (Agent.messages agent);
  [%expect
    {|
    state: running=true subagents=[]
    agent_start
    user: go
    assistant:  subagent{"task":"quick"} subagent{"task":"slow"}
    subagent_start a1: quick
    state: running=true subagents=[a1 running]
    subagent_start a2: slow
    state: running=true subagents=[a1 running; a2 running]
    tool_result subagent: started agent a1 (quick); its result will be delivered to you when it finishes; use subagent_wait to block on it
    tool_result subagent: started agent a2 (slow); its result will be delivered to you when it finishes; use subagent_wait to block on it
    subagent_end a1: ok quick report | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    state: running=true subagents=[a1 done; a2 running]
    assistant:  subagent_wait{"ids":["a1"]}
    state: running=true subagents=[a2 running]
    tool_result subagent_wait: [subagent a1 finished] quick | quick report | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    assistant:  subagent_status{}
    tool_result subagent_status: a1  finished, delivered  0s  quick  (last: finished) | a2  running  0s  slow  (last: waiting for the model)
    assistant:  subagent_wait{"timeout":0}
    tool_result subagent_wait: timed out; still running: a2 (slow)
    assistant:  subagent_wait{"ids":["a9"]}
    tool_result subagent_wait: invalid arguments: unknown subagent "a9"; known: a1, a2
    assistant:  subagent_cancel{"id":"a2"}
    subagent_end a2: error [cancelled] | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    state: running=true subagents=[a2 done]
    state: running=true subagents=[]
    tool_result subagent_cancel: [subagent a2 failed] slow | [cancelled] | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    assistant:  subagent_status{}
    tool_result subagent_status: a1  finished, delivered  0s  quick  (last: finished) | a2  failed, delivered  0s  slow  (last: finished)
    assistant:  subagent_wait{}
    tool_result subagent_wait: no subagents to wait for
    assistant: done
    agent_end
    state: running=false subagents=[]
    --- messages
    user: go
    assistant:  subagent{"task":"quick"} subagent{"task":"slow"}
    tool_result subagent: started agent a1 (quick); its result will be delivered to you when it finishes; use subagent_wait to block on it
    tool_result subagent: started agent a2 (slow); its result will be delivered to you when it finishes; use subagent_wait to block on it
    assistant:  subagent_wait{"ids":["a1"]}
    tool_result subagent_wait: [subagent a1 finished] quick | quick report | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    assistant:  subagent_status{}
    tool_result subagent_status: a1  finished, delivered  0s  quick  (last: finished) | a2  running  0s  slow  (last: waiting for the model)
    assistant:  subagent_wait{"timeout":0}
    tool_result subagent_wait: timed out; still running: a2 (slow)
    assistant:  subagent_wait{"ids":["a9"]}
    tool_result subagent_wait: invalid arguments: unknown subagent "a9"; known: a1, a2
    assistant:  subagent_cancel{"id":"a2"}
    tool_result subagent_cancel: [subagent a2 failed] slow | [cancelled] | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    assistant:  subagent_status{}
    tool_result subagent_status: a1  finished, delivered  0s  quick  (last: finished) | a2  failed, delivered  0s  slow  (last: finished)
    assistant:  subagent_wait{}
    tool_result subagent_wait: no subagents to wait for
    assistant: done
    |}]
;;

let%expect_test "aborting the main turn leaves subagents running" =
  let gates = Gates.create () in
  Gates.hold gates "background";
  Gates.hold gates "main";
  let main_requests = ref 0 in
  with_agent
    ~before:(fun ~key ->
      if String.equal key "main"
      then (
        incr main_requests;
        if !main_requests = 1 then Gates.release gates "main");
      Gates.before gates ~key)
    ~main:
      [ spawn "background"
      ; Reply.text "aborted"
      ; Reply.text "got it after all"
      ]
    [ "background", [ Reply.text "background report" ] ]
  @@ fun _t agent ->
  Or_error.ok_exn (Agent.prompt agent "go");
  (* The second main request blocks on its gate; abort it. *)
  while !main_requests < 2 do
    Eio.Fiber.yield ()
  done;
  Gates.hold gates "main";
  print_s [%sexp (Agent.abort agent : string list)];
  wait_turn agent;
  printf
    "--- aborted; subagents running: %b\n"
    (Agent.has_running_subagents agent);
  Gates.release gates "main";
  Gates.release gates "background";
  Agent.wait_idle agent;
  print_messages (Agent.messages agent);
  [%expect
    {|
    state: running=true subagents=[]
    agent_start
    user: go
    assistant:  subagent{"task":"background"}
    subagent_start a1: background
    state: running=true subagents=[a1 running]
    tool_result subagent: started agent a1 (background); its result will be delivered to you when it finishes; use subagent_wait to block on it
    ()
    assistant:
    agent_end
    state: running=false subagents=[a1 running]
    --- aborted; subagents running: true
    subagent_end a1: ok background report | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    state: running=true subagents=[]
    agent_start
    user: [subagent a1 finished] background | background report | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    assistant: got it after all
    agent_end
    state: running=false subagents=[]
    --- messages
    user: go
    assistant:  subagent{"task":"background"}
    tool_result subagent: started agent a1 (background); its result will be delivered to you when it finishes; use subagent_wait to block on it
    assistant:
    user: [subagent a1 finished] background | background report | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    assistant: got it after all
    |}]
;;

let%expect_test "reports ready at an abort wait for the next prompt" =
  let gates = Gates.create () in
  let finished, resolve_finished = Eio.Promise.create () in
  with_agent
    ~extra_tools:[ hold_tool (Eio.Promise.create () |> fst) ]
    ~before:(Gates.before gates)
    ~main:
      [ spawn "quick"
      ; Reply.tool_call ~id:"h1" ~name:"hold" ~arguments:"{}" ()
      ; Reply.text "both"
      ]
    [ "quick", [ Reply.text "quick report" ] ]
  @@ fun _t agent ->
  Agent.subscribe agent ~f:(function
    | Loop (Subagent_end _) -> Eio.Promise.resolve resolve_finished ()
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "go");
  Eio.Promise.await finished;
  print_s [%sexp (Agent.abort agent : string list)];
  wait_turn agent;
  print_endline "--- idle with a report pending";
  Or_error.ok_exn (Agent.prompt agent "next");
  Agent.wait_idle agent;
  [%expect
    {|
    state: running=true subagents=[]
    agent_start
    user: go
    assistant:  subagent{"task":"quick"}
    subagent_start a1: quick
    state: running=true subagents=[a1 running]
    tool_result subagent: started agent a1 (quick); its result will be delivered to you when it finishes; use subagent_wait to block on it
    subagent_end a1: ok quick report | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    state: running=true subagents=[a1 done]
    ()
    assistant:  hold
    tool_result hold: [cancelled]
    agent_end
    state: running=false subagents=[a1 done]
    --- idle with a report pending
    state: running=true subagents=[]
    agent_start
    user: [subagent a1 finished] quick | quick report | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    user: next
    assistant: both
    agent_end
    state: running=false subagents=[]
    |}]
;;

let%expect_test "cancel_subagent delivers the partial report; usage rolls up" =
  let gates = Gates.create () in
  Gates.hold gates "stuck";
  with_agent
    ~before:(Gates.before gates)
    ~main:[ spawn "stuck"; Reply.text "waiting"; Reply.text "it was cancelled" ]
    [ "stuck", [ Reply.text "never" ] ]
  @@ fun _t agent ->
  Or_error.ok_exn (Agent.prompt agent "go");
  wait_turn agent;
  print_s [%sexp (Agent.cancel_subagent agent ~agent_id:"a7" : unit Or_error.t)];
  print_s [%sexp (Agent.cancel_subagent agent ~agent_id:"a1" : unit Or_error.t)];
  Agent.wait_idle agent;
  print_s [%sexp (Agent.cancel_subagent agent ~agent_id:"a1" : unit Or_error.t)];
  let state = Agent.state agent in
  print_s [%sexp (state.usage : Usage.t), (state.cost_usd : float)];
  [%expect
    {|
    state: running=true subagents=[]
    agent_start
    user: go
    assistant:  subagent{"task":"stuck"}
    subagent_start a1: stuck
    state: running=true subagents=[a1 running]
    tool_result subagent: started agent a1 (stuck); its result will be delivered to you when it finishes; use subagent_wait to block on it
    assistant: waiting
    agent_end
    state: running=false subagents=[a1 running]
    (Error "unknown subagent \"a7\"; known: a1")
    (Ok ())
    subagent_end a1: error [cancelled] | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    state: running=true subagents=[]
    agent_start
    user: [subagent a1 failed] stuck | [cancelled] | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    assistant: it was cancelled
    agent_end
    state: running=false subagents=[]
    (Error "subagent a1 already finished")
    (((input 50) (output 23) (cache_read 5)) 4.1129999999999994E-05)
    |}]
;;

let%expect_test "nested subagents stay synchronous; ids continue after reload" =
  with_agent
    ~main:[ spawn "outer"; Reply.text "spawned"; Reply.text "outer finished" ]
    [ ( "outer"
      , [ Reply.tool_call
            ~id:"n1"
            ~name:"subagent"
            ~arguments:{|{"task":"inner"}|}
            ()
        ; Reply.text "outer report"
        ] )
    ; "inner", [ Reply.text "inner report" ]
    ]
  @@ fun t agent ->
  let tools = ref [] in
  Agent.subscribe agent ~f:(function
    | Loop (Subagent { event = Tool_end { result; _ }; _ }) ->
      tools := result.text :: !tools
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  print_endline "--- the outer agent's tool result";
  List.iter !tools ~f:(fun text -> print_endline (one_line text));
  (* A reloaded agent numbers new subagents after the ones in the session. *)
  Eio.Switch.run
  @@ fun sw ->
  let provider =
    Routed_provider.create
      ~main:[ spawn "again"; Reply.text "ok"; Reply.text "ok" ]
      [ "again", [ Reply.text "again report" ] ]
  in
  let reloaded =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:(Tools.all @ subagent_tools ~provider)
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~session:(Agent.session agent)
      ~cwd:t.dir
      ()
  in
  Or_error.ok_exn (Agent.prompt reloaded "more");
  Agent.wait_idle reloaded;
  print_messages (List.drop (Agent.messages reloaded) 6);
  [%expect
    {|
    state: running=true subagents=[]
    agent_start
    user: go
    assistant:  subagent{"task":"outer"}
    subagent_start a1: outer
    state: running=true subagents=[a1 running]
    tool_result subagent: started agent a1 (outer); its result will be delivered to you when it finishes; use subagent_wait to block on it
    assistant: spawned
    agent_end
    state: running=false subagents=[a1 running]
    subagent_end a1: ok outer report | [subagent: 2 turns, 40 in / 18 out tokens, $0.0000]
    state: running=true subagents=[]
    agent_start
    user: [subagent a1 finished] outer | outer report | [subagent: 2 turns, 40 in / 18 out tokens, $0.0000]
    assistant: outer finished
    agent_end
    state: running=false subagents=[]
    --- the outer agent's tool result
    inner report | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    --- messages
    user: more
    assistant:  subagent{"task":"again"}
    tool_result subagent: started agent a2 (again); its result will be delivered to you when it finishes; use subagent_wait to block on it
    assistant: ok
    user: [subagent a2 finished] again | again report | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    assistant: ok
    |}]
;;

let%expect_test "new_session in a single-agent embedding drops running agents" =
  let gates = Gates.create () in
  Gates.hold gates "doomed";
  with_agent
    ~before:(Gates.before gates)
    ~main:[ spawn "doomed"; Reply.text "spawned" ]
    [ "doomed", [ Reply.text "never" ] ]
  @@ fun _t agent ->
  Or_error.ok_exn (Agent.prompt agent "go");
  wait_turn agent;
  Agent.new_session agent;
  printf
    "--- new session: %d messages, subagents running: %b\n"
    (List.length (Agent.messages agent))
    (Agent.has_running_subagents agent);
  [%expect
    {|
    state: running=true subagents=[]
    agent_start
    user: go
    assistant:  subagent{"task":"doomed"}
    subagent_start a1: doomed
    state: running=true subagents=[a1 running]
    tool_result subagent: started agent a1 (doomed); its result will be delivered to you when it finishes; use subagent_wait to block on it
    assistant: spawned
    agent_end
    state: running=false subagents=[a1 running]
    state: running=false subagents=[]
    subagent_end a1: error [cancelled] | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    --- new session: 0 messages, subagents running: false
    |}]
;;
