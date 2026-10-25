open! Core
open! Prigh
open Test_background_subagents
module Reply = Faux_provider.Reply
module Gates = Routed_provider.Gates

(* What [list_subagents] says: no subagent may stay "running" once nothing
   runs it. *)
let print_subagents agent =
  print_endline "--- subagents";
  List.iter (Agent.subagents agent) ~f:(fun s ->
    printf
      "%s %s: %s\n"
      s.id
      (Subagent_log.State.to_string s.state)
      (Option.value_map s.result ~default:"" ~f:(fun r -> one_line r.text)))
;;

let%expect_test "a background subagent whose loop raises still ends" =
  with_agent
    ~main:[ spawn "explode"; Reply.text "started"; Reply.text "noted" ]
    []
  @@ fun _t agent ->
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  print_subagents agent;
  [%expect
    {|
    state: running=true subagents=[]
    agent_start
    user: go
    assistant:  subagent{"task":"explode"}
    subagent_start a1: explode
    state: running=true subagents=[a1 running]
    tool_result subagent: started agent a1 (explode); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    subagent_end a1: error subagent failed: ("no script for agent" (key explode))
    state: running=true subagents=[a1 done]
    assistant: started
    agent_end
    state: running=true subagents=[]
    agent_start
    user: [subagent a1 failed] explode | subagent failed: ("no script for agent" (key explode))
    assistant: noted
    agent_end
    state: running=false subagents=[]
    --- subagents
    a1 failed: subagent failed: ("no script for agent" (key explode))
    |}]
;;

let%expect_test "a nested synchronous subagent whose loop raises still ends" =
  with_agent
    ~main:[ spawn "outer"; Reply.text "started"; Reply.text "noted" ]
    [ "outer", [ spawn ~id:"n1" "inner"; Reply.text "inner broke" ] ]
  @@ fun _t agent ->
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  print_subagents agent;
  [%expect
    {|
    state: running=true subagents=[]
    agent_start
    user: go
    assistant:  subagent{"task":"outer"}
    subagent_start a1: outer
    state: running=true subagents=[a1 running]
    tool_result subagent: started agent a1 (outer); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: started
    agent_end
    state: running=false subagents=[a1 running]
    subagent_end a1: ok inner broke | [subagent: 2 turns, 30 in / 13 out tokens, $0.0000]
    state: running=true subagents=[]
    agent_start
    user: [subagent a1 finished] outer | inner broke | [subagent: 2 turns, 30 in / 13 out tokens, $0.0000]
    assistant: noted
    agent_end
    state: running=false subagents=[]
    --- subagents
    a1 complete: inner broke | [subagent: 2 turns, 30 in / 13 out tokens, $0.0000]
    a1/n1 failed: subagent failed: ("no script for agent" (key inner))
    |}]
;;

let%expect_test "cancelling a background subagent ends its nested one" =
  let gates = Gates.create () in
  Gates.hold gates "inner";
  with_agent
    ~before:(Gates.before gates)
    ~main:[ spawn "outer"; Reply.text "started"; Reply.text "noted" ]
    [ "outer", [ spawn ~id:"n1" "inner"; Reply.text "outer done" ]
    ; "inner", [ Reply.text "inner done" ]
    ]
  @@ fun _t agent ->
  Or_error.ok_exn (Agent.prompt agent "go");
  let rec wait_nested () =
    if
      not
        (List.exists (Agent.subagents agent) ~f:(fun s ->
           String.equal s.id "a1/n1"))
    then (
      Eio.Fiber.yield ();
      wait_nested ())
  in
  wait_nested ();
  wait_turn agent;
  print_subagents agent;
  print_s [%sexp (Agent.cancel_subagent agent ~agent_id:"a1" : unit Or_error.t)];
  Agent.wait_idle agent;
  print_subagents agent;
  [%expect
    {|
    state: running=true subagents=[]
    agent_start
    user: go
    assistant:  subagent{"task":"outer"}
    subagent_start a1: outer
    state: running=true subagents=[a1 running]
    tool_result subagent: started agent a1 (outer); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: started
    agent_end
    state: running=false subagents=[a1 running]
    --- subagents
    a1 running:
    a1/n1 running:
    (Ok ())
    subagent_end a1: error [cancelled] | [subagent: 1 turns, 30 in / 13 out tokens, $0.0000]
    state: running=true subagents=[]
    agent_start
    user: [subagent a1 failed] outer | [cancelled] | [subagent: 1 turns, 30 in / 13 out tokens, $0.0000]
    assistant: noted
    agent_end
    state: running=false subagents=[]
    --- subagents
    a1 failed: [cancelled] | [subagent: 1 turns, 30 in / 13 out tokens, $0.0000]
    a1/n1 failed: [cancelled] | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    |}]
;;

let emitting_tool name ~run =
  { Tool.spec =
      { Tool_spec.name
      ; description = name
      ; parameters = Tool_args.schema []
      ; parallel_safe = false
      ; destructive = false
      ; on_host = false
      ; freeform = None
      }
  ; run
  }
;;

let start ~call_id agent_id : Agent_event.t =
  Subagent_start
    { call_id
    ; agent_id
    ; task = agent_id
    ; model = "faux"
    ; tools = []
    ; worktree = None
    }
;;

(* Starts that never get an end, as if what ran them died: one directly in
   the main run, and one nested in a background subagent that ends. *)
let orphan_tools =
  [ emitting_tool "orphan" ~run:(fun context _ ->
      context.emit (start ~call_id:context.call_id context.call_id);
      Tool.Result.ok "started one")
  ; emitting_tool "orphan_bg" ~run:(fun context _ ->
      let background = Option.value_exn context.background in
      let id =
        Background_tasks.spawn
          background
          ~kind:Subagent
          ~label:"parent"
          ~run:(fun ~id ~cancel:_ ~emit ~on_output:_ ->
            emit (start ~call_id:context.call_id id);
            emit
              (Subagent
                 { call_id = context.call_id
                 ; agent_id = id
                 ; event = start ~call_id:"n1" (id ^ "/n1")
                 });
            emit
              (Subagent_end
                 { call_id = context.call_id
                 ; agent_id = id
                 ; usage = Usage.zero
                 ; turns = 0
                 ; cost_usd = 0.
                 ; result = Tool.Result.ok "parent done"
                 });
            { status = "finished"; body = "parent done"; is_error = false })
      in
      Tool.Result.ok ("started " ^ id))
  ]
;;

let%expect_test "the sweep ends subagents whose run or parent is gone" =
  with_agent
    ~extra_tools:orphan_tools
    ~main:
      [ Reply.tool_call ~id:"o1" ~name:"orphan" ~arguments:"{}" ()
      ; Reply.tool_call ~id:"o2" ~name:"orphan_bg" ~arguments:"{}" ()
      ; Reply.text "done"
      ; Reply.text "noted"
      ]
    []
  @@ fun _t agent ->
  let nested = ref [] in
  Agent.subscribe agent ~f:(function
    | Loop
        (Subagent
           { agent_id; event = Subagent_end { agent_id = inner; result; _ }; _ })
      ->
      nested
      := sprintf "%s > %s: %s" agent_id inner (one_line result.text) :: !nested
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  print_endline "--- nested ends";
  List.iter (List.rev !nested) ~f:print_endline;
  print_subagents agent;
  [%expect
    {|
    state: running=true subagents=[]
    agent_start
    user: go
    assistant:  orphan{}
    subagent_start o1: o1
    tool_result orphan: started one
    assistant:  orphan_bg{}
    subagent_start a1: a1
    subagent_end a1: ok parent done
    state: running=true subagents=[a1 done]
    tool_result orphan_bg: started a1
    state: running=true subagents=[]
    user: [subagent a1 finished] parent | parent done
    assistant: done
    agent_end
    subagent_end o1: error [interrupted] the run ended
    state: running=false subagents=[]
    --- nested ends
    a1 > a1/n1: [interrupted] background subagent a1 ended
    --- subagents
    o1 failed: [interrupted] the run ended
    a1 complete: parent done
    a1/n1 failed: [interrupted] background subagent a1 ended
    |}]
;;

(* An in-place session switch leaves nothing of the old session's
   background work listed. *)
let%expect_test "new_session forgets the old session's jobs" =
  with_agent ~main:[ Reply.text "noted" ] []
  @@ fun _t agent ->
  let job_id = Agent.start_job agent ~command:"echo hi" in
  Agent.wait_idle agent;
  let jobs () =
    List.map (Agent.jobs agent) ~f:(fun job ->
      Background_tasks.Task.id job, Background_tasks.Task.running job)
  in
  print_s [%sexp (job_id : string), (jobs () : (string * bool) list)];
  Agent.new_session agent;
  print_s [%sexp (jobs () : (string * bool) list)];
  [%expect
    {|
    state: running=false subagents=[]
    state: running=true subagents=[]
    agent_start
    user: [job j1 exited 0] echo hi | hi
    assistant: noted
    agent_end
    state: running=false subagents=[]
    (j1 ((j1 false)))
    ()
    |}]
;;
