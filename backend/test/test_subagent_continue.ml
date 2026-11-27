open! Core
open! Prigh
open Test_background_subagents
module Reply = Faux_provider.Reply
module Gates = Routed_provider.Gates

let step = Reply.tool_call ~id:"s1" ~name:"step" ~arguments:"{}" ()

let spawn_limited ?(id = "c1") ~max_turns task =
  Reply.tool_call
    ~id
    ~name:"subagent"
    ~arguments:(sprintf {|{"task":%S,"max_turns":%d}|} task max_turns)
    ()
;;

let continue_ ?(id = "k1") arguments =
  Reply.tool_call ~id ~name:"subagent_continue" ~arguments ()
;;

(* What each of the subagent's requests holds, so a continued run is seen to
   carry on from its saved context. *)
let print_subagent_requests ~task ~key (r : Provider.Request.t) =
  if String.equal key task
  then (
    printf "--- %s's request:\n" task;
    List.iter r.messages ~f:(fun m -> printf "    %s\n" (message_line m)))
;;

let print_spend agent =
  let usage, _ = Session.spend (Agent.session agent) in
  printf "session spend: %d in / %d out tokens\n" usage.input usage.output
;;

let rec wait_until f =
  if not (f ())
  then (
    Eio.Fiber.yield ();
    wait_until f)
;;

(* How many runs of the main agent have ended. *)
let count_runs agent =
  let ends = ref 0 in
  Agent.subscribe agent ~f:(function
    | Loop (Agent_end _) -> incr ends
    | _ -> ());
  fun n -> wait_until (fun () -> !ends >= n)
;;

let%expect_test
    "a background subagent out of turns is continued with its context and \
     reports again"
  =
  let gates = Gates.create () in
  Gates.hold gates "work";
  with_agent
    ~extra_tools:[ noop_tool ]
    ~before:(Gates.before gates)
    ~on_request:(print_subagent_requests ~task:"work")
    ~main:
      [ spawn_limited ~max_turns:1 "work"
      ; Reply.text "spawned"
      ; continue_ {|{"id":"a1","max_turns":3,"message":"finish up"}|}
      ; Reply.text "continued"
      ; Reply.text "thanks"
      ]
    [ "work", [ step; step; Reply.text "all done" ] ]
  @@ fun _t agent ->
  let runs = count_runs agent in
  Or_error.ok_exn (Agent.prompt agent "go");
  runs 1;
  (* Its continued run waits until the main agent has ended the run that
     continued it. *)
  Gates.release gates "work";
  Gates.hold gates "work";
  runs 2;
  Gates.release gates "work";
  Agent.wait_idle agent;
  print_spend agent;
  [%expect
    {|
    state: running=true subagents=[]
    agent_start
    user: [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).] |  | go
    assistant:  subagent{"task":"work","max_turns":1}
    subagent_start a1: work
    --- work's request:
        user: work
    state: running=true subagents=[a1 running]
    tool_result subagent: started agent a1 (work); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: spawned
    agent_end
    state: running=false subagents=[a1 running]
    subagent_end a1: error subagent hit the 1-turn limit without finishing | [subagent: 1 turns, 20 in / 8 out tokens, $0.0000] | [out of turns: call subagent_continue with id a1 (optionally max_turns, default 1, and a message) to give it more turns: it carries on from where it stopped; or subagent_cancel it, or leave it]
    state: running=true subagents=[]
    agent_start
    user: [subagent a1 out of turns] work | subagent hit the 1-turn limit without finishing | [subagent: 1 turns, 20 in / 8 out tokens, $0.0000] | [out of turns: call subagent_continue with id a1 (optionally max_turns, default 1, and a message) to give it more turns: it carries on from where it stopped; or subagent_cancel it, or leave it]
    assistant:  subagent_continue{"id":"a1","max_turns":3,"message":"finish up"}
    subagent_start a1: work
    --- work's request:
        user: work
        assistant:  step{}
        tool_result step: stepped
        user: [Your caller sends you this while you work.] |  | finish up
    state: running=true subagents=[a1 running]
    tool_result subagent_continue: continuing a1 with 3 more turns; its report will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: continued
    agent_end
    state: running=false subagents=[a1 running]
    --- work's request:
        user: work
        assistant:  step{}
        tool_result step: stepped
        user: [Your caller sends you this while you work.] |  | finish up
        assistant:  step{}
        tool_result step: stepped
    subagent_end a1: ok all done | [subagent: 3 turns, 50 in / 21 out tokens, $0.0000]
    state: running=true subagents=[]
    agent_start
    user: [subagent a1 finished] work | all done | [subagent: 3 turns, 50 in / 21 out tokens, $0.0000]
    assistant: thanks
    agent_end
    state: running=false subagents=[]
    session spend: 50 in / 21 out tokens
    |}]
;;

let%expect_test "subagent_continue's errors say what to do instead" =
  let gates = Gates.create () in
  Gates.hold gates "cancelled_one";
  Gates.hold gates "slow";
  with_agent
    ~main:
      [ spawn ~id:"c1" "cancelled_one"
      ; spawn ~id:"c2" "slow"
      ; Reply.text "spawned"
      ; Reply.tool_calls
          [ "k1", "subagent_continue", {|{"id":"a1"}|}
          ; "k2", "subagent_continue", {|{"id":"a2"}|}
          ; "k3", "subagent_continue", {|{"id":"a9"}|}
          ; "k4", "subagent_continue", {|{"id":"a2","max_turns":0}|}
          ]
      ; Reply.text "ok"
      ; Reply.text "ok"
      ]
    ~before:(Gates.before gates)
    [ "cancelled_one", [ Reply.text "never" ]
    ; "slow", [ Reply.text "slow done" ]
    ]
  @@ fun _t agent ->
  let runs = count_runs agent in
  Or_error.ok_exn (Agent.prompt agent "go");
  runs 1;
  Or_error.ok_exn (Agent.cancel_subagent agent ~agent_id:"a1");
  runs 2;
  Gates.release gates "slow";
  Agent.wait_idle agent;
  [%expect
    {|
    state: running=true subagents=[]
    agent_start
    user: [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).] |  | go
    assistant:  subagent{"task":"cancelled_one"}
    subagent_start a1: cancelled_one
    state: running=true subagents=[a1 running]
    tool_result subagent: started agent a1 (cancelled_one); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant:  subagent{"task":"slow"}
    subagent_start a2: slow
    state: running=true subagents=[a1 running; a2 running]
    tool_result subagent: started agent a2 (slow); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: spawned
    agent_end
    state: running=false subagents=[a1 running; a2 running]
    subagent_end a1: error [cancelled] | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    state: running=true subagents=[a2 running]
    agent_start
    user: [subagent a1 failed] cancelled_one | [cancelled] | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    assistant:  subagent_continue{"id":"a1"} subagent_continue{"id":"a2"} subagent_continue{"id":"a9"} subagent_continue{"id":"a2","max_turns":0}
    tool_result subagent_continue: invalid arguments: a1 ended (failed) and cannot be continued (those that can: none); start a new subagent for more work
    tool_result subagent_continue: invalid arguments: a2 is still running: use subagent_message to tell it something
    tool_result subagent_continue: invalid arguments: unknown subagent "a9": only a finished, failed, or out-of-turns subagent of yours can be continued (those that can: none)
    tool_result subagent_continue: invalid arguments: max_turns must be at least 1
    assistant: ok
    agent_end
    state: running=false subagents=[a2 running]
    subagent_end a2: ok slow done | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    state: running=true subagents=[]
    agent_start
    user: [subagent a2 finished] slow | slow done | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    assistant: ok
    agent_end
    state: running=false subagents=[]
    |}]
;;

let%expect_test "cancelling or resetting forgets an out-of-turns subagent" =
  let gates = Gates.create () in
  Gates.hold gates "one";
  Gates.hold gates "two";
  with_agent
    ~extra_tools:[ noop_tool ]
    ~before:(Gates.before gates)
    ~main:
      [ spawn_limited ~id:"c1" ~max_turns:1 "one"
      ; Reply.text "spawned"
      ; Reply.tool_call
          ~id:"x1"
          ~name:"subagent_cancel"
          ~arguments:{|{"id":"a1"}|}
          ()
      ; continue_ ~id:"k1" {|{"id":"a1"}|}
      ; Reply.text "ok"
      ; spawn_limited ~id:"c3" ~max_turns:1 "two"
      ; Reply.text "spawned two"
      ; Reply.text "noted"
      ; continue_ ~id:"k2" {|{"id":"a2"}|}
      ; Reply.text "ok"
      ]
    [ "one", [ step ]; "two", [ step ] ]
  @@ fun _t agent ->
  let runs = count_runs agent in
  Or_error.ok_exn (Agent.prompt agent "go");
  runs 1;
  Gates.release gates "one";
  runs 2;
  Or_error.ok_exn (Agent.prompt agent "more");
  runs 3;
  Gates.release gates "two";
  runs 4;
  print_endline "--- new session";
  Agent.new_session agent;
  Or_error.ok_exn (Agent.prompt agent "again");
  Agent.wait_idle agent;
  [%expect
    {|
    state: running=true subagents=[]
    agent_start
    user: [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).] |  | go
    assistant:  subagent{"task":"one","max_turns":1}
    subagent_start a1: one
    state: running=true subagents=[a1 running]
    tool_result subagent: started agent a1 (one); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: spawned
    agent_end
    state: running=false subagents=[a1 running]
    subagent_end a1: error subagent hit the 1-turn limit without finishing | [subagent: 1 turns, 20 in / 8 out tokens, $0.0000] | [out of turns: call subagent_continue with id a1 (optionally max_turns, default 1, and a message) to give it more turns: it carries on from where it stopped; or subagent_cancel it, or leave it]
    state: running=true subagents=[]
    agent_start
    user: [subagent a1 out of turns] one | subagent hit the 1-turn limit without finishing | [subagent: 1 turns, 20 in / 8 out tokens, $0.0000] | [out of turns: call subagent_continue with id a1 (optionally max_turns, default 1, and a message) to give it more turns: it carries on from where it stopped; or subagent_cancel it, or leave it]
    assistant:  subagent_cancel{"id":"a1"}
    tool_result subagent_cancel: [subagent a1 out of turns] one | subagent hit the 1-turn limit without finishing | [subagent: 1 turns, 20 in / 8 out tokens, $0.0000] | [out of turns: call subagent_continue with id a1 (optionally max_turns, default 1, and a message) to give it more turns: it carries on from where it stopped; or subagent_cancel it, or leave it]
    assistant:  subagent_continue{"id":"a1"}
    tool_result subagent_continue: invalid arguments: a1 ran out of turns but was cancelled since, so it cannot be continued (those that can: none); start a new subagent for more work
    assistant: ok
    agent_end
    state: running=false subagents=[]
    state: running=true subagents=[]
    agent_start
    user: more
    assistant:  subagent{"task":"two","max_turns":1}
    subagent_start a2: two
    state: running=true subagents=[a2 running]
    tool_result subagent: started agent a2 (two); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: spawned two
    agent_end
    state: running=false subagents=[a2 running]
    subagent_end a2: error subagent hit the 1-turn limit without finishing | [subagent: 1 turns, 20 in / 8 out tokens, $0.0000] | [out of turns: call subagent_continue with id a2 (optionally max_turns, default 1, and a message) to give it more turns: it carries on from where it stopped; or subagent_cancel it, or leave it]
    state: running=true subagents=[]
    agent_start
    user: [subagent a2 out of turns] two | subagent hit the 1-turn limit without finishing | [subagent: 1 turns, 20 in / 8 out tokens, $0.0000] | [out of turns: call subagent_continue with id a2 (optionally max_turns, default 1, and a message) to give it more turns: it carries on from where it stopped; or subagent_cancel it, or leave it]
    assistant: noted
    agent_end
    state: running=false subagents=[]
    --- new session
    state: running=true subagents=[]
    agent_start
    user: [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).] |  | again
    assistant:  subagent_continue{"id":"a2"}
    tool_result subagent_continue: invalid arguments: unknown subagent "a2": only a finished, failed, or out-of-turns subagent of yours can be continued (those that can: none)
    assistant: ok
    agent_end
    state: running=false subagents=[]
    |}]
;;

(* A background subagent's own subagents are synchronous: one that ran out of
   turns or finished can be continued by its caller, in its caller's run. *)
let%expect_test
    "a synchronous subagent (out of turns or finished) is continued by its \
     caller"
  =
  let subagent_events = ref [] in
  with_agent
    ~extra_tools:[ noop_tool ]
    ~on_request:(print_subagent_requests ~task:"inner")
    ~main:[ spawn "outer"; Reply.text "spawned"; Reply.text "thanks" ]
    [ ( "outer"
      , [ spawn_limited ~id:"n1" ~max_turns:1 "inner"
        ; continue_ ~id:"k1" {|{"id":"a1/n1","message":"wrap up"}|}
        ; continue_ ~id:"k2" {|{"id":"a1/n1","message":"one more check"}|}
        ; continue_ ~id:"k3" {|{"id":"a1/n9"}|}
        ; Reply.text "outer done"
        ] )
    ; ( "inner"
      , [ step; Reply.text "inner done"; Reply.text "extra check passed" ] )
    ]
  @@ fun _t agent ->
  Agent.subscribe agent ~f:(function
    | Loop (Subagent { event = Subagent_start s; _ }) ->
      subagent_events
      := sprintf
           "start %s (call %s, continued %b)"
           s.agent_id
           s.call_id
           s.continued
         :: !subagent_events
    | Loop (Subagent { event = Subagent_end e; _ }) ->
      subagent_events
      := sprintf "end %s (%d turns)" e.agent_id e.turns :: !subagent_events
    | Loop (Subagent { event = Tool_end { result; _ }; _ }) ->
      subagent_events
      := sprintf "outer's tool result: %s" (one_line result.text)
         :: !subagent_events
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  print_endline "--- outer's view";
  List.iter (List.rev !subagent_events) ~f:print_endline;
  print_spend agent;
  [%expect
    {|
    state: running=true subagents=[]
    agent_start
    user: [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).] |  | go
    assistant:  subagent{"task":"outer"}
    subagent_start a1: outer
    state: running=true subagents=[a1 running]
    tool_result subagent: started agent a1 (outer); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: spawned
    agent_end
    state: running=false subagents=[a1 running]
    --- inner's request:
        user: inner
    --- inner's request:
        user: inner
        assistant:  step{}
        tool_result step: stepped
        user: [Your caller sends you this while you work.] |  | wrap up
    --- inner's request:
        user: inner
        assistant:  step{}
        tool_result step: stepped
        user: [Your caller sends you this while you work.] |  | wrap up
        assistant: inner done
        user: [Your caller sends you this while you work.] |  | one more check
    subagent_end a1: ok outer done | [subagent: 5 turns, 130 in / 55 out tokens, $0.0001]
    state: running=true subagents=[]
    agent_start
    user: [subagent a1 finished] outer | outer done | [subagent: 5 turns, 130 in / 55 out tokens, $0.0001]
    assistant: thanks
    agent_end
    state: running=false subagents=[]
    --- outer's view
    start a1/n1 (call n1, continued false)
    end a1/n1 (1 turns)
    outer's tool result: subagent hit the 1-turn limit without finishing | [subagent: 1 turns, 20 in / 8 out tokens, $0.0000] | [out of turns: call subagent_continue with id a1/n1 (optionally max_turns, default 1, and a message) to give it more turns: it carries on from where it stopped; or leave it (it is dropped when you finish)]
    start a1/n1 (call k1, continued true)
    end a1/n1 (2 turns)
    outer's tool result: inner done | [subagent: 2 turns, 30 in / 13 out tokens, $0.0000]
    start a1/n1 (call k2, continued true)
    end a1/n1 (3 turns)
    outer's tool result: extra check passed | [subagent: 3 turns, 40 in / 18 out tokens, $0.0000]
    outer's tool result: invalid arguments: unknown subagent "a1/n9": only a finished, failed, or out-of-turns subagent of yours can be continued (those that can: a1/n1)
    session spend: 130 in / 55 out tokens
    |}]
;;

let%expect_test
    "continuing a finished background subagent carries on with its context"
  =
  let gates = Gates.create () in
  Gates.hold gates "worker";
  with_agent
    ~before:(Gates.before gates)
    ~on_request:(print_subagent_requests ~task:"worker")
    ~main:
      [ spawn "worker"
      ; Reply.text "spawned"
      ; continue_ ~id:"k1" {|{"id":"a1","message":"now check edge cases"}|}
      ; Reply.text "asked for follow-up"
      ; continue_ ~id:"k2" {|{"id":"a1"}|}
      ; Reply.text "continued without message"
      ; Reply.text "all set"
      ]
    [ ( "worker"
      , [ Reply.text "initial result"
        ; Reply.text "edge cases ok"
        ; Reply.text "final confirmation"
        ] )
    ]
  @@ fun _t agent ->
  let runs = count_runs agent in
  Or_error.ok_exn (Agent.prompt agent "go");
  runs 1;
  Gates.release gates "worker";
  Gates.hold gates "worker";
  runs 2;
  Gates.release gates "worker";
  Gates.hold gates "worker";
  runs 3;
  Gates.release gates "worker";
  Agent.wait_idle agent;
  [%expect
    {|
    state: running=true subagents=[]
    agent_start
    user: [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).] |  | go
    assistant:  subagent{"task":"worker"}
    subagent_start a1: worker
    --- worker's request:
        user: worker
    state: running=true subagents=[a1 running]
    tool_result subagent: started agent a1 (worker); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: spawned
    agent_end
    state: running=false subagents=[a1 running]
    subagent_end a1: ok initial result | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    state: running=true subagents=[]
    agent_start
    user: [subagent a1 finished] worker | initial result | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    assistant:  subagent_continue{"id":"a1","message":"now check edge cases"}
    subagent_start a1: worker
    --- worker's request:
        user: worker
        assistant: initial result
        user: [Your caller sends you this while you work.] |  | now check edge cases
    state: running=true subagents=[a1 running]
    tool_result subagent_continue: continuing a1; its report will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: asked for follow-up
    agent_end
    state: running=false subagents=[a1 running]
    subagent_end a1: ok edge cases ok | [subagent: 2 turns, 20 in / 10 out tokens, $0.0000]
    state: running=true subagents=[]
    agent_start
    user: [subagent a1 finished] worker | edge cases ok | [subagent: 2 turns, 20 in / 10 out tokens, $0.0000]
    assistant:  subagent_continue{"id":"a1"}
    subagent_start a1: worker
    --- worker's request:
        user: worker
        assistant: initial result
        user: [Your caller sends you this while you work.] |  | now check edge cases
        assistant: edge cases ok
        user: [Your caller sends you this while you work.] |  | Continue.
    state: running=true subagents=[a1 running]
    tool_result subagent_continue: continuing a1; its report will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: continued without message
    agent_end
    state: running=false subagents=[a1 running]
    subagent_end a1: ok final confirmation | [subagent: 3 turns, 30 in / 15 out tokens, $0.0000]
    state: running=true subagents=[]
    agent_start
    user: [subagent a1 finished] worker | final confirmation | [subagent: 3 turns, 30 in / 15 out tokens, $0.0000]
    assistant: all set
    agent_end
    state: running=false subagents=[]
    |}]
;;

let%expect_test
    "continuing a failed background subagent carries on, while cancelled \
     subagent_cancel unparks it"
  =
  let gates = Gates.create () in
  Gates.hold gates "flaky";
  with_agent
    ~before:(Gates.before gates)
    ~main:
      [ spawn "flaky"
      ; Reply.text "spawned"
      ; continue_ ~id:"k1" {|{"id":"a1","message":"retry with fallback"}|}
      ; Reply.text "retrying"
      ; Reply.tool_call
          ~id:"x1"
          ~name:"subagent_cancel"
          ~arguments:{|{"id":"a1"}|}
          ()
      ; continue_ ~id:"k2" {|{"id":"a1","message":"too late"}|}
      ; Reply.text "done"
      ]
    [ "flaky", [ Reply.error "bad request"; Reply.text "recovered!" ] ]
  @@ fun _t agent ->
  let runs = count_runs agent in
  Or_error.ok_exn (Agent.prompt agent "go");
  runs 1;
  Gates.release gates "flaky";
  Gates.hold gates "flaky";
  runs 2;
  Gates.release gates "flaky";
  Agent.wait_idle agent;
  [%expect
    {|
    state: running=true subagents=[]
    agent_start
    user: [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).] |  | go
    assistant:  subagent{"task":"flaky"}
    subagent_start a1: flaky
    state: running=true subagents=[a1 running]
    tool_result subagent: started agent a1 (flaky); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: spawned
    agent_end
    state: running=false subagents=[a1 running]
    subagent_end a1: error subagent failed: bad request | [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
    state: running=true subagents=[]
    agent_start
    user: [subagent a1 failed] flaky | subagent failed: bad request | [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
    assistant:  subagent_continue{"id":"a1","message":"retry with fallback"}
    subagent_start a1: flaky
    state: running=true subagents=[a1 running]
    tool_result subagent_continue: continuing a1; its report will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: retrying
    agent_end
    state: running=false subagents=[a1 running]
    subagent_end a1: ok recovered! | [subagent: 2 turns, 10 in / 5 out tokens, $0.0000]
    state: running=true subagents=[]
    agent_start
    user: [subagent a1 finished] flaky | recovered! | [subagent: 2 turns, 10 in / 5 out tokens, $0.0000]
    assistant:  subagent_cancel{"id":"a1"}
    tool_result subagent_cancel: [subagent a1 finished] flaky | recovered! | [subagent: 2 turns, 10 in / 5 out tokens, $0.0000]
    assistant:  subagent_continue{"id":"a1","message":"too late"}
    tool_result subagent_continue: invalid arguments: a1 finished and cannot be continued (those that can: none); start a new subagent for more work
    assistant: done
    agent_end
    state: running=false subagents=[]
    |}]
;;
