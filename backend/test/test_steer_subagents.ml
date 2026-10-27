open! Core
open! Prigh
open Test_background_subagents
module Reply = Faux_provider.Reply
module Gates = Routed_provider.Gates

(* Steering background subagents: what each request a steered subagent sends
   holds (the context must stay valid: every call answered before the next
   user turn), what its transcript and the main agent see, and what is
   refused. *)

(* The roles of a subagent's request, one letter each: u user, a assistant
   (with its calls), r tool result. *)
let print_requests ~key (request : Provider.Request.t) =
  if not (String.equal key "main")
  then
    printf
      "request %s: %s\n"
      key
      (String.concat
         ~sep:" "
         (List.map request.messages ~f:(fun (m : Message.t) ->
            match m with
            | User { text; _ } ->
              "u(" ^ String.prefix (one_line text) 24 ^ ")"
            | Assistant a ->
              "a"
              ^ String.concat
                  (List.map (Message.Assistant.tool_calls a) ~f:(fun c ->
                     "[" ^ c.id ^ "]"))
            | Tool_result r -> "r[" ^ r.tool_call_id ^ "]")))
;;

(* A tool that says when it starts and blocks until released. *)
let gate_tool name =
  let started, set_started = Eio.Promise.create () in
  let release, set_release = Eio.Promise.create () in
  ( { Tool.spec =
        { Tool_spec.name
        ; description = "wait"
        ; parameters = Tool_args.schema []
        ; parallel_safe = false
        ; destructive = Never
        ; on_host = false
        ; freeform = None
        }
    ; run =
        (fun _ _ ->
          Eio.Promise.resolve set_started ();
          Eio.Promise.await release;
          Tool.Result.ok "released")
    }
  , started
  , fun () -> Eio.Promise.resolve set_release () )
;;

let transcript agent id =
  printf "--- %s's transcript\n" id;
  match Agent.subagent agent id with
  | None -> print_endline "none"
  | Some (summary, messages) ->
    List.iter messages ~f:(fun (m : Timed_message.t) ->
      print_endline (message_line m.message));
    printf
      "(%s, %d messages)\n"
      (Subagent_log.State.to_string summary.state)
      summary.message_count
;;

let%expect_test "steering a subagent while its tool runs: after the result" =
  let tool, started, release = gate_tool "slow" in
  with_agent
    ~extra_tools:[ tool ]
    ~on_request:print_requests
    ~main:[ spawn "work"; Reply.text "spawned"; Reply.text "got it" ]
    [ ( "work"
      , [ Reply.tool_call ~id:"t1" ~name:"slow" ~arguments:"{}" ()
        ; Reply.text "steered"
        ] )
    ]
  @@ fun _t agent ->
  Or_error.ok_exn (Agent.prompt agent "go");
  wait_turn agent;
  Eio.Promise.await started;
  log_subagent agent;
  print_endline "--- steer while slow runs";
  send agent "use the tests";
  release ();
  Agent.wait_idle agent;
  transcript agent "a1";
  [%expect
    {|
    state: running=true subagents=[]
    agent_start
    user: go
    assistant:  subagent{"task":"work"}
    subagent_start a1: work
    request work: u(work)
    state: running=true subagents=[a1 running]
    tool_result subagent: started agent a1 (work); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: spawned
    agent_end
    state: running=false subagents=[a1 running]
    --- steer while slow runs
    queue: a1 steer=[use the tests] follow_up=[]
    (Ok ())
      [a1] tool_result slow: released
    queue:
      [a1] user: [The user sends you this while you work: take it into account and carry on.] |  | use the tests
    request work: u(work) a[t1] r[t1] u([The user sends you this)
      [a1] assistant: steered
    subagent_end a1: ok steered | [subagent: 2 turns, 30 in / 13 out tokens, $0.0000]
    state: running=true subagents=[]
    agent_start
    user: [subagent a1 finished] work | steered | [subagent: 2 turns, 30 in / 13 out tokens, $0.0000]
    assistant: got it
    agent_end
    state: running=false subagents=[]
    --- a1's transcript
    user: work
    assistant:  slow{}
    tool_result slow: released
    user: [The user sends you this while you work: take it into account and carry on.] |  | use the tests
    assistant: steered
    (complete, 5 messages)
    |}]
;;

let%expect_test
    "steering a subagent while it waits on its own subagent: after its result"
  =
  let gates = Gates.create () in
  Gates.hold gates "inner";
  with_agent
    ~before:(Gates.before gates)
    ~on_request:print_requests
    ~main:[ spawn "outer"; Reply.text "spawned"; Reply.text "got it" ]
    [ "outer", [ spawn ~id:"n1" "inner"; Reply.text "outer steered" ]
    ; "inner", [ Reply.text "inner done" ]
    ]
  @@ fun _t agent ->
  Or_error.ok_exn (Agent.prompt agent "go");
  wait_turn agent;
  log_subagent agent;
  print_endline "--- steer while inner runs";
  send agent "mind the docs";
  print_s
    [%sexp
      (Agent.steer_subagent agent ~agent_id:"a1/n1" ~kind:Steer "you too"
       : unit Or_error.t)];
  Gates.release gates "inner";
  Agent.wait_idle agent;
  transcript agent "a1";
  transcript agent "a1/n1";
  [%expect {|
    state: running=true subagents=[]
    agent_start
    user: go
    assistant:  subagent{"task":"outer"}
    subagent_start a1: outer
    request outer: u(outer)
    state: running=true subagents=[a1 running]
    tool_result subagent: started agent a1 (outer); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: spawned
    agent_end
    state: running=false subagents=[a1 running]
    --- steer while inner runs
    queue: a1 steer=[mind the docs] follow_up=[]
    (Ok ())
    (Error
     "a1/n1 runs inside a1 (a subagent's own subagent) and takes no messages: send it to a1")
      [a1] assistant:  subagent{"task":"inner"}
    request inner: u(inner)
      [a1] tool_result subagent: inner done | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    queue:
      [a1] user: [The user sends you this while you work: take it into account and carry on.] |  | mind the docs
    request outer: u(outer) a[n1] r[n1] u([The user sends you this)
      [a1] assistant: outer steered
    subagent_end a1: ok outer steered | [subagent: 2 turns, 40 in / 18 out tokens, $0.0000]
    state: running=true subagents=[]
    agent_start
    user: [subagent a1 finished] outer | outer steered | [subagent: 2 turns, 40 in / 18 out tokens, $0.0000]
    assistant: got it
    agent_end
    state: running=false subagents=[]
    --- a1's transcript
    user: outer
    assistant:  subagent{"task":"inner"}
    tool_result subagent: inner done | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    user: [The user sends you this while you work: take it into account and carry on.] |  | mind the docs
    assistant: outer steered
    (complete, 5 messages)
    --- a1/n1's transcript
    user: inner
    assistant: inner done
    (complete, 2 messages)
    |}]
;;

let%expect_test "steering a subagent just as it finishes" =
  with_agent
    ~on_request:print_requests
    ~main:[ spawn "work"; Reply.text "spawned"; Reply.text "got it" ]
    [ "work", [ Reply.text "first"; Reply.text "second" ] ]
  @@ fun _t agent ->
  (* From the event handlers, between its last reply and its end: nothing
     runs in between. *)
  Agent.subscribe agent ~f:(fun (event : Agent.Event.t) ->
    let steer when_ =
      printf
        "steer at %s: %s\n"
        when_
        (Sexp.to_string_hum
           [%sexp
             (Agent.steer_subagent agent ~agent_id:"a1" ~kind:Steer when_
              : unit Or_error.t)])
    in
    match event with
    | Loop
        (Subagent
          { agent_id = "a1"; event = Message_end (Assistant a); call_id = _ })
      ->
      if String.equal (Message.Assistant.text a) "first"
      then steer "its first reply"
    | Loop (Subagent { agent_id = "a1"; event = Agent_end _; call_id = _ }) ->
      steer "its loop's end"
    | Loop (Subagent_end { agent_id = "a1"; _ }) -> steer "its end"
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  transcript agent "a1";
  [%expect {|
    state: running=true subagents=[]
    agent_start
    user: go
    assistant:  subagent{"task":"work"}
    subagent_start a1: work
    request work: u(work)
    state: running=true subagents=[a1 running]
    tool_result subagent: started agent a1 (work); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    steer at its first reply: (Ok ())
    request work: u(work) a u([The user sends you this)
    assistant: spawned
    agent_end
    state: running=false subagents=[a1 running]
    steer at its loop's end: (Error
     "a1 has finished and its report is on its way to the main agent: tell the main agent instead, or have it start a new subagent")
    subagent_end a1: ok second | [subagent: 2 turns, 20 in / 10 out tokens, $0.0000]
    steer at its end: (Error
     "a1 has finished and its report is on its way to the main agent: tell the main agent instead, or have it start a new subagent")
    state: running=true subagents=[]
    agent_start
    user: [subagent a1 finished] work | second | [subagent: 2 turns, 20 in / 10 out tokens, $0.0000]
    assistant: got it
    agent_end
    state: running=false subagents=[]
    --- a1's transcript
    user: work
    assistant: first
    user: [The user sends you this while you work: take it into account and carry on.] |  | its first reply
    assistant: second
    (complete, 4 messages)
    |}]
;;

let%expect_test "steering a subagent on its last allowed turn" =
  let gates = Gates.create () in
  Gates.hold gates "work";
  with_agent
    ~before:(Gates.before gates)
    ~on_request:print_requests
    ~main:
      [ Reply.tool_call
          ~id:"c1"
          ~name:"subagent"
          ~arguments:{|{"task":"work","max_turns":1}|}
          ()
      ; Reply.text "spawned"
      ; Reply.text "got it"
      ]
    [ "work", [ Reply.text "done in one" ] ]
  @@ fun _t agent ->
  Or_error.ok_exn (Agent.prompt agent "go");
  wait_turn agent;
  log_subagent agent;
  send agent "one more thing";
  Gates.release gates "work";
  Agent.wait_idle agent;
  transcript agent "a1";
  [%expect {|
    state: running=true subagents=[]
    agent_start
    user: go
    assistant:  subagent{"task":"work","max_turns":1}
    subagent_start a1: work
    request work: u(work)
    state: running=true subagents=[a1 running]
    tool_result subagent: started agent a1 (work); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: spawned
    agent_end
    state: running=false subagents=[a1 running]
    queue: a1 steer=[one more thing] follow_up=[]
    (Ok ())
      [a1] assistant: done in one
    subagent_end a1: ok done in one | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    queue:
    notice: a1 stopped before it got your message "one more thing", which was not delivered: tell the main agent instead
    state: running=true subagents=[]
    agent_start
    user: [subagent a1 finished] work | done in one | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    assistant: got it
    agent_end
    state: running=false subagents=[]
    --- a1's transcript
    user: work
    assistant: done in one
    (complete, 2 messages)
    |}]
;;
