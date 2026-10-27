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

let%expect_test "steering a finished subagent is refused; nothing runs again" =
  with_agent
    ~on_request:print_requests
    ~main:[ spawn "work"; Reply.text "spawned"; Reply.text "got it" ]
    [ "work", [ Reply.text "done" ] ]
  @@ fun _t agent ->
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  log_subagent agent;
  print_endline "--- after its report was delivered";
  send agent "and another thing";
  send agent ~kind:Follow_up "and another thing";
  print_s
    [%sexp
      (Agent.steer_subagent agent ~agent_id:"a7" ~kind:Steer "hello"
       : unit Or_error.t)];
  Agent.wait_idle agent;
  print_s [%sexp (Agent.is_running agent : bool)];
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
    subagent_end a1: ok done | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    state: running=true subagents=[a1 done]
    assistant: spawned
    agent_end
    state: running=true subagents=[]
    agent_start
    user: [subagent a1 finished] work | done | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    assistant: got it
    agent_end
    state: running=false subagents=[]
    --- after its report was delivered
    (Error
     "a1 has finished and its report was delivered to the main agent: tell the main agent instead, or have it start a new subagent")
    (Error
     "a1 has finished and its report was delivered to the main agent: tell the main agent instead, or have it start a new subagent")
    (Error "unknown subagent \"a7\"; known: a1")
    false
    --- a1's transcript
    user: work
    assistant: done
    (complete, 2 messages)
    |}]
;;

let%expect_test "two steers and a follow-up queued while a tool runs" =
  let tool, started, release = gate_tool "slow" in
  with_agent
    ~extra_tools:[ tool ]
    ~on_request:print_requests
    ~main:[ spawn "work"; Reply.text "spawned"; Reply.text "got it" ]
    [ ( "work"
      , [ Reply.tool_call ~id:"t1" ~name:"slow" ~arguments:"{}" ()
        ; Reply.text "steered"
        ; Reply.text "followed up"
        ] )
    ]
  @@ fun _t agent ->
  Or_error.ok_exn (Agent.prompt agent "go");
  wait_turn agent;
  Eio.Promise.await started;
  log_subagent agent;
  send agent "one";
  send agent ~kind:Follow_up "later";
  send agent "two";
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
    queue: a1 steer=[one] follow_up=[]
    (Ok ())
    queue: a1 steer=[one] follow_up=[later]
    (Ok ())
    queue: a1 steer=[one; two] follow_up=[later]
    (Ok ())
      [a1] tool_result slow: released
    queue: a1 steer=[] follow_up=[later]
      [a1] user: [The user sends you this while you work: take it into account and carry on.] |  | one
      [a1] user: [The user sends you this while you work: take it into account and carry on.] |  | two
    request work: u(work) a[t1] r[t1] u([The user sends you this) u([The user sends you this)
      [a1] assistant: steered
    queue:
      [a1] user: [The user sends you this as you finish: deal with it before you report.] |  | later
    request work: u(work) a[t1] r[t1] u([The user sends you this) u([The user sends you this) a u([The user sends you this)
      [a1] assistant: followed up
    subagent_end a1: ok followed up | [subagent: 3 turns, 40 in / 18 out tokens, $0.0000]
    state: running=true subagents=[]
    agent_start
    user: [subagent a1 finished] work | followed up | [subagent: 3 turns, 40 in / 18 out tokens, $0.0000]
    assistant: got it
    agent_end
    state: running=false subagents=[]
    --- a1's transcript
    user: work
    assistant:  slow{}
    tool_result slow: released
    user: [The user sends you this while you work: take it into account and carry on.] |  | one
    user: [The user sends you this while you work: take it into account and carry on.] |  | two
    assistant: steered
    user: [The user sends you this as you finish: deal with it before you report.] |  | later
    assistant: followed up
    (complete, 8 messages)
    |}]
;;

let%expect_test
    "steering after the main context was compacted and trimmed: the same \
     agent, and new ids stay new"
  =
  let tool, started, release = gate_tool "slow" in
  let long = String.make 40_000 'x' in
  with_agent
    ~extra_tools:[ tool ]
    ~on_request:print_requests
    ~main:
      [ spawn "work"
      ; Reply.text long
      ; Reply.text "SUMMARY"
      ; spawn ~id:"c2" "again"
      ; Reply.text "spawned again"
      ; Reply.text "got both"
      ]
    [ ( "work"
      , [ Reply.tool_call ~id:"t1" ~name:"slow" ~arguments:"{}" ()
        ; Reply.text "steered"
        ] )
    ; "again", [ Reply.text "again done" ]
    ]
  @@ fun _t agent ->
  Or_error.ok_exn (Agent.prompt agent "go");
  wait_turn agent;
  Eio.Promise.await started;
  print_endline "--- compact and trim the main context while a1 runs";
  print_s [%sexp (Agent.compact agent : string Or_error.t)];
  let last =
    List.find_map_exn
      (List.rev (Session.active_path (Agent.session agent)))
      ~f:(fun (e : Session.Entry.t) ->
        match e.payload with
        | Message (Assistant _) -> Some e.id
        | _ -> None)
  in
  print_s
    [%sexp
      (Or_error.map (Agent.trim agent ~before:last) ~f:ignore : unit Or_error.t)];
  print_s
    [%sexp
      (List.map (Agent.messages agent) ~f:(fun m ->
         String.prefix (message_line m) 40)
       : string list)];
  log_subagent agent;
  send agent "use the tests";
  release ();
  Agent.wait_idle agent;
  transcript agent "a1";
  transcript agent "a2";
  [%expect {|
    state: running=true subagents=[]
    agent_start
    user: go
    assistant:  subagent{"task":"work"}
    subagent_start a1: work
    request work: u(work)
    state: running=true subagents=[a1 running]
    tool_result subagent: started agent a1 (work); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: <40000 chars>
    agent_end
    state: running=false subagents=[a1 running]
    --- compact and trim the main context while a1 runs
    (Ok SUMMARY)
    (Ok ())
    ("assistant: <40000 chars>")
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
    assistant:  subagent{"task":"again"}
    subagent_start a2: again
      [a2] user: again
    request again: u(again)
    state: running=true subagents=[a2 running]
    tool_result subagent: started agent a2 (again); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
      [a2] assistant: again done
    subagent_end a2: ok again done | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    state: running=true subagents=[a2 done]
    assistant: spawned again
    agent_end
    state: running=true subagents=[]
    agent_start
    user: [subagent a2 finished] again | again done | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    assistant: got both
    agent_end
    state: running=false subagents=[]
    --- a1's transcript
    user: work
    assistant:  slow{}
    tool_result slow: released
    user: [The user sends you this while you work: take it into account and carry on.] |  | use the tests
    assistant: steered
    (complete, 5 messages)
    --- a2's transcript
    user: again
    assistant: again done
    (complete, 2 messages)
    |}]
;;

let%expect_test
    "a steered subagent's transcript after a reload: once, and new ids skip \
     it"
  =
  Tool_test_helpers.with_sandbox
  @@ fun (t : Tool_test_helpers.t) ->
  Eio.Switch.run
  @@ fun sw ->
  let tool, started, release = gate_tool "slow" in
  let provider =
    Routed_provider.create
      ~on_request:print_requests
      ~main:
        [ spawn "work"
        ; Reply.text "spawned"
        ; Reply.text "got it"
        ; spawn ~id:"c2" "again"
        ; Reply.text "spawned again"
        ; Reply.text "got it again"
        ]
      [ ( "work"
        , [ Reply.tool_call ~id:"t1" ~name:"slow" ~arguments:"{}" ()
          ; Reply.text "steered"
          ] )
      ; "again", [ Reply.text "again done" ]
      ]
  in
  let create ?session () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:(Tools.all @ subagent_tools ~provider @ [ tool ])
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ?session
      ~cwd:t.dir
      ()
  in
  let agent = create () in
  Or_error.ok_exn (Agent.prompt agent "go");
  wait_turn agent;
  Eio.Promise.await started;
  send agent "use the tests";
  release ();
  Agent.wait_idle agent;
  print_endline "--- reloaded";
  let session =
    Or_error.ok_exn (Session.load (Session.path (Agent.session agent)))
  in
  let agent = create ~session () in
  log_events agent;
  transcript agent "a1";
  print_s [%sexp (send agent "more" : unit)];
  Or_error.ok_exn (Agent.prompt agent "again");
  Agent.wait_idle agent;
  transcript agent "a2";
  [%expect {|
    request work: u(work)
    (Ok ())
    request work: u(work) a[t1] r[t1] u([The user sends you this)
    --- reloaded
    --- a1's transcript
    user: work
    assistant:  slow{}
    tool_result slow: released
    user: [The user sends you this while you work: take it into account and carry on.] |  | use the tests
    assistant: steered
    (complete, 5 messages)
    (Error "unknown subagent \"a1\"; known: none")
    ()
    state: running=true subagents=[]
    agent_start
    user: again
    assistant:  subagent{"task":"again"}
    subagent_start a2: again
    request again: u(again)
    state: running=true subagents=[a2 running]
    tool_result subagent: started agent a2 (again); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    subagent_end a2: ok again done | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    state: running=true subagents=[a2 done]
    assistant: spawned again
    agent_end
    state: running=true subagents=[]
    agent_start
    user: [subagent a2 finished] again | again done | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    assistant: got it again
    agent_end
    state: running=false subagents=[]
    --- a2's transcript
    user: again
    assistant: again done
    (complete, 2 messages)
    |}]
;;
