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
