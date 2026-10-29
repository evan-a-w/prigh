open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Gates = Routed_provider.Gates
module Json = Jsonaf
module Fake_host = Test_host_reconnect.Fake_host

(* Background subagents of a session whose tools run on a frontend's
   machine, while that frontend's tool host comes and goes (a reload, a
   reconnect, a restart, a second frontend on that machine). They keep
   running with no client attached, so whatever their tool calls get while
   the host is away decides whether their work survives: calls wait for it
   to come back, calls it was running carry on when it comes back with them,
   and only those a restarted host lost fail, saying so. *)

let member_string = Test_host_reconnect.member_string

(* What the subagents' tools returned, as their last requests show it. *)
let seen = ref []

let print_subagent_results () =
  List.iter (List.rev !seen) ~f:(fun line -> print_endline ("subagent " ^ line));
  seen := []
;;

let with_session ?before ~main children f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  seen := [];
  let on_request ~key (request : Provider.Request.t) =
    if not (String.equal key "main")
    then
      seen
      := List.rev
           (List.filter_map request.messages ~f:(function
              | Message.Tool_result r ->
                Some (sprintf "tool_result %s: %s" r.tool_name r.text)
              | _ -> None))
  in
  let provider = Routed_provider.create ?before ~on_request ~main children in
  let agent, h =
    Test_rpc.make_server
      t
      ~sw
      ~provider
      ~tools:(Tools.all @ Test_background_subagents.subagent_tools ~provider)
  in
  f t ~sw agent h
;;

let connect_laptop ?running ~sw h agent =
  Fake_host.connect
    ~sw
    h
    ?running
    ~name:"laptop"
    ~host_id:"host-laptop"
    ~cwd:"/home/me"
    ~session:agent
;;

let spawn task =
  Reply.tool_call
    ~id:"c1"
    ~name:"subagent"
    ~arguments:(sprintf {|{"task":%S}|} task)
    ()
;;

let bash id =
  Reply.tool_call ~id ~name:"bash" ~arguments:{|{"command":"make test"}|} ()
;;

let start t (h : Test_rpc.H.t) agent =
  let ended, resolve = Eio.Promise.create () in
  Agent.subscribe agent ~f:(function
    | Loop (Agent_end _) when not (Eio.Promise.is_resolved ended) ->
      Eio.Promise.resolve resolve ()
    | _ -> ());
  Test_rpc.call t h ~params:{|{"text": "go"}|} "prompt";
  Eio.Promise.await ended
;;

(* With [host.answer] off: answers anything but the subagent's bash and
   returns that exec, left running on the host. *)
let wait_bash (h : Test_rpc.H.t) (host : Fake_host.t) =
  let rec loop tries =
    if tries = 0 then failwith "the subagent's bash never reached the host";
    match Queue.peek host.sent with
    | Some json when String.equal (member_string json "name") "bash" ->
      Queue.dequeue_exn host.sent
    | Some json when String.equal (member_string json "event") "tool_exec" ->
      ignore (Queue.dequeue_exn host.sent : Json.t);
      Fake_host.answer_exec h host ~name:"laptop" json;
      loop tries
    | _ ->
      Eio.Fiber.yield ();
      loop (tries - 1)
  in
  loop 100_000
;;

let result_from (h : Test_rpc.H.t) (host : Fake_host.t) exec =
  let reply =
    Rpc_server.handle
      h.server
      host.client
      (`Object
          [ "id", `Number "0"
          ; "method", `String "tool_exec_result"
          ; ( "params"
            , `Object
                [ "exec_id", `String (member_string exec "exec_id")
                ; "text", `String "tests passed"
                ; "is_error", `False
                ] )
          ])
  in
  printf
    "laptop's late result: %s\n"
    (match Json.member "error" reply with
     | Some (`String e) -> "rejected: " ^ List.hd_exn (String.split e ~on:'"')
     | _ -> "accepted")
;;

(* Resolved when a subagent's next tool call starts: its executor runs
   before the fiber waiting for it does. *)
let subagent_tool_started agent =
  let started, resolve = Eio.Promise.create () in
  Agent.subscribe agent ~f:(function
    | Loop (Subagent { event = Tool_start _; _ })
      when not (Eio.Promise.is_resolved started) ->
      Eio.Promise.resolve resolve ()
    | _ -> ());
  started
;;

let%expect_test "a subagent's tool calls wait while its host reconnects" =
  let gates = Gates.create () in
  Gates.hold gates "run the tests";
  let waiting, arrive = Eio.Promise.create () in
  with_session
    ~before:(fun ~key ->
      if
        String.equal key "run the tests"
        && not (Eio.Promise.is_resolved waiting)
      then Eio.Promise.resolve arrive ();
      Gates.before gates ~key)
    ~main:[ spawn "run the tests"; Reply.text "started"; Reply.text "noted" ]
    [ "run the tests", [ bash "s1"; bash "s2"; Reply.text "the tests pass" ] ]
  @@ fun t ~sw agent h ->
  let laptop = connect_laptop ~sw h agent in
  Test_background_subagents.log_events agent;
  start t h agent;
  (* The subagent is about to call the model when the frontend reloads: its
     tool host is away for a moment. *)
  Eio.Promise.await waiting;
  Fake_host.disconnect h laptop;
  let started = subagent_tool_started agent in
  Gates.release gates "run the tests";
  (* Its first call waits for the host, which comes back. *)
  Eio.Promise.await started;
  let (_ : Fake_host.t) = connect_laptop ~sw h agent in
  Agent.wait_idle agent;
  print_subagent_results ();
  Test_background_subagents.print_messages (Agent.messages agent);
  [%expect
    {|
    laptop hello: client_id=client-2 host_id=host-laptop
    state: running=true subagents=[]
    {"type":"response","id":"r1","ok":true,"result":{}}
    agent_start
    user: go
    assistant:  subagent{"task":"run the tests"}
    state: running=true subagents=[a1 running]
    tool_result subagent: started agent a1 (run the tests); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: started
    agent_end
    state: running=false subagents=[a1 running]
    subagent_start a1: run the tests
    laptop hello: client_id=client-3 host_id=host-laptop
    laptop ran bash in /home/me
    laptop ran bash in /home/me
    subagent_end a1: ok the tests pass | [subagent: 3 turns, 50 in / 21 out tokens, $0.0000]
    state: running=true subagents=[]
    agent_start
    user: [subagent a1 finished] run the tests | the tests pass | [subagent: 3 turns, 50 in / 21 out tokens, $0.0000]
    assistant: noted
    agent_end
    state: running=false subagents=[]
    subagent tool_result bash: ran on laptop in /home/me
    subagent tool_result bash: ran on laptop in /home/me
    --- messages
    user: go
    assistant:  subagent{"task":"run the tests"}
    tool_result subagent: started agent a1 (run the tests); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: started
    user: [subagent a1 finished] run the tests | the tests pass | [subagent: 3 turns, 50 in / 21 out tokens, $0.0000]
    assistant: noted
    |}]
;;

let%expect_test "cancelling a subagent whose call waits for its host" =
  with_session
    ~main:[ spawn "run the tests"; Reply.text "started"; Reply.text "noted" ]
    [ "run the tests", [ bash "s1"; Reply.text "unreachable" ] ]
  @@ fun t ~sw agent h ->
  let laptop = connect_laptop ~sw h agent in
  Fake_host.disconnect h laptop;
  let started = subagent_tool_started agent in
  Test_rpc.call t h ~params:{|{"text": "go"}|} "prompt";
  Eio.Promise.await started;
  Test_rpc.call t h ~params:{|{"agent_id": "a1"}|} "cancel_subagent";
  Agent.wait_idle agent;
  Test_background_subagents.print_messages (Agent.messages agent);
  [%expect
    {|
    laptop hello: client_id=client-2 host_id=host-laptop
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    --- messages
    user: go
    assistant:  subagent{"task":"run the tests"}
    tool_result subagent: started agent a1 (run the tests); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: started
    user: [subagent a1 failed] run the tests | [cancelled] | [subagent: 2 turns, 30 in / 13 out tokens, $0.0000]
    assistant: noted
    |}]
;;

let%expect_test "a subagent's call in flight when its host restarts fails" =
  with_session
    ~main:[ spawn "run the tests"; Reply.text "started"; Reply.text "noted" ]
    [ "run the tests", [ bash "s1"; Reply.text "the tests were cut short" ] ]
  @@ fun t ~sw agent h ->
  let laptop = connect_laptop ~sw h agent in
  laptop.answer <- false;
  Test_rpc.call t h ~params:{|{"text": "go"}|} "prompt";
  let (_ : Json.t) = wait_bash h laptop in
  (* The frontend restarts, and its tool host with it: it comes back without
     the exec. *)
  Fake_host.disconnect h laptop;
  let (_ : Fake_host.t) = connect_laptop ~sw h agent in
  Agent.wait_idle agent;
  print_subagent_results ();
  Test_background_subagents.print_messages (Agent.messages agent);
  [%expect
    {|
    laptop hello: client_id=client-2 host_id=host-laptop
    {"type":"response","id":"r1","ok":true,"result":{}}
    laptop hello: client_id=client-3 host_id=host-laptop
    subagent tool_result bash: [the tool host restarted during this call, so its result is lost: run it again if you still need it]
    --- messages
    user: go
    assistant:  subagent{"task":"run the tests"}
    tool_result subagent: started agent a1 (run the tests); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: started
    user: [subagent a1 finished] run the tests | the tests were cut short | [subagent: 2 turns, 30 in / 13 out tokens, $0.0000]
    assistant: noted
    |}]
;;

let%expect_test
    "a second frontend on the same machine fails the first one's calls"
  =
  with_session
    ~main:[ spawn "run the tests"; Reply.text "started"; Reply.text "noted" ]
    [ "run the tests", [ bash "s1"; Reply.text "the tests were cut short" ] ]
  @@ fun t ~sw agent h ->
  let laptop = connect_laptop ~sw h agent in
  laptop.answer <- false;
  Test_rpc.call t h ~params:{|{"text": "go"}|} "prompt";
  let exec = wait_bash h laptop in
  (* The first frontend is still connected and still running the tests. *)
  let (_ : Fake_host.t) =
    Fake_host.connect
      ~sw
      h
      ~name:"second"
      ~host_id:"host-laptop"
      ~cwd:"/home/me"
  in
  (* Its call carries on there: the subagent waits for its result. *)
  result_from h laptop exec;
  Agent.wait_idle agent;
  print_subagent_results ();
  Test_background_subagents.print_messages (Agent.messages agent);
  [%expect
    {|
    laptop hello: client_id=client-2 host_id=host-laptop
    {"type":"response","id":"r1","ok":true,"result":{}}
    second hello: client_id=client-3 host_id=host-laptop
    laptop's late result: accepted
    subagent tool_result bash: tests passed
    --- messages
    user: go
    assistant:  subagent{"task":"run the tests"}
    tool_result subagent: started agent a1 (run the tests); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: started
    user: [subagent a1 finished] run the tests | the tests were cut short | [subagent: 2 turns, 30 in / 13 out tokens, $0.0000]
    assistant: noted
    |}]
;;
