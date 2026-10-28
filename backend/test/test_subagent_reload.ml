open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Gates = Routed_provider.Gates
module Json = Jsonaf

let member_string = Test_host_reconnect.member_string

let spawn task =
  Reply.tool_call
    ~id:"c1"
    ~name:"subagent"
    ~arguments:(sprintf {|{"task":%S}|} task)
    ()
;;

let request meth params =
  Json.of_string
    (sprintf {|{"id": "r1", "method": "%s", "params": %s}|} meth params)
;;

let result (h : Test_rpc.H.t) client meth =
  Json.member_exn
    "result"
    (Rpc_server.handle h.server client (request meth "{}"))
;;

(* The subagents a client is shown: the agents panel and the status. *)
let print_view (h : Test_rpc.H.t) client =
  let items = function
    | `Array items -> items
    | _ -> []
  in
  let listed =
    List.map
      (items (result h client "list_subagents"))
      ~f:(fun item ->
        sprintf
          "%s %s%s"
          (member_string item "id")
          (member_string item "state")
          (match Json.member "stale" item with
           | Some `True -> " (stale)"
           | _ -> ""))
  in
  let status =
    List.map
      (items (Json.member_exn "subagents" (result h client "get_state")))
      ~f:(fun item -> member_string item "id")
  in
  printf
    "panel=[%s] status=[%s]\n"
    (String.concat ~sep:"; " listed)
    (String.concat ~sep:"; " status)
;;

(* A frontend coming back to [session], as prigh-web and the TUI do. *)
let rejoin (h : Test_rpc.H.t) ~session =
  let client = Rpc_server.connect h.server ~send:ignore in
  let reply =
    Rpc_server.handle
      h.server
      client
      (request
         "hello"
         (sprintf
            {|{"name": "prigh-web", "tools": false, "session": %S}|}
            session))
  in
  (match Json.member "error" reply with
   | Some (`String e) -> printf "hello: %s\n" e
   | _ -> print_endline "hello: ok");
  client
;;

let tools ~provider =
  Tools.all @ Test_background_subagents.subagent_tools ~provider
;;

(* The first backend: the main agent starts a background subagent, which
   says something and is then held (before its second turn) until [stop]
   says what becomes of the backend. *)
let first_backend t ~stop =
  let gates = Gates.create () in
  let turns = ref 0 in
  let held, hold = Eio.Promise.create () in
  Eio.Switch.run
  @@ fun sw ->
  let provider =
    Routed_provider.create
      ~before:(fun ~key ->
        if String.equal key "run the tests"
        then (
          incr turns;
          if !turns = 2
          then (
            Gates.hold gates key;
            Eio.Promise.resolve hold ()));
        Gates.before gates ~key)
      ~main:[ spawn "run the tests"; Reply.text "started"; Reply.text "heard" ]
      [ ( "run the tests"
        , [ Reply.tool_calls
              ~text:"halfway: 3 of 6 suites pass"
              [ "n1", "step", "{}" ]
          ; Reply.text "all tests pass"
          ] )
      ]
  in
  let agent, h =
    Test_rpc.make_server
      t
      ~sw
      ~provider
      ~tools:(Test_background_subagents.noop_tool :: tools ~provider)
  in
  Test_rpc.call t h ~params:{|{"text": "go"}|} "prompt";
  Test_background_subagents.wait_turn agent;
  Eio.Promise.await held;
  print_view h h.client;
  stop h agent ~release:(fun () -> Gates.release gates "run the tests")
;;

(* The backend after the restart; the main agent replies [noted] to
   whatever it is told. *)
let second_backend t ~session =
  Eio.Switch.run
  @@ fun sw ->
  let provider =
    Routed_provider.create ~main:[ Reply.text "noted"; Reply.text "noted" ] []
  in
  let _, h = Test_rpc.make_server t ~sw ~provider ~tools:(tools ~provider) in
  let client = rejoin h ~session in
  let agent = Rpc_server.agent_of_client h.server client in
  Agent.wait_idle agent;
  print_view h client;
  Test_background_subagents.print_messages (Agent.messages agent)
;;

(* What a killed process leaves on disk: the sessions as they were when it
   died, whatever the test's teardown writes afterwards. *)
let killed t ~session _ agent ~release:_ =
  session := Session.id (Agent.session agent);
  Sys_unix.command_exn
    (sprintf "cp -a %s/sessions %s/at-kill" t.Tool_test_helpers.dir t.dir);
  failwith "killed"
;;

let%expect_test "the backend is killed while a background subagent runs" =
  with_sandbox
  @@ fun t ->
  let session = ref "" in
  (match first_backend t ~stop:(killed t ~session) with
   | () -> ()
   | exception Failure msg -> print_endline msg);
  Sys_unix.command_exn
    (sprintf
       "rm -rf %s/sessions && mv %s/at-kill %s/sessions"
       t.dir
       t.dir
       t.dir);
  second_backend t ~session:!session;
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{}}
    panel=[a1 running] status=[a1]
    killed
    hello: ok
    panel=[a1 failed (stale)] status=[]
    --- messages
    user: go
    assistant:  subagent{"task":"run the tests"}
    tool_result subagent: started agent a1 (run the tests); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: started
    user: [subagent a1 interrupted] run the tests | The backend stopped while it ran (1 turn, 1 tool call), so it never finished: its work may be partly done. Start a new subagent if the task still needs doing. | Its last message: | halfway: 3 of 6 suites pass
    assistant: noted
    |}]
;;

let%expect_test
    "the backend stops (its stdio frontend quit) while a subagent runs"
  =
  with_sandbox
  @@ fun t ->
  let session = ref "" in
  first_backend t ~stop:(fun h agent ~release:_ ->
    session := Session.id (Agent.session agent);
    Rpc_server.shutdown h.server;
    print_endline "shut down");
  second_backend t ~session:!session;
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{}}
    panel=[a1 running] status=[a1]
    shut down
    hello: ok
    panel=[a1 failed (stale)] status=[]
    --- messages
    user: go
    assistant:  subagent{"task":"run the tests"}
    tool_result subagent: started agent a1 (run the tests); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: started
    user: [subagent a1 interrupted] run the tests | The backend stopped while it ran (1 turn, 1 tool call), so it never finished: its work may be partly done. Start a new subagent if the task still needs doing. | Its last message: | halfway: 3 of 6 suites pass
    assistant: noted
    |}]
;;

let%expect_test "reported once: loading the session again reports nothing new" =
  with_sandbox
  @@ fun t ->
  let session = ref "" in
  first_backend t ~stop:(fun h agent ~release:_ ->
    session := Session.id (Agent.session agent);
    Rpc_server.shutdown h.server);
  second_backend t ~session:!session;
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{}}
    panel=[a1 running] status=[a1]
    hello: ok
    panel=[a1 failed (stale)] status=[]
    --- messages
    user: go
    assistant:  subagent{"task":"run the tests"}
    tool_result subagent: started agent a1 (run the tests); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: started
    user: [subagent a1 interrupted] run the tests | The backend stopped while it ran (1 turn, 1 tool call), so it never finished: its work may be partly done. Start a new subagent if the task still needs doing. | Its last message: | halfway: 3 of 6 suites pass
    assistant: noted
    |}];
  second_backend t ~session:!session;
  [%expect
    {|
    hello: ok
    panel=[a1 failed (stale)] status=[]
    --- messages
    user: go
    assistant:  subagent{"task":"run the tests"}
    tool_result subagent: started agent a1 (run the tests); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: started
    user: [subagent a1 interrupted] run the tests | The backend stopped while it ran (1 turn, 1 tool call), so it never finished: its work may be partly done. Start a new subagent if the task still needs doing. | Its last message: | halfway: 3 of 6 suites pass
    assistant: noted
    |}]
;;

(* Not a restart: every client goes away and the page comes back to the
   same backend, which finds the live session again; the subagent carries
   on and its report reaches the main agent. *)
let%expect_test
    "a frontend that disconnects and comes back while a subagent runs"
  =
  with_sandbox
  @@ fun t ->
  first_backend t ~stop:(fun h agent ~release ->
    let session = Session.id (Agent.session agent) in
    Rpc_server.disconnect h.server h.client;
    let client = rejoin h ~session in
    let again = Rpc_server.agent_of_client h.server client in
    printf "same agent: %b\n" (phys_equal agent again);
    print_view h client;
    release ();
    Agent.wait_idle agent;
    print_view h client;
    Test_background_subagents.print_messages (Agent.messages agent));
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{}}
    panel=[a1 running] status=[a1]
    hello: ok
    same agent: true
    panel=[a1 running] status=[a1]
    panel=[a1 complete (stale)] status=[]
    --- messages
    user: go
    assistant:  subagent{"task":"run the tests"}
    tool_result subagent: started agent a1 (run the tests); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: started
    user: [subagent a1 finished] run the tests | all tests pass | [subagent: 2 turns, 30 in / 13 out tokens, $0.0000]
    assistant: heard
    |}]
;;
