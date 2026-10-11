open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf

(* Two clients in one session, as two frontends would be. *)
let with_server replies f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let login =
    Login_manager.create
      ~env:t.env
      ~sw
      ~getenv:(fun _ -> None)
      ~store:(Auth_store.create ~path:(Filename.concat t.dir "auth.json"))
      ()
  in
  let provider = Faux_provider.create replies in
  let new_agent ?session ~cwd () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:(Tools.all @ Tools.interaction)
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ?session
      ~cwd
      ()
  in
  let server =
    Rpc_server.create
      ~env:t.env
      ~sw
      ~login
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~cwd:t.dir
      ~new_agent
      ()
  in
  let client () =
    let sent = Queue.create () in
    Rpc_server.connect server ~send:(Queue.enqueue sent), sent
  in
  let a, a_sent = client () in
  let b, b_sent = client () in
  let agent = Rpc_server.agent_of_client server a in
  ignore
    (Rpc_server.handle
       server
       b
       (Json.of_string
          (sprintf
             {|{"id": 1, "method": "hello", "params": {"name": "b", "session": "%s"}}|}
             (Session.id (Agent.session agent))))
     : Json.t);
  let call client ?(params = "{}") meth =
    let request =
      Json.of_string
        (sprintf {|{"id": "r", "method": "%s", "params": %s}|} meth params)
    in
    Rpc_server.handle server client request
  in
  let show json = print_endline (mask t (Json.to_string json)) in
  f ~agent ~call ~show ~a:(a, a_sent) ~b:(b, b_sent)
;;

let events name sent =
  Queue.to_list sent
  |> List.filter ~f:(fun json ->
    match Json.member "event" json with
    | Some (`String e) -> String.equal e name
    | _ -> false)
;;

let wait_for name sent =
  let rec go n =
    if not (List.is_empty (events name sent))
    then ()
    else if n = 0
    then failwithf "timed out waiting for %s" name ()
    else (
      Eio_unix.sleep 0.005;
      go (n - 1))
  in
  go 2_000
;;

let field name json = Option.value (Json.member name json) ~default:`Null

let%expect_test "set_mode, confirmations with rules, the first answer wins" =
  with_server
    [ Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"echo one"}|}
        ()
    ; Reply.tool_call ~id:"c2" ~name:"bash" ~arguments:{|{"command":"rm x"}|} ()
    ; Reply.text "done"
    ]
  @@ fun ~agent ~call ~show ~a:(a, a_sent) ~b:(b, b_sent) ->
  let mode json =
    print_s
      [%sexp
        (Json.to_string (field "approval_mode" json) : string)
      , (Json.to_string (field "plan_mode" json) : string)]
  in
  mode (field "result" (call a ~params:{|{"approval_mode": "ask"}|} "set_mode"));
  mode (field "result" (call a ~params:{|{"plan_mode": true}|} "set_mode"));
  mode (field "result" (call a ~params:{|{"plan_mode": false}|} "set_mode"));
  show (call a ~params:{|{"approval_mode": "yolo"}|} "set_mode");
  show (call a ~params:{|{"plan_mode": "yes"}|} "set_mode");
  ignore (call a ~params:{|{"text": "go"}|} "prompt" : Json.t);
  wait_for "tool_confirm" a_sent;
  wait_for "tool_confirm" b_sent;
  List.iter (events "tool_confirm" b_sent) ~f:show;
  show (field "result" (call a "get_pending"));
  show
    (call
       b
       ~params:{|{"call_id": "c1", "allow": true, "always": true}|}
       "tool_confirm_respond");
  show
    (call a ~params:{|{"call_id": "c1", "allow": true}|} "tool_confirm_respond");
  Queue.clear a_sent;
  wait_for "tool_confirm" a_sent;
  show
    (call
       a
       ~params:{|{"call_id": "c2", "allow": false, "reason": "keep x"}|}
       "tool_confirm_respond");
  Agent.wait_idle agent;
  List.iter (Agent.messages agent) ~f:(function
    | Tool_result r -> print_endline r.text
    | _ -> ());
  let entries = field "entries" (field "result" (call a "get_entries")) in
  (match entries with
   | `Array entries ->
     List.iter entries ~f:(fun e ->
       match Json.member "kind" e with
       | Some (`String ("approval" | "allow_rule")) ->
         show
           (`Object
               (List.filter (Json.assoc_list_exn e) ~f:(fun (k, _) ->
                  not (List.mem [ "id"; "parent"; "at" ] k ~equal:String.equal))))
       | _ -> ())
   | _ -> ());
  [%expect
    {|
    ("\"ask\"" false)
    ("\"ask\"" true)
    ("\"ask\"" false)
    {"type":"response","id":"r","ok":false,"error":"unknown approval mode \"yolo\"; the modes are ask, auto-edit, auto"}
    {"type":"response","id":"r","ok":false,"error":"param \"plan_mode\" must be a boolean"}
    {"type":"event","event":"tool_confirm","call_id":"c1","agent_id":null,"name":"bash","summary":"echo one","rules":["bash(echo:*)"]}
    {"steer_texts":[],"follow_up_texts":[],"confirms":[{"call_id":"c1","agent_id":null,"name":"bash","summary":"echo one","rules":["bash(echo:*)"]}],"questions":[]}
    {"type":"response","id":"r","ok":true,"result":{}}
    {"type":"response","id":"r","ok":false,"error":"no pending confirmation for tool call \"c1\""}
    {"type":"response","id":"r","ok":true,"result":{}}
    one

    [denied by user] Their reason: keep x
    {"kind":"approval","mode":"ask","plan":false}
    {"kind":"approval","mode":"ask","plan":true}
    {"kind":"approval","mode":"ask","plan":false}
    {"kind":"allow_rule","rule":"bash(echo:*)"}
    |}]
;;

let%expect_test
    "ask_user: every client sees it, get_pending re-sends it, the first answer \
     wins"
  =
  with_server
    [ Reply.tool_call
        ~id:"q1"
        ~name:"ask_user"
        ~arguments:
          {|{"questions":[{"question":"Which?","options":[{"label":"x"},{"label":"y","description":"the other"}]},{"question":"Why?"}]}|}
        ()
    ; Reply.text "thanks"
    ]
  @@ fun ~agent ~call ~show ~a:(a, a_sent) ~b:(b, b_sent) ->
  ignore (call a ~params:{|{"text": "go"}|} "prompt" : Json.t);
  wait_for "ask_user" a_sent;
  wait_for "ask_user" b_sent;
  List.iter (events "ask_user" a_sent) ~f:show;
  (* What a frontend that reconnects asks for. *)
  show (field "questions" (field "result" (call b "get_pending")));
  show (call b ~params:{|{"call_id": "q1"}|} "ask_user_respond");
  show
    (call
       b
       ~params:{|{"call_id": "q1", "answers": [["x"]]}|}
       "ask_user_respond");
  show
    (call
       b
       ~params:{|{"call_id": "q1", "answers": [["x"], ["because"]]}|}
       "ask_user_respond");
  show
    (call a ~params:{|{"call_id": "q1", "decline": true}|} "ask_user_respond");
  Agent.wait_idle agent;
  List.iter (Agent.messages agent) ~f:(function
    | Tool_result r -> print_string r.text
    | _ -> ());
  show (field "questions" (field "result" (call a "get_pending")));
  [%expect
    {|
    {"type":"event","event":"ask_user","call_id":"q1","questions":[{"question":"Which?","header":null,"options":[{"label":"x","description":null},{"label":"y","description":"the other"}],"multiple":false},{"question":"Why?","header":null,"options":[],"multiple":false}]}
    [{"call_id":"q1","questions":[{"question":"Which?","header":null,"options":[{"label":"x","description":null},{"label":"y","description":"the other"}],"multiple":false},{"question":"Why?","header":null,"options":[],"multiple":false}]}]
    {"type":"response","id":"r","ok":false,"error":"missing param \"answers\" (or \"decline\": true)"}
    {"type":"response","id":"r","ok":false,"error":"expected 2 answers (one list per question), got 1"}
    {"type":"response","id":"r","ok":true,"result":{}}
    {"type":"response","id":"r","ok":false,"error":"no pending question for tool call \"q1\": it was answered or withdrawn"}
    The user answered:
    Q: Which?
    A: x
    Q: Why?
    A: because
    []
    |}]
;;

let%expect_test
    "set_config with confirm_tools (an older frontend) sets the mode"
  =
  with_server []
  @@ fun ~agent ~call ~show:_ ~a:(a, _) ~b:_ ->
  let mode () =
    print_endline (Approval_mode.to_string (Agent.state agent).approval_mode)
  in
  ignore
    (call a ~params:{|{"config": {"confirm_tools": true}}|} "set_config"
     : Json.t);
  mode ();
  ignore
    (call a ~params:{|{"config": {"approval_mode": "auto-edit"}}|} "set_config"
     : Json.t);
  mode ();
  ignore
    (call a ~params:{|{"config": {"confirm_tools": false}}|} "set_config"
     : Json.t);
  mode ();
  [%expect
    {|
    ask
    auto-edit
    auto
    |}]
;;
