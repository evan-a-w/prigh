open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

let sessions_dir t = Filename.concat t.dir "sessions"

let other_model =
  List.find_exn Model.all ~f:(fun m -> not (String.equal m.id Model.default.id))
;;

let saved t ?model ?system messages =
  let s = Session.create ~dir:(sessions_dir t) ~cwd:"/work/app" () in
  Option.iter model ~f:(fun model ->
    ignore
      (Session.set_model s ~model ~thinking:(On (Some High)) : Session.Entry.t));
  Option.iter system ~f:(fun text ->
    ignore (Session.set_system_prompt s ~text : Session.Entry.t));
  List.iter messages ~f:(fun message ->
    ignore (Session.append_message s message : Session.Entry.t));
  s
;;

let conversation =
  [ Message.user "where does the cache live?"
  ; Message.Assistant
      { content =
          [ Content.Text "Checking."
          ; Content.Tool_call
              { id = "c1"; name = "grep"; arguments = {|{"pattern": "cache"}|} }
          ]
      ; stop_reason = Tool_use
      ; usage = Usage.zero
      ; model = "m"
      }
  ]
;;

let print_request t (r : Provider.Request.t) =
  printf
    "model=%s thinking=%s tools=%d cache_key=%s\nsystem: %s\n"
    (if String.equal r.model.id Model.default.id
     then "<the caller's>"
     else if String.equal r.model.id other_model.id
     then "<the asked session's>"
     else r.model.id)
    (Sexp.to_string [%sexp (r.thinking : Thinking.t)])
    (List.length r.tools)
    (mask t (Option.value r.cache_key ~default:"none"))
    (Option.value r.system ~default:"none");
  List.iter r.messages ~f:(function
    | Message.User u -> printf "user: %s\n" u.text
    | Tool_result r -> printf "result %s: %s\n" r.tool_call_id r.text
    | Assistant a ->
      printf
        "assistant: %s%s\n"
        (Message.Assistant.text a)
        (String.concat
           (List.map (Message.Assistant.tool_calls a) ~f:(fun c ->
              sprintf " [call %s]" c.name))))
;;

let ask
      ?cancel
      ?(current = "")
      ?(replies = [ Reply.text "In lib/cache." ])
      t
      args
  =
  let tool =
    Tool_ask_session.create
      ~sessions_dir:(fun () -> sessions_dir t)
      ~current:(fun () -> Some current)
      ~provider:(Faux_provider.create ~on_request:(print_request t) replies)
      ~resolve_model:Model.resolve
      ~current_model:(fun () -> Model.default)
      ~record_spend:(fun ~usage ~cost_usd ->
        printf
          !"spend on the caller: %{sexp:Usage.t}%s\n"
          usage
          (if Float.(cost_usd > 0.) then ", costing something" else ""))
      ()
  in
  run ?cancel ~on_output:(printf "delta: %s\n") t tool args
;;

let question session =
  sprintf
    {|{"session": "%s", "question": "Where is the cache?"}|}
    (Session.id session)
;;

let%expect_test
    "asks the session's conversation with its own model, leaving its file alone"
  =
  with_sandbox
  @@ fun t ->
  let session =
    saved
      t
      ~model:(Model.key other_model)
      ~system:"You are prigh (the session's own prompt)."
      conversation
  in
  let before = In_channel.read_all (Session.path session) in
  ask
    t
    ~replies:
      [ { Reply.events = [ Text_delta "In "; Text_delta "lib/cache." ]
        ; stop_reason = End_turn
        ; usage = { input = 1000; output = 10; cache_read = 0 }
        }
      ]
    (question session);
  [%expect
    {|
    model=<the asked session's> thinking=Off tools=0 cache_key=<id>
    system: You are prigh (the session's own prompt).
    user: where does the cache live?
    assistant: Checking. [call grep]
    result c1: [still running: no result yet]
    user: <ask_session>Another prigh session asks a question about this conversation. Answer it concisely from the conversation so far alone, with the specifics it needs (paths, names, decisions, state of the work); say so when the conversation does not tell. You cannot use tools in this reply, and neither the question nor your answer will be added to the conversation.</ask_session>

    Where is the cache?
    delta: In
    delta: lib/cache.
    spend on the caller: ((input 1000) (output 10) (cache_read 0)), costing something
    In lib/cache.
    |}];
  print_s
    [%sexp
      (String.equal before (In_channel.read_all (Session.path session)) : bool)];
  [%expect {| true |}]
;;

let%expect_test
    "a session whose model is unknown or unrecorded is asked with the caller's"
  =
  with_sandbox
  @@ fun t ->
  ask t (question (saved t ~model:"nowhere/no-such-model" conversation));
  [%expect
    {|
    model=<the caller's> thinking=Off tools=0 cache_key=<id>
    system: You are prigh, a coding agent. This is the conversation of one of your sessions.
    user: where does the cache live?
    assistant: Checking. [call grep]
    result c1: [still running: no result yet]
    user: <ask_session>Another prigh session asks a question about this conversation. Answer it concisely from the conversation so far alone, with the specifics it needs (paths, names, decisions, state of the work); say so when the conversation does not tell. You cannot use tools in this reply, and neither the question nor your answer will be added to the conversation.</ask_session>

    Where is the cache?
    delta: In lib/cache.
    spend on the caller: ((input 10) (output 5) (cache_read 0)), costing something
    In lib/cache.
    |}];
  ask t (question (saved t [ Message.user "hello" ]));
  [%expect
    {|
    model=<the caller's> thinking=Off tools=0 cache_key=<id>
    system: You are prigh, a coding agent. This is the conversation of one of your sessions.
    user: hello
    user: <ask_session>Another prigh session asks a question about this conversation. Answer it concisely from the conversation so far alone, with the specifics it needs (paths, names, decisions, state of the work); say so when the conversation does not tell. You cannot use tools in this reply, and neither the question nor your answer will be added to the conversation.</ask_session>

    Where is the cache?
    delta: In lib/cache.
    spend on the caller: ((input 10) (output 5) (cache_read 0)), costing something
    In lib/cache.
    |}]
;;

let%expect_test "what it refuses, and failures" =
  with_sandbox
  @@ fun t ->
  let session = saved t conversation in
  ask t ~current:(Session.id session) (question session);
  [%expect
    {| ERROR: that is this session: its conversation is your own context, so answer from it |}];
  ask t {|{"session": "zz", "question": "what?"}|};
  [%expect
    {| ERROR: no session "zz"; action search of the sessions tool finds sessions and their ids |}];
  ask
    t
    (sprintf {|{"session": "%s", "question": "what?"}|} (Session.path session));
  [%expect
    {| ERROR: "$DIR/sessions/<stamp>_<id>.jsonl" is a path: give the session's id (action search of the sessions tool finds it) |}];
  ask t (sprintf {|{"session": "%s", "question": "  "}|} (Session.id session));
  [%expect {| ERROR: question is empty: say what you want to know |}];
  let named = Session.create ~dir:(sessions_dir t) ~cwd:"/work" () in
  ignore (Session.set_name named ~name:"Empty" : Session.Entry.t);
  ask t (question named);
  [%expect {| ERROR: that session has no messages to answer from |}];
  ask t ~replies:[ Reply.error "overloaded" ] (question session);
  [%expect
    {|
    model=<the caller's> thinking=Off tools=0 cache_key=<id>
    system: You are prigh, a coding agent. This is the conversation of one of your sessions.
    user: where does the cache live?
    assistant: Checking. [call grep]
    result c1: [still running: no result yet]
    user: <ask_session>Another prigh session asks a question about this conversation. Answer it concisely from the conversation so far alone, with the specifics it needs (paths, names, decisions, state of the work); say so when the conversation does not tell. You cannot use tools in this reply, and neither the question nor your answer will be added to the conversation.</ask_session>

    Where is the cache?
    spend on the caller: ((input 0) (output 0) (cache_read 0))
    ERROR: DeepSeek V4.1 Flash could not answer: overloaded
    |}];
  let cancel = Cancellation.create () in
  Cancellation.cancel cancel;
  ask t ~cancel (question session);
  [%expect
    {|
    model=<the caller's> thinking=Off tools=0 cache_key=<id>
    system: You are prigh, a coding agent. This is the conversation of one of your sessions.
    user: where does the cache live?
    assistant: Checking. [call grep]
    result c1: [still running: no result yet]
    user: <ask_session>Another prigh session asks a question about this conversation. Answer it concisely from the conversation so far alone, with the specifics it needs (paths, names, decisions, state of the work); say so when the conversation does not tell. You cannot use tools in this reply, and neither the question nor your answer will be added to the conversation.</ask_session>

    Where is the cache?
    spend on the caller: ((input 10) (output 5) (cache_read 0)), costing something
    ERROR: cancelled
    |}];
  ask
    t
    ~replies:[ Reply.text (String.make 7000 'x' ^ "\nend") ]
    (question session);
  print_endline
    (String.concat
       ~sep:"\n"
       (List.map
          (String.split_lines [%expect.output])
          ~f:(fun line ->
            if String.length line > 80
            then
              sprintf
                "%s… (%d bytes)"
                (String.prefix line 30)
                (String.length line)
            else line)));
  [%expect
    {|
    model=<the caller's> thinking=Off tools=0 cache_key=<id>
    system: You are prigh, a codin… (88 bytes)
    user: where does the cache live?
    assistant: Checking. [call grep]
    result c1: [still running: no result yet]
    user: <ask_session>Another pri… (381 bytes)

    Where is the cache?
    delta: xxxxxxxxxxxxxxxxxxxxxxx… (7007 bytes)
    end
    spend on the caller: ((input 10) (output 5) (cache_read 0)), costing something
    xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx… (6000 bytes)
    [the answer was cut here: ask for less]
    |}]
;;

(* Through an agent, as [prigh] wires it: the spend lands on the asking
   session. *)
let%expect_test "an agent asks another session" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let other = saved t conversation in
  let agent_ref = ref None in
  let provider =
    Faux_provider.create
      [ Reply.tool_call
          ~id:"a1"
          ~name:"ask_session"
          ~arguments:(question other)
          ()
      ; { Reply.events = [ Text_delta "In lib/cache." ]
        ; stop_reason = End_turn
        ; usage = { input = 1000; output = 10; cache_read = 0 }
        }
      ; Reply.text "It lives in lib/cache."
      ]
  in
  let tool =
    Tool_ask_session.create
      ~sessions_dir:(fun () -> sessions_dir t)
      ~current:(fun () ->
        Option.map !agent_ref ~f:(fun a -> Session.id (Agent.session a)))
      ~provider
      ~resolve_model:Model.resolve
      ~current_model:(fun () -> Model.default)
      ~record_spend:(fun ~usage ~cost_usd ->
        Option.iter !agent_ref ~f:(fun a ->
          Agent.record_spend a ~usage ~cost_usd))
      ()
  in
  let agent, h = Test_rpc.make_server t ~sw ~provider ~tools:[ tool ] in
  agent_ref := Some agent;
  let before = In_channel.read_all (Session.path other) in
  Test_rpc.call t h ~params:{|{"text": "ask the other session"}|} "prompt";
  Agent.wait_idle agent;
  List.iter (Agent.messages agent) ~f:(function
    | Message.User u -> printf "user: %s\n" u.text
    | Tool_result r -> printf "result: %s\n" r.text
    | Assistant a -> printf "assistant: %s\n" (Message.Assistant.text a));
  print_s [%sexp (fst (Session.spend (Agent.session agent)) : Usage.t)];
  print_s
    [%sexp
      (String.equal before (In_channel.read_all (Session.path other)) : bool)];
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{}}
    user: [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]

    ask the other session
    assistant:
    result: In lib/cache.
    assistant: It lives in lib/cache.
    ((input 1000) (output 10) (cache_read 0))
    true
    |}]
;;
