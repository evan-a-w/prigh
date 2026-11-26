open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

(* A scripted ACP agent: [script] reads the requests prigh sends (printing
   their methods) and writes the agent's side. *)

module P = struct
  let read p =
    match Line_transport.Peer.read_line p with
    | None -> None
    | Some line -> Some (Json.parse line |> Or_error.ok_exn)
  ;;

  let str k j = Option.bind (Json.member k j) ~f:Json.string
  let id j = Option.value (Json.member "id" j) ~default:`Null

  (* The next request or notification, printed; answers are skipped. *)
  let rec next p =
    match read p with
    | None -> None
    | Some j ->
      (match str "method" j with
       | Some m ->
         let params = Option.value (Json.member "params" j) ~default:`Null in
         let detail =
           match m with
           | "session/new" | "session/load" | "session/resume" ->
             let servers =
               match Json.member "mcpServers" params with
               | Some (`Array l) -> List.filter_map l ~f:(str "name")
               | _ -> []
             in
             sprintf
               " session=%s mcp=[%s]"
               (Option.value (str "sessionId" params) ~default:"-")
               (String.concat ~sep:"," servers)
           | "session/prompt" ->
             (match Json.member "prompt" params with
              | Some (`Array (first :: _)) ->
                let text = Option.value (str "text" first) ~default:"" in
                sprintf
                  " %S"
                  (match
                     String.substr_index_all
                       text
                       ~may_overlap:false
                       ~pattern:"\n\n"
                     |> List.last
                   with
                   | Some i -> String.drop_prefix text (i + 2)
                   | None -> text)
              | _ -> "")
           | "session/set_mode" ->
             " " ^ Option.value (str "modeId" params) ~default:""
           | "session/set_model" ->
             " " ^ Option.value (str "modelId" params) ~default:""
           | "session/set_config_option" ->
             sprintf
               " %s=%s"
               (Option.value (str "configId" params) ~default:"")
               (Option.value (str "value" params) ~default:"")
           | _ -> ""
         in
         printf "agent got %s%s\n" m detail;
         Some (m, j)
       | None ->
         printf
           "agent got answer %s\n"
           (Json.to_string
              (Option.value (Json.member "result" j) ~default:`Null));
         next p)
  ;;

  let answer p =
    match read p with
    | Some j ->
      printf
        "agent got answer %s\n"
        (Json.to_string (Option.value (Json.member "result" j) ~default:`Null))
    | None -> ()
  ;;

  let write p fields =
    Line_transport.Peer.write_line p (Json.to_string (`Object fields))
  ;;

  let result p j result =
    write
      p
      [ "jsonrpc", `String "2.0"
      ; "id", id j
      ; "result", Json.parse result |> Or_error.ok_exn
      ]
  ;;

  let error p j ~code message =
    write
      p
      [ "jsonrpc", `String "2.0"
      ; "id", id j
      ; ( "error"
        , `Object
            [ "code", `Number (Int.to_string code); "message", `String message ]
        )
      ]
  ;;

  let update p session update =
    write
      p
      [ "jsonrpc", `String "2.0"
      ; "method", `String "session/update"
      ; ( "params"
        , `Object
            [ "sessionId", `String session
            ; "update", Json.parse update |> Or_error.ok_exn
            ] )
      ]
  ;;

  let text p session s =
    update
      p
      session
      (sprintf
         {|{"sessionUpdate":"agent_message_chunk","content":{"type":"text","text":%S}}|}
         s)
  ;;

  let expect p m =
    match next p with
    | Some (m', j) when String.equal m m' -> j
    | Some (m', _) -> raise_s [%message "expected" m "got" m']
    | None -> raise_s [%message "expected" m "got end of input"]
  ;;

  let initialize ?(caps = {|{"loadSession":true}|}) p =
    let j = expect p "initialize" in
    result
      p
      j
      (sprintf
         {|{"protocolVersion":1,"agentCapabilities":%s,"authMethods":[{"id":"login","name":"opencode auth login"}]}|}
         caps)
  ;;

  let modes =
    {|"modes":{"currentModeId":"build","availableModes":[{"id":"build","name":"Build"},{"id":"plan","name":"Plan"}]}|}
  ;;

  let new_session p session =
    let j = expect p "session/new" in
    result p j (sprintf {|{"sessionId":%S,%s}|} session modes)
  ;;
end

let opencode : Acp_agent.t = Option.value_exn (Acp_agent.find_known "opencode")

let print_event (e : Agent.Event.t) =
  match e with
  | Loop (Message_end (Assistant a)) ->
    printf
      "assistant (%s, %s): %S\n"
      a.model
      (Sexp.to_string [%sexp (a.stop_reason : Stop_reason.t)])
      (Message.Assistant.text a)
  | Loop (Tool_start c) -> printf "tool_start %s\n" c.name
  | Loop (Tool_end { call; result }) ->
    printf
      "tool_end %s -> %s%S\n"
      call.name
      (if result.is_error then "ERROR " else "")
      result.text
  | Notice n -> printf "notice: %s\n" n
  | _ -> ()
;;

let with_acp
      ?(agent = opencode)
      ?(approval : Approval_mode.t option)
      ?(on_launch = ignore)
      ?start
      ~peer
      f
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let launches = ref 0 in
  let adapter =
    Acp_adapter.create ~load_quiet:(Time_float.Span.of_sec 0.2) agent
  in
  let composer =
    Composer_runner.create
      ~sw
      ~adapter
      ~spawn:(fun (launch : Adapter.Launch.t) ->
        incr launches;
        printf
          "spawn #%d: %s\n"
          !launches
          (String.concat
             ~sep:" "
             (List.map launch.argv ~f:(fun a ->
                if String.mem a '\n' then "<script>" else a)));
        on_launch launch;
        Ok (Line_transport.Peer.spawn ~sw (peer !launches)))
      ?start
      ~interrupt_grace:(Time_float.Span.of_sec 0.5)
      ()
  in
  let acp = agent in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [])
      ~tools:Tools.interaction
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~runner:(Composer_runner.runner composer)
      ~cwd:t.dir
      ()
  in
  Agent.set_engine agent { engine = Composer; adapter = Some (Acp acp.name) };
  Option.iter approval ~f:(fun approval -> Agent.set_mode agent ~approval);
  Agent.subscribe agent ~f:print_event;
  f agent composer;
  Composer_runner.close composer
;;

let prompt agent text =
  Or_error.ok_exn (Agent.prompt agent text);
  Agent.wait_idle agent
;;

let%expect_test "a turn with a command whose permission the user gives" =
  with_acp ~approval:Ask ~peer:(fun _ p ->
    P.initialize p;
    P.new_session p "ses_1";
    let prompt = P.expect p "session/prompt" in
    P.text p "ses_1" "Listing.";
    P.update
      p
      "ses_1"
      {|{"sessionUpdate":"tool_call","toolCallId":"c1","title":"ls","kind":"execute","status":"pending","rawInput":{"command":"ls"}}|};
    P.write
      p
      [ "jsonrpc", `String "2.0"
      ; "id", `Number "7"
      ; "method", `String "session/request_permission"
      ; ( "params"
        , Json.parse
            {|{"sessionId":"ses_1","toolCall":{"toolCallId":"c1","title":"ls"},"options":[{"optionId":"yes","name":"Allow","kind":"allow_once"},{"optionId":"no","name":"Reject","kind":"reject_once"}]}|}
          |> Or_error.ok_exn )
      ];
    P.answer p;
    P.update
      p
      "ses_1"
      {|{"sessionUpdate":"tool_call_update","toolCallId":"c1","status":"completed","content":[{"type":"content","content":{"type":"text","text":"a.ml"}}]}|};
    P.text p "ses_1" "One file.";
    P.result p prompt {|{"stopReason":"end_turn"}|})
  @@ fun agent composer ->
  Agent.subscribe agent ~f:(function
    | Loop (Tool_confirm { call_id; name; _ }) ->
      printf "confirm %s %s\n" call_id name;
      Or_error.ok_exn (Agent.respond_confirm agent ~call_id Allow)
    | _ -> ());
  prompt agent "what's here";
  print_s [%sexp (Composer_runner.thread_id composer : string option)];
  [%expect
    {|
    spawn #1: opencode acp
    agent got initialize
    agent got session/new session=- mcp=[]
    agent got session/prompt "what's here"
    assistant (acp:opencode/default, Tool_use): "Listing."
    tool_start bash
    confirm c1 bash
    agent got answer {"outcome":{"outcome":"selected","optionId":"yes"}}
    tool_end bash -> "a.ml"
    assistant (acp:opencode/default, End_turn): "One file."
    (ses_1)
    |}]
;;

let%expect_test "plan mode is the agent's plan mode" =
  with_acp ~peer:(fun _ p ->
    P.initialize p;
    P.new_session p "ses_1";
    let j = P.expect p "session/set_mode" in
    P.result p j "{}";
    let prompt = P.expect p "session/prompt" in
    P.text p "ses_1" "A plan.";
    P.result p prompt {|{"stopReason":"end_turn"}|})
  @@ fun agent _ ->
  Agent.set_mode agent ~plan:true;
  prompt agent "plan it";
  [%expect
    {|
    spawn #1: opencode acp
    agent got initialize
    agent got session/new session=- mcp=[]
    agent got session/set_mode plan
    agent got session/prompt "plan it"
    assistant (acp:opencode/default, End_turn): "A plan."
    |}]
;;

let%expect_test "aborting sends session/cancel" =
  with_acp ~peer:(fun _ p ->
    P.initialize p;
    P.new_session p "ses_1";
    let prompt = P.expect p "session/prompt" in
    P.text p "ses_1" "Working";
    ignore (P.expect p "session/cancel" : Json.t);
    P.result p prompt {|{"stopReason":"cancelled"}|})
  @@ fun agent _ ->
  let started, resolve = Eio.Promise.create () in
  Agent.subscribe agent ~f:(function
    | Loop (Message_update _) ->
      if not (Eio.Promise.is_resolved started)
      then Eio.Promise.resolve resolve ()
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "go");
  Eio.Promise.await started;
  ignore (Agent.abort agent : string list);
  Agent.wait_idle agent;
  [%expect
    {|
    spawn #1: opencode acp
    agent got initialize
    agent got session/new session=- mcp=[]
    agent got session/prompt "go"
    agent got session/cancel
    assistant (acp:opencode/default, Aborted): "Working"
    |}]
;;

let%expect_test
    "a crash, then the session is loaded again (its replay going quiet)"
  =
  with_acp ~peer:(fun n p ->
    match n with
    | 1 ->
      P.initialize p;
      P.new_session p "ses_1";
      ignore (P.expect p "session/prompt" : Json.t);
      P.text p "ses_1" "Half";
      Line_transport.Peer.exit p ~code:1
    | _ ->
      P.initialize p;
      let _load = P.expect p "session/load" in
      (* replays the old turn and never answers *)
      P.text p "ses_1" "Half";
      let prompt = P.expect p "session/prompt" in
      P.text p "ses_1" "Back.";
      P.result p prompt {|{"stopReason":"end_turn"}|})
  @@ fun agent _ ->
  prompt agent "first";
  prompt agent "second";
  [%expect
    {|
    spawn #1: opencode acp
    agent got initialize
    agent got session/new session=- mcp=[]
    agent got session/prompt "first"
    assistant (acp:opencode/default, (Error"OpenCode exited (code 1)")): "Half"
    spawn #2: opencode acp
    agent got initialize
    agent got session/load session=ses_1 mcp=[]
    agent got session/prompt "second"
    assistant (acp:opencode/default, End_turn): "Back."
    |}]
;;

let%expect_test "an agent with no session/load starts afresh, saying so" =
  with_acp ~peer:(fun n p ->
    P.initialize ~caps:"{}" p;
    match n with
    | 1 ->
      P.new_session p "ses_1";
      ignore (P.expect p "session/prompt" : Json.t);
      Line_transport.Peer.exit p ~code:1
    | _ ->
      P.new_session p "ses_2";
      let prompt = P.expect p "session/prompt" in
      P.text p "ses_2" "Fresh.";
      P.result p prompt {|{"stopReason":"end_turn"}|})
  @@ fun agent composer ->
  prompt agent "first";
  prompt agent "second";
  print_s [%sexp (Composer_runner.thread_id composer : string option)];
  [%expect
    {|
    spawn #1: opencode acp
    agent got initialize
    agent got session/new session=- mcp=[]
    agent got session/prompt "first"
    assistant (acp:opencode/default, (Error"OpenCode exited (code 1)")): ""
    spawn #2: opencode acp
    agent got initialize
    agent got session/new session=- mcp=[]
    notice: OpenCode can't continue its earlier session: it starts a new one
    agent got session/prompt "second"
    assistant (acp:opencode/default, End_turn): "Fresh."
    (ses_2)
    |}]
;;

let%expect_test "not logged in" =
  with_acp ~peer:(fun _ p ->
    P.initialize p;
    let j = P.expect p "session/new" in
    P.error p j ~code:(-32000) "Authentication required")
  @@ fun agent _ ->
  prompt agent "hi";
  [%expect
    {|
    spawn #1: opencode acp
    agent got initialize
    agent got session/new session=- mcp=[]
    assistant (acp:opencode/default, (Error"OpenCode is not logged in on this host: run `opencode auth login` there")): ""
    |}]
;;

let%expect_test "an agent on another ACP version is refused, saying why" =
  with_acp ~peer:(fun _ p ->
    let j = P.expect p "initialize" in
    P.result p j {|{"protocolVersion":2,"agentCapabilities":{}}|})
  @@ fun agent _ ->
  prompt agent "hi";
  [%expect
    {|
    spawn #1: opencode acp
    agent got initialize
    assistant (acp:opencode/default, (Error"OpenCode speaks ACP version 2, and prigh speaks version 1: update prigh or OpenCode")): ""
    |}]
;;

let%expect_test "capabilities come from what the agent advertises" =
  let module C = Acp_client_codec in
  let c = C.create () in
  ignore (C.initialize c ~client_name:"prigh" ~client_version:"0" : string);
  ignore
    (C.handle_line
       c
       {|{"jsonrpc":"2.0","id":0,"result":{"protocolVersion":1,"agentCapabilities":{"promptCapabilities":{"image":true}}}}|}
     : C.Output.t list);
  ignore (C.new_session c ~cwd:"/w" ~mcp_servers:[] : string);
  ignore
    (C.handle_line
       c
       (sprintf
          {|{"jsonrpc":"2.0","id":1,"result":{"sessionId":"s",%s}}|}
          P.modes)
     : C.Output.t list);
  print_s
    [%sexp
      (Acp_adapter.capabilities_of (C.agent_capabilities c) (C.session_config c)
       : Capabilities.t)];
  [%expect
    {|
    ((fork By_handoff) (rewind Unsupported) (context_edit false)
     (compact Unsupported) (steer Between_turns) (plan_mode true) (btw false)
     (subagents Unsupported) (model_fallback false) (images true)
     (ask_user true))
    |}]
;;

let%expect_test "the prigh adapter: a composer session prigh's own harness runs"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:
        (Faux_provider.create [ Faux_provider.Reply.text "From the harness." ])
      ~tools:Tools.interaction
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:t.dir
      ()
  in
  Agent.set_engine agent { engine = Composer; adapter = Some Prigh };
  Agent.set_runner
    agent
    (Some (Prigh_adapter.runner ~harness:(fun () -> Agent.harness_runner agent)));
  Agent.subscribe agent ~f:print_event;
  prompt agent "hi";
  print_s
    [%message
      ""
        ~engine:(Agent.engine agent : Engine_id.t)
        ~label:(Agent.engine_label agent : string)
        ~compact:(Agent.require agent Compact : unit Or_error.t)
        ~rewind:(Agent.require agent Rewind : unit Or_error.t)];
  [%expect
    {|
    assistant (deepseek-flash, End_turn): "From the harness."
    ((engine Composer) (label prigh) (compact (Ok ())) (rewind (Ok ())))
    |}]
;;

let%expect_test "-adapters: ACP agents and prigh, and new sessions on them" =
  let show adapters =
    match
      Engines.of_flags
        ~engines:(Some "composer")
        ~adapters:(Some adapters)
        ~getenv:(Fn.const None)
    with
    | Error e -> print_endline (Error.to_string_hum e)
    | Ok t ->
      print_s
        [%message
          ""
            ~adapters:(Engines.adapter_strings t : string list)
            ~check:(Engines.check_build t : unit Or_error.t)
            ~opencode:(Engines.acp_agent t "opencode" : Acp_agent.t option)
            ~mine:(Engines.acp_agent t "mine" : Acp_agent.t option)]
  in
  show "codex,acp:opencode,prigh";
  [%expect
    {|
    ((adapters (codex acp:opencode prigh)) (check (Ok ()))
     (opencode
      (((name opencode) (display_name OpenCode) (command (opencode acp))
        (quirks
         ((login ("opencode auth login")) (min_version ()) (prompt_usage Ignored)
          (steer Between_turns) (xai_extensions false))))))
     (mine ()))
    |}];
  show "acp:mine=my-agent --acp --verbose,claude";
  [%expect
    {|
    ((adapters (acp:mine claude)) (check (Ok ()))
     (opencode
      (((name opencode) (display_name OpenCode) (command (opencode acp))
        (quirks
         ((login ("opencode auth login")) (min_version ()) (prompt_usage Ignored)
          (steer Between_turns) (xai_extensions false))))))
     (mine
      (((name mine) (display_name mine) (command (my-agent --acp --verbose))
        (quirks
         ((login ()) (min_version ()) (prompt_usage Ignored)
          (steer Between_turns) (xai_extensions false)))))))
    |}];
  show "acp:unheard-of";
  [%expect
    {|
    ((adapters (acp:unheard-of))
     (check
      (Error
       "prigh does not know how to start the ACP agent unheard-of: give its command with -adapters acp:unheard-of=<command and args> (it knows opencode, gemini, antigravity, grok, goose)"))
     (opencode
      (((name opencode) (display_name OpenCode) (command (opencode acp))
        (quirks
         ((login ("opencode auth login")) (min_version ()) (prompt_usage Ignored)
          (steer Between_turns) (xai_extensions false))))))
     (mine ()))
    |}];
  List.iter [ Adapter_id.Prigh; Acp "opencode"; Acp "mine" ] ~f:(fun id ->
    let engines =
      Or_error.ok_exn
        (Engines.create
           ~engines:[ Composer ]
           ~adapters:[ Prigh; Acp "opencode" ]
           ())
    in
    print_s
      [%message
        (Adapter_id.to_string id)
          ~find:
            (Result.map
               (Composer_adapters.find engines id)
               ~f:(fun (module A : Adapter.S) -> A.id)
             : string Or_error.t)]);
  [%expect
    {|
    (prigh
     (find
      (Error
       "prigh's own harness runs in the backend, not as an external process")))
    (acp:opencode (find (Ok acp:opencode)))
    (acp:mine
     (find
      (Error
       "prigh does not know how to start the ACP agent mine: give its command with -adapters acp:mine=<command and args> (it knows opencode, gemini, antigravity, grok, goose)")))
    |}]
;;

(* Antigravity's and Grok's shape: the session model API, and a thought
   level option. *)
let session_with_models p session =
  let j = P.expect p "session/new" in
  P.result
    p
    j
    (sprintf
       {|{"sessionId":%S,
          "models":{"currentModelId":"gemini-flash","availableModels":[
            {"modelId":"gpt-oss","name":"GPT-OSS 120B (Medium)"},
            {"modelId":"gemini-flash","name":"Gemini 3.8 Flash"},
            {"modelId":"sonnet","name":"Claude Sonnet 4.6 (Thinking)"}]},
          "configOptions":[{"id":"effort","name":"Effort","category":"thought_level",
            "type":"select","currentValue":"medium","options":[
              {"value":"low","name":"Low"},{"value":"medium","name":"Medium"},
              {"value":"high","name":"High"}]}]}|}
       session)
;;

let print_models agent =
  List.iter
    (Option.value (Agent.external_models agent) ~default:[])
    ~f:(fun (m : Model.t) ->
      printf
        "  %s %S thinking=[%s]\n"
        (Model.key m)
        m.name
        (String.concat
           ~sep:","
           (List.map m.thinking_levels ~f:Thinking.Level.to_string)))
;;

let%expect_test
    "models from the session model API: the default first, set with \
     session/set_model"
  =
  with_acp ~peer:(fun _ p ->
    P.initialize p;
    session_with_models p "ses_1";
    let prompt = P.expect p "session/prompt" in
    P.result p prompt {|{"stopReason":"end_turn"}|};
    let j = P.expect p "session/set_model" in
    P.result p j "{}";
    let j = P.expect p "session/set_config_option" in
    P.result p j "{}";
    let prompt = P.expect p "session/prompt" in
    P.result p prompt {|{"stopReason":"end_turn"}|};
    let j = P.expect p "session/set_model" in
    P.result p j "{}";
    let prompt = P.expect p "session/prompt" in
    P.result p prompt {|{"stopReason":"end_turn"}|})
  @@ fun agent _ ->
  prompt agent "one";
  print_models agent;
  let models = Option.value_exn (Agent.external_models agent) in
  Agent.set_model
    agent
    (List.find_exn models ~f:(fun m -> String.equal m.id "sonnet"));
  Or_error.ok_exn (Agent.set_thinking agent (On (Some High)));
  prompt agent "two";
  print_s [%sexp (Model.key (Agent.state agent).model : string)];
  Agent.set_model agent (List.hd_exn models);
  prompt agent "three";
  [%expect
    {|
    spawn #1: opencode acp
    agent got initialize
    agent got session/new session=- mcp=[]
    agent got session/prompt "one"
      acp:opencode/default "Gemini 3.8 Flash" thinking=[low,medium,high]
      acp:opencode/gpt-oss "GPT-OSS 120B (Medium)" thinking=[low,medium,high]
      acp:opencode/sonnet "Claude Sonnet 4.6 (Thinking)" thinking=[low,medium,high]
    agent got session/set_model sonnet
    agent got session/set_config_option effort=high
    agent got session/prompt "two"
    acp:opencode/sonnet
    agent got session/set_model gemini-flash
    agent got session/prompt "three"
    |}]
;;

let%expect_test "an ACP agent's catalog: a session opened to read its models" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let (module A : Adapter.S) = Acp_adapter.create opencode in
  let transport =
    Line_transport.Peer.spawn ~sw (fun p ->
      P.initialize p;
      session_with_models p "ses_1")
  in
  (match A.catalog ~sw transport (Adapter.Start.default ~cwd:t.dir) with
   | Error e -> print_s [%sexp (e : Error.t)]
   | Ok { models; _ } ->
     List.iter models ~f:(fun (m : Model.t) ->
       printf "%s %S\n" (Model.key m) m.name));
  [%expect
    {|
    agent got initialize
    agent got session/new session=- mcp=[]
    acp:opencode/default "Gemini 3.8 Flash"
    acp:opencode/gpt-oss "GPT-OSS 120B (Medium)"
    acp:opencode/sonnet "Claude Sonnet 4.6 (Thinking)"
    |}]
;;

(* The frontends name the known agents themselves
   ([Session_engine.adapter_label]): their names must agree. Silent where
   the frontend's source is not there (the backend built alone). *)
let%expect_test "the frontends name the known ACP agents as prigh does" =
  let rec find dir n =
    let path = Filename.concat dir "frontend/protocol/session_engine.ml" in
    if Sys_unix.file_exists_exn path
    then Some path
    else if n = 0
    then None
    else find (Filename.dirname dir) (n - 1)
  in
  Option.iter
    (find (Sys_unix.getcwd ()) 8)
    ~f:(fun path ->
      let source = In_channel.read_all path in
      List.iter Acp_agent.known ~f:(fun (agent : Acp_agent.t) ->
        let line = sprintf "Some %S -> %S" agent.name agent.display_name in
        if not (String.is_substring source ~substring:line)
        then printf "%s does not say: | %s\n" path line));
  [%expect {| |}]
;;

let%expect_test "an agent that exits before starting says why and what to do" =
  let antigravity = Option.value_exn (Acp_agent.find_known "antigravity") in
  let exits stderr =
    with_acp ~agent:antigravity ~peer:(fun _ p ->
      Line_transport.Peer.write_stderr p stderr;
      Line_transport.Peer.exit p ~code:2)
    @@ fun agent _ -> prompt agent "hi"
  in
  exits
    "Error: unexpected argument \"acp\".\n\
     Prompts are read only from -p/--print, -i/--prompt-interactive, or stdin, \
     so this argument would have been ignored.\n";
  [%expect
    {|
    spawn #1: agy acp
    assistant (acp:antigravity/default, (Error"Antigravity exited before starting (code 2):\nError: unexpected argument \"acp\".\nPrompts are read only from -p/--print, -i/--prompt-interactive, or stdin, so this argument would have been ignored.\nUpdate Antigravity to 1.5.0 or later: older versions do not have `agy acp`.")): ""
    |}];
  exits "panic: no config\n";
  [%expect
    {|
    spawn #1: agy acp
    assistant (acp:antigravity/default, (Error"Antigravity exited before starting (code 2):\npanic: no config\nCheck Antigravity in Settings \226\134\146 Engines (/adapters).")): ""
    |}]
;;

let%expect_test "versions and quirks of the known agents" =
  List.iter
    [ "1.4.12", "1.5.0"; "1.5.0", "1.5.0"; "v1.10.0", "1.5.0"; "dev", "1.5.0" ]
    ~f:(fun (v, than) ->
      printf "%s < %s: %b\n" v than (Acp_agent.older v ~than));
  List.iter Acp_agent.known ~f:(fun (a : Acp_agent.t) ->
    print_s [%sexp (a.name : string), (a.quirks : Acp_agent.Quirks.t)]);
  [%expect
    {|
    1.4.12 < 1.5.0: true
    1.5.0 < 1.5.0: false
    v1.10.0 < 1.5.0: false
    dev < 1.5.0: false
    (opencode
     ((login ("opencode auth login")) (min_version ()) (prompt_usage Ignored)
      (steer Between_turns) (xai_extensions false)))
    (gemini
     ((login (gemini)) (min_version ()) (prompt_usage Ignored)
      (steer Between_turns) (xai_extensions false)))
    (antigravity
     ((login ("agy acp --login")) (min_version (1.5.0)) (prompt_usage Gemini)
      (steer Prompt) (xai_extensions false)))
    (grok
     ((login ("grok login")) (min_version ()) (prompt_usage Ignored)
      (steer Xai_interject) (xai_extensions true)))
    (goose
     ((login ("goose configure")) (min_version ()) (prompt_usage Ignored)
      (steer Between_turns) (xai_extensions false)))
    |}]
;;

let known name = Option.value_exn (Acp_agent.find_known name)

(* Starts [go], steers with [also this] once the agent is writing, and
   prints the session's log after. *)
let steer_mid_run agent =
  let started, resolve = Eio.Promise.create () in
  Agent.subscribe agent ~f:(function
    | Loop (Message_update _) ->
      if not (Eio.Promise.is_resolved started)
      then Eio.Promise.resolve resolve ()
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "go");
  Eio.Promise.await started;
  Or_error.ok_exn (Agent.steer agent "also this");
  Agent.wait_idle agent;
  print_endline "--- log";
  List.iter (Agent.messages agent) ~f:(fun m ->
    print_endline (Test_background_subagents.message_line m))
;;

let%expect_test "Antigravity: a message mid-run steers with another prompt" =
  with_acp ~agent:(known "antigravity") ~peer:(fun _ p ->
    P.initialize p;
    P.new_session p "ses_1";
    let first = P.expect p "session/prompt" in
    P.text p "ses_1" "Working";
    let second = P.expect p "session/prompt" in
    (* The running prompt ends; the turn goes on under the steer. *)
    P.result p first {|{"stopReason":"end_turn"}|};
    P.text p "ses_1" " and that too.";
    P.result p second {|{"stopReason":"end_turn"}|})
  @@ fun agent _ ->
  steer_mid_run agent;
  [%expect
    {|
    spawn #1: agy acp
    agent got initialize
    agent got session/new session=- mcp=[]
    agent got session/prompt "go"
    assistant (acp:antigravity/default, End_turn): "Working"
    agent got session/prompt "also this"
    assistant (acp:antigravity/default, End_turn): " and that too."
    --- log
    user: [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).] |  | go
    assistant: Working
    user: also this
    assistant: and that too.
    |}]
;;

let%expect_test "Grok: a message mid-run interjects" =
  with_acp ~agent:(known "grok") ~peer:(fun _ p ->
    P.initialize p;
    P.new_session p "ses_1";
    let prompt = P.expect p "session/prompt" in
    P.text p "ses_1" "Working";
    let interject = P.expect p "_x.ai/interject" in
    printf
      "interjected: %s\n"
      (Json.to_string
         (Option.value (Json.member "params" interject) ~default:`Null));
    P.result p interject "{}";
    P.text p "ses_1" " and that too.";
    P.result p prompt {|{"stopReason":"end_turn"}|})
  @@ fun agent _ ->
  steer_mid_run agent;
  [%expect
    {|
    spawn #1: grok agent stdio
    agent got initialize
    agent got session/new session=- mcp=[]
    agent got session/prompt "go"
    assistant (acp:grok/default, End_turn): "Working"
    agent got _x.ai/interject
    interjected: {"sessionId":"ses_1","text":"also this"}
    assistant (acp:grok/default, End_turn): " and that too."
    --- log
    user: [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).] |  | go
    assistant: Working
    user: also this
    assistant: and that too.
    |}]
;;

let%expect_test "Antigravity: stopping a steered turn" =
  let steered, steered_resolve = Eio.Promise.create () in
  with_acp ~agent:(known "antigravity") ~peer:(fun _ p ->
    P.initialize p;
    P.new_session p "ses_1";
    let first = P.expect p "session/prompt" in
    P.text p "ses_1" "Working";
    let second = P.expect p "session/prompt" in
    P.result p first {|{"stopReason":"end_turn"}|};
    Eio.Promise.resolve steered_resolve ();
    ignore (P.expect p "session/cancel" : Json.t);
    P.result p second {|{"stopReason":"cancelled"}|})
  @@ fun agent _ ->
  let started, resolve = Eio.Promise.create () in
  Agent.subscribe agent ~f:(function
    | Loop (Message_update _) ->
      if not (Eio.Promise.is_resolved started)
      then Eio.Promise.resolve resolve ()
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "go");
  Eio.Promise.await started;
  Or_error.ok_exn (Agent.steer agent "also this");
  Eio.Promise.await steered;
  ignore (Agent.abort agent : string list);
  Agent.wait_idle agent;
  print_endline "--- log";
  List.iter (Agent.messages agent) ~f:(fun m ->
    print_endline (Test_background_subagents.message_line m));
  [%expect
    {|
    spawn #1: agy acp
    agent got initialize
    agent got session/new session=- mcp=[]
    agent got session/prompt "go"
    assistant (acp:antigravity/default, End_turn): "Working"
    agent got session/prompt "also this"
    agent got session/cancel
    --- log
    user: [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).] |  | go
    assistant: Working
    user: also this
    |}]
;;

let%expect_test
    "Grok's plan is put to the user: start working, or keep planning"
  =
  let exit_plan p ~id plan =
    P.write
      p
      [ "jsonrpc", `String "2.0"
      ; "id", `String id
      ; "method", `String "_x.ai/exit_plan_mode"
      ; ( "params"
        , `Object
            [ "sessionId", `String "ses_1"
            ; "toolCallId", `String ("tc-" ^ id)
            ; "planContent", `String plan
            ] )
      ]
  in
  with_acp ~agent:(known "grok") ~peer:(fun _ p ->
    P.initialize p;
    P.new_session p "ses_1";
    let mode = P.expect p "session/set_mode" in
    P.result p mode "{}";
    let prompt = P.expect p "session/prompt" in
    exit_plan p ~id:"p1" "1. Fix\n2. Test";
    P.answer p;
    P.text p "ses_1" "Changed it.";
    exit_plan p ~id:"p2" "1. Fix\n2. Test\n3. Document";
    P.answer p;
    (* Approved, it leaves plan mode itself: prigh does not put it back. *)
    P.update
      p
      "ses_1"
      {|{"sessionUpdate":"current_mode_update","currentModeId":"build"}|};
    P.text p "ses_1" "Done.";
    P.result p prompt {|{"stopReason":"end_turn"}|};
    let again = P.expect p "session/prompt" in
    P.text p "ses_1" "Built.";
    P.result p again {|{"stopReason":"end_turn"}|})
  @@ fun agent _ ->
  let answers = Queue.of_list [ "Keep planning"; "Start working" ] in
  Agent.subscribe agent ~f:(function
    | Loop (Ask_user { call_id; questions; _ }) ->
      List.iter questions ~f:(fun q ->
        printf
          "ask_user [%s]: %s\n"
          (Option.value q.header ~default:"")
          q.question);
      Or_error.ok_exn
        (Agent.answer_question
           agent
           ~call_id
           (Answered [ [ Queue.dequeue_exn answers ] ]))
    | _ -> ());
  Agent.set_mode agent ~plan:true;
  prompt agent "plan it";
  prompt agent "now build";
  [%expect
    {|
    spawn #1: grok agent stdio
    agent got initialize
    agent got session/new session=- mcp=[]
    agent got session/set_mode plan
    agent got session/prompt "plan it"
    ask_user [Plan]: Grok Build's plan:

    1. Fix
    2. Test
    agent got answer {"outcome":"request_changes","feedback":"The user wants to keep planning: stay in plan mode and wait for what they want changed."}
    ask_user [Plan]: Grok Build's plan:

    1. Fix
    2. Test
    3. Document
    agent got answer {"outcome":"approved"}
    assistant (acp:grok/default, End_turn): "Changed it.Done."
    agent got session/prompt "now build"
    assistant (acp:grok/default, End_turn): "Built."
    |}]
;;

(* prigh's Antigravity login [g]: [ya29.N], expiring in [expires_min.(N-1)]
   minutes. *)
let antigravity_login ?(fail = false) ~expires_min () =
  let n = ref 0 in
  Cli_credential.create
    ~provider:Google_antigravity
    ~account:(Own (Account_id.of_string "g" |> Or_error.ok_exn))
    ~label:"g"
    ~fetch:(fun ?refused ?valid_for_ms () ->
      incr n;
      printf
        "prigh fetches a token valid for %s min%s\n"
        (Option.value_map valid_for_ms ~default:"?" ~f:(fun ms ->
           Int.to_string (ms / 60_000)))
        (Option.value_map refused ~default:"" ~f:(sprintf ", refused %s"));
      if fail
      then Or_error.error_string "refreshing failed: /login google-antigravity"
      else
        Ok
          { Provider_auth.Resolved.token = sprintf "ya29.%d" !n
          ; method_ = Oauth
          ; account_id = None
          ; source = "oauth"
          ; account = None
          ; expires_ms =
              Some
                (Credential.now_ms ()
                 + (60_000 * List.nth_exn expires_min (!n - 1)))
          })
    ~record:(fun usage ~cost_usd ->
      printf
        "recorded on g: %s, $%.2f\n"
        (Sexp.to_string [%sexp (usage : Usage.t)])
        cost_usd)
    ()
;;

let print_agy_launch (launch : Adapter.Launch.t) =
  let token =
    Option.map launch.secret ~f:(fun s ->
      match Json.parse s.value with
      | Ok json ->
        let token = Option.value (Json.member "token" json) ~default:`Null in
        sprintf
          "%s %s on %s (else %s)"
          (Option.value_map
             (Json.member "access_token" token)
             ~default:"?"
             ~f:Json.to_string)
          (Option.value_map
             (Json.member "auth_method" json)
             ~default:"?"
             ~f:Json.to_string)
          s.fd_env
          s.env
      | Error _ -> "?")
  in
  printf
    "  %sunset %s; %s\n"
    (if
       List.mem
         launch.argv
         Antigravity_login.For_testing.wrapper
         ~equal:String.equal
     then "the wrapper; "
     else "")
    (String.concat ~sep:" " launch.env_unset)
    (Option.value token ~default:"agy's own login")
;;

let antigravity = Option.value_exn (Acp_agent.find_known "antigravity")

let agy_turn ?(refuse = false) p ~session =
  let prompt = P.expect p "session/prompt" in
  if refuse
  then P.error p prompt ~code:(-32000) "Authentication required"
  else (
    P.text p session "ok";
    P.result
      p
      prompt
      {|{"stopReason":"end_turn","usage":{"inputTokens":3,"outputTokens":1,"totalTokens":4}}|})
;;

let%expect_test "Antigravity on prigh's login: a token file, renewed, refused" =
  let login = antigravity_login ~expires_min:[ 15; 60; 60 ] () in
  with_acp
    ~agent:antigravity
    ~on_launch:print_agy_launch
    ~start:(fun ~cwd ->
      { (Adapter.Start.default ~cwd) with login = Some login })
    ~peer:(fun n p ->
      P.initialize p;
      (match n with
       | 1 -> P.new_session p "s1"
       | _ ->
         let load = P.expect p "session/load" in
         P.result p load "{}");
      let rec turns i =
        match n, i with
        | 2, 2 | 3, _ ->
          agy_turn p ~session:"s1" ~refuse:true;
          turns (i + 1)
        | _ ->
          agy_turn p ~session:"s1";
          turns (i + 1)
      in
      try turns 1 with
      | _ -> ())
  @@ fun agent _ ->
  prompt agent "hi";
  [%expect
    {|
    prigh fetches a token valid for 40 min
    spawn #1: bash -c <script> agy agy acp
      the wrapper; unset GEMINI_API_KEY GOOGLE_API_KEY GOOGLE_APPLICATION_CREDENTIALS GOOGLE_GENAI_USE_VERTEXAI GOOGLE_CLOUD_PROJECT ANTIGRAVITY_PROJECT_ID; "ya29.1" "consumer" on PRIGH_AGY_TOKEN_FD (else PRIGH_AGY_TOKEN)
    agent got initialize
    agent got session/new session=- mcp=[]
    agent got session/prompt "hi"
    recorded on g: ((input 3)(output 1)(cache_read 0)), $0.00
    assistant (acp:antigravity/default, End_turn): "ok"
    |}];
  (* expires within 20 minutes: a new process, loading the session *)
  prompt agent "again";
  [%expect
    {|
    prigh fetches a token valid for 40 min
    spawn #2: bash -c <script> agy agy acp
      the wrapper; unset GEMINI_API_KEY GOOGLE_API_KEY GOOGLE_APPLICATION_CREDENTIALS GOOGLE_GENAI_USE_VERTEXAI GOOGLE_CLOUD_PROJECT ANTIGRAVITY_PROJECT_ID; "ya29.2" "consumer" on PRIGH_AGY_TOKEN_FD (else PRIGH_AGY_TOKEN)
    agent got initialize
    agent got session/load session=s1 mcp=[]
    agent got session/prompt "again"
    recorded on g: ((input 3)(output 1)(cache_read 0)), $0.00
    assistant (acp:antigravity/default, End_turn): "ok"
    |}];
  (* refused: a newer token, then (refused again) agy's own login *)
  prompt agent "refused";
  [%expect
    {|
    agent got session/prompt "refused"
    assistant (acp:antigravity/default, (Error"Authentication required: Google refused prigh's Antigravity login \"g\": prigh refreshes it and restarts agy on your next message (/login google-antigravity if it is refused again)")): ""
    |}];
  prompt agent "once more";
  [%expect
    {|
    prigh fetches a token valid for 40 min, refused ya29.2
    spawn #3: bash -c <script> agy agy acp
      the wrapper; unset GEMINI_API_KEY GOOGLE_API_KEY GOOGLE_APPLICATION_CREDENTIALS GOOGLE_GENAI_USE_VERTEXAI GOOGLE_CLOUD_PROJECT ANTIGRAVITY_PROJECT_ID; "ya29.3" "consumer" on PRIGH_AGY_TOKEN_FD (else PRIGH_AGY_TOKEN)
    agent got initialize
    agent got session/load session=s1 mcp=[]
    agent got session/prompt "once more"
    assistant (acp:antigravity/default, (Error"Authentication required: Google refused prigh's Antigravity login \"g\": prigh refreshes it and restarts agy on your next message (/login google-antigravity if it is refused again)")): ""
    |}];
  prompt agent "last";
  [%expect
    {|
    notice: Antigravity was refused prigh's Antigravity (Google) login "g" again, after a refresh: this session runs on Antigravity's own login on its tool host from now on (/login google-antigravity logs in again, for new sessions).
    spawn #4: agy acp
      unset ; agy's own login
    agent got initialize
    agent got session/load session=s1 mcp=[]
    agent got session/prompt "last"
    assistant (acp:antigravity/default, End_turn): "ok"
    |}]
;;

let%expect_test "Antigravity: no token for agy, so its own login, saying so" =
  with_acp
    ~agent:antigravity
    ~on_launch:print_agy_launch
    ~start:(fun ~cwd ->
      { (Adapter.Start.default ~cwd) with
        login = Some (antigravity_login ~fail:true ~expires_min:[] ())
      })
    ~peer:(fun _ p ->
      P.initialize p;
      P.new_session p "s1";
      agy_turn p ~session:"s1")
  @@ fun agent _ ->
  prompt agent "hi";
  [%expect
    {|
    prigh fetches a token valid for 40 min
    spawn #1: agy acp
      unset ; agy's own login
    agent got initialize
    agent got session/new session=- mcp=[]
    notice: prigh's Antigravity login "g" has no token for agy (refreshing failed: /login google-antigravity): this session runs on agy's own login on its tool host (agy acp --login there).
    agent got session/prompt "hi"
    assistant (acp:antigravity/default, End_turn): "ok"
    |}]
;;
