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

(* Durations vary from run to run. *)
let seconds_re = Re.compile (Re.Perl.re {|[0-9]+\.[0-9] s|})
let mask_seconds s = Re.replace_string seconds_re ~by:"_ s" s
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
  | Notice n -> printf "notice: %s\n" (mask_seconds n)
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
     (compact Summary) (steer Between_turns) (plan_mode true) (btw false)
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
         ((login ("opencode auth login")) (login_file ()) (min_version ())
          (prompt_usage Ignored) (steer Between_turns) (xai_extensions false))))))
     (mine ()))
    |}];
  show "acp:mine=my-agent --acp --verbose,claude";
  [%expect
    {|
    ((adapters (acp:mine claude)) (check (Ok ()))
     (opencode
      (((name opencode) (display_name OpenCode) (command (opencode acp))
        (quirks
         ((login ("opencode auth login")) (login_file ()) (min_version ())
          (prompt_usage Ignored) (steer Between_turns) (xai_extensions false))))))
     (mine
      (((name mine) (display_name mine) (command (my-agent --acp --verbose))
        (quirks
         ((login ()) (login_file ()) (min_version ()) (prompt_usage Ignored)
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
         ((login ("opencode auth login")) (login_file ()) (min_version ())
          (prompt_usage Ignored) (steer Between_turns) (xai_extensions false))))))
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
      acp:opencode/gemini-flash "Gemini 3.8 Flash" thinking=[low,medium,high]
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

let%expect_test
    "a session on the placeholder model (as recorded before prigh named the \
     harness's default) takes the real one once the agent lists its models"
  =
  with_acp ~peer:(fun _ p ->
    P.initialize p;
    session_with_models p "ses_1";
    List.iter [ "Hi."; "Again." ] ~f:(fun reply ->
      let prompt = P.expect p "session/prompt" in
      P.text p "ses_1" reply;
      P.result p prompt {|{"stopReason":"end_turn"}|}))
  @@ fun agent _ ->
  let show () =
    let state = Agent.state agent in
    printf
      "model %s %S, window %d; recorded %s\n"
      (Model.key state.model)
      state.model.name
      state.model.context_window
      (Option.value_map
         (Session.model (Agent.session agent))
         ~default:"-"
         ~f:fst)
  in
  Agent.set_model agent (External_model.model ~adapter:"acp:opencode" "default");
  show ();
  prompt agent "one";
  show ();
  prompt agent "two";
  [%expect
    {|
    model acp:opencode/default "loading models\226\128\166", window 0; recorded acp:opencode/default
    spawn #1: opencode acp
    agent got initialize
    agent got session/new session=- mcp=[]
    agent got session/prompt "one"
    assistant (acp:opencode/gemini-flash, End_turn): "Hi."
    model acp:opencode/gemini-flash "Gemini 3.8 Flash", window 1048576; recorded acp:opencode/gemini-flash
    agent got session/prompt "two"
    assistant (acp:opencode/gemini-flash, End_turn): "Again."
    |}];
  (* Recorded on the placeholder: the agent's default once listed, nothing
     before. *)
  let restore models =
    External_model.restore
      ~adapter:"acp:opencode"
      ~models
      "acp:opencode/default"
    |> Option.map ~f:Model.key
  in
  print_s
    [%message
      ""
        ~before:(restore [] : string option)
        ~listed:
          (restore (Option.value_exn (Agent.external_models agent))
           : string option)];
  [%expect {| ((before ()) (listed (acp:opencode/gemini-flash))) |}]
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
    acp:opencode/gemini-flash "Gemini 3.8 Flash"
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
     ((login ("opencode auth login")) (login_file ()) (min_version ())
      (prompt_usage Ignored) (steer Between_turns) (xai_extensions false)))
    (gemini
     ((login (gemini)) (login_file ()) (min_version ()) (prompt_usage Ignored)
      (steer Between_turns) (xai_extensions false)))
    (antigravity
     ((login ("agy acp --login"))
      (login_file (~/.gemini/antigravity-cli/antigravity-oauth-token))
      (min_version (1.5.0)) (prompt_usage Gemini) (steer Prompt)
      (xai_extensions false)))
    (grok
     ((login ("grok login")) (login_file ()) (min_version ())
      (prompt_usage Ignored) (steer Xai_interject) (xai_extensions true)))
    (goose
     ((login ("goose configure")) (login_file ()) (min_version ())
      (prompt_usage Ignored) (steer Between_turns) (xai_extensions false)))
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
  let login = antigravity_login ~expires_min:[ 8; 60; 60 ] () in
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
  (* expires within 10 minutes: a new process, loading the session *)
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

let%expect_test "two turns: one process, one session/new, nothing replayed" =
  with_acp ~peer:(fun _ p ->
    P.initialize
      ~caps:{|{"loadSession":true,"sessionCapabilities":{"resume":{}}}|}
      p;
    P.new_session p "ses_1";
    List.iter [ "One."; "Two." ] ~f:(fun reply ->
      let prompt = P.expect p "session/prompt" in
      P.text p "ses_1" reply;
      P.result p prompt {|{"stopReason":"end_turn"}|}))
  @@ fun agent _ ->
  prompt agent "first";
  prompt agent "second";
  [%expect
    {|
    spawn #1: opencode acp
    agent got initialize
    agent got session/new session=- mcp=[]
    agent got session/prompt "first"
    assistant (acp:opencode/default, End_turn): "One."
    agent got session/prompt "second"
    assistant (acp:opencode/default, End_turn): "Two."
    |}]
;;

let%expect_test
    "a session opened on an ACP agent starts it before the first message"
  =
  let opened, open_ = Eio.Promise.create () in
  with_acp ~peer:(fun _ p ->
    P.initialize p;
    P.new_session p "ses_1";
    Eio.Promise.resolve open_ ();
    let prompt = P.expect p "session/prompt" in
    P.text p "ses_1" "Warm.";
    P.result p prompt {|{"stopReason":"end_turn"}|};
    let prompt = P.expect p "session/prompt" in
    P.text p "ses_1" "Still warm.";
    P.result p prompt {|{"stopReason":"end_turn"}|})
  @@ fun agent _ ->
  Agent.prewarm agent;
  (* again while it starts: nothing more *)
  Agent.prewarm agent;
  print_s [%message (Agent.external_live agent : bool)];
  Eio.Promise.await opened;
  print_endline "the user sends a message";
  prompt agent "go";
  Agent.prewarm agent;
  prompt agent "again";
  [%expect
    {|
    spawn #1: opencode acp
    ("Agent.external_live agent" true)
    agent got initialize
    agent got session/new session=- mcp=[]
    the user sends a message
    agent got session/prompt "go"
    assistant (acp:opencode/default, End_turn): "Warm."
    agent got session/prompt "again"
    assistant (acp:opencode/default, End_turn): "Still warm."
    |}]
;;

let%expect_test "start timings, as the trace says them" =
  let phase ?(alongside = false) ?(children = []) name seconds =
    { Turn_timings.Phase.name; seconds; alongside; children }
  in
  print_endline
    (Turn_timings.to_string_hum
       [ phase "login" 0.01
       ; phase
           "process"
           3.2
           ~children:
             [ phase "spawn" 0.4
             ; phase
                 "handshake"
                 2.8
                 ~children:
                   [ phase "initialize" 0.1; phase "session/resume" 2.7 ]
             ]
       ; phase "send" 0.
       ; phase "first_output" 1.8
       ; phase "checkpoint" 0.6 ~alongside:true
       ]);
  [%expect
    {| login 0.0 s, process 3.2 s (spawn 0.4 s, handshake 2.8 s (initialize 0.1 s, session/resume 2.7 s)), send 0.0 s, first_output 1.8 s, checkpoint 0.6 s (alongside) |}]
;;

let%expect_test
    "ACP autonomous wakeup after end_turn: tool, approval, text, and settle"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let wake_peer, trigger_wake = Eio.Promise.create () in
  let wake_ran, mark_wake_ran = Eio.Promise.create () in
  let agent_ref = ref None in
  let adapter =
    Acp_adapter.create
      ~load_quiet:(Time_float.Span.of_sec 0.2)
      ~autonomous_quiet:(Time_float.Span.of_sec 0.03)
      opencode
  in
  let composer =
    Composer_runner.create
      ~sw
      ~adapter
      ~spawn:(fun _ ->
        Ok
          (Line_transport.Peer.spawn ~sw (fun p ->
             P.initialize p;
             P.new_session p "ses_1";
             let req = P.expect p "session/prompt" in
             P.text p "ses_1" "Started background task.";
             P.result p req {|{"stopReason":"end_turn"}|};
             Eio.Promise.await wake_peer;
             (* Agent wakes autonomously after its background task finishes:
                runs a tool needing approval, then reports the result. *)
             P.update
               p
               "ses_1"
               {|{"sessionUpdate":"tool_call","toolCallId":"bg1","title":"bash","kind":"execute","status":"in_progress","rawInput":{"command":"echo ok"}}|};
             P.write
               p
               [ "jsonrpc", `String "2.0"
               ; "id", `Number "99"
               ; "method", `String "session/request_permission"
               ; ( "params"
                 , `Object
                     [ "sessionId", `String "ses_1"
                     ; ( "toolCall"
                       , `Object
                           [ "toolCallId", `String "bg1"
                           ; "title", `String "echo ok"
                           ; "kind", `String "execute"
                           ] )
                     ; ( "options"
                       , `Array
                           [ `Object
                               [ "optionId", `String "allow"
                               ; "name", `String "Allow"
                               ; "kind", `String "allow_once"
                               ]
                           ] )
                     ] )
               ];
             P.answer p;
             P.update
               p
               "ses_1"
               {|{"sessionUpdate":"tool_call_update","toolCallId":"bg1","status":"completed","content":[{"type":"content","content":{"type":"text","text":"ok"}}]}|};
             P.text p "ses_1" "Background task finished!";
             Eio.Promise.resolve mark_wake_ran ();
             ignore (P.read p : Json.t option))))
      ~on_autonomous:(fun () -> Option.iter !agent_ref ~f:Agent.external_woke)
      ~interrupt_grace:(Time_float.Span.of_sec 0.5)
      ()
  in
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
  agent_ref := Some agent;
  Agent.set_engine agent { engine = Composer; adapter = Some (Acp "opencode") };
  Agent.set_mode agent ~approval:Ask;
  let auton_ended, mark_auton_ended = Eio.Promise.create () in
  let seen_turns = ref 0 in
  Agent.subscribe agent ~f:(fun e ->
    print_event e;
    match e with
    | Loop (Tool_confirm { call_id; name; summary; _ }) ->
      printf "confirm %s: %s\n" name summary;
      Or_error.ok_exn (Agent.respond_confirm agent ~call_id Allow)
    | Loop (Agent_end _) ->
      incr seen_turns;
      if !seen_turns = 2 then Eio.Promise.resolve mark_auton_ended ()
    | _ -> ());
  prompt agent "start work";
  Eio.Promise.resolve trigger_wake ();
  Eio.Promise.await wake_ran;
  Eio.Promise.await auton_ended;
  Agent.wait_idle agent;
  Composer_runner.close composer;
  printf "messages in session: %d\n" (List.length (Agent.messages agent));
  [%expect
    {|
    agent got initialize
    agent got session/new session=- mcp=[]
    agent got session/prompt "start work"
    assistant (acp:opencode/default, End_turn): "Started background task."
    assistant (acp:opencode/default, Tool_use): ""
    tool_start bash
    confirm bash: echo ok
    agent got answer {"outcome":{"outcome":"selected","optionId":"allow"}}
    tool_end bash -> "ok"
    assistant (acp:opencode/default, End_turn): "Background task finished!"
    messages in session: 5
    |}]
;;

let%expect_test "aborting an ACP autonomous turn ends it cleanly" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let wake_peer, trigger_wake = Eio.Promise.create () in
  let tool_seen, mark_tool_seen = Eio.Promise.create () in
  let agent_ref = ref None in
  let adapter =
    Acp_adapter.create
      ~load_quiet:(Time_float.Span.of_sec 0.2)
      ~autonomous_quiet:(Time_float.Span.of_sec 5.)
      opencode
  in
  let composer =
    Composer_runner.create
      ~sw
      ~adapter
      ~spawn:(fun _ ->
        Ok
          (Line_transport.Peer.spawn ~sw (fun p ->
             P.initialize p;
             P.new_session p "ses_1";
             let req = P.expect p "session/prompt" in
             P.text p "ses_1" "Waiting.";
             P.result p req {|{"stopReason":"end_turn"}|};
             Eio.Promise.await wake_peer;
             P.update
               p
               "ses_1"
               {|{"sessionUpdate":"tool_call","toolCallId":"bg2","title":"bash","kind":"execute","status":"in_progress","rawInput":{"command":"sleep 99"}}|};
             ignore (P.expect p "session/cancel" : Json.t);
             ignore (P.read p : Json.t option))))
      ~on_autonomous:(fun () -> Option.iter !agent_ref ~f:Agent.external_woke)
      ~interrupt_grace:(Time_float.Span.of_sec 0.5)
      ()
  in
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
  agent_ref := Some agent;
  Agent.set_engine agent { engine = Composer; adapter = Some (Acp "opencode") };
  Agent.subscribe agent ~f:(fun e ->
    print_event e;
    match e with
    | Loop (Tool_start _) ->
      ignore (Eio.Promise.try_resolve mark_tool_seen () : bool)
    | _ -> ());
  prompt agent "go";
  Eio.Promise.resolve trigger_wake ();
  Eio.Promise.await tool_seen;
  ignore (Agent.abort agent : string list);
  Agent.wait_idle agent;
  Composer_runner.close composer;
  [%expect
    {|
    agent got initialize
    agent got session/new session=- mcp=[]
    agent got session/prompt "go"
    assistant (acp:opencode/default, End_turn): "Waiting."
    assistant (acp:opencode/default, Tool_use): ""
    tool_start bash
    agent got session/cancel
    tool_end bash -> ERROR "[failed]"
    |}]
;;

let%expect_test
    "Rpc_server keeps an idle external harness alive across disconnect for \
     external_idle_grace and survives autonomous wakeups"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let sessions_dir = Filename.concat t.dir "sessions" in
  let sleeps = Queue.create () in
  let sleep _seconds =
    let p, r = Eio.Promise.create () in
    Queue.enqueue sleeps r;
    Eio.Promise.await p
  in
  let wake_peer, trigger_wake = Eio.Promise.create () in
  let thread_closed, mark_thread_closed = Eio.Promise.create () in
  let peer_stopped, mark_peer_stopped = Eio.Promise.create () in
  let engines =
    Or_error.ok_exn
      (Engines.create ~engines:[ Harness; Composer ] ~adapters:[ Codex ] ())
  in
  let new_agent ?session ~cwd () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [])
      ~tools:Tools.interaction
      ~sessions_dir
      ~home:t.dir
      ?session
      ~cwd
      ()
  in
  let server =
    Rpc_server.create
      ~env:t.env
      ~sw
      ~external_idle_grace:1800.
      ~sleep
      ~login:
        (Login_manager.create
           ~env:t.env
           ~sw
           ~getenv:(fun _ -> None)
           ~store:(Auth_store.create ~path:(Filename.concat t.dir "auth.json"))
           ())
      ~sessions_dir
      ~cwd:t.dir
      ~engines
      ~spawn_external:(fun _agent ~host:_ _launch ->
        Ok
          (Line_transport.Peer.spawn ~sw (fun p ->
             let read () =
               Option.map (Line_transport.Peer.read_line p) ~f:(fun line ->
                 Json.parse line |> Or_error.ok_exn)
             in
             let reply j result =
               Line_transport.Peer.write_line
                 p
                 (Json.to_string
                    (`Object
                        [ "id", Option.value_exn (Json.member "id" j)
                        ; "result", result
                        ]))
             in
             let rec wait_turn () =
               match read () with
               | None -> ()
               | Some j ->
                 (match Option.bind (Json.member "method" j) ~f:Json.string with
                  | Some "initialize" ->
                    reply j (`Object [ "userAgent", `String "codex/0.166.0" ]);
                    wait_turn ()
                  | Some "thread/start" ->
                    reply
                      j
                      (`Object
                          [ "thread", `Object [ "id", `String "th-1" ]
                          ; "model", `String "gpt-5"
                          ]);
                    wait_turn ()
                  | Some "turn/start" ->
                    reply
                      j
                      (`Object
                          [ ( "turn"
                            , `Object
                                [ "id", `String "tu-1"
                                ; "status", `String "inProgress"
                                ] )
                          ])
                  | Some _ when Option.is_some (Json.member "id" j) ->
                    reply j (`Object []);
                    wait_turn ()
                  | _ -> wait_turn ())
             in
             wait_turn ();
             List.iter
               ~f:(Line_transport.Peer.write_line p)
               [ {|{"method":"turn/started","params":{"threadId":"th-1","turn":{"id":"tu-1","items":[],"status":"inProgress"}}}|}
               ; {|{"method":"item/completed","params":{"threadId":"th-1","turnId":"tu-1","item":{"type":"agentMessage","id":"m1","text":"first reply"}}}|}
               ; {|{"method":"turn/completed","params":{"threadId":"th-1","turn":{"id":"tu-1","items":[],"status":"completed"}}}|}
               ];
             Eio.Promise.await wake_peer;
             (* Autonomous turn while no clients are attached. *)
             List.iter
               ~f:(Line_transport.Peer.write_line p)
               [ {|{"method":"turn/started","params":{"threadId":"th-1","turn":{"id":"tu-2","items":[],"status":"inProgress"}}}|}
               ; {|{"method":"item/completed","params":{"threadId":"th-1","turnId":"tu-2","item":{"type":"agentMessage","id":"m2","text":"woke up while away"}}}|}
               ; {|{"method":"turn/completed","params":{"threadId":"th-1","turn":{"id":"tu-2","items":[],"status":"completed"}}}|}
               ];
             let rec drain () =
               match Line_transport.Peer.read_line p with
               | Some line ->
                 (match Json.parse line with
                  | Ok j
                    when Option.equal
                           String.equal
                           (Option.bind (Json.member "method" j) ~f:Json.string)
                           (Some "thread/unsubscribe") ->
                    ignore
                      (Eio.Promise.try_resolve mark_thread_closed () : bool)
                  | _ -> ());
                 drain ()
               | None ->
                 ignore (Eio.Promise.try_resolve mark_peer_stopped () : bool)
             in
             drain ())))
      ~new_agent
      ()
  in
  let client = Rpc_server.connect server ~send:ignore in
  let call ?(params = "{}") meth =
    ignore
      (Rpc_server.handle
         server
         client
         (Json.of_string
            (sprintf {|{"id":"r","method":%S,"params":%s}|} meth params))
       : Json.t)
  in
  call "hello";
  call ~params:{|{"adapter":"codex"}|} "new_session";
  let agent = Rpc_server.agent_of_client server client in
  let seen = ref 0 in
  let second_done, mark_second_done = Eio.Promise.create () in
  Agent.subscribe agent ~f:(function
    | Loop (Agent_end _) ->
      incr seen;
      if !seen = 2 then Eio.Promise.resolve mark_second_done ()
    | _ -> ());
  call ~params:{|{"text":"start"}|} "prompt";
  Agent.wait_idle agent;
  (* Disconnect the only client: live external harness must stay alive! *)
  Rpc_server.disconnect server client;
  Eio.Fiber.yield ();
  printf
    "after disconnect: live=%b stopped=%b timers=%d\n"
    (Agent.external_live agent)
    (Eio.Promise.is_resolved peer_stopped)
    (Queue.length sleeps);
  (* External harness wakes autonomously while disconnected. *)
  Eio.Promise.resolve trigger_wake ();
  Eio.Promise.await second_done;
  Agent.wait_idle agent;
  Eio.Fiber.yield ();
  (* Firing the first timer (from before the wakeup) does NOT evict it. *)
  Eio.Promise.resolve (Queue.dequeue_exn sleeps) ();
  Eio.Fiber.yield ();
  printf
    "after wakeup + stale timer: live=%b stopped=%b messages=%d\n"
    (Agent.external_live agent)
    (Eio.Promise.is_resolved peer_stopped)
    (List.length (Agent.messages agent));
  (* Reconnecting a client cancels the pending eviction timer too. *)
  let client2 = Rpc_server.connect server ~send:ignore in
  ignore
    (Rpc_server.handle
       server
       client2
       (Json.of_string
          (sprintf
             {|{"id":"r2","method":"hello","params":{"session":%S}}|}
             (Session.id (Agent.session agent))))
     : Json.t);
  Eio.Promise.resolve (Queue.dequeue_exn sleeps) ();
  Eio.Fiber.yield ();
  printf
    "after reconnect + stale timer: live=%b stopped=%b\n"
    (Agent.external_live agent)
    (Eio.Promise.is_resolved peer_stopped);
  (* Disconnecting again and letting the new grace period elapse evicts the
     session (unsubscribing the thread while the pooled server stays warm)
     and shutdown stops the physical server. *)
  Rpc_server.disconnect server client2;
  Eio.Fiber.yield ();
  Eio.Promise.resolve (Queue.dequeue_exn sleeps) ();
  Eio.Promise.await thread_closed;
  printf
    "after grace elapsed: live=%b unsubscribed=%b stopped=%b\n"
    (Agent.external_live agent)
    (Eio.Promise.is_resolved thread_closed)
    (Eio.Promise.is_resolved peer_stopped);
  Rpc_server.shutdown server;
  Eio.Promise.await peer_stopped;
  printf "after shutdown: stopped=%b\n" (Eio.Promise.is_resolved peer_stopped);
  [%expect
    {|
    after disconnect: live=true stopped=false timers=1
    after wakeup + stale timer: live=true stopped=false messages=3
    after reconnect + stale timer: live=true stopped=false
    after grace elapsed: live=false unsubscribed=true stopped=false
    after shutdown: stopped=true
    |}]
;;

let%expect_test "ACP skips git checkpoints when rollback is Unsupported" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let checkpoint_calls = ref 0 in
  let adapter =
    Acp_adapter.create ~load_quiet:(Time_float.Span.of_sec 0.05) opencode
  in
  let composer =
    Composer_runner.create
      ~sw
      ~adapter
      ~checkpoint:(fun _ ->
        incr checkpoint_calls;
        Tool.Result.ok "{}")
      ~spawn:(fun _ ->
        Ok
          (Line_transport.Peer.spawn ~sw (fun p ->
             P.initialize p;
             P.new_session p "ses_1";
             let req = P.expect p "session/prompt" in
             P.text p "ses_1" "No checkpoint needed.";
             P.result p req {|{"stopReason":"end_turn"}|};
             ignore (P.read p : Json.t option))))
      ()
  in
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
  Agent.set_engine agent { engine = Composer; adapter = Some (Acp "opencode") };
  Agent.subscribe agent ~f:print_event;
  prompt agent "hi";
  Composer_runner.close composer;
  printf "checkpoint_calls=%d\n" !checkpoint_calls;
  [%expect
    {|
    agent got initialize
    agent got session/new session=- mcp=[]
    agent got session/prompt "hi"
    assistant (acp:opencode/default, End_turn): "No checkpoint needed."
    checkpoint_calls=0
    |}]
;;

let%expect_test
    "ACP prewarm applies initial mode, model and thinking before first prompt"
  =
  let prewarmed, mark_prewarmed = Eio.Promise.create () in
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let adapter =
    Acp_adapter.create ~load_quiet:(Time_float.Span.of_sec 0.05) opencode
  in
  let composer =
    Composer_runner.create
      ~sw
      ~adapter
      ~on_prewarm:(fun _ _ ->
        ignore (Eio.Promise.try_resolve mark_prewarmed () : bool))
      ~spawn:(fun _ ->
        Ok
          (Line_transport.Peer.spawn ~sw (fun p ->
             P.initialize p;
             let ns = P.expect p "session/new" in
             P.result
               p
               ns
               (sprintf
                  {|{"sessionId":"ses_1",%s,
                     "models":{"currentModelId":"gemini-flash","availableModels":[
                       {"modelId":"gemini-flash","name":"Gemini Flash"},
                       {"modelId":"sonnet","name":"Sonnet"}]},
                     "configOptions":[{"id":"effort","name":"Effort","category":"thought_level",
                       "type":"select","currentValue":"medium","options":[
                         {"value":"low","name":"Low"},{"value":"medium","name":"Medium"},
                         {"value":"high","name":"High"}]}]}|}
                  P.modes);
             let m = P.expect p "session/set_mode" in
             P.result p m "{}";
             let md = P.expect p "session/set_model" in
             P.result p md "{}";
             let th = P.expect p "session/set_config_option" in
             P.result p th "{}";
             let req = P.expect p "session/prompt" in
             P.text p "ses_1" "Zero extra roundtrips.";
             P.result p req {|{"stopReason":"end_turn"}|};
             ignore (P.read p : Json.t option))))
      ()
  in
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
  Agent.set_engine agent { engine = Composer; adapter = Some (Acp "opencode") };
  Agent.set_mode agent ~plan:true;
  Agent.set_model
    agent
    (External_model.model
       ~adapter:"acp:opencode"
       ~name:"Sonnet"
       ~thinking_levels:[ Low; Medium; High ]
       ~can_disable_thinking:false
       "sonnet");
  Or_error.ok_exn (Agent.set_thinking agent (On (Some High)));
  Agent.subscribe agent ~f:print_event;
  print_endline "--- prewarm ---";
  Agent.prewarm agent;
  Eio.Promise.await prewarmed;
  print_endline "--- first prompt ---";
  prompt agent "go";
  Composer_runner.close composer;
  [%expect
    {|
    --- prewarm ---
    agent got initialize
    agent got session/new session=- mcp=[]
    agent got session/set_mode plan
    agent got session/set_model sonnet
    agent got session/set_config_option effort=high
    --- first prompt ---
    agent got session/prompt "go"
    assistant (acp:opencode/sonnet, End_turn): "Zero extra roundtrips."
    |}]
;;

let%expect_test
    "ACP session/load quiet-wait finishes promptly without overshoot"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let quiet = 0.06 in
  let adapter =
    Acp_adapter.create ~load_quiet:(Time_float.Span.of_sec quiet) opencode
  in
  let load_started = ref None in
  let composer =
    Composer_runner.create
      ~sw
      ~adapter
      ~start:(fun ~cwd ->
        { (Adapter.Start.default ~cwd) with resume = Some "ses_prev" })
      ~spawn:(fun _ ->
        Ok
          (Line_transport.Peer.spawn ~sw (fun p ->
             P.initialize p;
             ignore (P.expect p "session/load" : Json.t);
             load_started := Some (Time_float.now ());
             (* Replay a chunk 20ms in, then go quiet and never answer session/load. *)
             Eio_unix.sleep 0.02;
             P.text p "ses_prev" "old replay";
             let req = P.expect p "session/prompt" in
             P.text p "ses_prev" "Resumed.";
             P.result p req {|{"stopReason":"end_turn"}|};
             ignore (P.read p : Json.t option))))
      ()
  in
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
  Agent.set_engine agent { engine = Composer; adapter = Some (Acp "opencode") };
  Agent.subscribe agent ~f:print_event;
  prompt agent "continue";
  let elapsed_ms =
    Time_float.Span.to_ms
      (Time_float.diff (Time_float.now ()) (Option.value_exn !load_started))
  in
  Composer_runner.close composer;
  (* With 20ms replay + 60ms quiet, ~80ms total (well under 140ms, vs >120ms+
     full-quiet overshoot on the old loop). *)
  printf "no_overshoot=%b\n" Float.(elapsed_ms < 150.);
  [%expect
    {|
    agent got initialize
    agent got session/load session=ses_prev mcp=[]
    agent got session/prompt "continue"
    assistant (acp:opencode/default, End_turn): "Resumed."
    no_overshoot=true
    |}]
;;

let%expect_test
    "ACP session_info_update sets native title in-turn and between turns"
  =
  let send_between, trigger_between = Eio.Promise.create () in
  let between_sent, mark_between_sent = Eio.Promise.create () in
  let send_after_manual, trigger_after_manual = Eio.Promise.create () in
  let after_manual_sent, mark_after_manual_sent = Eio.Promise.create () in
  with_acp ~peer:(fun _ p ->
    P.initialize p;
    P.new_session p "ses_1";
    let req = P.expect p "session/prompt" in
    P.update
      p
      "ses_1"
      {|{"sessionUpdate":"session_info_update","title":"ACP in-turn title"}|};
    P.text p "ses_1" "Hello.";
    P.result p req {|{"stopReason":"end_turn"}|};
    Eio.Promise.await send_between;
    P.update
      p
      "ses_1"
      {|{"sessionUpdate":"session_info_update","title":"ACP refined title"}|};
    Eio.Promise.resolve mark_between_sent ();
    Eio.Promise.await send_after_manual;
    P.update
      p
      "ses_1"
      {|{"sessionUpdate":"session_info_update","title":"Ignored after manual"}|};
    Eio.Promise.resolve mark_after_manual_sent ();
    ignore (P.read p : Json.t option))
  @@ fun agent _ ->
  let state_cond = Eio.Condition.create () in
  Agent.subscribe agent ~f:(function
    | State_changed _ -> Eio.Condition.broadcast state_cond
    | _ -> ());
  let show label =
    let s = Agent.session agent in
    printf
      "%s: name=%s desc=%s native=%b wanted=%b\n"
      label
      (Option.value (Session.name s) ~default:"-")
      (Option.value (Session.description s) ~default:"-")
      (Session_description.has_native s)
      (Session_description.wanted s)
  in
  prompt agent "hi";
  show "in-turn";
  Eio.Promise.resolve trigger_between ();
  Eio.Promise.await between_sent;
  while
    not
      (Option.equal
         String.equal
         (Session.description (Agent.session agent))
         (Some "ACP refined title"))
  do
    Eio.Condition.await_no_mutex state_cond
  done;
  show "between-turns";
  ignore
    (Session.set_name (Agent.session agent) ~name:"Manual title"
     : Session.Entry.t);
  Eio.Promise.resolve trigger_after_manual ();
  Eio.Promise.await after_manual_sent;
  Eio.Fiber.yield ();
  Eio.Fiber.yield ();
  show "after-manual";
  [%expect
    {|
    spawn #1: opencode acp
    agent got initialize
    agent got session/new session=- mcp=[]
    agent got session/prompt "hi"
    assistant (acp:opencode/default, End_turn): "Hello."
    in-turn: name=- desc=ACP in-turn title native=true wanted=false
    between-turns: name=- desc=ACP refined title native=true wanted=false
    after-manual: name=Manual title desc=ACP refined title native=true wanted=false
    |}]
;;

let%expect_test
    "failed injected MCP tool substitutes error text when ACP agent omits \
     output"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let injected_ref = ref None in
  let mcp_reply_ref = ref "" in
  let adapter =
    Acp_adapter.create ~load_quiet:(Time_float.Span.of_sec 0.2) antigravity
  in
  let composer =
    Composer_runner.create
      ~sw
      ~adapter
      ~spawn:(fun _ ->
        let inner =
          Line_transport.Peer.spawn ~sw (fun p ->
            P.initialize p;
            P.new_session p "ses_mcp";
            let req = P.expect p "session/prompt" in
            P.update
              p
              "ses_mcp"
              {|{"sessionUpdate":"tool_call","toolCallId":"m1","title":"Running mcp_prigh_subagent_continue","kind":"other","status":"in_progress","rawInput":{"id":"a9"}}|};
            (* Agent calls prigh's MCP server; prigh returns isError:true with
               the real error text. *)
            let injected = Option.value_exn !injected_ref in
            mcp_reply_ref
            := Injected_tools.handle_line
                 injected
                 {|{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"subagent_continue","arguments":{"id":"a9"}}}|}
               |> Option.value_exn;
            (* Agent drops the MCP error text in its ACP tool_call_update. *)
            P.update
              p
              "ses_mcp"
              {|{"sessionUpdate":"tool_call_update","toolCallId":"m1","status":"failed"}|};
            P.text p "ses_mcp" "Done.";
            P.result p req {|{"stopReason":"end_turn"}|};
            ignore (P.read p : Json.t option))
        in
        let mcp : Line_transport.Mcp.t =
          { bridge =
              Injected_tools.Bridge.create
                ~prigh:"prigh"
                ~socket:"/tmp/test.sock"
                ~token:"tok"
          ; serve = (fun injected -> injected_ref := Some injected)
          ; route = None
          }
        in
        Ok
          (Line_transport.create
             ~read_line:(fun () -> Line_transport.read_line inner)
             ~write_line:(Line_transport.write_line inner)
             ~stop:(fun () -> Line_transport.stop inner)
             ~ended:(fun () -> Line_transport.ended inner)
             ~is_ended:(fun () -> Line_transport.is_ended inner)
             ~mcp
             ()))
      ()
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [])
      ~tools:(Tools.interaction @ Tool_subagent.control_tools)
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~runner:(Composer_runner.runner composer)
      ~cwd:t.dir
      ()
  in
  Agent.set_engine
    agent
    { engine = Composer; adapter = Some (Acp "antigravity") };
  Agent.set_mode agent ~approval:Auto;
  Agent.subscribe agent ~f:print_event;
  prompt agent "continue a9";
  printf "mcp reply: %s\n" !mcp_reply_ref;
  Composer_runner.close composer;
  [%expect
    {|
    agent got initialize
    agent got session/new session=- mcp=[prigh]
    agent got session/prompt "continue a9"
    assistant (acp:antigravity/default, Tool_use): ""
    tool_start Running mcp_prigh_subagent_continue
    tool_end Running mcp_prigh_subagent_continue -> ERROR "invalid arguments: unknown subagent \"a9\": only a finished, failed, or out-of-turns subagent of yours can be continued (those that can: none)"
    assistant (acp:antigravity/default, End_turn): "Done."
    mcp reply: {"jsonrpc":"2.0","id":1,"result":{"content":[{"type":"text","text":"invalid arguments: unknown subagent \"a9\": only a finished, failed, or out-of-turns subagent of yours can be continued (those that can: none)"}],"isError":true}}
    |}]
;;

let%expect_test
    "backend-hosted antigravity: launch wrapper, system_prompt_append, all \
     tools, host/cwd stability"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let adapter =
    Acp_adapter.create
      ~backend_hosted:true
      ~load_quiet:(Time_float.Span.of_sec 0.2)
      antigravity
  in
  let (module A) = adapter in
  printf
    "id=%s display=%s all_tools=%b on_backend=%b plan_mode=%b\n"
    A.id
    A.display_name
    A.all_tools
    A.on_backend
    A.capabilities.plan_mode;
  let own_launch = A.launch (Adapter.Start.default ~cwd:"/empty") in
  printf
    "own launch uses backend_wrapper=%b secret=%b\n"
    (List.mem
       own_launch.argv
       Antigravity_login.For_testing.backend_wrapper
       ~equal:String.equal)
    (Option.is_some own_launch.secret);
  let login = antigravity_login ~expires_min:[ 60 ] () in
  let login_launch =
    A.launch { (Adapter.Start.default ~cwd:"/empty") with login = Some login }
  in
  printf
    "login launch uses backend_wrapper=%b secret=%b\n"
    (List.mem
       login_launch.argv
       Antigravity_login.For_testing.backend_wrapper
       ~equal:String.equal)
    (Option.is_some login_launch.secret);
  let spawns = ref 0 in
  let host = ref "mac" in
  let injected_ref = ref None in
  let fake_bash =
    { Tool.spec = (Tools.find "bash" |> Option.value_exn).spec
    ; run =
        (fun ctx args ->
          Tool_result.ok
            (sprintf "ran on %s in %s: %s" !host ctx.cwd (Json.to_string args)))
    }
  in
  let composer =
    Composer_runner.create
      ~sw
      ~adapter
      ~start:(fun ~cwd ->
        { (Adapter.Start.default ~cwd) with
          developer_instructions = Some "SYSTEM PROMPT FOR MAC"
        })
      ~host:(fun () -> !host)
      ~spawn:(fun _launch ->
        incr spawns;
        printf "spawn #%d\n" !spawns;
        let inner =
          Line_transport.Peer.spawn ~sw (fun p ->
            P.initialize
              ~caps:
                {|{"loadSession":true,"sessionCapabilities":{"close":{}},"_meta":{"antigravity":{"systemPrompt":{"append":true}}}}|}
              p;
            let append_of req =
              Option.bind (Json.member "params" req) ~f:(fun params ->
                Option.bind (Json.member "_meta" params) ~f:(fun m ->
                  Option.bind (Json.member "antigravity" m) ~f:(fun a ->
                    Option.bind (Json.member "systemPrompt" a) ~f:(fun s ->
                      Option.bind (Json.member "append" s) ~f:Json.string))))
            in
            let ns = P.expect p "session/new" in
            printf
              "session/new systemPrompt.append=%s\n"
              (Option.value (append_of ns) ~default:"-");
            P.result p ns (sprintf {|{"sessionId":"ses_be",%s}|} P.modes);
            let ls = P.expect p "session/load" in
            printf
              "session/load systemPrompt.append=%s\n"
              (Option.value (append_of ls) ~default:"-");
            P.result p ls {|{}|};
            let rec turns n =
              match P.next p with
              | Some ("session/prompt", req) ->
                if n = 1
                then (
                  P.update
                    p
                    "ses_be"
                    {|{"sessionUpdate":"tool_call","toolCallId":"b1","kind":"other","title":"mcp_prigh_79ce654a_bash","status":"pending","rawInput":{"command":"pwd"}}|};
                  let injected = Option.value_exn !injected_ref in
                  let reply =
                    Injected_tools.handle_line
                      injected
                      {|{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"bash","arguments":{"command":"pwd"}}}|}
                    |> Option.value_exn
                  in
                  printf "mcp bash -> %s\n" (mask t reply);
                  P.update
                    p
                    "ses_be"
                    {|{"sessionUpdate":"tool_call_update","toolCallId":"b1","status":"completed"}|});
                P.text p "ses_be" (sprintf "reply %d" n);
                P.result p req {|{"stopReason":"end_turn"}|};
                turns (n + 1)
              | _ -> ()
            in
            turns 1)
        in
        let mcp : Line_transport.Mcp.t =
          { bridge =
              Injected_tools.Bridge.create
                ~prigh:"prigh"
                ~socket:"/tmp/test.sock"
                ~token:"tok"
          ; serve = (fun injected -> injected_ref := Some injected)
          ; route = None
          }
        in
        Ok
          (Line_transport.create
             ~read_line:(fun () -> Line_transport.read_line inner)
             ~write_line:(Line_transport.write_line inner)
             ~stop:(fun () -> Line_transport.stop inner)
             ~ended:(fun () -> Line_transport.ended inner)
             ~is_ended:(fun () -> Line_transport.is_ended inner)
             ~mcp
             ()))
      ~interrupt_grace:(Time_float.Span.of_sec 0.5)
      ()
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [])
      ~tools:(fake_bash :: Tools.interaction)
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~runner:(Composer_runner.runner composer)
      ~cwd:t.dir
      ()
  in
  Agent.set_engine
    agent
    { engine = Composer; adapter = Some (Backend (Acp "antigravity")) };
  Agent.set_mode agent ~approval:Auto;
  Agent.subscribe agent ~f:(function
    | Loop (Tool_end { call; result }) ->
      print_event
        (Loop
           (Tool_end
              { call; result = { result with text = mask t result.text } }))
    | e -> print_event e);
  prompt agent "first on mac";
  (* Switch tool host and plan mode: backend process stays alive, no ACP set_mode. *)
  host := "linux";
  Agent.set_mode agent ~plan:true;
  prompt agent "second on linux";
  Composer_runner.close composer;
  printf "total spawns=%d\n" !spawns;
  [%expect
    {|
    id=backend:acp:antigravity display=Antigravity (bridge) all_tools=true on_backend=true plan_mode=true
    own launch uses backend_wrapper=true secret=false
    prigh fetches a token valid for 40 min
    login launch uses backend_wrapper=true secret=true
    spawn #1
    agent got initialize
    agent got session/new session=- mcp=[prigh]
    session/new systemPrompt.append=SYSTEM PROMPT FOR MAC
    agent got session/load session=ses_be mcp=[prigh]
    session/load systemPrompt.append=SYSTEM PROMPT FOR MAC
    agent got session/prompt "first on mac"
    mcp bash -> {"jsonrpc":"2.0","id":1,"result":{"content":[{"type":"text","text":"ran on mac in $DIR: {\"command\":\"pwd\"}"}],"isError":false}}
    assistant (backend:acp:antigravity/default, Tool_use): ""
    tool_start bash
    tool_end bash -> "ran on mac in $DIR: {\"command\":\"pwd\"}"
    assistant (backend:acp:antigravity/default, End_turn): "reply 1"
    agent got session/prompt "second on linux"
    assistant (backend:acp:antigravity/default, End_turn): "reply 2"
    total spawns=1
    |}]
;;
