open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf

(* Subagents across engines and tool hosts: engine/model resolution,
   explicit user opt-in approval guards, harness <-> composer 2x2 matrix,
   cross-host composer subagents, fork handoff, continuation/steering, and
   Subagent_log persistence. *)

let laptop t =
  { Agent.Host.id = "host-laptop"
  ; name = "laptop"
  ; cwd = Filename.concat t.dir "laptop"
  ; session_id = None
  ; session_name = None
  ; home = Some (Filename.concat t.dir "laptop")
  ; exec_input = true
  ; git_location = false
  ; computer = None
  }
;;

let call ?(id = "s") name args = Reply.tool_call ~id ~name ~arguments:args ()

let%expect_test "Engine_choice.names and Engines.resolve" =
  let deployed =
    Or_error.ok_exn
      (Engines.create
         ~engines:[ Harness; Composer ]
         ~adapters:[ Codex; Claude; Acp "antigravity"; Acp "gemini" ]
         ())
  in
  List.iter (Engines.choices deployed) ~f:(fun c ->
    printf
      "%s (%s): %s\n"
      (Engine_choice.key c)
      (Engine_choice.display_name c)
      (String.concat ~sep:", " (Engine_choice.names c)));
  let resolve ?(t = deployed) q =
    match Engines.resolve t q with
    | Ok c -> printf "resolve %S -> %s\n" q (Engine_choice.key c)
    | Error e -> printf "resolve %S -> ERROR %s\n" q (Error.to_string_hum e)
  in
  resolve "harness";
  resolve "prigh harness";
  resolve "codex";
  resolve "Claude Code";
  resolve "cla";
  resolve "antigravity";
  resolve "acp:antigravity";
  resolve "agy";
  resolve "gemini";
  resolve "c";
  resolve "composer";
  resolve "cursor";
  resolve ~t:Engines.default "codex";
  let single_composer =
    Or_error.ok_exn
      (Engines.create ~engines:[ Harness; Composer ] ~adapters:[ Codex ] ())
  in
  resolve ~t:single_composer "composer";
  [%expect
    {|
    harness (prigh harness): harness, prigh harness
    codex (Codex): codex
    claude (Claude Code): claude, claude code
    acp:antigravity (Antigravity): agy, antigravity, acp:antigravity, Antigravity
    acp:gemini (Gemini CLI): gemini, acp:gemini, Gemini CLI
    resolve "harness" -> harness
    resolve "prigh harness" -> harness
    resolve "codex" -> codex
    resolve "Claude Code" -> claude
    resolve "cla" -> claude
    resolve "antigravity" -> acp:antigravity
    resolve "acp:antigravity" -> acp:antigravity
    resolve "agy" -> acp:antigravity
    resolve "gemini" -> acp:gemini
    resolve "c" -> ERROR "c" matches several engines (codex, claude): name one of them
    resolve "composer" -> ERROR composer needs an adapter: this backend drives codex, claude, acp:antigravity, acp:gemini
    resolve "cursor" -> ERROR no engine "cursor" here: this backend runs harness, codex, claude, acp:antigravity, acp:gemini
    resolve "codex" -> ERROR no engine "codex" here: this backend runs harness
    resolve "composer" -> codex
    |}]
;;

let%expect_test "External_model.resolve" =
  let models =
    [ External_model.model ~adapter:"codex" ~name:"GPT-5.4" "gpt-5.4"
    ; External_model.model ~adapter:"codex" ~name:"GPT-5.4 Mini" "gpt-5.4-mini"
    ; External_model.model
        ~adapter:"codex"
        ~name:"Claude Sonnet 4.6"
        "claude-sonnet-4-6-20260101"
    ]
  in
  let check ?models q =
    match External_model.resolve ~adapter:"codex" ~models q with
    | Ok id -> printf "%S -> %s\n" q id
    | Error e -> printf "%S -> ERROR %s\n" q (Error.to_string_hum e)
  in
  (* Without a real catalog, the trimmed query passes through. *)
  check "gpt-5.4";
  check "codex/gpt-5.4";
  check ~models:[ External_model.pending ~adapter:"codex" ] "codex/o3";
  (* With a real catalog, matches id, adapter/id, name, same_model, prefix. *)
  check ~models "gpt-5.4";
  check ~models "codex/gpt-5.4-mini";
  check ~models "GPT-5.4 Mini";
  check ~models "sonnet-4-6";
  check ~models "gpt-5.4-m";
  check ~models "gpt-5";
  check ~models "opus";
  [%expect
    {|
    "gpt-5.4" -> gpt-5.4
    "codex/gpt-5.4" -> gpt-5.4
    "codex/o3" -> o3
    "gpt-5.4" -> gpt-5.4
    "codex/gpt-5.4-mini" -> gpt-5.4-mini
    "GPT-5.4 Mini" -> gpt-5.4-mini
    "sonnet-4-6" -> claude-sonnet-4-6-20260101
    "gpt-5.4-m" -> gpt-5.4-mini
    "gpt-5" -> ERROR model "gpt-5" is ambiguous on codex (gpt-5.4, gpt-5.4-mini): name one of them
    "opus" -> ERROR unknown model "opus" for codex (available: gpt-5.4 (GPT-5.4), gpt-5.4-mini (GPT-5.4 Mini), claude-sonnet-4-6-20260101 (Claude Sonnet 4.6))
    |}]
;;

(* A minimal in-process Codex app-server peer that logs thread/turn requests
   and replies with [reply_text]. *)
let fake_codex_peer
      ?(thread_id = "thr-codex-1")
      ?(reply_text = "codex report")
      ?(log_prompt = false)
      (p : Line_transport.Peer.t)
  =
  let read () =
    Option.map (Line_transport.Peer.read_line p) ~f:(fun line ->
      Json.parse line |> Or_error.ok_exn)
  in
  let reply id result =
    Line_transport.Peer.write_line
      p
      (Json.to_string (`Object [ "id", id; "result", result ]))
  in
  let rec loop () =
    match read () with
    | None -> ()
    | Some j ->
      let id = Json.member "id" j in
      let params = Option.value (Json.member "params" j) ~default:`Null in
      (match Json.member "method" j, id with
       | Some (`String "initialize"), Some id ->
         reply id (`Object [ "userAgent", `String "codex/0.166.0" ])
       | Some (`String (("thread/start" | "thread/resume") as m)), Some id ->
         let tid =
           match Json.member "threadId" params with
           | Some (`String s) -> s
           | _ -> thread_id
         in
         printf "  [codex] %s thread=%s\n" m tid;
         reply
           id
           (`Object
               [ "thread", `Object [ "id", `String tid ]
               ; "model", `String "gpt-5.4"
               ])
       | Some (`String "turn/start"), Some id ->
         let model =
           Option.bind (Json.member "model" params) ~f:Json.string
           |> Option.value ~default:"<default>"
         in
         let effort =
           Option.bind (Json.member "effort" params) ~f:Json.string
           |> Option.value ~default:"<default>"
         in
         let text =
           match Json.member "input" params with
           | Some (`Array (`Object fields :: _)) ->
             List.Assoc.find fields "text" ~equal:String.equal
             |> Option.bind ~f:Json.string
             |> Option.value ~default:""
           | _ -> ""
         in
         printf "  [codex] turn/start model=%s effort=%s" model effort;
         if log_prompt
         then
           printf
             " has_handoff=%b"
             (String.is_substring text ~substring:"Context handoff");
         printf "\n";
         reply
           id
           (`Object
               [ ( "turn"
                 , `Object
                     [ "id", `String "tu-1"; "status", `String "inProgress" ] )
               ]);
         List.iter
           ~f:(Line_transport.Peer.write_line p)
           [ sprintf
               {|{"method":"turn/started","params":{"threadId":%S,"turn":{"id":"tu-1","items":[],"status":"inProgress"}}}|}
               thread_id
           ; sprintf
               {|{"method":"item/completed","params":{"threadId":%S,"turnId":"tu-1","item":{"type":"agentMessage","id":"m1","text":%S}}}|}
               thread_id
               reply_text
           ; sprintf
               {|{"method":"turn/completed","params":{"threadId":%S,"turn":{"id":"tu-1","items":[],"status":"completed"}}}|}
               thread_id
           ]
       | Some _, Some id -> reply id (`Object [])
       | _ -> ());
      loop ()
  in
  loop ()
;;

let deployed_engines =
  Or_error.ok_exn
    (Engines.create
       ~engines:[ Harness; Composer ]
       ~adapters:[ Codex; Claude ]
       ())
;;

let with_harness_and_engines
      ?(mode = Approval_mode.Auto)
      ?(answer = fun (_ : string) -> Agent.Confirm_answer.Allow)
      ?(codex_models =
        [ External_model.model ~adapter:"codex" ~name:"GPT-5.4" "gpt-5.4"
        ; External_model.model ~adapter:"codex" ~name:"GPT-5" "gpt-5"
        ])
      ?(peer = fun ~host:_ ~adapter:_ _ p -> fake_codex_peer p)
      ~main
      children
      f
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  Core_unix.mkdir_p (Filename.concat t.dir "laptop");
  let agent_ref = ref None in
  let hosts () = Option.value_map !agent_ref ~default:[] ~f:Agent.tool_hosts in
  let provider = Routed_provider.create ~main children in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:
        (Tools.all
         @ (Tool_subagent.create
              ~hosts
              ~engines:(fun () ->
                Option.bind !agent_ref ~f:Agent.subagent_engines)
              ~provider
              ~current_model:(fun () -> Model.default)
              ~current_thinking:(fun () -> Off)
              ~home:t.dir
              ()
            :: Tool_subagent.control_tools)
         @ [ Tool_on_host.create ~hosts () ])
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:t.dir
      ()
  in
  agent_ref := Some agent;
  let ext =
    Composer_subagent.create
      ~engines:deployed_engines
      ~current:(fun () -> Agent.engine_choice agent)
      ~harness_model:(fun () -> Agent.harness_model agent)
      ~external_models:(fun ~host:_ adapter ->
        match adapter with
        | Codex -> Some codex_models
        | _ -> None)
      ~parent_messages:(fun () -> Agent.messages agent)
      ~approval_mode:(fun () -> (Agent.state agent).approval_mode)
      ~plan_mode:(fun () -> (Agent.state agent).plan_mode)
      ~add_spend:(Agent.add_spend agent)
      ~notice:(Agent.notice agent)
      ~spawn:(fun ~sw ~host ~host_name ~adapter (launch : Adapter.Launch.t) ->
        printf
          "spawn %s on %s (%s) in %s\n"
          (Adapter_id.to_string adapter)
          host_name
          host
          (mask t launch.cwd);
        Ok (Line_transport.Peer.spawn ~sw (peer ~host ~adapter launch)))
      ()
  in
  Agent.set_subagent_engines agent (Some ext);
  Agent.set_mode agent ~approval:mode;
  Agent.set_hosts agent [ laptop t ];
  Agent.subscribe agent ~f:(function
    | Loop (Tool_confirm { call_id; agent_id; name; summary; _ }) ->
      printf "confirm %s: %s\n" name (mask t summary);
      Or_error.ok_exn
        (Agent.respond_confirm ?agent_id agent ~call_id (answer summary))
    | Loop (Subagent_start { agent_id; model; host; engine; continued; _ }) ->
      printf
        "subagent %s %s engine=%s host=%s model=%s\n"
        agent_id
        (if continued then "continues" else "starts")
        (Option.value engine ~default:"<same>")
        (Option.value host ~default:"<same>")
        model
    | Loop (Subagent_end { agent_id; result; _ }) ->
      printf "subagent %s ends: %s\n" agent_id (mask t result.text)
    | Loop (Tool_end { call; result }) ->
      printf
        "%s -> %s%s\n"
        call.name
        (if result.is_error then "ERROR " else "")
        (mask t (String.strip result.text))
    | _ -> ());
  f t agent
;;

let run ~prompt agent =
  Or_error.ok_exn (Agent.prompt agent prompt);
  Agent.wait_idle agent
;;

let%expect_test "harness -> Codex subagent on another host with model/thinking" =
  with_harness_and_engines
    ~main:
      [ call
          "subagent"
          {|{"task":"audit parser","engine":"codex","host":"laptop","model":"gpt-5","thinking":"high"}|}
      ; Reply.text "started"
      ; Reply.text "done"
      ]
    []
  @@ fun _t agent ->
  run agent ~prompt:"please run a codex subagent on laptop";
  [%expect
    {|
    subagent a1 starts engine=codex host=laptop model=codex/gpt-5
    spawn codex on laptop (host-laptop) in $DIR/laptop
    subagent -> started agent a1 on Codex, laptop (audit parser); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
      [codex] thread/start thread=thr-codex-1
      [codex] turn/start model=gpt-5 effort=high
    subagent a1 ends: codex report
    [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
    |}]
;;

let%expect_test
    "Codex subagent without model uses Codex default, never harness model"
  =
  with_harness_and_engines
    ~main:
      [ call "subagent" {|{"task":"quick check","engine":"codex"}|}
      ; Reply.text "started"
      ; Reply.text "done"
      ]
    []
  @@ fun _t agent ->
  run agent ~prompt:"delegate this to codex";
  [%expect
    {|
    subagent a1 starts engine=codex host=<same> model=codex/gpt-5.4
    spawn codex on <host> (backend) in $DIR
    subagent -> started agent a1 on Codex (quick check); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
      [codex] thread/start thread=thr-codex-1
      [codex] turn/start model=<default> effort=<default>
    subagent a1 ends: codex report
    [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
    |}]
;;

let%expect_test "unrequested engine switch asks even in Auto mode; denied fails"
  =
  with_harness_and_engines
    ~answer:(fun _ -> Deny None)
    ~main:
      [ call "subagent" {|{"task":"audit","engine":"codex"}|}; Reply.text "ok" ]
    []
  @@ fun _t agent ->
  run agent ~prompt:"audit the codebase";
  [%expect
    {|
    confirm subagent: "audit" (runs on Codex, which you haven't asked for in this session)
    subagent -> ERROR [denied by user]
    |}]
;;

let%expect_test "unrequested host AND engine asks with both reasons" =
  with_harness_and_engines
    ~answer:(fun _ -> Deny None)
    ~main:
      [ call "subagent" {|{"task":"audit","engine":"codex","host":"laptop"}|}
      ; Reply.text "ok"
      ]
    []
  @@ fun _t agent ->
  run agent ~prompt:"audit the codebase";
  [%expect
    {|
    confirm on_host: on laptop: subagent (runs on laptop, which you haven't named in this session; runs on Codex, which you haven't asked for in this session)
    subagent -> ERROR [denied by user]
    |}]
;;

let%expect_test "allowing engine permits it until forget_allowed_calls" =
  with_harness_and_engines
    ~main:
      [ call "subagent" {|{"task":"first","engine":"codex"}|}
      ; Reply.text "s1"
      ; Reply.text "r1"
      ; call "subagent" {|{"task":"second","engine":"codex"}|}
      ; Reply.text "s2"
      ; Reply.text "r2"
      ; call "subagent" {|{"task":"third","engine":"codex"}|}
      ; Reply.text "s3"
      ; Reply.text "r3"
      ]
    []
  @@ fun _t agent ->
  run agent ~prompt:"audit part 1";
  (* Second call needs no confirmation. *)
  run agent ~prompt:"audit part 2";
  (* Forgetting permissions requires confirmation again. *)
  Or_error.ok_exn (Agent.forget_allowed_calls agent None);
  run agent ~prompt:"audit part 3";
  [%expect
    {|
    confirm subagent: "first" (runs on Codex, which you haven't asked for in this session)
    subagent a1 starts engine=codex host=<same> model=codex/gpt-5.4
    spawn codex on <host> (backend) in $DIR
    subagent -> started agent a1 on Codex (first); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
      [codex] thread/start thread=thr-codex-1
      [codex] turn/start model=<default> effort=<default>
    subagent a1 ends: codex report
    [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
    subagent a2 starts engine=codex host=<same> model=codex/gpt-5.4
    spawn codex on <host> (backend) in $DIR
    subagent -> started agent a2 on Codex (second); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
      [codex] thread/start thread=thr-codex-1
      [codex] turn/start model=<default> effort=<default>
    subagent a2 ends: codex report
    [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
    confirm subagent: "third" (runs on Codex, which you haven't asked for in this session)
    subagent a3 starts engine=codex host=<same> model=codex/gpt-5.4
    spawn codex on <host> (backend) in $DIR
    subagent -> started agent a3 on Codex (third); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
      [codex] thread/start thread=thr-codex-1
      [codex] turn/start model=<default> effort=<default>
    subagent a3 ends: codex report
    [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
    |}]
;;

let%expect_test
    "fork: true across engines hands off context, and user continuation \
     resumes thread"
  =
  with_harness_and_engines
    ~peer:(fun ~host:_ ~adapter:_ _ p -> fake_codex_peer ~log_prompt:true p)
    ~main:
      [ call
          "subagent"
          {|{"task":"continue analysis","engine":"codex","fork":true}|}
      ; Reply.text "started"
      ; Reply.text "got first"
      ; Reply.text "got second"
      ]
    []
  @@ fun _t agent ->
  run agent ~prompt:"use codex to fork and analyze";
  (* User continues the finished Codex subagent from its own chat: resumes
     the recorded thread without re-sending the handoff. *)
  Or_error.ok_exn
    (Agent.steer_subagent
       agent
       ~agent_id:"a1"
       ~kind:Follow_up
       "check edge cases too");
  Agent.wait_idle agent;
  [%expect
    {|
    subagent a1 starts engine=codex host=<same> model=codex/gpt-5.4
    spawn codex on <host> (backend) in $DIR
    subagent -> started agent a1 on Codex (forked: continue analysis); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
      [codex] thread/start thread=thr-codex-1
      [codex] turn/start model=<default> effort=<default> has_handoff=true
    subagent a1 ends: codex report
    [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
    subagent a1 continues engine=codex host=<same> model=codex/gpt-5.4
    spawn codex on <host> (backend) in $DIR
      [codex] thread/resume thread=thr-codex-1
      [codex] turn/start model=<default> effort=<default> has_handoff=false
    subagent a1 ends: codex report
    [subagent: 2 turns, 0 in / 0 out tokens, $0.0000]
    |}]
;;

(* A fake Claude Code peer that calls prigh's MCP [subagent] tool in-band. *)
let claude_calling_subagent ~calls (p : Line_transport.Peer.t) =
  let read () =
    Option.map (Line_transport.Peer.read_line p) ~f:(fun line ->
      Json.parse line |> Or_error.ok_exn)
  in
  let req_id j =
    Option.bind (Json.member "request_id" j) ~f:Json.string
    |> Option.value ~default:""
  in
  (match read () with
   | Some init ->
     Line_transport.Peer.write_line
       p
       (sprintf
          {|{"type":"control_response","response":{"subtype":"success","request_id":%S,"response":{"claude_code_version":"2.1.268","models":[],"commands":[]}}}|}
          (req_id init))
   | None -> ());
  let turn = ref 0 in
  let rec loop () =
    match read () with
    | None -> ()
    | Some _prompt ->
      incr turn;
      (match List.nth calls (!turn - 1) with
       | Some (tool_name, args_json) ->
         Line_transport.Peer.write_line
           p
           (sprintf
              {|{"type":"control_request","request_id":"r-%d","request":{"subtype":"mcp_message","server_name":"prigh","message":{"jsonrpc":"2.0","id":%d,"method":"tools/call","params":{"name":%S,"arguments":%s}}}}|}
              !turn
              !turn
              tool_name
              args_json);
         (match read () with
          | Some resp ->
            let text =
              Json.member "response" resp
              |> Option.bind ~f:(Json.member "response")
              |> Option.bind ~f:(Json.member "mcp_response")
              |> Option.bind ~f:(Json.member "result")
              |> Option.bind ~f:(Json.member "content")
              |> Option.bind ~f:(function
                | `Array (`Object fs :: _) ->
                  List.Assoc.find fs "text" ~equal:String.equal
                  |> Option.bind ~f:Json.string
                | _ -> None)
              |> Option.value ~default:(Json.to_string resp)
            in
            printf "  [claude] mcp %s -> %s\n" tool_name text
          | None -> ());
         Line_transport.Peer.write_line
           p
           {|{"type":"result","subtype":"success","is_error":false,"result":"done","session_id":"s-claude","total_cost_usd":0}|}
       | None ->
         Line_transport.Peer.write_line
           p
           {|{"type":"result","subtype":"success","is_error":false,"result":"noted","session_id":"s-claude","total_cost_usd":0}|});
      loop ()
  in
  loop ()
;;

let%expect_test
    "Composer (Claude) -> Harness subagent via MCP, and bare 'prigh' does not \
     permit harness"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let agent_ref = ref None in
  let provider =
    Routed_provider.create
      ~main:[]
      [ "harness task", [ Reply.text "harness reply" ] ]
  in
  let composer =
    Composer_runner.create
      ~sw
      ~adapter:(module Claude_adapter)
      ~spawn:(fun _ ->
        Ok
          (Line_transport.Peer.spawn
             ~sw
             (claude_calling_subagent
                ~calls:
                  [ "subagent", {|{"task":"harness task","engine":"harness"}|}
                  ; "subagent", {|{"task":"harness task","engine":"harness"}|}
                  ])))
      ()
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:
        (Tools.all
         @ (Tool_subagent.create
              ~engines:(fun () ->
                Option.bind !agent_ref ~f:Agent.subagent_engines)
              ~provider
              ~current_model:(fun () -> Model.default)
              ~current_thinking:(fun () -> Off)
              ~home:t.dir
              ()
            :: Tool_subagent.control_tools))
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~runner:(Composer_runner.runner composer)
      ~cwd:t.dir
      ()
  in
  agent_ref := Some agent;
  Agent.set_engine agent { engine = Composer; adapter = Some Claude };
  Agent.set_mode agent ~approval:Auto;
  let ext =
    Composer_subagent.create
      ~engines:deployed_engines
      ~current:(fun () -> Agent.engine_choice agent)
      ~harness_model:(fun () -> Agent.harness_model agent)
      ~external_models:(fun ~host:_ _ -> None)
      ~parent_messages:(fun () -> Agent.messages agent)
      ~approval_mode:(fun () -> (Agent.state agent).approval_mode)
      ~plan_mode:(fun () -> (Agent.state agent).plan_mode)
      ~add_spend:(Agent.add_spend agent)
      ~notice:(Agent.notice agent)
      ~spawn:(fun ~sw:_ ~host:_ ~host_name:_ ~adapter:_ _ ->
        Or_error.error_string "unexpected external spawn")
      ()
  in
  Agent.set_subagent_engines agent (Some ext);
  Agent.subscribe agent ~f:(function
    | Loop (Tool_confirm { call_id; name; summary; _ }) ->
      printf "confirm %s: %s\n" name summary;
      Or_error.ok_exn (Agent.respond_confirm agent ~call_id (Deny None))
    | Loop (Subagent_start { agent_id; model; engine; _ }) ->
      printf
        "subagent %s starts engine=%s model=%s\n"
        agent_id
        (Option.value engine ~default:"<same>")
        model
    | Loop (Subagent_end { agent_id; result; _ }) ->
      printf "subagent %s ends: %s\n" agent_id result.text
    | _ -> ());
  (* Mentioning "prigh" alone does NOT authorize switching to the harness. *)
  run agent ~prompt:"fix this bug in prigh";
  (* Explicitly naming "harness" authorizes it without confirmation, and the
     subagent uses Agent.harness_model (deepseek-flash), not Claude's model. *)
  run agent ~prompt:"use the harness for this task";
  Composer_runner.close composer;
  [%expect
    {|
    confirm subagent: "harness task" (runs on prigh harness, which you haven't asked for in this session)
      [claude] mcp subagent -> [denied by user]
    subagent a1 starts engine=harness model=deepseek-flash
      [claude] mcp subagent -> started agent a1 on prigh harness (harness task); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    subagent a1 ends: harness reply
    [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    |}]
;;

let%expect_test
    "Subagent_log preserves engine across save/load and handles legacy logs"
  =
  with_sandbox
  @@ fun t ->
  let dir = Filename.concat t.dir "subs" in
  let log = Subagent_log.create () in
  let start ?engine agent_id task : Agent_event.t =
    Subagent_start
      { call_id = "c-" ^ agent_id
      ; agent_id
      ; task
      ; name = Subagent_name.of_task task
      ; model = "codex/gpt-5"
      ; thinking = Off
      ; tools = []
      ; host = Some "laptop"
      ; engine
      ; cwd = "/w"
      ; worktree = None
      ; forked = false
      ; continued = false
      }
  in
  Subagent_log.load log ~dir:(fun () -> Some dir);
  Subagent_log.record log ~now:1. (start ~engine:"codex" "a1" "codex task");
  (* Append a legacy log line with no "engine" field (as written by older
     versions) alongside it. *)
  let path = Filename.concat dir "subagents.jsonl" in
  let legacy =
    In_channel.read_lines path
    |> List.hd_exn
    |> String.substr_replace_all ~pattern:{|"engine":"codex",|} ~with_:""
    |> String.substr_replace_all ~pattern:{|"a1"|} ~with_:{|"legacy"|}
    |> String.substr_replace_all ~pattern:{|"c-a1"|} ~with_:{|"c-legacy"|}
    |> String.substr_replace_all ~pattern:{|"host":"laptop",|} ~with_:""
  in
  Out_channel.with_file ~append:true path ~f:(fun oc ->
    Out_channel.output_string oc (legacy ^ "\n"));
  let reloaded = Subagent_log.create () in
  Subagent_log.load reloaded ~dir:(fun () -> Some dir);
  List.iter (Subagent_log.summaries reloaded) ~f:(fun s ->
    printf
      "%s engine=%s host=%s\n"
      s.id
      (Option.value s.engine ~default:"<none>")
      (Option.value s.host ~default:"<none>"));
  (* Rpc_json emits "engine" iff Some. *)
  let show_rpc e =
    let j = Rpc_json.event (Loop e) in
    printf
      "rpc engine=%s\n"
      (Option.value_map
         (Json.member "engine" j)
         ~default:"<omitted>"
         ~f:Json.to_string)
  in
  show_rpc (start ~engine:"codex" "a1" "x");
  show_rpc (start "a2" "y");
  [%expect
    {|
    a1 engine=codex host=laptop
    legacy engine=<none> host=<none>
    rpc engine="codex"
    rpc engine=<omitted>
    |}]
;;
