open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf
module Reply = Faux_provider.Reply

(* --- Unit tests: fast-model picking and external model spec parsing --- *)

let%expect_test
    "External_complete.pick_fast_model prefers fast tiers and skips \
     placeholders"
  =
  let m adapter ?name id = External_model.model ~adapter ?name id in
  let check label models =
    printf
      "%s -> %s\n"
      label
      (Option.value
         (External_complete.pick_fast_model models)
         ~default:"<none>")
  in
  check "empty" [];
  check "placeholder only" [ External_model.pending ~adapter:"acp:antigravity" ];
  check
    "acp prefers flash-lite over flash over pro"
    [ External_model.pending ~adapter:"acp:antigravity"
    ; m "acp:antigravity" ~name:"Gemini 3 Pro" "gemini-3-pro"
    ; m "acp:antigravity" ~name:"Gemini 3 Flash" "gemini-3-flash"
    ; m "acp:antigravity" ~name:"Gemini 3 Flash Lite" "gemini-3-flash-lite"
    ];
  check
    "acp picks flash when no flash-lite"
    [ m "acp:antigravity" ~name:"Gemini 3 Pro" "gemini-3-pro"
    ; m "acp:antigravity" ~name:"Gemini 3 Flash" "gemini-3-flash"
    ];
  check
    "claude picks haiku"
    [ m "claude" ~name:"Opus" "claude-opus-4-7"
    ; m "claude" ~name:"Sonnet" "claude-sonnet-4-6"
    ; m "claude" ~name:"Haiku" "claude-haiku-4-5"
    ];
  check
    "codex picks mini"
    [ m "codex" ~name:"GPT-5.4" "gpt-5.4"
    ; m "codex" ~name:"GPT-5.4 Mini" "gpt-5.4-mini"
    ];
  check
    "no fast tier"
    [ m "codex" ~name:"GPT-5.4" "gpt-5.4"; m "codex" ~name:"o3" "o3" ];
  [%expect
    {|
    empty -> <none>
    placeholder only -> <none>
    acp prefers flash-lite over flash over pro -> gemini-3-flash-lite
    acp picks flash when no flash-lite -> gemini-3-flash
    claude picks haiku -> claude-haiku-4-5
    codex picks mini -> gpt-5.4-mini
    no fast tier -> <none>
    |}]
;;

let%expect_test
    "External_model.parse_spec handles external specs, errors, and direct \
     models"
  =
  List.iter
    [ "acp:antigravity/gemini-3-flash"
    ; "acp:antigravity"
    ; "codex/gpt-5-mini"
    ; "claude/haiku"
    ; "acp:antigravity/"
    ; "codex/"
    ; "acp:"
    ; "deepseek/deepseek-flash"
    ; "anthropic/claude-haiku-4-5"
    ; "claude"
    ; "codex"
    ]
    ~f:(fun s ->
      print_s
        [%message
          ""
            ~spec:s
            ~parsed:
              (External_model.parse_spec s
               : (Adapter_id.t * string) Or_error.t option)]);
  [%expect
    {|
    ((spec acp:antigravity/gemini-3-flash)
     (parsed ((Ok ((Acp antigravity) gemini-3-flash)))))
    ((spec acp:antigravity) (parsed ((Ok ((Acp antigravity) default)))))
    ((spec codex/gpt-5-mini) (parsed ((Ok (Codex gpt-5-mini)))))
    ((spec claude/haiku) (parsed ((Ok (Claude haiku)))))
    ((spec acp:antigravity/)
     (parsed ((Error "empty model name for acp:antigravity"))))
    ((spec codex/) (parsed ((Error "empty model name for codex"))))
    ((spec acp:)
     (parsed
      ((Error
        "unknown adapter \"acp:\": expected codex, claude, prigh or acp:<name>"))))
    ((spec deepseek/deepseek-flash) (parsed ()))
    ((spec anthropic/claude-haiku-4-5) (parsed ()))
    ((spec claude) (parsed ()))
    ((spec codex) (parsed ()))
    |}]
;;

(* --- Helpers for scripted in-memory ACP / Codex / Claude peers --- *)

let read_json p =
  match Line_transport.Peer.read_line p with
  | None -> None
  | Some line -> Some (Json.parse line |> Or_error.ok_exn)
;;

let str k j = Option.bind (Json.member k j) ~f:Json.string
let id_of j = Option.value (Json.member "id" j) ~default:`Null

let write_json p fields =
  Line_transport.Peer.write_line p (Json.to_string (`Object fields))
;;

let acp_antigravity () = Option.value_exn (Acp_agent.find_known "antigravity")

let%expect_test
    "ACP complete: tool-free, fast model, lowest thought, auto-deny, \
     session/close"
  =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let adapter = Acp_adapter.create (acp_antigravity ()) in
  let peer_eof = ref false in
  let peer (p : Line_transport.Peer.t) =
    let rec loop () =
      match read_json p with
      | None -> peer_eof := true
      | Some j ->
        (match str "method" j with
         | Some "initialize" ->
           printf "acp got initialize\n";
           write_json
             p
             [ "jsonrpc", `String "2.0"
             ; "id", id_of j
             ; ( "result"
               , Json.parse
                   {|{"protocolVersion":1,"agentCapabilities":{"sessionCapabilities":{"close":{}}}}|}
                 |> Or_error.ok_exn )
             ];
           loop ()
         | Some "session/new" ->
           let params = Option.value (Json.member "params" j) ~default:`Null in
           let mcp =
             match Json.member "mcpServers" params with
             | Some (`Array l) -> List.length l
             | _ -> -1
           in
           printf "acp got session/new mcp=%d\n" mcp;
           write_json
             p
             [ "jsonrpc", `String "2.0"
             ; "id", id_of j
             ; ( "result"
               , Json.parse
                   {|{
                     "sessionId":"ses_1",
                     "configOptions":[
                       {"id":"model","name":"Model","category":"model","type":"select","currentValue":"gemini-3-pro","options":[
                         {"value":"gemini-3-pro","name":"Gemini 3 Pro"},
                         {"value":"gemini-3-flash","name":"Gemini 3 Flash"},
                         {"value":"gemini-3-flash-lite","name":"Gemini 3 Flash Lite"}
                       ]},
                       {"id":"thought","name":"Thinking","category":"thought_level","type":"select","currentValue":"high","options":[
                         {"value":"off","name":"Off"},
                         {"value":"low","name":"Low"},
                         {"value":"high","name":"High"}
                       ]}
                     ]
                   }|}
                 |> Or_error.ok_exn )
             ];
           loop ()
         | Some "session/set_config_option" ->
           let params = Option.value (Json.member "params" j) ~default:`Null in
           printf
             "acp got set_config_option %s=%s\n"
             (Option.value (str "configId" params) ~default:"")
             (Option.value (str "value" params) ~default:"");
           write_json
             p
             [ "jsonrpc", `String "2.0"
             ; "id", id_of j
             ; "result", `Object [ "configOptions", `Array [] ]
             ];
           loop ()
         | Some "session/prompt" ->
           let params = Option.value (Json.member "params" j) ~default:`Null in
           let text =
             match Json.member "prompt" params with
             | Some (`Array (first :: _)) ->
               Option.value (str "text" first) ~default:""
             | _ -> ""
           in
           printf
             "acp got prompt has_no_tools=%b has_system=%b has_user=%b\n"
             (String.is_substring text ~substring:"Do not call any tools")
             (String.is_substring text ~substring:"Name the session.")
             (String.is_substring text ~substring:"Fix parser crash");
           (* Unexpected permission request must be auto-denied. *)
           write_json
             p
             [ "jsonrpc", `String "2.0"
             ; "id", `Number "99"
             ; "method", `String "session/request_permission"
             ; ( "params"
               , Json.parse
                   {|{"sessionId":"ses_1","toolCall":{"toolCallId":"c1","title":"ls"},"options":[{"optionId":"yes","name":"Allow","kind":"allow_once"},{"optionId":"no","name":"Reject","kind":"reject_once"}]}|}
                 |> Or_error.ok_exn )
             ];
           (match read_json p with
            | Some ans ->
              printf
                "acp permission reply: %s\n"
                (Json.to_string
                   (Option.value (Json.member "result" ans) ~default:`Null))
            | None -> ());
           write_json
             p
             [ "jsonrpc", `String "2.0"
             ; "method", `String "session/update"
             ; ( "params"
               , Json.parse
                   {|{"sessionId":"ses_1","update":{"sessionUpdate":"agent_message_chunk","content":{"type":"text","text":"Fix "}}}|}
                 |> Or_error.ok_exn )
             ];
           write_json
             p
             [ "jsonrpc", `String "2.0"
             ; "method", `String "session/update"
             ; ( "params"
               , Json.parse
                   {|{"sessionId":"ses_1","update":{"sessionUpdate":"agent_message_chunk","content":{"type":"text","text":"parser crash"}}}|}
                 |> Or_error.ok_exn )
             ];
           write_json
             p
             [ "jsonrpc", `String "2.0"
             ; "id", id_of j
             ; ( "result"
               , Json.parse
                   {|{"stopReason":"end_turn","usage":{"inputTokens":20,"outputTokens":5,"totalTokens":25}}|}
                 |> Or_error.ok_exn )
             ];
           loop ()
         | Some "session/close" ->
           printf "acp got session/close\n";
           write_json
             p
             [ "jsonrpc", `String "2.0"; "id", id_of j; "result", `Object [] ];
           loop ()
         | Some m ->
           printf "acp unexpected method %s\n" m;
           loop ()
         | None -> loop ())
    in
    loop ()
  in
  let deltas = Queue.create () in
  let result =
    External_complete.run
      ~adapter
      ~spawn:(fun launch ->
        printf "launch mcp=%b\n" launch.mcp;
        Ok (Line_transport.Peer.spawn ~sw peer))
      ~cancel:Cancellation.never
      ~on_delta:(Queue.enqueue deltas)
      ~cwd:"/work"
      ~model:None
      ~system:"Name the session."
      ~prompt:"Fix parser crash"
      ()
  in
  print_s [%message "deltas" (Queue.to_list deltas : string list)];
  print_s [%sexp (result : Adapter.Completion.t Or_error.t)];
  (* Wait a tick for peer fiber to observe EOF after transport stop. *)
  Eio.Fiber.yield ();
  printf "peer_eof=%b\n" !peer_eof;
  [%expect
    {|
    launch mcp=false
    acp got initialize
    acp got session/new mcp=0
    acp got set_config_option model=gemini-3-flash-lite
    acp got set_config_option thought=off
    acp got prompt has_no_tools=true has_system=true has_user=true
    acp permission reply: {"outcome":{"outcome":"selected","optionId":"no"}}
    acp got session/close
    (deltas ("Queue.to_list deltas" ("Fix " "parser crash")))
    (Ok
     ((text "Fix parser crash") (usage ((input 20) (output 5) (cache_read 0)))
      (cost_usd 0) (model (acp:antigravity/gemini-3-flash-lite))))
    peer_eof=true
    |}]
;;

let%expect_test
    "Codex complete: ephemeral thread, no MCP/tools, fast model, auto-deny"
  =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let peer (p : Line_transport.Peer.t) =
    let rec loop () =
      match read_json p with
      | None -> ()
      | Some j ->
        (match str "method" j with
         | Some "initialize" ->
           write_json
             p
             [ "id", id_of j
             ; "result", `Object [ "userAgent", `String "codex/1.0" ]
             ];
           loop ()
         | Some "model/list" ->
           printf "codex got model/list\n";
           write_json
             p
             [ "id", id_of j
             ; ( "result"
               , Json.parse
                   {|{"data":[
                     {"id":"gpt-5.4","displayName":"GPT-5.4","isDefault":true,"supportedReasoningEfforts":["low","high"]},
                     {"id":"gpt-5.4-mini","displayName":"GPT-5.4 Mini","isDefault":false,"supportedReasoningEfforts":["none","low"]}
                   ]}|}
                 |> Or_error.ok_exn )
             ];
           loop ()
         | Some "thread/start" ->
           let params = Option.value (Json.member "params" j) ~default:`Null in
           let ephemeral =
             match Json.member "ephemeral" params with
             | Some `True -> true
             | _ -> false
           in
           let config_keys =
             match Json.member "config" params with
             | Some (`Object fields) -> List.map fields ~f:fst
             | _ -> []
           in
           let dev =
             Option.value (str "developerInstructions" params) ~default:""
           in
           printf
             "codex thread/start ephemeral=%b model=%s sandbox=%s \
              has_dynamic=%b config_keys=%d has_no_tools=%b\n"
             ephemeral
             (Option.value (str "model" params) ~default:"-")
             (Option.value (str "sandbox" params) ~default:"-")
             (Option.is_some (Json.member "dynamicTools" params))
             (List.length config_keys)
             (String.is_substring dev ~substring:"Do not call any tools");
           write_json
             p
             [ "id", id_of j
             ; ( "result"
               , Json.parse
                   {|{"thread":{"id":"th-ephemeral"},"model":"gpt-5.4-mini"}|}
                 |> Or_error.ok_exn )
             ];
           loop ()
         | Some "turn/start" ->
           write_json p [ "id", id_of j; "result", `Object [] ];
           (* Unexpected command approval request must be denied. *)
           Line_transport.Peer.write_line
             p
             {|{"id":7,"method":"item/commandExecution/requestApproval","params":{"threadId":"th-ephemeral","turnId":"tu-1","itemId":"c-1","command":"ls","cwd":"/work"}}|};
           (match read_json p with
            | Some ans ->
              printf
                "codex approval reply: %s\n"
                (Json.to_string
                   (Option.value (Json.member "result" ans) ~default:`Null))
            | None -> ());
           Line_transport.Peer.write_line
             p
             {|{"method":"item/agentMessage/delta","params":{"threadId":"th-ephemeral","turnId":"tu-1","itemId":"m-1","delta":"feat/external-naming"}}|};
           Line_transport.Peer.write_line
             p
             {|{"method":"turn/completed","params":{"threadId":"th-ephemeral","turn":{"id":"tu-1","status":"completed"}}}|};
           loop ()
         | Some _ | None -> loop ())
    in
    loop ()
  in
  let deltas = Queue.create () in
  let result =
    External_complete.run
      ~adapter:(module Codex_adapter)
      ~spawn:(fun launch ->
        printf "launch mcp=%b\n" launch.mcp;
        Ok (Line_transport.Peer.spawn ~sw peer))
      ~cancel:Cancellation.never
      ~on_delta:(Queue.enqueue deltas)
      ~cwd:"/work"
      ~model:None
      ~system:"Name a git branch."
      ~prompt:"external harness naming"
      ()
  in
  print_s [%message "deltas" (Queue.to_list deltas : string list)];
  print_s [%sexp (result : Adapter.Completion.t Or_error.t)];
  [%expect
    {|
    launch mcp=false
    codex got model/list
    codex thread/start ephemeral=true model=gpt-5.4-mini sandbox=read-only has_dynamic=false config_keys=0 has_no_tools=true
    codex approval reply: {"decision":"decline"}
    (deltas ("Queue.to_list deltas" (feat/external-naming)))
    (Ok
     ((text feat/external-naming) (usage ((input 0) (output 0) (cache_read 0)))
      (cost_usd 0) (model (codex/gpt-5.4-mini))))
    |}]
;;

let%expect_test
    "Claude complete: --no-session-persistence, --tools \"\", --max-turns 1, \
     fast model, auto-deny"
  =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let catalog_models =
    [ External_model.model ~adapter:"claude" ~name:"Opus" "claude-opus-4-7"
    ; External_model.model ~adapter:"claude" ~name:"Haiku" "claude-haiku-4-5"
    ]
  in
  let peer (p : Line_transport.Peer.t) =
    let rec loop () =
      match read_json p with
      | None -> ()
      | Some j ->
        (match str "type" j with
         | Some "control_request" ->
           let req_id = Option.value (str "request_id" j) ~default:"" in
           let req = Option.value (Json.member "request" j) ~default:`Null in
           (match str "subtype" req with
            | Some "initialize" ->
              let append =
                Option.value (str "appendSystemPrompt" req) ~default:""
              in
              let sdk_servers =
                match Json.member "sdkMcpServers" req with
                | Some (`Array l) -> List.length l
                | _ -> -1
              in
              printf
                "claude init sdk_mcp=%d has_no_tools=%b\n"
                sdk_servers
                (String.is_substring append ~substring:"Do not call any tools");
              Line_transport.Peer.write_line
                p
                (sprintf
                   {|{"type":"control_response","response":{"subtype":"success","request_id":"%s","response":{"claude_code_version":"2.1.330","models":[{"value":"claude-opus-4-7","displayName":"Opus"},{"value":"claude-haiku-4-5","displayName":"Haiku"}],"commands":[]}}}|}
                   req_id);
              loop ()
            | _ -> loop ())
         | Some "user" ->
           (* Unexpected tool permission request must be denied. *)
           Line_transport.Peer.write_line
             p
             {|{"type":"control_request","request_id":"r-tool","request":{"subtype":"can_use_tool","tool_name":"Bash","tool_use_id":"tu-1","input":{"command":"pwd"}}}|};
           (match read_json p with
            | Some ans ->
              printf "claude approval reply: %s\n" (Json.to_string ans)
            | None -> ());
           Line_transport.Peer.write_line
             p
             {|{"type":"stream_event","event":{"type":"message_start","message":{"model":"claude-haiku-4-5","id":"msg_1","type":"message","role":"assistant","content":[]}}}|};
           Line_transport.Peer.write_line
             p
             {|{"type":"stream_event","event":{"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}}|};
           Line_transport.Peer.write_line
             p
             {|{"type":"stream_event","event":{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"Draft commit title"}}}|};
           Line_transport.Peer.write_line
             p
             {|{"type":"result","subtype":"success","is_error":false,"result":"Draft commit title","session_id":"s-ephemeral","total_cost_usd":0.001,"usage":{"input_tokens":12,"output_tokens":4}}|};
           loop ()
         | _ -> loop ())
    in
    loop ()
  in
  let deltas = Queue.create () in
  let result =
    External_complete.run
      ~adapter:(module Claude_adapter)
      ~spawn:(fun launch ->
        let has flag = List.mem launch.argv flag ~equal:String.equal in
        let rec flag_val flag = function
          | a :: b :: _ when String.equal a flag -> Some b
          | _ :: tl -> flag_val flag tl
          | [] -> None
        in
        printf
          "claude launch mcp=%b no_persist=%b strict_mcp=%b has_mcp_config=%b \
           tools=%S max_turns=%s model=%s\n"
          launch.mcp
          (has "--no-session-persistence")
          (has "--strict-mcp-config")
          (has "--mcp-config")
          (Option.value (flag_val "--tools" launch.argv) ~default:"<missing>")
          (Option.value (flag_val "--max-turns" launch.argv) ~default:"-")
          (Option.value (flag_val "--model" launch.argv) ~default:"-");
        Ok (Line_transport.Peer.spawn ~sw peer))
      ~catalog_models
      ~cancel:Cancellation.never
      ~on_delta:(Queue.enqueue deltas)
      ~cwd:"/work"
      ~model:None
      ~system:"Write a commit message."
      ~prompt:"diff..."
      ()
  in
  print_s [%message "deltas" (Queue.to_list deltas : string list)];
  print_s [%sexp (result : Adapter.Completion.t Or_error.t)];
  [%expect
    {|
    claude launch mcp=false no_persist=true strict_mcp=true has_mcp_config=false tools="" max_turns=1 model=claude-haiku-4-5
    claude init sdk_mcp=0 has_no_tools=true
    claude approval reply: {"type":"control_response","response":{"subtype":"success","request_id":"r-tool","response":{"behavior":"deny","message":"tools are disabled for this completion"}}}
    (deltas ("Queue.to_list deltas" ("Draft commit title")))
    (Ok
     ((text "Draft commit title") (usage ((input 12) (output 4) (cache_read 0)))
      (cost_usd 0.001) (model (claude/claude-haiku-4-5))))
    |}]
;;

let%expect_test
    "External_complete cancellation and timeout tear down cleanly without \
     leaking"
  =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let adapter = Acp_adapter.create (acp_antigravity ()) in
  (* 1. Pre-cancelled never spawns. *)
  let spawned = ref 0 in
  let pre_cancel = Cancellation.create () in
  Cancellation.cancel pre_cancel;
  let r1 =
    External_complete.run
      ~adapter
      ~spawn:(fun _ ->
        incr spawned;
        Or_error.error_string "should not spawn")
      ~cancel:pre_cancel
      ~cwd:"/work"
      ~model:None
      ~system:""
      ~prompt:"hi"
      ()
  in
  printf
    "pre_cancel spawned=%d result=%s\n"
    !spawned
    (Sexp.to_string [%sexp (r1 : Adapter.Completion.t Or_error.t)]);
  (* 2. Mid-turn cancel sends session/cancel and returns cancelled. *)
  let cancel = Cancellation.create () in
  let saw_cancel = ref false in
  let saw_eof = ref false in
  let peer_cancel (p : Line_transport.Peer.t) =
    let rec loop () =
      match read_json p with
      | None -> saw_eof := true
      | Some j ->
        (match str "method" j with
         | Some "initialize" ->
           write_json
             p
             [ "jsonrpc", `String "2.0"
             ; "id", id_of j
             ; "result", Json.parse {|{"protocolVersion":1}|} |> Or_error.ok_exn
             ];
           loop ()
         | Some "session/new" ->
           write_json
             p
             [ "jsonrpc", `String "2.0"
             ; "id", id_of j
             ; "result", `Object [ "sessionId", `String "ses_c" ]
             ];
           loop ()
         | Some "session/prompt" ->
           Cancellation.cancel cancel;
           loop ()
         | Some "session/cancel" ->
           saw_cancel := true;
           loop ()
         | _ -> loop ())
    in
    loop ()
  in
  let r2 =
    External_complete.run
      ~adapter
      ~spawn:(fun _ -> Ok (Line_transport.Peer.spawn ~sw peer_cancel))
      ~cancel
      ~cwd:"/work"
      ~model:None
      ~system:""
      ~prompt:"slow"
      ()
  in
  Eio.Fiber.yield ();
  printf
    "mid_cancel saw_cancel=%b saw_eof=%b result=%s\n"
    !saw_cancel
    !saw_eof
    (Sexp.to_string [%sexp (r2 : Adapter.Completion.t Or_error.t)]);
  (* 3. Short timeout during hung turn exits promptly without waiting for session/close. *)
  let timeout_eof = ref false in
  let saw_close = ref false in
  let peer_hang (p : Line_transport.Peer.t) =
    let rec loop () =
      match read_json p with
      | None -> timeout_eof := true
      | Some j ->
        (match str "method" j with
         | Some "initialize" ->
           write_json
             p
             [ "jsonrpc", `String "2.0"
             ; "id", id_of j
             ; ( "result"
               , Json.parse
                   {|{"protocolVersion":1,"agentCapabilities":{"sessionCapabilities":{"close":{}}}}|}
                 |> Or_error.ok_exn )
             ];
           loop ()
         | Some "session/new" ->
           write_json
             p
             [ "jsonrpc", `String "2.0"
             ; "id", id_of j
             ; "result", `Object [ "sessionId", `String "ses_t" ]
             ];
           loop ()
         | Some "session/close" ->
           saw_close := true;
           loop ()
         | _ -> loop ())
    in
    loop ()
  in
  let r3 =
    External_complete.run
      ~adapter
      ~spawn:(fun _ -> Ok (Line_transport.Peer.spawn ~sw peer_hang))
      ~timeout:(Time_float.Span.of_ms 20.)
      ~cancel:Cancellation.never
      ~cwd:"/work"
      ~model:None
      ~system:""
      ~prompt:"hang"
      ()
  in
  Eio.Fiber.yield ();
  printf
    "timeout saw_close=%b saw_eof=%b result=%s\n"
    !saw_close
    !timeout_eof
    (Sexp.to_string [%sexp (r3 : Adapter.Completion.t Or_error.t)]);
  [%expect
    {|
    pre_cancel spawned=0 result=(Error cancelled)
    mid_cancel saw_cancel=true saw_eof=true result=(Error cancelled)
    timeout saw_close=false saw_eof=true result=(Error"completion timed out after 20ms")
    |}]
;;

(* --- Agent routing tests: auto_describe, Generate_text, name_subagent, config --- *)

module Scripted_runner = struct
  type t = { reply : string }

  let engine = Engine_id.Composer
  let capabilities _ = Capabilities.harness
  let models _ = None
  let context _ = None, None

  let run_turn
        t
        (services : Engine_intf.Services.t)
        ~cancel:_
        (_input : Engine_intf.Turn.Input.t)
    =
    let reply =
      Message.Assistant
        { content = [ Text t.reply ]
        ; stop_reason = End_turn
        ; usage = Usage.zero
        ; model = "acp:antigravity/gemini-3-pro"
        ; account = None
        }
    in
    services.emit (Message_start reply);
    services.emit (Message_end reply);
    [ reply ]
  ;;
end

let%expect_test
    "External session with no direct provider routes auto_describe, \
     Generate_text, and name_subagent through external_complete"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let direct_calls = ref 0 in
  let provider =
    Faux_provider.create
      ~on_request:(fun _ -> incr direct_calls)
      [ Reply.error "not logged in to DeepSeek" ]
  in
  let ext_calls = Queue.create () in
  let external_complete
        ~adapter
        ~model
        ~system
        ~prompt:_
        ~cwd:_
        ~cancel:_
        ~on_delta
    =
    let kind =
      if String.is_substring system ~substring:"Name a git branch"
      then "branch_name"
      else if String.is_substring system ~substring:"You name the subagents"
      then "subagent_name"
      else if String.is_substring system ~substring:"You name the sessions"
      then "session_title"
      else "other"
    in
    Queue.enqueue
      ext_calls
      (sprintf
         "%s/%s:%s"
         (Adapter_id.to_string adapter)
         (Option.value model ~default:"<fast>")
         kind);
    let text =
      match kind with
      | "branch_name" -> "feat/acp-naming"
      | "subagent_name" -> "Survey ACP adapters"
      | "session_title" -> "Route naming through ACP"
      | _ -> "ok"
    in
    on_delta text;
    Ok
      { Adapter.Completion.text
      ; usage = { input = 10; output = 4; cache_read = 0 }
      ; cost_usd = 0.001
      ; model = Some "acp:antigravity/gemini-3-flash-lite"
      }
  in
  let runner = Runner.create (module Scripted_runner) { reply = "Done." } in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:[]
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~auto_describe:true
      ~runner
      ~external_complete
      ~direct_available:(fun _ -> false)
      ~cwd:t.dir
      ()
  in
  Agent.set_engine
    agent
    { engine = Composer; adapter = Some (Acp "antigravity") };
  Agent.subscribe agent ~f:(function
    | Notice n -> print_endline ("notice: " ^ n)
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "Route naming through ACP");
  Agent.wait_idle agent;
  print_s
    [%message
      "description" (Session.description (Agent.session agent) : string option)];
  let sub =
    Agent.name_subagent
      agent
      ~task:"Survey ACP adapters"
      ~siblings:[]
      ~cancel:Cancellation.never
  in
  print_s [%message "subagent" (sub : string option)];
  let deltas = Queue.create () in
  let branch =
    Generate_text.run
      agent
      Branch_name
      { Generate_text.Target.empty with task = Some "add acp naming" }
      ~cancel:Cancellation.never
      ~on_delta:(Queue.enqueue deltas)
  in
  print_s [%message "branch" (branch : Generate_text.Result.t Or_error.t)];
  let title =
    Generate_text.run
      agent
      Session_title
      Generate_text.Target.empty
      ~cancel:Cancellation.never
      ~on_delta:ignore
  in
  print_s [%message "title" (title : Generate_text.Result.t Or_error.t)];
  printf "direct_calls=%d\n" !direct_calls;
  print_s [%message "ext_calls" (Queue.to_list ext_calls : string list)];
  [%expect
    {|
    (description
     ("Session.description (Agent.session agent)" ("Route naming through ACP")))
    (subagent (sub ("Survey ACP adapters")))
    (branch
     (branch
      (Ok
       ((text feat/acp-naming) (title ()) (body ())
        (model acp:antigravity/gemini-3-flash-lite) (cost_usd 0.001)))))
    (title
     (title
      (Ok
       ((text "Route naming through ACP") (title ()) (body ())
        (model acp:antigravity/gemini-3-flash-lite) (cost_usd 0.001)))))
    direct_calls=0
    (ext_calls
     ("Queue.to_list ext_calls"
      (acp:antigravity/<fast>:session_title acp:antigravity/<fast>:subagent_name
       acp:antigravity/<fast>:branch_name acp:antigravity/<fast>:session_title)))
    |}]
;;

let%expect_test
    "Explicit external naming_model and generate_model work on direct \
     sessions; unset stays direct"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let direct_calls = Queue.create () in
  let provider =
    Faux_provider.create
      ~on_request:(fun r -> Queue.enqueue direct_calls (Model.key r.model))
      [ Reply.text "Hello back."
      ; Reply.text "Direct title"
      ; Reply.text "fix/direct-branch"
      ]
  in
  let ext_calls = Queue.create () in
  let external_complete
        ~adapter
        ~model
        ~system:_
        ~prompt:_
        ~cwd:_
        ~cancel:_
        ~on_delta
    =
    let key =
      External_model.name ~adapter:(Adapter_id.to_string adapter) model
    in
    Queue.enqueue ext_calls key;
    let text =
      match adapter with
      | Acp _ -> "External ACP title"
      | Codex -> "feat/codex-branch"
      | Claude -> "Claude subagent"
      | Prigh -> "unused"
    in
    on_delta text;
    Ok
      { Adapter.Completion.text
      ; usage = Usage.zero
      ; cost_usd = 0.
      ; model = Some key
      }
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:[]
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~auto_describe:true
      ~external_complete
      ~direct_available:(fun _ -> true)
      ~cwd:t.dir
      ()
  in
  (* 1. Direct session with unset naming_model/generate_model uses Faux_provider. *)
  Or_error.ok_exn (Agent.prompt agent "say hello");
  Agent.wait_idle agent;
  let direct_branch =
    Generate_text.run
      agent
      Branch_name
      { Generate_text.Target.empty with task = Some "direct" }
      ~cancel:Cancellation.never
      ~on_delta:ignore
  in
  print_s
    [%message
      "direct_desc" (Session.description (Agent.session agent) : string option)];
  print_s
    [%message
      "direct_branch"
        (Result.map direct_branch ~f:(fun r -> r.text, r.model)
         : (string * string) Or_error.t)];
  print_s [%message "direct_calls" (Queue.to_list direct_calls : string list)];
  print_s [%message "ext_calls_before" (Queue.to_list ext_calls : string list)];
  Queue.clear direct_calls;
  (* 2. Unknown ACP adapter is rejected; valid external specs are accepted. *)
  let check_cfg name f =
    match Agent.set_config agent (f (Agent.config agent)) with
    | Ok () -> printf "set_config %s -> ok\n" name
    | Error e ->
      printf "set_config %s -> ERROR %s\n" name (Error.to_string_hum e)
  in
  check_cfg "unknown acp" (fun c ->
    { c with naming_model = Some "acp:bogus/fast" });
  check_cfg "empty model" (fun c -> { c with naming_model = Some "codex/" });
  check_cfg "acp bare" (fun c ->
    { c with naming_model = Some "acp:antigravity" });
  printf
    "stored bare: %s\n"
    (Option.value ~default:"-" (Agent.config agent).naming_model);
  check_cfg "acp explicit" (fun c ->
    { c with naming_model = Some "acp:antigravity/gemini-3-flash" });
  check_cfg "codex generate" (fun c ->
    { c with generate_model = Some "codex/gpt-5-mini" });
  (* Model_registry accepts external naming_model in config.json without warning. *)
  let registry =
    Model_registry.create
      ~env:t.env
      ~sw
      ~auto_fetch:false
      ~home:t.dir
      ~store:(Auth_store.create ~path:(Filename.concat t.dir "auth.json"))
      ~getenv:(fun _ -> None)
      ()
  in
  print_s
    [%message
      "registry_problems" (Model_registry.problems registry : string list)];
  (* 3. Generate_text and name_subagent now route through external_complete. *)
  let ext_title =
    Generate_text.run
      agent
      Session_title
      Generate_text.Target.empty
      ~cancel:Cancellation.never
      ~on_delta:ignore
  in
  let ext_branch =
    Generate_text.run
      agent
      Branch_name
      { Generate_text.Target.empty with task = Some "external" }
      ~cancel:Cancellation.never
      ~on_delta:ignore
  in
  check_cfg "claude naming" (fun c ->
    { c with naming_model = Some "claude/haiku" });
  let ext_sub =
    Agent.name_subagent
      agent
      ~task:"Check claude naming"
      ~siblings:[]
      ~cancel:Cancellation.never
  in
  print_s
    [%message
      "ext_title"
        (Result.map ext_title ~f:(fun r -> r.text, r.model)
         : (string * string) Or_error.t)];
  print_s
    [%message
      "ext_branch"
        (Result.map ext_branch ~f:(fun r -> r.text, r.model)
         : (string * string) Or_error.t)];
  print_s [%message "ext_sub" (ext_sub : string option)];
  print_s
    [%message "direct_calls_after" (Queue.to_list direct_calls : string list)];
  print_s [%message "ext_calls_after" (Queue.to_list ext_calls : string list)];
  [%expect
    {|
    (direct_desc ("Session.description (Agent.session agent)" ("Direct title")))
    (direct_branch
     ("Result.map direct_branch ~f:(fun r -> (r.text, r.model))"
      (Ok (fix/direct-branch deepseek/deepseek-flash))))
    (direct_calls
     ("Queue.to_list direct_calls"
      (deepseek/deepseek-flash deepseek/deepseek-flash deepseek/deepseek-flash)))
    (ext_calls_before ("Queue.to_list ext_calls" ()))
    set_config unknown acp -> ERROR naming_model: prigh does not know how to start the ACP agent bogus: give its command with -adapters acp:bogus=<command and args> (it knows opencode, gemini, antigravity, grok, goose)
    set_config empty model -> ERROR naming_model: empty model name for codex
    set_config acp bare -> ok
    stored bare: acp:antigravity/default
    set_config acp explicit -> ok
    set_config codex generate -> ok
    (registry_problems ("Model_registry.problems registry" ()))
    set_config claude naming -> ok
    (ext_title
     ("Result.map ext_title ~f:(fun r -> (r.text, r.model))"
      (Ok ("External ACP title" acp:antigravity/gemini-3-flash))))
    (ext_branch
     ("Result.map ext_branch ~f:(fun r -> (r.text, r.model))"
      (Ok (feat/codex-branch codex/gpt-5-mini))))
    (ext_sub (ext_sub ("Claude subagent")))
    (direct_calls_after ("Queue.to_list direct_calls" ()))
    (ext_calls_after
     ("Queue.to_list ext_calls"
      (acp:antigravity/gemini-3-flash codex/gpt-5-mini claude/haiku)))
    |}]
;;

let%expect_test
    "Rpc_server wires external_complete and direct_available end-to-end on ACP \
     session"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let sessions_dir = Filename.concat t.dir "sessions" in
  let engines =
    Or_error.ok_exn
      (Engines.create
         ~engines:[ Harness; Composer ]
         ~adapters:[ Acp "antigravity"; Codex; Claude ]
         ())
  in
  let provider =
    Faux_provider.create [ Reply.error "not logged in to DeepSeek" ]
  in
  let spawn_count = ref 0 in
  let fake_acp (p : Line_transport.Peer.t) =
    let rec loop () =
      match read_json p with
      | None -> ()
      | Some j ->
        (match str "method" j with
         | Some "initialize" ->
           write_json
             p
             [ "jsonrpc", `String "2.0"
             ; "id", id_of j
             ; ( "result"
               , Json.parse
                   {|{"protocolVersion":1,"agentCapabilities":{"sessionCapabilities":{"close":{}}}}|}
                 |> Or_error.ok_exn )
             ];
           loop ()
         | Some "session/new" ->
           write_json
             p
             [ "jsonrpc", `String "2.0"
             ; "id", id_of j
             ; ( "result"
               , Json.parse
                   {|{"sessionId":"ses_rpc","configOptions":[{"id":"model","name":"Model","category":"model","type":"select","currentValue":"gemini-3-pro","options":[{"value":"gemini-3-pro","name":"Gemini 3 Pro"},{"value":"gemini-3-flash","name":"Gemini 3 Flash"}]}]}|}
                 |> Or_error.ok_exn )
             ];
           loop ()
         | Some "session/set_config_option" ->
           write_json
             p
             [ "jsonrpc", `String "2.0"
             ; "id", id_of j
             ; "result", `Object [ "configOptions", `Array [] ]
             ];
           loop ()
         | Some "session/prompt" ->
           write_json
             p
             [ "jsonrpc", `String "2.0"
             ; "method", `String "session/update"
             ; ( "params"
               , Json.parse
                   {|{"sessionId":"ses_rpc","update":{"sessionUpdate":"agent_message_chunk","content":{"type":"text","text":"feat/rpc-external-draft"}}}|}
                 |> Or_error.ok_exn )
             ];
           write_json
             p
             [ "jsonrpc", `String "2.0"
             ; "id", id_of j
             ; ( "result"
               , Json.parse {|{"stopReason":"end_turn"}|} |> Or_error.ok_exn )
             ];
           loop ()
         | Some "session/close" ->
           write_json
             p
             [ "jsonrpc", `String "2.0"; "id", id_of j; "result", `Object [] ];
           loop ()
         | _ -> loop ())
    in
    loop ()
  in
  let server =
    Rpc_server.create
      ~env:t.env
      ~sw
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
      ~spawn_external:(fun _agent ~host:_ launch ->
        incr spawn_count;
        printf "rpc spawn #%d mcp=%b\n" !spawn_count launch.mcp;
        Ok (Line_transport.Peer.spawn ~sw fake_acp))
      ~new_agent:(fun ?session ~cwd () ->
        Agent.create
          ~env:t.env
          ~sw
          ~provider
          ~tools:[]
          ~sessions_dir
          ~home:t.dir
          ?session
          ~backend_host:true
          ~cwd
          ())
      ()
  in
  let client = Rpc_server.connect server ~send:ignore in
  let call ?(params = "{}") meth =
    Rpc_server.handle
      server
      client
      (Json.of_string
         (sprintf {|{"id":"r","method":"%s","params":%s}|} meth params))
  in
  (* Switch session to acp:antigravity with no direct HTTP logins configured. *)
  ignore (call ~params:{|{"adapter":"acp:antigravity"}|} "set_adapter" : Json.t);
  let res =
    call
      ~params:{|{"kind":"branch_name","task":"wire external naming"}|}
      "generate_text"
  in
  print_endline (Json.to_string res);
  (* Also verify set_config accepts external naming_model on Rpc_server. *)
  let cfg_ok =
    call
      ~params:{|{"config":{"naming_model":"acp:antigravity/gemini-3-flash"}}|}
      "set_config"
  in
  print_endline (Json.to_string cfg_ok);
  Rpc_server.shutdown server;
  [%expect
    {|
    rpc spawn #1 mcp=true
    rpc spawn #2 mcp=false
    {"type":"response","id":"r","ok":true,"result":{"gen_id":"gen-1","kind":"branch_name","text":"feat/rpc-external-draft","model":"acp:antigravity/gemini-3-flash","cost_usd":0}}
    {"type":"response","id":"r","ok":true,"result":{"scoped_models":[],"approval_mode":"auto","default_chat_mode":false,"permissions":{"allow":[],"ask":[],"deny":[]},"default_model":null,"default_thinking":null,"fallback_models":[],"default_cwd":null,"default_host":null,"worktrees_dir":"~/.prigh/worktrees","max_budget_usd":null,"goal_max_continuations":20,"resume_on_limit_reset":true,"ask_user_timeout_seconds":300,"archive_after_days":7,"notify":"bell","snippets":[],"auto_background_seconds":30,"generate_model":null,"mask_observations":true,"computer_screenshots":3,"computer_screenshots_max":8,"computer_native":true,"compact_at_tokens":200000,"compaction_model":null,"naming_model":"acp:antigravity/gemini-3-flash","approval_model":null,"trace":"default"}}
    |}]
;;
