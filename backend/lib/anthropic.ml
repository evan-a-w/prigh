open! Core
open! Import

let default_base_url = "https://api.anthropic.com"
let api_version = "2023-06-01"
let claude_code_version = "2.1.280"

let claude_code_identity =
  "You are Claude Code, Anthropic's official CLI for Claude."
;;

let oauth_betas = [ "claude-code-20250219"; "oauth-2025-04-20" ]
let interleaved_thinking_beta = "interleaved-thinking-2025-05-14"

(* Claude Code 2.x tool names; OAuth requests must use this casing. *)
let claude_code_tools =
  [ "Read"
  ; "Write"
  ; "Edit"
  ; "Bash"
  ; "Grep"
  ; "Glob"
  ; "AskUserQuestion"
  ; "EnterPlanMode"
  ; "ExitPlanMode"
  ; "KillShell"
  ; "NotebookEdit"
  ; "Skill"
  ; "Task"
  ; "TaskOutput"
  ; "TodoWrite"
  ; "WebFetch"
  ; "WebSearch"
  ]
;;

let to_claude_code_name name =
  List.find claude_code_tools ~f:(String.Caseless.equal name)
  |> Option.value ~default:name
;;

let from_claude_code_name ~(tools : Tool_spec.t list) name =
  List.find_map tools ~f:(fun t ->
    Option.some_if (String.Caseless.equal t.name name) t.name)
  |> Option.value ~default:name
;;

let is_oauth_token token = String.is_substring token ~substring:"sk-ant-oat"

module Auth = struct
  type t =
    | Api_key of string
    | Oauth of string
    | Gateway of string option
end

let cache_control = "cache_control", `Object [ "type", `String "ephemeral" ]

let text_block ?(cached = false) text =
  `Object
    ([ "type", `String "text"; "text", `String text ]
     @ if cached then [ cache_control ] else [])
;;

let redacted_prefix = "redacted:"

let assistant_block ~oauth ~replay_thinking (block : Content.t) =
  match block with
  | Content.Text "" | Server_tool _ -> None
  | Text text -> Some (`Object [ "type", `String "text"; "text", `String text ])
  | Thinking { signature = None; _ } -> None
  | Thinking { text; signature = Some signature } ->
    if not replay_thinking
    then None
    else if String.is_prefix signature ~prefix:redacted_prefix
    then
      Some
        (`Object
            [ "type", `String "redacted_thinking"
            ; ( "data"
              , `String
                  (String.chop_prefix_exn signature ~prefix:redacted_prefix) )
            ])
    else
      Some
        (`Object
            [ "type", `String "thinking"
            ; "thinking", `String text
            ; "signature", `String signature
            ])
  | Tool_call call ->
    let input =
      match Content.Tool_call.parse_arguments call with
      | Ok json -> json
      | Error _ -> `Object []
    in
    Some
      (`Object
          [ "type", `String "tool_use"
          ; "id", `String call.id
          ; ( "name"
            , `String
                (if oauth then to_claude_code_name call.name else call.name) )
          ; "input", input
          ])
;;

(* A search is replayed as the provider sent it (its results carry
   encrypted content the model needs for citations); another provider's
   becomes text. One without a result yet is only replayed to continue the
   turn it is in ([continuing], after a [pause_turn]); otherwise it is
   dropped: a [server_tool_use] needs its result. *)
let server_tool_blocks ~same_provider ~continuing (tool : Content.Server_tool.t)
  =
  let as_text () = [ text_block (Content.Server_tool.as_text tool) ] in
  let use input =
    `Object
      [ "type", `String "server_tool_use"
      ; "id", `String tool.id
      ; "name", `String tool.name
      ; "input", input
      ]
  in
  match tool.raw, tool.output with
  | Some raw, _ when same_provider ->
    (match Json.parse tool.input, Json.parse raw with
     | Ok input, Ok result -> [ use input; result ]
     | _ -> as_text ())
  | None, None when same_provider ->
    (match Json.parse tool.input with
     | Ok input when continuing -> [ use input ]
     | Ok _ | Error _ -> [])
  | Some _, _ | None, _ -> as_text ()
;;

let assistant_blocks
      ~oauth
      ~replay_thinking
      ?(continuing = false)
      (a : Message.Assistant.t)
  =
  List.concat_map a.content ~f:(fun block ->
    match block with
    | Content.Server_tool tool ->
      server_tool_blocks ~same_provider:replay_thinking ~continuing tool
    | Text _ | Thinking _ | Tool_call _ ->
      Option.to_list (assistant_block ~oauth ~replay_thinking block))
;;

let image_block (image : Image.t) =
  `Object
    [ "type", `String "image"
    ; ( "source"
      , `Object
          [ "type", `String "base64"
          ; "media_type", `String image.mime_type
          ; "data", `String image.data
          ] )
    ]
;;

(* Text blocks may not be empty. *)
let user_blocks ({ text; images; origin = _ } : Message.User.t) =
  (if String.is_empty text && not (List.is_empty images)
   then []
   else [ text_block text ])
  @ List.map images ~f:image_block
;;

let tool_result_block (r : Message.Tool_result.t) =
  let text = if String.is_empty r.text then "(no output)" else r.text in
  `Object
    [ "type", `String "tool_result"
    ; "tool_use_id", `String r.tool_call_id
    ; ( "content"
      , match r.images with
        | [] -> `String text
        | images -> `Array (text_block text :: List.map images ~f:image_block)
      )
    ; ("is_error", if r.is_error then `True else `False)
    ]
;;

let same_provider ~provider (a : Message.Assistant.t) =
  Model.written_by a.model provider
;;

(* Not on thinking or a [server_tool_use], which a continued turn (after a
   [pause_turn]) may end with. *)
let cacheable = function
  | `Object fields ->
    (match List.Assoc.find fields ~equal:String.equal "type" with
     | Some (`String ("text" | "image" | "tool_use" | "tool_result")) -> true
     | _ -> false)
  | _ -> false
;;

let add_cache_control = function
  | `Object fields -> `Object (fields @ [ cache_control ])
  | other -> other
;;

(* Consecutive same-role turns are merged. Two cache breakpoints: the end of
   the conversation, and the end of the request before this one (what comes
   before the last assistant reply), whose prefix the provider has cached.
   The provider only looks about 20 blocks back from a breakpoint for a
   cached prefix, so with the end alone a turn that adds more blocks (many
   parallel tool calls and their results) would miss the cache and process
   the whole context again. *)
let wire_messages ~oauth ~provider (messages : Message.t list) =
  let last = List.length messages - 1 in
  let previous_end =
    List.foldi messages ~init:(-1) ~f:(fun i acc m ->
      match m with
      | Assistant _ -> i - 1
      | User _ | Tool_result _ -> acc)
  in
  let blocks =
    List.concat_mapi messages ~f:(fun i m ->
      let role, blocks =
        match m with
        | User u -> "user", user_blocks u
        | Tool_result r -> "user", [ tool_result_block r ]
        | Assistant a ->
          ( "assistant"
          , assistant_blocks
              ~oauth
              ~replay_thinking:(same_provider ~provider a)
              ~continuing:(i = last)
              a )
      in
      List.map blocks ~f:(fun block -> i, role, block))
  in
  let last_cacheable ~upto =
    List.foldi blocks ~init:None ~f:(fun pos acc (i, _, block) ->
      if i <= upto && cacheable block then Some pos else acc)
  in
  let breakpoints =
    List.filter_opt
      [ last_cacheable ~upto:previous_end; last_cacheable ~upto:last ]
  in
  let merged =
    List.foldi blocks ~init:[] ~f:(fun pos acc (_, role, block) ->
      let block =
        if List.mem breakpoints pos ~equal:Int.equal
        then add_cache_control block
        else block
      in
      match acc with
      | (prev_role, prev_blocks) :: rest when String.equal prev_role role ->
        (role, block :: prev_blocks) :: rest
      | _ -> (role, [ block ]) :: acc)
  in
  List.rev_map merged ~f:(fun (role, blocks) ->
    `Object [ "role", `String role; "content", `Array (List.rev blocks) ])
;;

let web_search_max_uses = 5

let wire_tool ~oauth (t : Tool_spec.t) =
  if Tool_web_search.is_spec t
  then
    `Object
      [ "type", `String "web_search_20250305"
      ; "name", `String Tool_web_search.name
      ; "max_uses", `Number (Int.to_string web_search_max_uses)
      ]
  else
    `Object
      [ "name", `String (if oauth then to_claude_code_name t.name else t.name)
      ; "description", `String t.description
      ; "input_schema", t.parameters
      ]
;;

let thinking_budget ~max_tokens (thinking : Thinking.t) =
  let budget =
    match thinking with
    | Off -> None
    | On None -> Some 8_192
    | On (Some Low) -> Some 2_048
    | On (Some High) -> Some 16_384
    | On (Some Max) -> Some 32_000
  in
  Option.map budget ~f:(fun b -> Int.max 1_024 (Int.min b (max_tokens - 1_024)))
;;

let effort_of_thinking : Thinking.t -> string option = function
  | Off -> None
  | On None -> Some "medium"
  | On (Some Low) -> Some "low"
  | On (Some High) -> Some "high"
  | On (Some Max) -> Some "max"
;;

(* The thinking-related fields of the request. Budget models get
   [thinking.enabled] with a token budget (pi's older Claude path); adaptive
   models get [thinking.adaptive] and the effort in [output_config], or
   [thinking.disabled] when off (omitted for models that are always on). *)
let thinking_fields ~max_tokens (model : Model.t) (thinking : Thinking.t) =
  if not model.supports_thinking
  then []
  else (
    match model.thinking_style with
    | Budget ->
      (match thinking_budget ~max_tokens thinking with
       | None -> []
       | Some budget_tokens ->
         [ ( "thinking"
           , `Object
               [ "type", `String "enabled"
               ; "budget_tokens", `Number (Int.to_string budget_tokens)
               ] )
         ])
    | Adaptive { can_disable } ->
      (match effort_of_thinking thinking with
       | None ->
         if can_disable
         then [ "thinking", `Object [ "type", `String "disabled" ] ]
         else []
       | Some effort ->
         [ "thinking", `Object [ "type", `String "adaptive" ]
         ; "output_config", `Object [ "effort", `String effort ]
         ]))
;;

(* Only budget models need the beta; adaptive thinking is interleaved
   already. *)
let thinking_on (model : Model.t) (thinking : Thinking.t) =
  match model.thinking_style with
  | Budget -> model.supports_thinking && not (Thinking.equal thinking Off)
  | Adaptive _ -> false
;;

let request_body ~oauth (r : Provider.Request.t) : Json.t =
  let r = Provider.Request.prepare r in
  let max_tokens = Option.value r.max_tokens ~default:r.model.max_output in
  (* One breakpoint at the end of the system prompt (tools come before
     it); the identity alone would gain nothing from another. *)
  let system =
    match oauth, r.system with
    | false, None -> []
    | false, Some s -> [ text_block ~cached:true s ]
    | true, None -> [ text_block ~cached:true claude_code_identity ]
    | true, Some s ->
      [ text_block claude_code_identity; text_block ~cached:true s ]
  in
  let thinking = thinking_fields ~max_tokens r.model r.thinking in
  `Object
    (List.concat
       [ [ "model", `String r.model.id
         ; "max_tokens", `Number (Int.to_string max_tokens)
         ; "stream", `True
         ]
       ; (if List.is_empty system then [] else [ "system", `Array system ])
       ; [ ( "messages"
           , `Array (wire_messages ~oauth ~provider:r.model.provider r.messages)
           )
         ]
       ; (if List.is_empty r.tools
          then []
          else [ "tools", `Array (List.map r.tools ~f:(wire_tool ~oauth)) ])
       ; thinking
       ])
;;

let headers ~(auth : Auth.t) ~thinking_on =
  let betas =
    (match auth with
     | Oauth _ -> oauth_betas
     | Api_key _ | Gateway _ -> [])
    @ if thinking_on then [ interleaved_thinking_beta ] else []
  in
  List.concat
    [ [ "anthropic-version", api_version ]
    ; (match auth with
       | Api_key key -> [ "x-api-key", key ]
       | Gateway None -> []
       | Gateway (Some key) ->
         [ "x-api-key", key; "Authorization", "Bearer " ^ key ]
       | Oauth token ->
         [ "Authorization", "Bearer " ^ token
         ; "user-agent", "claude-cli/" ^ claude_code_version
         ; "x-app", "cli"
         ])
    ; (if List.is_empty betas
       then []
       else [ "anthropic-beta", String.concat ~sep:"," betas ])
    ]
;;

module Stream_state = struct
  type t =
    { mutable block_types : (int * [ `Text | `Thinking | `Tool_use ]) list
    ; mutable tool_index : int
    ; mutable tool_indices : (int * int) list (* content index -> tool index *)
    ; mutable server_tools : (int * (string * string * Buffer.t)) list
      (* content index -> id, name, input so far *)
    ; mutable stop_reason : string option
    ; mutable input : int
    ; mutable cache_read : int
    ; mutable output : int
    ; mutable error : string option
    }

  let create () =
    { block_types = []
    ; tool_index = 0
    ; tool_indices = []
    ; server_tools = []
    ; stop_reason = None
    ; input = 0
    ; cache_read = 0
    ; output = 0
    ; error = None
    }
  ;;
end

let member = Sse_request.member_string
let int_member name json = Option.bind (Json.member name json) ~f:Json.int

let apply_usage (state : Stream_state.t) usage =
  let get name = Option.value (int_member name usage) ~default:0 in
  let input = get "input_tokens" in
  let cache_read = get "cache_read_input_tokens" in
  let cache_write = get "cache_creation_input_tokens" in
  if Json.member "input_tokens" usage |> Option.is_some
  then (
    state.input <- input + cache_read + cache_write;
    state.cache_read <- cache_read);
  Option.iter (int_member "output_tokens" usage) ~f:(fun n -> state.output <- n)
;;

(* The readable output of a [web_search_tool_result] block's [content]: the
   results, or the error code. *)
let search_result (content : Json.t option) =
  match content with
  | Some (`Array results) ->
    ( Tool_web_search.results_text
        (List.filter_map results ~f:(fun r ->
           Option.map (member "url" r) ~f:(fun url ->
             ( Option.value (member "title" r) ~default:""
             , url
             , member "page_age" r ))))
    , false )
  | Some json ->
    ( sprintf
        "search failed: %s"
        (Option.value (member "error_code" json) ~default:(Json.to_string json))
    , true )
  | None -> "search failed: no results in the reply", true
;;

(* Returns the assistant events for one SSE event and records usage/stop
   reason in [state]. *)
let parse_event ~tools (state : Stream_state.t) (event : Sse.Event.t)
  : Assistant_event.t list
  =
  match Json.parse event.data with
  | Error e ->
    state.error
    <- Some (sprintf "bad JSON in stream: %s" (Error.to_string_hum e));
    []
  | Ok json ->
    let index () = Option.value (int_member "index" json) ~default:0 in
    (match Option.value (member "type" json) ~default:"" with
     | "message_start" ->
       Option.iter (Json.member "message" json) ~f:(fun m ->
         Option.iter (Json.member "usage" m) ~f:(apply_usage state));
       []
     | "content_block_start" ->
       let index = index () in
       (match Json.member "content_block" json with
        | None -> []
        | Some block ->
          (match Option.value (member "type" block) ~default:"" with
           | "text" ->
             state.block_types <- (index, `Text) :: state.block_types;
             []
           | "thinking" ->
             (* Shown as soon as it starts: with [display] omitted (the
                default of newer models) its text stays empty, and nothing
                else arrives until it ends. *)
             state.block_types <- (index, `Thinking) :: state.block_types;
             [ Assistant_event.Thinking_delta "" ]
           | "redacted_thinking" ->
             let data = Option.value (member "data" block) ~default:"" in
             [ Assistant_event.Thinking_delta ""
             ; Thinking_signature (redacted_prefix ^ data)
             ]
           | "server_tool_use" ->
             let input = Buffer.create 64 in
             (match Json.member "input" block with
              | Some (`Object (_ :: _) as json) ->
                Buffer.add_string input (Json.to_string json)
              | _ -> ());
             state.server_tools
             <- ( index
                , ( Option.value (member "id" block) ~default:""
                  , Option.value (member "name" block) ~default:""
                  , input ) )
                :: state.server_tools;
             []
           | "web_search_tool_result" ->
             let output, is_error =
               search_result (Json.member "content" block)
             in
             [ Server_tool_result
                 { id = Option.value (member "tool_use_id" block) ~default:""
                 ; output
                 ; is_error
                 ; raw = Some (Json.to_string block)
                 }
             ]
           | "tool_use" ->
             let tool_index = state.tool_index in
             state.tool_index <- tool_index + 1;
             state.tool_indices <- (index, tool_index) :: state.tool_indices;
             state.block_types <- (index, `Tool_use) :: state.block_types;
             [ Tool_call_start
                 { index = tool_index
                 ; id = Option.value (member "id" block) ~default:""
                 ; name =
                     from_claude_code_name
                       ~tools
                       (Option.value (member "name" block) ~default:"")
                 }
             ]
           | _ -> []))
     | "content_block_delta" ->
       let index = index () in
       (match Json.member "delta" json with
        | None -> []
        | Some delta ->
          (match Option.value (member "type" delta) ~default:"" with
           | "text_delta" ->
             Option.value_map (member "text" delta) ~default:[] ~f:(fun s ->
               [ Assistant_event.Text_delta s ])
           | "thinking_delta" ->
             Option.value_map (member "thinking" delta) ~default:[] ~f:(fun s ->
               [ Assistant_event.Thinking_delta s ])
           | "signature_delta" ->
             Option.value_map
               (member "signature" delta)
               ~default:[]
               ~f:(fun s -> [ Assistant_event.Thinking_signature s ])
           | "input_json_delta"
             when List.Assoc.mem state.server_tools ~equal:Int.equal index ->
             let _, _, input =
               List.Assoc.find_exn state.server_tools ~equal:Int.equal index
             in
             Option.iter
               (member "partial_json" delta)
               ~f:(Buffer.add_string input);
             []
           | "input_json_delta" ->
             (match
                ( List.Assoc.find state.tool_indices ~equal:Int.equal index
                , member "partial_json" delta )
              with
              | Some tool_index, Some arguments
                when not (String.is_empty arguments) ->
                [ Tool_call_delta { index = tool_index; arguments } ]
              | _ -> [])
           | _ -> []))
     | "content_block_stop" ->
       (match
          List.Assoc.find state.server_tools ~equal:Int.equal (index ())
        with
        | None -> []
        | Some (id, name, input) ->
          [ Server_tool_use
              { id
              ; name
              ; input =
                  (if Buffer.length input = 0
                   then "{}"
                   else Buffer.contents input)
              }
          ])
     | "message_delta" ->
       Option.iter (Json.member "delta" json) ~f:(fun d ->
         Option.iter (member "stop_reason" d) ~f:(fun r ->
           state.stop_reason <- Some r));
       Option.iter (Json.member "usage" json) ~f:(apply_usage state);
       []
     | "error" ->
       (* Typed, so that a transient one (overloaded_error, api_error) is
          retried. *)
       let error = Json.member "error" json in
       let message =
         Option.bind error ~f:(member "message")
         |> Option.value ~default:event.data
       in
       state.error
       <- Some
            (match Option.bind error ~f:(member "type") with
             | Some type_ -> sprintf "%s: %s" type_ message
             | None -> message);
       []
     | _ -> [])
;;

let stop_reason_of_string = function
  | Some "tool_use" -> Stop_reason.Tool_use
  | Some "max_tokens" -> Length
  | Some ("end_turn" | "stop_sequence" | "pause_turn") | None -> End_turn
  | Some "refusal" -> Error "refusal"
  | Some other -> Error ("unexpected stop_reason: " ^ other)
;;

(* How many times a turn the API paused ([pause_turn], e.g. during long
   server-side searches) is sent back to continue. *)
let max_continuations = 10

let stream
      ~env
      ~url
      ~extra_headers
      ~timeout
      ~(auth : Auth.t)
      (request : Provider.Request.t)
      ~cancel
      ~on_event
  =
  let oauth =
    match auth with
    | Oauth _ -> true
    | Api_key _ | Gateway _ -> false
  in
  let builder = Assistant_builder.create ~model:(Model.key request.model) in
  let thinking_on = thinking_on request.model request.thinking in
  let usage = ref Usage.zero in
  let tool_index = ref 0 in
  let original = request.messages in
  (* A paused turn is continued by sending what it has so far back as the
     last (assistant) message; its blocks go on in the same message. *)
  let rec go (request : Provider.Request.t) ~continuations =
    let state = Stream_state.create () in
    state.tool_index <- !tool_index;
    let outcome =
      Sse_request.run
        ~env
        ?timeout
        ~cancel
        ~url
        ~headers:(headers ~auth ~thinking_on @ extra_headers)
        ~body:(Json.to_string (request_body ~oauth request))
        ~on_event:(fun event ->
          List.iter (parse_event ~tools:request.tools state event) ~f:(fun e ->
            Assistant_builder.apply builder e;
            on_event e))
        ()
    in
    tool_index := state.tool_index;
    usage
    := Usage.add
         !usage
         { input = state.input
         ; output = state.output
         ; cache_read = state.cache_read
         };
    let stop_reason : Stop_reason.t =
      match outcome with
      | Aborted -> Aborted
      | Failed e -> Error e
      | Completed ->
        (match state.error with
         | Some e -> Error e
         | None -> stop_reason_of_string state.stop_reason)
    in
    match stop_reason, state.stop_reason with
    | End_turn, Some "pause_turn"
      when continuations < max_continuations
           && not (Cancellation.is_cancelled cancel) ->
      go
        { request with
          messages =
            original @ [ Assistant (Assistant_builder.snapshot builder) ]
        }
        ~continuations:(continuations + 1)
    | _ -> stop_reason
  in
  let stop_reason = go request ~continuations:0 in
  Assistant_builder.finish builder ~stop_reason ~usage:!usage
;;

let auth_of_token ~(method_ : Provider_auth.Method.t) token : Auth.t =
  match method_ with
  | Oauth -> Oauth token
  | Api_key -> if is_oauth_token token then Oauth token else Api_key token
;;

let create
      ~env
      ?(base_url = default_base_url)
      ?(path = "/v1/messages")
      ?(extra_headers = [])
      ?timeout
      ~auth
      ()
  =
  { Provider.name = "anthropic"
  ; stream = stream ~env ~url:(base_url ^ path) ~extra_headers ~timeout ~auth
  }
;;

module For_testing = struct
  let thinking_on = thinking_on
  let request_body = request_body
  let headers = headers

  let parse_events ~tools events =
    let state = Stream_state.create () in
    let events =
      List.concat_map events ~f:(fun data ->
        parse_event ~tools state { Sse.Event.event = None; data; id = None })
    in
    ( events
    , `Stop_reason state.stop_reason
    , `Usage
        { Usage.input = state.input
        ; output = state.output
        ; cache_read = state.cache_read
        }
    , `Error state.error )
  ;;
end
