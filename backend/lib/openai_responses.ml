open! Core
open! Import

module Endpoint = struct
  type t =
    | Openai of { api_key : string }
    | Codex of
        { access_token : string
        ; account_id : string
        }

  let default_base_url = function
    | Openai _ -> "https://api.openai.com/v1"
    | Codex _ -> "https://chatgpt.com/backend-api"
  ;;

  let url t ~base_url =
    match t with
    | Openai _ -> base_url ^ "/responses"
    | Codex _ -> base_url ^ "/codex/responses"
  ;;

  let headers t =
    match t with
    | Openai { api_key } -> [ "Authorization", "Bearer " ^ api_key ]
    | Codex { access_token; account_id } ->
      [ "Authorization", "Bearer " ^ access_token
      ; "chatgpt-account-id", account_id
      ; "originator", "prigh"
      ; "OpenAI-Beta", "responses=experimental"
      ; "User-Agent", "prigh/" ^ Version.to_string
      ]
  ;;

  let provider_id : t -> Provider_id.t = function
    | Openai _ -> Openai
    | Codex _ -> Openai_codex
  ;;
end

let effort_of_thinking : Thinking.t -> string option = function
  | Off -> None
  | On None -> Some "medium"
  | On (Some Low) -> Some "low"
  | On (Some High) -> Some "high"
  | On (Some Max) -> Some "xhigh"
;;

let same_provider ~provider (a : Message.Assistant.t) =
  match Model.find a.model with
  | Some m -> Provider_id.equal m.provider provider
  | None -> false
;;

let input_items ~provider (messages : Message.t list) =
  List.concat_mapi messages ~f:(fun message_index m ->
    match m with
    | User { text } ->
      [ `Object
          [ "type", `String "message"
          ; "role", `String "user"
          ; ( "content"
            , `Array
                [ `Object [ "type", `String "input_text"; "text", `String text ]
                ] )
          ]
      ]
    | Tool_result r ->
      [ `Object
          [ "type", `String "function_call_output"
          ; "call_id", `String r.tool_call_id
          ; "output", `String r.text
          ]
      ]
    | Assistant a ->
      let replay_reasoning = same_provider ~provider a in
      let text_index = ref 0 in
      List.filter_map a.content ~f:(function
        | Content.Text "" -> None
        | Text text ->
          let id =
            if !text_index = 0
            then sprintf "msg_prigh_%d" message_index
            else sprintf "msg_prigh_%d_%d" message_index !text_index
          in
          incr text_index;
          Some
            (`Object
                [ "type", `String "message"
                ; "role", `String "assistant"
                ; "id", `String id
                ; "status", `String "completed"
                ; ( "content"
                  , `Array
                      [ `Object
                          [ "type", `String "output_text"
                          ; "text", `String text
                          ; "annotations", `Array []
                          ]
                      ] )
                ])
        | Thinking { signature = Some item; _ } when replay_reasoning ->
          Result.ok (Json.parse item)
        | Thinking _ -> None
        | Tool_call call ->
          Some
            (`Object
                [ "type", `String "function_call"
                ; "call_id", `String call.id
                ; "name", `String call.name
                ; "arguments", `String call.arguments
                ])))
;;

let wire_tool (t : Tool_spec.t) =
  `Object
    [ "type", `String "function"
    ; "name", `String t.name
    ; "description", `String t.description
    ; "parameters", t.parameters
    ; "strict", `False
    ]
;;

let request_body ~(endpoint : Endpoint.t) (r : Provider.Request.t) : Json.t =
  let provider = Endpoint.provider_id endpoint in
  let reasoning =
    if not r.model.supports_thinking
    then []
    else
      Option.value_map
        (effort_of_thinking r.thinking)
        ~default:[]
        ~f:(fun effort ->
          [ ( "reasoning"
            , `Object [ "effort", `String effort; "summary", `String "auto" ] )
          ])
  in
  `Object
    (List.concat
       [ [ "model", `String r.model.id
         ; "stream", `True
         ; "store", `False
         ; ( "instructions"
           , `String
               (Option.value r.system ~default:"You are a helpful assistant.") )
         ; "input", `Array (input_items ~provider r.messages)
         ; "include", `Array [ `String "reasoning.encrypted_content" ]
         ; "tool_choice", `String "auto"
         ; "parallel_tool_calls", `True
         ]
       ; (if List.is_empty r.tools
          then []
          else [ "tools", `Array (List.map r.tools ~f:wire_tool) ])
       ; reasoning
       ; (match endpoint with
          | Codex _ -> [ "text", `Object [ "verbosity", `String "low" ] ]
          | Openai _ ->
            Option.value_map r.max_tokens ~default:[] ~f:(fun n ->
              [ "max_output_tokens", `Number (Int.to_string n) ]))
       ])
;;

module Stream_state = struct
  type t =
    { mutable tools : (int * int) list (* output_index -> tool index *)
    ; mutable tool_arguments_seen : int list
    ; mutable tool_count : int
    ; mutable saw_tool_call : bool
    ; mutable incomplete_reason : string option
    ; mutable usage : Usage.t
    ; mutable error : string option
    }

  let create () =
    { tools = []
    ; tool_arguments_seen = []
    ; tool_count = 0
    ; saw_tool_call = false
    ; incomplete_reason = None
    ; usage = Usage.zero
    ; error = None
    }
  ;;
end

let member = Sse_request.member_string
let int_member name json = Option.bind (Json.member name json) ~f:Json.int

let parse_usage json =
  let get name = Option.value (int_member name json) ~default:0 in
  let cached =
    Option.bind
      (Json.member "input_tokens_details" json)
      ~f:(int_member "cached_tokens")
    |> Option.value ~default:0
  in
  { Usage.input = get "input_tokens"
  ; output = get "output_tokens"
  ; cache_read = cached
  }
;;

let parse_event (state : Stream_state.t) (event : Sse.Event.t)
  : Assistant_event.t list
  =
  match Json.parse event.data with
  | Error e ->
    state.error
    <- Some (sprintf "bad JSON in stream: %s" (Error.to_string_hum e));
    []
  | Ok json ->
    let output_index () =
      Option.value (int_member "output_index" json) ~default:0
    in
    let delta () = Option.value (member "delta" json) ~default:"" in
    let item () = Json.member "item" json in
    let item_type () = Option.bind (item ()) ~f:(member "type") in
    (match Option.value (member "type" json) ~default:"" with
     | "response.output_item.added" ->
       (match item_type (), item () with
        | Some "function_call", Some item ->
          let tool_index = state.tool_count in
          state.tool_count <- tool_index + 1;
          state.tools <- (output_index (), tool_index) :: state.tools;
          state.saw_tool_call <- true;
          [ Assistant_event.Tool_call_start
              { index = tool_index
              ; id = Option.value (member "call_id" item) ~default:""
              ; name = Option.value (member "name" item) ~default:""
              }
          ]
        | _ -> [])
     | "response.output_text.delta" | "response.refusal.delta" ->
       let d = delta () in
       if String.is_empty d then [] else [ Text_delta d ]
     | "response.reasoning_summary_text.delta" | "response.reasoning_text.delta"
       ->
       let d = delta () in
       if String.is_empty d then [] else [ Thinking_delta d ]
     | "response.reasoning_summary_part.added" ->
       if Option.value (int_member "summary_index" json) ~default:0 > 0
       then [ Thinking_delta "\n\n" ]
       else []
     | "response.function_call_arguments.delta" ->
       (match
          List.Assoc.find state.tools ~equal:Int.equal (output_index ())
        with
        | Some index ->
          let d = delta () in
          if String.is_empty d
          then []
          else (
            state.tool_arguments_seen <- index :: state.tool_arguments_seen;
            [ Tool_call_delta { index; arguments = d } ])
        | None -> [])
     | "response.output_item.done" ->
       (match item_type (), item () with
        | Some "reasoning", Some item ->
          (match Json.member "encrypted_content" item with
           | Some (`String _) -> [ Thinking_signature (Json.to_string item) ]
           | _ -> [])
        | Some "function_call", Some item ->
          (match
             List.Assoc.find state.tools ~equal:Int.equal (output_index ())
           with
           | Some index
             when not
                    (List.mem state.tool_arguments_seen index ~equal:Int.equal)
             ->
             (match member "arguments" item with
              | Some arguments when not (String.is_empty arguments) ->
                [ Tool_call_delta { index; arguments } ]
              | _ -> [])
           | _ -> [])
        | _ -> [])
     | "response.completed" | "response.incomplete" ->
       Option.iter (Json.member "response" json) ~f:(fun response ->
         Option.iter (Json.member "usage" response) ~f:(fun u ->
           state.usage <- parse_usage u);
         Option.iter (Json.member "incomplete_details" response) ~f:(fun d ->
           state.incomplete_reason
           <- Some (Option.value (member "reason" d) ~default:"incomplete")));
       []
     | "response.failed" ->
       let message =
         Option.bind (Json.member "response" json) ~f:(Json.member "error")
         |> Option.bind ~f:(member "message")
         |> Option.value ~default:event.data
       in
       state.error <- Some message;
       []
     | "error" ->
       let message =
         Option.first_some
           (member "message" json)
           (Option.bind (Json.member "error" json) ~f:(member "message"))
         |> Option.value ~default:event.data
       in
       state.error <- Some message;
       []
     | _ -> [])
;;

let stop_reason (state : Stream_state.t) : Stop_reason.t =
  match state.error, state.incomplete_reason with
  | Some e, _ -> Error e
  | None, Some "max_output_tokens" -> Length
  | None, Some reason -> Error ("incomplete: " ^ reason)
  | None, None -> if state.saw_tool_call then Tool_use else End_turn
;;

let stream
      ~env
      ~base_url
      ~timeout
      ~(endpoint : Endpoint.t)
      (request : Provider.Request.t)
      ~cancel
      ~on_event
  =
  let builder = Assistant_builder.create ~model:(Model.key request.model) in
  let state = Stream_state.create () in
  let outcome =
    Sse_request.run
      ~env
      ?timeout
      ~cancel
      ~url:(Endpoint.url endpoint ~base_url)
      ~headers:(Endpoint.headers endpoint)
      ~body:(Json.to_string (request_body ~endpoint request))
      ~on_event:(fun event ->
        List.iter (parse_event state event) ~f:(fun e ->
          Assistant_builder.apply builder e;
          on_event e))
      ()
  in
  let stop_reason : Stop_reason.t =
    match outcome with
    | Aborted -> Aborted
    | Failed e -> Error e
    | Completed -> stop_reason state
  in
  Assistant_builder.finish builder ~stop_reason ~usage:state.usage
;;

let create ~env ?base_url ?timeout ~endpoint () =
  let base_url =
    Option.value base_url ~default:(Endpoint.default_base_url endpoint)
  in
  { Provider.name = Provider_id.to_string (Endpoint.provider_id endpoint)
  ; stream = stream ~env ~base_url ~timeout ~endpoint
  }
;;

module For_testing = struct
  let request_body = request_body
  let headers = Endpoint.headers

  let parse_events events =
    let state = Stream_state.create () in
    let events =
      List.concat_map events ~f:(fun data ->
        parse_event state { Sse.Event.event = None; data; id = None })
    in
    events, stop_reason state, state.usage
  ;;
end
