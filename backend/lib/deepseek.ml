open! Core
open! Import

let default_base_url = "https://api.deepseek.com"

let wire_tool_call (c : Content.Tool_call.t) : Json.t =
  `Object
    [ "id", `String c.id
    ; "type", `String "function"
    ; ( "function"
      , `Object [ "name", `String c.name; "arguments", `String c.arguments ] )
    ]
;;

let wire_message (m : Message.t) : Json.t =
  match m with
  | User { text } -> `Object [ "role", `String "user"; "content", `String text ]
  | Tool_result { tool_call_id; text; tool_name = _; is_error = _ } ->
    `Object
      [ "role", `String "tool"
      ; "tool_call_id", `String tool_call_id
      ; "content", `String text
      ]
  | Assistant a ->
    let thinking = Message.Assistant.thinking a in
    let tool_calls = Message.Assistant.tool_calls a in
    `Object
      (List.concat
         [ [ "role", `String "assistant"
           ; "content", `String (Message.Assistant.text a)
           ]
         ; (if String.is_empty thinking
            then []
            else [ "reasoning_content", `String thinking ])
         ; (if List.is_empty tool_calls
            then []
            else
              [ "tool_calls", `Array (List.map tool_calls ~f:wire_tool_call) ])
         ])
;;

let wire_tool (t : Tool_spec.t) : Json.t =
  `Object
    [ "type", `String "function"
    ; ( "function"
      , `Object
          [ "name", `String t.name
          ; "description", `String t.description
          ; "parameters", t.parameters
          ] )
    ]
;;

let request_body (r : Provider.Request.t) : Json.t =
  let system =
    match r.system with
    | None -> []
    | Some s -> [ `Object [ "role", `String "system"; "content", `String s ] ]
  in
  let thinking =
    match r.thinking with
    | Off -> [ "thinking", `Object [ "type", `String "disabled" ] ]
    | On level ->
      [ "thinking", `Object [ "type", `String "enabled" ] ]
      @ Option.value_map level ~default:[] ~f:(fun level ->
        [ "reasoning_effort", `String (Thinking.Level.to_string level) ])
  in
  `Object
    (List.concat
       [ [ "model", `String r.model.id
         ; "messages", `Array (system @ List.map r.messages ~f:wire_message)
         ; "stream", `True
         ; "stream_options", `Object [ "include_usage", `True ]
         ]
       ; (if List.is_empty r.tools
          then []
          else [ "tools", `Array (List.map r.tools ~f:wire_tool) ])
       ; Option.value_map r.max_tokens ~default:[] ~f:(fun n ->
           [ "max_tokens", `Number (Int.to_string n) ])
       ; (if r.model.supports_thinking then thinking else [])
       ])
;;

module Chunk = struct
  type t =
    { events : Assistant_event.t list
    ; finish_reason : string option
    ; usage : Usage.t option
    }
  [@@deriving sexp_of]
end

let member_string name json =
  match Json.member name json with
  | Some (`String s) -> Some s
  | _ -> None
;;

let member_int name json = Option.bind (Json.member name json) ~f:Json.int

let parse_usage json =
  let get name = Option.value (member_int name json) ~default:0 in
  { Usage.input = get "prompt_tokens"
  ; output = get "completion_tokens"
  ; cache_read = get "prompt_cache_hit_tokens"
  }
;;

let parse_tool_call_delta json : Assistant_event.t list =
  let index = Option.value (member_int "index" json) ~default:0 in
  let function_ = Json.member "function" json in
  let name = Option.bind function_ ~f:(member_string "name") in
  let arguments = Option.bind function_ ~f:(member_string "arguments") in
  let start =
    match member_string "id" json, name with
    | Some id, Some name when not (String.is_empty id) ->
      [ Assistant_event.Tool_call_start { index; id; name } ]
    | _ -> []
  in
  let delta =
    match arguments with
    | Some arguments when not (String.is_empty arguments) ->
      [ Assistant_event.Tool_call_delta { index; arguments } ]
    | _ -> []
  in
  start @ delta
;;

let parse_chunk (json : Json.t) : Chunk.t Or_error.t =
  match Json.member "error" json with
  | Some err ->
    let message =
      Option.value (member_string "message" err) ~default:(Json.to_string err)
    in
    Or_error.error_string message
  | None ->
    let choice =
      match Json.member "choices" json with
      | Some (`Array (c :: _)) -> Some c
      | _ -> None
    in
    let delta = Option.bind choice ~f:(Json.member "delta") in
    let events =
      match delta with
      | None -> []
      | Some delta ->
        let text s =
          if String.is_empty s then [] else [ Assistant_event.Text_delta s ]
        in
        let thinking s =
          if String.is_empty s then [] else [ Assistant_event.Thinking_delta s ]
        in
        List.concat
          [ Option.value_map
              (member_string "reasoning_content" delta)
              ~default:[]
              ~f:thinking
          ; Option.value_map (member_string "content" delta) ~default:[] ~f:text
          ; (match Json.member "tool_calls" delta with
             | Some (`Array calls) ->
               List.concat_map calls ~f:parse_tool_call_delta
             | _ -> [])
          ]
    in
    let finish_reason = Option.bind choice ~f:(member_string "finish_reason") in
    let usage =
      match Json.member "usage" json with
      | Some (`Object _ as u) -> Some (parse_usage u)
      | _ -> None
    in
    Ok { Chunk.events; finish_reason; usage }
;;

let stop_reason_of_finish = function
  | Some "tool_calls" -> Stop_reason.Tool_use
  | Some "length" -> Length
  | Some "stop" | None -> End_turn
  | Some other -> Error ("unexpected finish_reason: " ^ other)
;;

let stream
      ~env
      ~base_url
      ~timeout
      ~api_key
      (request : Provider.Request.t)
      ~cancel
      ~on_event
  =
  let builder = Assistant_builder.create ~model:(Model.key request.model) in
  let finish_reason = ref None in
  let usage = ref Usage.zero in
  let api_error = ref None in
  let handle_event (event : Sse.Event.t) =
    if not (String.equal (String.strip event.data) "[DONE]")
    then (
      match Json.parse event.data with
      | Error e ->
        api_error
        := Some (sprintf "bad JSON in stream: %s" (Error.to_string_hum e))
      | Ok json ->
        (match parse_chunk json with
         | Error e -> api_error := Some (Error.to_string_hum e)
         | Ok chunk ->
           List.iter chunk.events ~f:(fun e ->
             Assistant_builder.apply builder e;
             on_event e);
           Option.iter chunk.finish_reason ~f:(fun r -> finish_reason := Some r);
           Option.iter chunk.usage ~f:(fun u -> usage := u)))
  in
  let outcome =
    Sse_request.run
      ~env
      ?timeout
      ~cancel
      ~url:(base_url ^ "/chat/completions")
      ~headers:[ "Authorization", "Bearer " ^ api_key ]
      ~body:(Json.to_string (request_body request))
      ~on_event:handle_event
      ()
  in
  let stop_reason : Stop_reason.t =
    match outcome with
    | Aborted -> Aborted
    | Failed e -> Error e
    | Completed ->
      (match !api_error with
       | Some e -> Error e
       | None -> stop_reason_of_finish !finish_reason)
  in
  Assistant_builder.finish builder ~stop_reason ~usage:!usage
;;

let create ~env ?(base_url = default_base_url) ?timeout ~api_key () =
  { Provider.name = "deepseek"
  ; stream = stream ~env ~base_url ~timeout ~api_key
  }
;;

module For_testing = struct
  let request_body = request_body

  module Chunk = Chunk

  let parse_chunk = parse_chunk
end
