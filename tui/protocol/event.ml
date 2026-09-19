open! Core

type t =
  | Agent_start
  | Agent_end of Message.t list
  | Turn_start
  | Turn_end of
      { assistant : Message.Assistant.t
      ; tool_results : Message.Tool_result.t list
      }
  | Message_start of Message.t
  | Message_update of
      { partial : Message.Assistant.t
      ; delta : Delta.t
      }
  | Message_end of Message.t
  | Tool_start of Tool_call.t
  | Tool_output of
      { call_id : string
      ; chunk : string
      }
  | Tool_end of
      { call : Tool_call.t
      ; result : Message.Tool_result.t
      }
  | State of State.t
  | Compacted of string
  | Notice of string
  | Auth of Auth_event.t
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  match%bind Json.string_field j "event" with
  | "agent_start" -> Ok Agent_start
  | "agent_end" ->
    Json.list_field j "messages" ~f:Message.of_json >>| fun m -> Agent_end m
  | "turn_start" -> Ok Turn_start
  | "turn_end" ->
    let%bind assistant =
      Json.object_field j "assistant" >>= Message.Assistant.of_json
    in
    let%map tool_results =
      Json.list_field j "tool_results" ~f:Message.Tool_result.of_json
    in
    Turn_end { assistant; tool_results }
  | "message_start" ->
    Json.object_field j "message"
    >>= Message.of_json
    >>| fun m -> Message_start m
  | "message_update" ->
    let%bind partial =
      Json.object_field j "partial" >>= Message.Assistant.of_json
    in
    let%map delta = Json.object_field j "delta" >>= Delta.of_json in
    Message_update { partial; delta }
  | "message_end" ->
    Json.object_field j "message" >>= Message.of_json >>| fun m -> Message_end m
  | "tool_start" ->
    Json.object_field j "call" >>= Tool_call.of_json >>| fun c -> Tool_start c
  | "tool_output" ->
    let%bind call_id = Json.string_field j "call_id" in
    let%map chunk = Json.string_field j "chunk" in
    Tool_output { call_id; chunk }
  | "tool_end" ->
    let%bind call = Json.object_field j "call" >>= Tool_call.of_json in
    let%map result =
      Json.object_field j "result" >>= Message.Tool_result.of_json
    in
    Tool_end { call; result }
  | "state" ->
    Json.object_field j "state" >>= State.of_json >>| fun s -> State s
  | "compacted" -> Json.string_field j "summary" >>| fun s -> Compacted s
  | "notice" -> Json.string_field j "text" >>| fun t -> Notice t
  | "auth" -> Auth_event.of_json j >>| fun a -> Auth a
  | other -> Or_error.errorf "unknown event %S" other
;;
