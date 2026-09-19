open! Core

module Subagent_result = struct
  type t =
    { text : string
    ; is_error : bool
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind text = Json.string_field j "text" in
    let%map is_error = Json.bool_field j "is_error" in
    { text; is_error }
  ;;
end

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
  | Tool_confirm of
      { call_id : string
      ; name : string
      ; summary : string
      }
  | State of State.t
  | Compacted of string
  | Config_changed of Config.t
  | Notice of string
  | Queue_update of
      { steer : int
      ; follow_up : int
      }
  | Subagent_start of
      { call_id : string
      ; agent_id : string
      ; task : string
      ; model : string
      ; tools : string list
      }
  | Subagent of
      { call_id : string
      ; agent_id : string
      ; event : t
      }
  | Subagent_end of
      { call_id : string
      ; agent_id : string
      ; usage : Usage.t
      ; turns : int
      ; cost_usd : float
      ; result : Subagent_result.t
      }
  | Auth of Auth_event.t
[@@deriving sexp_of, equal]

let rec of_json j =
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
  | "tool_confirm" ->
    let%bind call_id = Json.string_field j "call_id" in
    let%bind name = Json.string_field j "name" in
    let%map summary = Json.string_field j "summary" in
    Tool_confirm { call_id; name; summary }
  | "state" ->
    Json.object_field j "state" >>= State.of_json >>| fun s -> State s
  | "compacted" -> Json.string_field j "summary" >>| fun s -> Compacted s
  | "config_changed" ->
    Json.object_field j "config"
    >>= Config.of_json
    >>| fun c -> Config_changed c
  | "notice" -> Json.string_field j "text" >>| fun t -> Notice t
  | "queue_update" ->
    let%bind steer = Json.int_field j "steer" in
    let%map follow_up = Json.int_field j "follow_up" in
    Queue_update { steer; follow_up }
  | "subagent_start" ->
    let%bind call_id = Json.string_field j "call_id" in
    let%bind agent_id = Json.string_field j "agent_id" in
    let%bind task = Json.string_field j "task" in
    let%bind model = Json.string_field j "model" in
    let%map tools = Json.list_field j "tools" ~f:Json.to_string_or_error in
    Subagent_start { call_id; agent_id; task; model; tools }
  | "subagent" ->
    let%bind call_id = Json.string_field j "call_id" in
    let%bind agent_id = Json.string_field j "agent_id" in
    let%bind inner = Json.object_field j "inner" in
    let%map event = of_json inner in
    Subagent { call_id; agent_id; event }
  | "subagent_end" ->
    let%bind call_id = Json.string_field j "call_id" in
    let%bind agent_id = Json.string_field j "agent_id" in
    let%bind usage = Json.object_field j "usage" >>= Usage.of_json in
    let%bind turns = Json.int_field j "turns" in
    let%bind cost_usd = Json.float_field j "cost_usd" in
    let%map result = Json.object_field j "result" >>= Subagent_result.of_json in
    Subagent_end { call_id; agent_id; usage; turns; cost_usd; result }
  | "auth" -> Auth_event.of_json j >>| fun a -> Auth a
  | other -> Or_error.errorf "unknown event %S" other
;;
