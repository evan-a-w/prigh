open! Core
open! Import

let str s = `String s
let int i = `Number (Int.to_string i)
let float f = `Number (sprintf "%.15g" f)
let bool b = if b then `True else `False

let usage (u : Usage.t) =
  `Object
    [ "input", int u.input
    ; "output", int u.output
    ; "cache_read", int u.cache_read
    ]
;;

let stop_reason (r : Stop_reason.t) =
  match r with
  | End_turn -> `Object [ "type", str "end_turn" ]
  | Tool_use -> `Object [ "type", str "tool_use" ]
  | Length -> `Object [ "type", str "length" ]
  | Aborted -> `Object [ "type", str "aborted" ]
  | Error message -> `Object [ "type", str "error"; "message", str message ]
;;

let tool_call (c : Content.Tool_call.t) =
  `Object [ "id", str c.id; "name", str c.name; "arguments", str c.arguments ]
;;

let content (c : Content.t) =
  match c with
  | Text text -> `Object [ "type", str "text"; "text", str text ]
  | Thinking text -> `Object [ "type", str "thinking"; "text", str text ]
  | Tool_call call ->
    `Object
      [ "type", str "tool_call"
      ; "id", str call.id
      ; "name", str call.name
      ; "arguments", str call.arguments
      ]
;;

let tool_result (r : Message.Tool_result.t) =
  `Object
    [ "role", str "tool_result"
    ; "tool_call_id", str r.tool_call_id
    ; "tool_name", str r.tool_name
    ; "text", str r.text
    ; "is_error", bool r.is_error
    ]
;;

let assistant (a : Message.Assistant.t) =
  `Object
    [ "role", str "assistant"
    ; "content", `Array (List.map a.content ~f:content)
    ; "stop_reason", stop_reason a.stop_reason
    ; "usage", usage a.usage
    ; "model", str a.model
    ]
;;

let message (m : Message.t) =
  match m with
  | User u -> `Object [ "role", str "user"; "text", str u.text ]
  | Assistant a -> assistant a
  | Tool_result r -> tool_result r
;;

let delta (d : Assistant_event.t) =
  match d with
  | Text_delta text -> `Object [ "type", str "text_delta"; "text", str text ]
  | Thinking_delta text ->
    `Object [ "type", str "thinking_delta"; "text", str text ]
  | Tool_call_start { index; id; name } ->
    `Object
      [ "type", str "tool_call_start"
      ; "index", int index
      ; "id", str id
      ; "name", str name
      ]
  | Tool_call_delta { index; arguments } ->
    `Object
      [ "type", str "tool_call_delta"
      ; "index", int index
      ; "arguments", str arguments
      ]
;;

let thinking (t : Thinking.t) =
  match t with
  | Off -> str "off"
  | On None -> str "on"
  | On (Some level) -> str (Thinking.Level.to_string level)
;;

let thinking_of_string s : Thinking.t Or_error.t =
  match String.lowercase s with
  | "off" -> Ok Off
  | "on" -> Ok (On None)
  | s ->
    (match Thinking.Level.of_string s with
     | Some level -> Ok (On (Some level))
     | None ->
       Or_error.error_string "thinking must be one of: off, on, low, high, max")
;;

let model (m : Model.t) =
  `Object
    [ "id", str m.id
    ; "name", str m.name
    ; "context_window", int m.context_window
    ; "max_output", int m.max_output
    ; "supports_thinking", bool m.supports_thinking
    ; ( "cost"
      , `Object
          [ "input", float m.cost.input
          ; "output", float m.cost.output
          ; "cache_read", float m.cost.cache_read
          ] )
    ]
;;

let state (s : Agent.State.t) =
  `Object
    [ "session_id", str s.session_id
    ; "session_path", str s.session_path
    ; "cwd", str s.cwd
    ; "model", model s.model
    ; "thinking", thinking s.thinking
    ; "running", bool s.running
    ; "message_count", int s.message_count
    ; "usage", usage s.usage
    ; "cost_usd", float s.cost_usd
    ; "context_tokens", int s.context_tokens
    ]
;;

let session_summary (s : Session.Summary.t) =
  `Object
    [ "id", str s.id
    ; "path", str s.path
    ; "cwd", str s.cwd
    ; "created_at", str s.created_at
    ; "first_prompt", Option.value_map s.first_prompt ~default:`Null ~f:str
    ; "message_count", int s.message_count
    ]
;;

let entry (e : Session.Entry.t) =
  let payload =
    match e.payload with
    | Message m -> [ "kind", str "message"; "message", message m ]
    | Model { model; thinking = t } ->
      [ "kind", str "model"; "model", str model; "thinking", thinking t ]
    | Compaction { summary; kept_from } ->
      [ "kind", str "compaction"
      ; "summary", str summary
      ; "kept_from", str kept_from
      ]
  in
  `Object
    ([ "id", str e.id
     ; "parent", Option.value_map e.parent ~default:`Null ~f:str
     ]
     @ payload)
;;

let event (e : Agent.Event.t) =
  let fields =
    match e with
    | Loop Agent_start -> [ "event", str "agent_start" ]
    | Loop (Agent_end added) ->
      [ "event", str "agent_end"
      ; "messages", `Array (List.map added ~f:message)
      ]
    | Loop Turn_start -> [ "event", str "turn_start" ]
    | Loop (Turn_end { assistant = a; tool_results }) ->
      [ "event", str "turn_end"
      ; "assistant", assistant a
      ; "tool_results", `Array (List.map tool_results ~f:tool_result)
      ]
    | Loop (Message_start m) ->
      [ "event", str "message_start"; "message", message m ]
    | Loop (Message_update { partial; delta = d }) ->
      [ "event", str "message_update"
      ; "partial", assistant partial
      ; "delta", delta d
      ]
    | Loop (Message_end m) ->
      [ "event", str "message_end"; "message", message m ]
    | Loop (Tool_start call) ->
      [ "event", str "tool_start"; "call", tool_call call ]
    | Loop (Tool_output { call_id; chunk }) ->
      [ "event", str "tool_output"; "call_id", str call_id; "chunk", str chunk ]
    | Loop (Tool_end { call; result }) ->
      [ "event", str "tool_end"
      ; "call", tool_call call
      ; "result", tool_result result
      ]
    | State_changed s -> [ "event", str "state"; "state", state s ]
    | Compacted { summary } ->
      [ "event", str "compacted"; "summary", str summary ]
    | Notice text -> [ "event", str "notice"; "text", str text ]
  in
  `Object (("type", str "event") :: fields)
;;
