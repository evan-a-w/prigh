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

module Frame_kind = struct
  type t =
    | Binary
    | Text
  [@@deriving sexp_of, equal]

  let of_string = function
    | "binary" -> Ok Binary
    | "text" -> Ok Text
    | other -> Or_error.errorf "unknown frame kind %S" other
  ;;

  let to_string = function
    | Binary -> "binary"
    | Text -> "text"
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
  | Retry of Retry.t
  | Tool_start of Tool_call.t
  | Tool_output of
      { call_id : string
      ; chunk : string
      }
  | Tool_end of
      { call : Tool_call.t
      ; result : Message.Tool_result.t
      }
  | Tool_confirm of Pending.Confirm.t
  | Tool_confirm_done of
      { call_id : string
      ; agent_id : string option
      } (** answered by any frontend, let through or withdrawn: close it *)
  | Ask_user of Pending.Ask.t
  | Ask_user_done of
      { call_id : string
      ; timed_out : bool
        (** nobody answered in time: the agent carries on (a [Notice] says
            so) *)
      } (** answered (by any frontend), timed out or withdrawn *)
  | State of State.t
  | Compacted of string
  | Config_changed of Config.t
  | Notice of string
  | Queue_update of
      { steer : string list
      ; follow_up : string list
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
  | Tool_exec of
      { exec_id : string
      ; call_id : string
      ; name : string
      ; arguments : Json.t
      ; cwd : string
      ; stdin : bool (** keep a stdin pipe open: a background job *)
      } (** run this tool on our machine (we are the active host) *)
  | Tool_exec_cancel of string
  | Tool_exec_input of
      { exec_id : string
      ; data : string
      ; close : bool
      } (** text for a running exec's stdin, then maybe end it *)
  | Terminal_open of
      { term_id : string
      ; key : string
      ; cwd : string
      ; cols : int
      ; rows : int
      } (** relayed to our tool host worker (we are the active host) *)
  | Terminal_frame of
      { term_id : string
      ; kind : Frame_kind.t
      ; data : string
      }
  | Terminal_close of string
  | Terminal_kill of string
  | Btw_delta of
      { btw_id : string
      ; delta : string
      } (** streamed answer to our own [btw] request *)
  | Sessions_changed
  | Prefs_changed of Prefs.Change.t list
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
  | "retry" -> Retry.of_json j >>| fun r -> Retry r
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
  | "tool_confirm" -> Pending.Confirm.of_json j >>| fun c -> Tool_confirm c
  | "tool_confirm_done" ->
    let%bind call_id = Json.string_field j "call_id" in
    let%map agent_id = Json.string_opt_field j "agent_id" in
    Tool_confirm_done { call_id; agent_id }
  | "ask_user_done" ->
    let%bind call_id = Json.string_field j "call_id" in
    let%map timed_out = Json.bool_default_field j "timed_out" ~default:false in
    Ask_user_done { call_id; timed_out }
  | "ask_user" -> Pending.Ask.of_json j >>| fun a -> Ask_user a
  | "state" ->
    Json.object_field j "state" >>= State.of_json >>| fun s -> State s
  | "compacted" -> Json.string_field j "summary" >>| fun s -> Compacted s
  | "config_changed" ->
    Json.object_field j "config"
    >>= Config.of_json
    >>| fun c -> Config_changed c
  | "notice" -> Json.string_field j "text" >>| fun t -> Notice t
  | "sessions_changed" -> Ok Sessions_changed
  | "prefs_changed" ->
    Json.object_field j "prefs"
    >>= Prefs.changes_of_json
    >>| fun changes -> Prefs_changed changes
  | "queue_update" ->
    let texts name = Json.list_field j name ~f:Json.to_string_or_error in
    let%bind steer = texts "steer_texts" in
    let%map follow_up = texts "follow_up_texts" in
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
  | "tool_exec" ->
    let%bind exec_id = Json.string_field j "exec_id" in
    let%bind call_id = Json.string_field j "call_id" in
    let%bind name = Json.string_field j "name" in
    let%bind arguments = Json.object_field j "arguments" in
    let%bind cwd = Json.string_field j "cwd" in
    let%map stdin = Json.bool_default_field j "stdin" ~default:false in
    Tool_exec { exec_id; call_id; name; arguments; cwd; stdin }
  | "tool_exec_cancel" ->
    Json.string_field j "exec_id" >>| fun id -> Tool_exec_cancel id
  | "tool_exec_input" ->
    let%bind exec_id = Json.string_field j "exec_id" in
    let%bind data = Json.string_field j "data" in
    let%map close = Json.bool_default_field j "close" ~default:false in
    Tool_exec_input { exec_id; data; close }
  | "terminal_open" ->
    let%bind term_id = Json.string_field j "term_id" in
    let%bind key = Json.string_field j "key" in
    let%bind cwd = Json.string_field j "cwd" in
    let%bind cols = Json.int_field j "cols" in
    let%map rows = Json.int_field j "rows" in
    Terminal_open { term_id; key; cwd; cols; rows }
  | "terminal_frame" ->
    let%bind term_id = Json.string_field j "term_id" in
    let%bind kind = Json.string_field j "kind" >>= Frame_kind.of_string in
    let%map data = Json.string_field j "data" in
    Terminal_frame { term_id; kind; data }
  | "terminal_close" ->
    Json.string_field j "term_id" >>| fun id -> Terminal_close id
  | "terminal_kill" -> Json.string_field j "key" >>| fun key -> Terminal_kill key
  | "btw_delta" ->
    let%bind btw_id = Json.string_field j "btw_id" in
    let%map delta = Json.string_field j "delta" in
    Btw_delta { btw_id; delta }
  | other -> Or_error.errorf "unknown event %S" other
;;
