open! Core
open! Import

let str s = `String s
let int i = `Number (Int.to_string i)
let float f = `Number (sprintf "%.15g" f)
let bool b = if b then `True else `False
let field name json = Option.value (Json.member name json) ~default:`Null

let string_field name json =
  Option.value (Json.string (field name json)) ~default:""
;;

let int_field name json = Option.value (Json.int (field name json)) ~default:0

let float_field name json =
  Option.value (Json.float (field name json)) ~default:0.
;;

let bool_field name json =
  Option.value (Json.bool (field name json)) ~default:false
;;

let list_field name json =
  Option.value (Json.list (field name json)) ~default:[]
;;

let opt_string_field name json =
  match field name json with
  | `String s -> Some s
  | _ -> None
;;

module Thinking_level = struct
  let of_prigh = function
    | "on" -> "medium"
    | ("off" | "low" | "high" | "max") as s -> s
    | _ -> "off"
  ;;

  let to_prigh = function
    | "minimal" -> "low"
    | "medium" -> "on"
    | "xhigh" -> "max"
    | s -> s
  ;;

  let all = [ "off"; "low"; "medium"; "high"; "max" ]

  let available ~supports_thinking =
    if supports_thinking then all else [ "off" ]
  ;;

  let next ~supports_thinking ~current =
    let levels = available ~supports_thinking in
    let rec go = function
      | l :: next :: _ when String.equal l current -> next
      | [ l ] when String.equal l current -> List.hd_exn levels
      | _ :: rest -> go rest
      | [] -> List.hd_exn levels
    in
    go levels
  ;;
end

let arguments_object s =
  match Json.parse s with
  | Ok (`Object _ as o) -> o
  | Ok _ | Error _ -> `Object []
;;

let text_block text = `Object [ "type", str "text"; "text", str text ]

let image_blocks json =
  List.map (list_field "images" json) ~f:(fun image ->
    `Object
      [ "type", str "image"
      ; "data", field "data" image
      ; "mimeType", field "mime_type" image
      ])
;;

let tool_result_content ?(images = []) text = `Array (text_block text :: images)

let prigh_images json =
  List.filter_map (list_field "images" json) ~f:(fun image ->
    match field "data" image, field "mimeType" image with
    | (`String _ as data), (`String _ as mime_type) ->
      Some (`Object [ "mime_type", mime_type; "data", data ])
    | _ -> None)
;;

let stop_reason json =
  let reason = string_field "type" json in
  let pi =
    match reason with
    | "end_turn" -> "stop"
    | "tool_use" -> "toolUse"
    | "length" -> "length"
    | "aborted" -> "aborted"
    | _ -> "error"
  in
  let error =
    if String.equal pi "error"
    then [ "errorMessage", str (string_field "message" json) ]
    else []
  in
  ("stopReason", str pi) :: error
;;

let content_block json =
  match string_field "type" json with
  | "thinking" ->
    `Object
      [ "type", str "thinking"; "thinking", str (string_field "text" json) ]
  | "tool_call" ->
    `Object
      [ "type", str "toolCall"
      ; "id", str (string_field "id" json)
      ; "name", str (string_field "name" json)
      ; "arguments", arguments_object (string_field "arguments" json)
      ]
  | _ -> `Object [ "type", str "text"; "text", str (string_field "text" json) ]
;;

let message ~timestamp json =
  let ts = [ "timestamp", int timestamp ] in
  match string_field "role" json with
  | "assistant" ->
    let model = string_field "model" json in
    `Object
      ([ "role", str "assistant"
       ; ( "content"
         , `Array (List.map (list_field "content" json) ~f:content_block) )
       ; "provider", str "prigh"
       ; "model", str model
       ]
       @ stop_reason (field "stop_reason" json)
       @ ts)
  | "tool_result" ->
    `Object
      ([ "role", str "toolResult"
       ; "toolCallId", str (string_field "tool_call_id" json)
       ; "toolName", str (string_field "tool_name" json)
       ; ( "content"
         , tool_result_content
             ~images:(image_blocks json)
             (string_field "text" json) )
       ; "isError", bool (bool_field "is_error" json)
       ]
       @ ts)
  | _ ->
    let text = string_field "text" json in
    let content =
      match image_blocks json with
      | [] -> str text
      | images ->
        `Array
          ((if String.is_empty text then [] else [ text_block text ]) @ images)
    in
    `Object ([ "role", str "user"; "content", content ] @ ts)
;;

let model json =
  `Object
    [ "id", str (string_field "id" json)
    ; "name", str (string_field "name" json)
    ; "provider", str (string_field "provider" json)
    ; "reasoning", bool (bool_field "supports_thinking" json)
    ; "contextWindow", int (int_field "context_window" json)
    ]
;;

let session_state json =
  let name =
    match opt_string_field "session_name" json with
    | Some n -> [ "sessionName", str n ]
    | None -> []
  in
  `Object
    ([ "model", model (field "model" json)
     ; "cwd", str (string_field "cwd" json)
     ; ( "thinkingLevel"
       , str (Thinking_level.of_prigh (string_field "thinking" json)) )
     ; "isStreaming", bool (bool_field "running" json)
     ; "isCompacting", `False
     ; "steeringMode", str "all"
     ; "followUpMode", str "all"
     ; "sessionFile", str (string_field "session_path" json)
     ; "sessionId", str (string_field "session_id" json)
     ; "autoCompactionEnabled", `True
     ; "messageCount", int (int_field "message_count" json)
     ; "pendingMessageCount", int 0
     ]
     @ name)
;;

let session_stats ~state ~stats =
  let usage = field "usage" stats in
  let input = int_field "input" usage in
  let output = int_field "output" usage in
  let cache_read = int_field "cache_read" usage in
  let tool_calls =
    match field "tool_calls" stats with
    | `Object counts ->
      List.sum
        (module Int)
        counts
        ~f:(fun (_, n) -> Option.value (Json.int n) ~default:0)
    | _ -> 0
  in
  let total = int_field "message_count" stats in
  let turns = int_field "turns" stats in
  let context_window = int_field "context_window" (field "model" state) in
  `Object
    [ "sessionFile", str (string_field "session_path" state)
    ; "sessionId", str (string_field "session_id" state)
    ; "userMessages", int (Int.max 0 (total - turns - tool_calls))
    ; "assistantMessages", int turns
    ; "toolCalls", int tool_calls
    ; "toolResults", int tool_calls
    ; "totalMessages", int total
    ; ( "tokens"
      , `Object
          [ "input", int input
          ; "output", int output
          ; "cacheRead", int cache_read
          ; "cacheWrite", int 0
          ; "total", int (input + output + cache_read)
          ] )
    ; "cost", float (float_field "cost_usd" stats)
    ; ( "contextUsage"
      , `Object
          [ "tokens", int (int_field "context_tokens" state)
          ; "contextWindow", int context_window
          ; "percent", float (float_field "context_percent" stats)
          ] )
    ]
;;

let builtin_commands =
  [ "compact", "Compact the context", Some "[instructions]"
  ; "new", "Start a new session", None
  ; "name", "Name the session", Some "<name>"
  ; "model", "Pick a model", Some "[name]"
  ; "thinking", "Set or cycle the thinking level", Some "[level]"
  ; "session", "Show session info", None
  ; "export", "Export the session as markdown (on the backend)", Some "[path]"
  ; "copy", "Copy the last assistant message", None
  ; "fork", "Fork the session from an earlier message", None
  ; "clone", "Clone the session", None
  ; "cd", "Change the working directory", Some "[dir]"
  ]
;;

let extension_commands =
  [ "login", "Log in to a provider", Some "[provider] [oauth|api_key]"
  ; "logout", "Log out of a provider", Some "<provider>"
  ; "auth", "Show provider credentials", None
  ; "sessions", "Pick a saved session to switch to", None
  ; "switch", "Switch to a session by id or path", Some "<id|path>"
  ; "host", "Pick where tools run", None
  ; "setusr", "Act as another user (superusers)", Some "[user]"
  ; ( "change_default"
    , "Save the current model and thinking level as the default"
    , None )
  ; "help", "List the commands", None
  ]
;;

let server_commands = List.map extension_commands ~f:(fun (n, _, _) -> n)

let command ~source (name, description, hint) =
  `Object
    ([ "name", str name; "description", str description; "source", str source ]
     @ Option.value_map hint ~default:[] ~f:(fun h -> [ "argumentHint", str h ])
    )
;;

let commands =
  `Object
    [ ( "commands"
      , `Array
          (List.map builtin_commands ~f:(command ~source:"builtin")
           @ List.map extension_commands ~f:(command ~source:"extension")) )
    ]
;;

let response ~id ~command result =
  let base = [ "id", id; "type", str "response"; "command", str command ] in
  match result with
  | Ok data -> `Object (base @ [ "success", `True; "data", data ])
  | Error e ->
    `Object (base @ [ "success", `False; "error", str (Error.to_string_hum e) ])
;;

let event name fields = `Object (("type", str name) :: fields)

let ui_request ~id ~meth fields =
  `Object
    ([ "type", str "extension_ui_request"; "id", str id; "method", str meth ]
     @ fields)
;;

let notify_seq = ref 0

let notify ?(kind = "info") message =
  incr notify_seq;
  ui_request
    ~id:(sprintf "notify-%d" !notify_seq)
    ~meth:"notify"
    [ "message", str message; "notifyType", str kind ]
;;

let custom_message ~timestamp ~kind text =
  `Object
    [ "role", str "custom"
    ; "customType", str kind
    ; "content", str text
    ; "display", `True
    ; "timestamp", int timestamp
    ]
;;
