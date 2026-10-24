open! Core

type t =
  | Text_delta of string
  | Thinking_delta of string
  | Thinking_signature
  | Tool_call_start of
      { index : int
      ; id : string
      ; name : string
      }
  | Tool_call_delta of
      { index : int
      ; arguments : string
      }
  | Server_tool_use of
      { id : string
      ; name : string
      ; input : string
      }
  | Server_tool_result of
      { id : string
      ; output : string
      ; is_error : bool
      }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  match%bind Json.string_field j "type" with
  | "text_delta" -> Json.string_field j "text" >>| fun t -> Text_delta t
  | "thinking_delta" -> Json.string_field j "text" >>| fun t -> Thinking_delta t
  | "thinking_signature" -> Ok Thinking_signature
  | "tool_call_start" ->
    let%bind index = Json.int_field j "index" in
    let%bind id = Json.string_field j "id" in
    let%map name = Json.string_field j "name" in
    Tool_call_start { index; id; name }
  | "tool_call_delta" ->
    let%bind index = Json.int_field j "index" in
    let%map arguments = Json.string_field j "arguments" in
    Tool_call_delta { index; arguments }
  | "server_tool_use" ->
    let%bind id = Json.string_field j "id" in
    let%bind name = Json.string_field j "name" in
    let%map input = Json.string_field j "input" in
    Server_tool_use { id; name; input }
  | "server_tool_result" ->
    let%bind id = Json.string_field j "id" in
    let%bind output = Json.string_field j "output" in
    let%map is_error = Json.bool_default_field j "is_error" ~default:false in
    Server_tool_result { id; output; is_error }
  | other -> Or_error.errorf "unknown delta type %S" other
;;
