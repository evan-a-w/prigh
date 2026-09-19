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
  | other -> Or_error.errorf "unknown delta type %S" other
;;
