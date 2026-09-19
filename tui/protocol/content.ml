open! Core

type t =
  | Text of string
  | Thinking of string
  | Tool_call of Tool_call.t
[@@deriving sexp_of, equal]

let of_json j =
  match%bind.Or_error Json.string_field j "type" with
  | "text" -> Or_error.map (Json.string_field j "text") ~f:(fun t -> Text t)
  | "thinking" ->
    Or_error.map (Json.string_field j "text") ~f:(fun t -> Thinking t)
  | "tool_call" -> Or_error.map (Tool_call.of_json j) ~f:(fun c -> Tool_call c)
  | other -> Or_error.errorf "unknown content type %S" other
;;
