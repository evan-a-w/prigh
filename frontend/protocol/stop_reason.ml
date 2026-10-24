open! Core

type t =
  | End_turn
  | Tool_use
  | Length
  | Aborted
  | Error of string
[@@deriving sexp_of, equal]

let of_json j =
  match%bind.Or_error Json.string_field j "type" with
  | "end_turn" -> Ok End_turn
  | "tool_use" -> Ok Tool_use
  | "length" -> Ok Length
  | "aborted" -> Ok Aborted
  | "error" ->
    Or_error.map (Json.string_field j "message") ~f:(fun m -> Error m)
  | other -> Or_error.errorf "unknown stop reason %S" other
;;
