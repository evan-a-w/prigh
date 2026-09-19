open! Core

type t =
  | Text of string
  | Thinking of string
  | Tool_call of Tool_call.t
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
