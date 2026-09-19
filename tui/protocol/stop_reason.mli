open! Core

type t =
  | End_turn
  | Tool_use
  | Length
  | Aborted
  | Error of string
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
