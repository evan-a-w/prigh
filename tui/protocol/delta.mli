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

val of_json : Json.t -> t Or_error.t
