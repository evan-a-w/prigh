open! Core

type t =
  { id : string
  ; path : string
  ; cwd : string
  ; created_at : string
  ; first_prompt : string option
  ; message_count : int
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
