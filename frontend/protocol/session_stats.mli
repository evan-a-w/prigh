open! Core

type t =
  { message_count : int
  ; turns : int
  ; tool_calls : (string * int) list
  ; usage : Usage.t
  ; cost_usd : float
  ; context_percent : float
  ; model_changes : int
  ; compactions : int
  ; duration_seconds : float
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
