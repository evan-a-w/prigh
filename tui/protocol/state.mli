open! Core

type t =
  { session_id : string
  ; session_path : string
  ; cwd : string
  ; model : Model.t
  ; thinking : string
  ; running : bool
  ; message_count : int
  ; usage : Usage.t
  ; cost_usd : float
  ; context_tokens : int
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
