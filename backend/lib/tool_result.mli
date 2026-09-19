open! Core

type t =
  { text : string
  ; is_error : bool
  }
[@@deriving sexp_of]

val ok : string -> t
val error : string -> t
