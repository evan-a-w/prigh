open! Core

type t =
  { steer : int
  ; follow_up : int
  }
[@@deriving sexp_of, equal]

val zero : t
val total : t -> int
