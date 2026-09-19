open! Core

type t =
  { input : int
  ; output : int
  ; cache_read : int
  }
[@@deriving sexp_of, equal]

val zero : t
val of_json : Json.t -> t Or_error.t
