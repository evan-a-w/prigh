open! Core

type t =
  { input : float
  ; output : float
  ; cache_read : float
  }
[@@deriving sexp_of, equal]

val zero : t
val of_json : Json.t -> t Or_error.t
