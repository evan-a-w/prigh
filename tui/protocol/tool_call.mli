open! Core

type t =
  { id : string
  ; name : string
  ; arguments : string
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
