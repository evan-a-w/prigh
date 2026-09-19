open! Core
open! Import

type t =
  { name : string
  ; description : string
  ; parameters : Json.t
  }
[@@deriving sexp_of]
