open! Core
open! Import

type t =
  { name : string
  ; description : string
  ; parameters : Json.t
  ; parallel_safe : bool
  ; destructive : bool
  }
[@@deriving sexp_of]
