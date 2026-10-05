open! Core
open! Import

type t =
  { message : Message.t
  ; at : float option
  }
[@@deriving sexp_of]
