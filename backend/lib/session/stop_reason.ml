open! Core
open! Import

type t =
  | End_turn
  | Tool_use
  | Length
  | Error of string
  | Aborted
[@@deriving sexp, jsonaf, equal]
