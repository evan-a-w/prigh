open! Core

type t =
  | Unknown
  | Focused
  | Unfocused
[@@deriving sexp_of, equal]
