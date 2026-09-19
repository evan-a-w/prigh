open! Core

type t =
  | Info
  | Warn
  | Error
[@@deriving sexp_of, equal]
