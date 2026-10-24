open! Core

type t =
  | Debug
  | Info
  | Warn
  | Error
[@@deriving sexp_of, equal]
