open! Core

type t =
  | Follow
  | Anchored of
      { top : int
      ; new_lines : int
      }
[@@deriving sexp_of, equal]
