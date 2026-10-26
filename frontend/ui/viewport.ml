open! Core

type t =
  | Follow
  | Anchored of
      { top : int
      ; new_lines : int
      ; new_items : int
      }
[@@deriving sexp_of, equal]

let anchored top = Anchored { top; new_lines = 0; new_items = 0 }
