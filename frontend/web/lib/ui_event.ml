open! Core
open! Import

type t =
  | Set of
      { key : string
      ; value : string
      }
  | Send of Ui_action.t
[@@deriving sexp_of, equal]
