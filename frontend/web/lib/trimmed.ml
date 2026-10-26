open! Core
open! Import

module Shown = struct
  type t =
    | Folded
    | Loading
    | Shown of Chat.t
  [@@deriving sexp_of]
end

type t =
  { shown : Shown.t
  ; restoring : bool
  ; reveal : string option
  }
[@@deriving sexp_of]

let empty = { shown = Folded; restoring = false; reveal = None }
