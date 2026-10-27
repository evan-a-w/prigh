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
  ; ranges : Shown.t String.Map.t
  ; restoring_ranges : String.Set.t
  ; deleting : Int.Set.t
  ; reveal_hidden : (string * int option) option
  }
[@@deriving sexp_of]

let empty =
  { shown = Folded
  ; restoring = false
  ; reveal = None
  ; ranges = String.Map.empty
  ; restoring_ranges = String.Set.empty
  ; deleting = Int.Set.empty
  ; reveal_hidden = None
  }
;;

let range t entry = Option.value (Map.find t.ranges entry) ~default:Shown.Folded
