open! Core
open! Import

module Key = struct
  type t =
    | Worktree_hours
    | Archive_days
    | Compact_at
  [@@deriving sexp_of, equal, compare]
end

type t = (Key.t * float) list [@@deriving sexp_of, equal]

let empty = []

let remember t key value =
  if Float.(value > 0.)
  then (key, value) :: List.Assoc.remove t key ~equal:Key.equal
  else t
;;

let value t key ~default =
  Option.value (List.Assoc.find t key ~equal:Key.equal) ~default
;;
