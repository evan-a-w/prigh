open! Core
open! Import

type t =
  { listed : Worktree.Listing.t String.Map.t
  ; asked : String.Set.t
  }
[@@deriving sexp_of]

let empty = { listed = String.Map.empty; asked = String.Set.empty }
let wanted t roots = List.filter roots ~f:(fun r -> not (Set.mem t.asked r))

let requested t roots =
  { t with asked = Set.union t.asked (String.Set.of_list roots) }
;;

let loaded t ~root listing =
  { t with listed = Map.set t.listed ~key:root ~data:listing }
;;

let failed t ~root = { t with listed = Map.remove t.listed root }
let stale t = { t with asked = String.Set.empty }
let listings t = Map.data t.listed
