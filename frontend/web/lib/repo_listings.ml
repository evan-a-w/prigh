open! Core
open! Import

type t =
  { listed : Worktree.Listing.t String.Map.t
  ; asked : String.Set.t
  ; ever_asked : String.Set.t
  ; settled : String.Set.t
  }
[@@deriving sexp_of]

let empty =
  { listed = String.Map.empty
  ; asked = String.Set.empty
  ; ever_asked = String.Set.empty
  ; settled = String.Set.empty
  }
;;

let wanted t roots = List.filter roots ~f:(fun r -> not (Set.mem t.asked r))

let requested t roots =
  let roots = String.Set.of_list roots in
  { t with
    asked = Set.union t.asked roots
  ; ever_asked = Set.union t.ever_asked roots
  }
;;

let loaded t ~root listing =
  { t with
    listed = Map.set t.listed ~key:root ~data:listing
  ; asked = Set.add t.asked root
  ; settled = Set.add t.settled root
  }
;;

let answered t ~root listing =
  if Set.mem t.ever_asked root then loaded t ~root listing else t
;;

let failed t ~root = { t with listed = Map.remove t.listed root }
let stale t = { t with asked = String.Set.empty; settled = String.Set.empty }

let keep t ~roots =
  if
    Map.for_alli t.listed ~f:(fun ~key ~data:_ -> Set.mem roots key)
    && Set.is_subset t.ever_asked ~of_:roots
  then t
  else
    { listed = Map.filter_keys t.listed ~f:(Set.mem roots)
    ; asked = Set.inter t.asked roots
    ; ever_asked = Set.inter t.ever_asked roots
    ; settled = Set.inter t.settled roots
    }
;;

let listings t = Map.data t.listed

let unsettled t =
  Map.keys t.listed |> List.filter ~f:(fun root -> not (Set.mem t.settled root))
;;
