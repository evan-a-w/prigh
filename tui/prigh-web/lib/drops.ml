open! Core

type t =
  { lost : int
  ; settling : bool
  ; resyncing : bool
  ; rejoined : bool
  }
[@@deriving sexp_of, equal]

let initial = { lost = 0; settling = true; resyncing = false; rejoined = false }
let give_up_after = 6

let rejoined t ~resyncing =
  { t with settling = true; resyncing; rejoined = true }
;;

let resynced t = { t with resyncing = false }
let settled t = { t with lost = 0; settling = false; resyncing = false }

let lost t =
  { t with
    lost = (if t.settling then t.lost + 1 else 0)
  ; settling = true
  ; resyncing = false
  }
;;

let gave_up t = t.lost >= give_up_after
let retry t = { t with lost = Int.min t.lost (give_up_after - 1) }
