open! Core

type t =
  | Model
  | Thinking
  | Mode
  | Host
[@@deriving sexp_of, equal, enumerate]

let step t delta ~skip =
  let chips = Array.of_list all in
  let n = Array.length chips in
  let index =
    Option.value_map
      (Array.findi chips ~f:(fun _ c -> equal c t))
      ~default:0
      ~f:fst
  in
  let rec go i tries =
    let c = chips.((i + delta) % n) in
    if tries > 0 && skip c then go (i + delta) (tries - 1) else c
  in
  go index n
;;
