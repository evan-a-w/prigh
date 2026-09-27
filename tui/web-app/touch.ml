open! Core

type t =
  { start_x : float
  ; start_y : float
  ; last_y : float
  ; carry : float
  ; moved : bool
  }
[@@deriving sexp_of]

let slop = 10.

let start ~x ~y =
  { start_x = x; start_y = y; last_y = y; carry = 0.; moved = false }
;;

let move t ~x ~y ~step =
  let moved =
    t.moved
    || Float.(abs (x - t.start_x) > slop)
    || Float.(abs (y - t.start_y) > slop)
  in
  let pending = t.carry +. (t.last_y -. y) in
  let step = Float.max 1. step in
  let steps = Float.to_int (pending /. step) in
  let carry = pending -. (Float.of_int steps *. step) in
  { t with last_y = y; carry; moved }, steps
;;

let finish t = if t.moved then `Swipe else `Tap
