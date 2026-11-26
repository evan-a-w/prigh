open! Import
open Js_of_ocaml

let num x =
  if Float.is_integer x && Float.(abs x < 1e15)
  then Int.to_string (Float.to_int x)
  else Js.to_string (Js.number_of_float x)##toString
;;

let round4 x =
  let r = Float.round_down ((x *. 1e4) +. 0.5) /. 1e4 in
  if Float.(r = 0.) then 0. else r
;;

module Linear = struct
  type t =
    { domain : float * float
    ; range : float * float
    }

  let apply { domain = d0, d1; range = r0, r1 } x =
    let t = if Float.(d1 = d0) then 0.5 else (x -. d0) /. (d1 -. d0) in
    (r0 *. (1. -. t)) +. (r1 *. t)
  ;;

  let invert { domain = d0, d1; range = r0, r1 } y =
    let t = if Float.(r1 = r0) then 0.5 else (y -. r0) /. (r1 -. r0) in
    (d0 *. (1. -. t)) +. (d1 *. t)
  ;;
end

module Band = struct
  type t =
    { start : float
    ; step : float
    ; bandwidth : float
    ; n : int
    ; reverse : bool
    }

  let create ?(padding_inner = 0.) ?(padding_outer = 0.) ?(point = false) ~n ~range () =
    let r0, r1 = range in
    let padding_inner = if point then 1. else padding_inner in
    let reverse = Float.(r1 < r0) in
    let start, stop = if reverse then r1, r0 else r0, r1 in
    let nf = Float.of_int n in
    let step =
      (stop -. start) /. Float.max 1. (nf -. padding_inner +. (padding_outer *. 2.))
    in
    let start = start +. ((stop -. start -. (step *. (nf -. padding_inner))) *. 0.5) in
    let bandwidth = step *. (1. -. padding_inner) in
    { start; step; bandwidth; n; reverse }
  ;;

  let apply t i =
    let i = if t.reverse then t.n - 1 - i else i in
    t.start +. (t.step *. Float.of_int i)
  ;;

  let bandwidth t = t.bandwidth
  let step t = t.step
  let count t = t.n

  let index_at t x =
    if t.n = 0
    then None
    else (
      let centre i = apply t i +. (t.bandwidth /. 2.) in
      let best = ref 0 in
      for i = 1 to t.n - 1 do
        if Float.(abs (centre i -. x) < abs (centre !best -. x)) then best := i
      done;
      Some !best)
  ;;
end

(* decimal.js-light, which Recharts uses, is exact on these short decimals;
   floats get there by dropping the last digits of noise. *)
let clean x =
  if Float.is_finite x then Float.of_string (sprintf "%.12g" x) else x
;;

let digit_count v =
  if Float.(v = 0.) then 1 else Float.to_int (Float.round_down (Float.log10 (Float.abs v))) + 1
;;

let adaptive_step rough ~allow_decimals ~correction =
  if Float.(rough <= 0.)
  then 0.
  else (
    let digits = digit_count rough in
    let scale_value = 10. **. Float.of_int digits in
    let ratio = clean (rough /. scale_value) in
    let ratio_scale = if digits <> 1 then 0.05 else 0.1 in
    let amend =
      clean
        ((Float.round_up (clean (ratio /. ratio_scale)) +. Float.of_int correction)
         *. ratio_scale)
    in
    let step = clean (amend *. scale_value) in
    if allow_decimals then step else Float.round_up step)
;;

let rec calculate_step ~min ~max ~count ~allow_decimals ~correction =
  let step =
    adaptive_step
      (clean ((max -. min) /. Float.of_int (count - 1)))
      ~allow_decimals
      ~correction
  in
  if Float.(step = 0.)
  then 0., 0., 0.
  else (
    let middle =
      if Float.(<=) min 0. && Float.(>=) max 0.
      then 0.
      else (
        let m = clean ((min +. max) /. 2.) in
        clean (m -. Float.mod_float m step))
    in
    let below = Float.to_int (Float.round_up (clean ((middle -. min) /. step))) in
    let up = Float.to_int (Float.round_up (clean ((max -. middle) /. step))) in
    let scale_count = below + up + 1 in
    if scale_count > count
    then calculate_step ~min ~max ~count ~allow_decimals ~correction:(correction + 1)
    else (
      let below, up =
        if scale_count < count
        then (
          if Float.(>) max 0.
          then (below, up + (count - scale_count))
          else (below + (count - scale_count), up))
        else (below, up)
      in
      ( step
      , clean (middle -. (Float.of_int below *. step))
      , clean (middle +. (Float.of_int up *. step)) )))
;;

let range_step start stop step =
  let rec go acc x i =
    if Float.(x < stop) && i < 100_000
    then go (x :: acc) (clean (x +. step)) (i + 1)
    else List.rev acc
  in
  go [] start 0
;;

let single_value_ticks value ~count ~allow_decimals =
  let step, middle =
    if (not (Float.is_integer value)) && allow_decimals
    then (
      let abs = Float.abs value in
      if Float.(<) abs 1.
      then (
        let step = 10. **. Float.of_int (digit_count value - 1) in
        step, clean (Float.round_down (value /. step) *. step))
      else if Float.(>) abs 1.
      then 1., Float.round_down value
      else 1., value)
    else if Float.(value = 0.)
    then 1., Float.of_int ((count - 1) / 2)
    else if not allow_decimals
    then 1., Float.round_down value
    else 1., value
  in
  let middle_index = (count - 1) / 2 in
  List.init count ~f:(fun i -> clean (middle +. (Float.of_int (i - middle_index) *. step)))
;;

let nice_ticks ?(allow_decimals = true) ?(count = 5) (min, max) =
  let reversed = Float.(>) min max in
  let lo, hi = if reversed then max, min else min, max in
  let values =
    if Float.(lo = hi)
    then single_value_ticks lo ~count ~allow_decimals
    else (
      let step, tick_min, tick_max =
        calculate_step
          ~min:lo
          ~max:hi
          ~count:(Int.max count 2)
          ~allow_decimals
          ~correction:0
      in
      if Float.(step = 0.)
      then [ lo; hi ]
      else range_step tick_min (tick_max +. (0.1 *. step)) step)
  in
  if reversed then List.rev values else values
;;

let fixed_domain_ticks ?(allow_decimals = true) ?(count = 5) (min, max) =
  let reversed = Float.(>) min max in
  let lo, hi = if reversed then max, min else min, max in
  let values =
    if Float.(lo = hi)
    then [ lo ]
    else (
      let step =
        adaptive_step
          (clean ((hi -. lo) /. Float.of_int (Int.max count 2 - 1)))
          ~allow_decimals
          ~correction:0
      in
      let values = range_step lo hi step @ [ hi ] in
      if allow_decimals then values else List.map values ~f:Float.round_nearest)
  in
  if reversed then List.rev values else values
;;
