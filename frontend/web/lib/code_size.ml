open! Core
open! Import

type t =
  | Small
  | Medium
  | Large
  | Huge
[@@deriving sexp_of, equal, enumerate]

let default = Medium

let name = function
  | Small -> "small"
  | Medium -> "medium"
  | Large -> "large"
  | Huge -> "huge"
;;

let of_name s = List.find all ~f:(fun t -> String.equal (name t) s)

let px = function
  | Small -> 11.
  | Medium -> 12.5
  | Large -> 14.
  | Huge -> 16.
;;

let step t by =
  let sizes = Array.of_list all in
  let i, _ = Array.findi_exn sizes ~f:(fun _ s -> equal s t) in
  sizes.(Int.clamp_exn (i + by) ~min:0 ~max:(Array.length sizes - 1))
;;
