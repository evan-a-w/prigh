open! Core
open! Import

type t =
  | Harness
  | Composer
[@@deriving sexp, equal, compare, enumerate]

let to_string = function
  | Harness -> "harness"
  | Composer -> "composer"
;;

let of_string s =
  match List.find all ~f:(fun t -> String.equal (to_string t) s) with
  | Some t -> Ok t
  | None ->
    Or_error.error_s
      [%message
        "unknown engine" s ~expected:(List.map all ~f:to_string : string list)]
;;
