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
    Or_error.errorf
      "unknown engine %S: expected %s"
      s
      (String.concat ~sep:" or " (List.map all ~f:to_string))
;;
