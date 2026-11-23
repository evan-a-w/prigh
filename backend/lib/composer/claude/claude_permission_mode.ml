open! Core
open! Import

type t =
  | Default
  | Accept_edits
  | Plan
  | Bypass_permissions
  | Dont_ask
  | Auto
[@@deriving sexp_of, equal, enumerate]

let to_string = function
  | Default -> "default"
  | Accept_edits -> "acceptEdits"
  | Plan -> "plan"
  | Bypass_permissions -> "bypassPermissions"
  | Dont_ask -> "dontAsk"
  | Auto -> "auto"
;;

let of_string s =
  match s with
  | "manual" -> Some Default
  | _ -> List.find all ~f:(fun t -> String.equal (to_string t) s)
;;
