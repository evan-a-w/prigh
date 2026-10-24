open! Core

type t =
  | Quiet
  | Normal
  | Verbose
[@@deriving sexp_of, equal, enumerate]

let next = function
  | Quiet -> Normal
  | Normal -> Verbose
  | Verbose -> Quiet
;;

let name = function
  | Quiet -> "quiet"
  | Normal -> "normal"
  | Verbose -> "verbose"
;;
