open! Core
open! Import

type t =
  | Explain
  | Why
  | Revert
  | Add_test
[@@deriving sexp_of, equal, enumerate]

let label = function
  | Explain -> "Explain this"
  | Why -> "Why?"
  | Revert -> "Revert this"
  | Add_test -> "Add a test"
;;

let text = function
  | Explain -> "Explain this code: what it does and how it fits in."
  | Why -> "Why this change? What does it fix or enable?"
  | Revert -> "Revert this change."
  | Add_test -> "Add a test that covers this."
;;
