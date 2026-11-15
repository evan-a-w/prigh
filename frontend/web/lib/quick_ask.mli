open! Core
open! Import

(** The one-tap comments a phone's comment sheet offers on the selected
    lines: each posts its text as a comment on them and sends the review to
    the agent at once. *)
type t =
  | Explain
  | Why
  | Revert
  | Add_test
[@@deriving sexp_of, equal, enumerate]

(** The chip's label: "Explain this", "Why?", "Revert this", "Add a test". *)
val label : t -> string

(** The comment posted. *)
val text : t -> string
