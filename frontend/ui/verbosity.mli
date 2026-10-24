open! Core

(** How much of the transcript to show. Ctrl+O cycles through the levels. *)
type t =
  | Quiet
  | Normal
  | Verbose
[@@deriving sexp_of, equal, enumerate]

val next : t -> t
val name : t -> string
