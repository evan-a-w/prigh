open! Core

(** Which REST API a git host speaks. [Forgejo] covers Gitea too. *)

type t =
  | Github
  | Forgejo
[@@deriving sexp, equal, compare, enumerate]

val to_string : t -> string
val of_string : string -> t Or_error.t
val label : t -> string
