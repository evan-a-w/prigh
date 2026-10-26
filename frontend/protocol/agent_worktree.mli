open! Core

(** A git worktree made for a subagent (the [subagent] tool's [worktree]):
    [subagent_start]'s [worktree]. The session that ran it records it until
    it is removed ({!Session_worktree}, source [agent]). *)

type t =
  { agent : string (** the subagent's id in its session *)
  ; task : string
  ; path : string
  ; branch : string
  ; root : string (** the repository's main checkout *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
