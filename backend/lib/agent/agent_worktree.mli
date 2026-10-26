open! Core
open! Import

(** A git worktree made for a subagent (the [subagent] tool's [worktree]
    option): it works there on a branch of its own, which the user reviews,
    merges or removes once it reports. The session that ran the subagent
    records it ({!Session.worktrees}) until it is removed. *)

type t =
  { agent : string (** the subagent's id in its session *)
  ; task : string
  ; path : string
  ; branch : string
  ; root : string (** the repository's main checkout ({!Git_location}) *)
  }
[@@deriving sexp, jsonaf, equal]

(** For the subagent's report: where its work is and what to do with it. *)
val report_line : t -> string
