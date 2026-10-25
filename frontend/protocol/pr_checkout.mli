open! Core

(** [pr_checkout]'s reply: the git worktree holding the pull request's
    branch, on the session's tool host. *)

type t =
  { path : string
  ; branch : string
  ; created : bool (** [false]: it was there already and was updated *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
