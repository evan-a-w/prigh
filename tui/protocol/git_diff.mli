open! Core

(** [git_diff]'s reply: the working tree's changes on the active tool host. *)
type t =
  { root : string (** the repository *)
  ; against : string (** [HEAD], a commit, or the empty tree's id *)
  ; diff : string (** [git diff] with untracked files as new ones *)
  ; truncated : bool
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
