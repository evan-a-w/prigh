open! Core

(** [git_diff]'s reply: a worktree's changes on the active tool host (the
    session directory's, or [dir]'s), against [HEAD] or a base: for a linked
    worktree, by default, the branch it was made from (or the remote's
    default branch), committed changes included. *)
type t =
  { root : string (** the worktree's top directory *)
  ; against : string (** [HEAD], a commit, or the empty tree's id *)
  ; diff : string (** [git diff] with untracked files as new ones *)
  ; truncated : bool
  ; branch : string option (** checked out there; [None]: detached *)
  ; base : string (** [HEAD], or the base's name ([main], [origin/main]) *)
  ; main : bool (** the repository's main checkout *)
  ; branches : string list
    (** local then remote branches, to compare with instead *)
  }
[@@deriving sexp_of, equal]

(** [git_diff]'s parameters: one [path] only, compared with [base] (its
    merge base), in the worktree at [dir]. *)
val params
  :  ?path:string
  -> ?base:string
  -> ?dir:string
  -> unit
  -> (string * Json.t) list

val of_json : Json.t -> t Or_error.t
