open! Core

(** Where a session's cwd is in git ([list_sessions]' [repo]): its project
    and worktree. *)

type t =
  { root : string
    (** the repository's main checkout: the project, the same for all of its
        worktrees *)
  ; worktree : string (** the top of the checkout the cwd is in *)
  ; branch : string option
  ; main : bool (** [worktree] is the main checkout *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
