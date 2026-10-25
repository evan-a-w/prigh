open! Core

(** A git worktree of the session cwd's repository on the active tool host,
    as [list_worktrees], [create_worktree] and [remove_worktree] give it. New
    ones go under the backend config's [worktrees_dir], in
    [<project>/<branch-slug>]. *)

module Session_ref : sig
  (** A session whose cwd is in the worktree. *)
  type t =
    { id : string
    ; path : string
    ; name : string option
    ; live : bool (** loaded in the backend *)
    ; running : bool (** a run or background work going on *)
    }
  [@@deriving sexp_of, equal]

  val of_json : Json.t -> t Or_error.t
end

type t =
  { path : string
  ; branch : string option (** [None]: detached HEAD *)
  ; head : string option (** the commit, [None] before the first one *)
  ; main : bool (** the repository's own checkout; never removed *)
  ; managed : bool (** under the standard location (made by prigh) *)
  ; locked : bool
  ; prunable : bool (** its directory is gone *)
  ; changes : int option
    (** files with uncommitted changes; [None] when unknown *)
  ; upstream : string option
  ; ahead : int
  ; behind : int
  ; current : bool (** this session's cwd is in it *)
  ; sessions : Session_ref.t list
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t

(** [branch], else the short commit, else [(detached)]. *)
val label : t -> string

(** Whether it has uncommitted changes. *)
val dirty : t -> bool

module Listing : sig
  (** [list_worktrees]'s reply. *)
  type nonrec t =
    { root : string (** the main worktree *)
    ; project_dir : string (** where new worktrees go *)
    ; worktrees : t list (** the main one first *)
    }
  [@@deriving sexp_of, equal]

  val of_json : Json.t -> t Or_error.t
end

module Created : sig
  (** [create_worktree]'s reply. *)
  type nonrec t =
    { worktree : t
    ; new_branch : bool (** the branch was created rather than checked out *)
    }
  [@@deriving sexp_of, equal]

  val of_json : Json.t -> t Or_error.t
end

(** [remove_worktree]'s reply: the worktree removed, as it was. *)
val removed_of_json : Json.t -> t Or_error.t
