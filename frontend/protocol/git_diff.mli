open! Core

(** [git_diff]'s reply: a worktree's changes on the active tool host (the
    session directory's, or [dir]'s), against [HEAD] or a base: for a linked
    worktree, by default, the branch it was made from (or the remote's
    default branch), committed changes included; or a branch's ([rev]), not
    checked out, against its base. Since the merge base, or, [direct], the
    base itself. *)

module Commit : sig
  type t =
    { sha : string
    ; subject : string
    }
  [@@deriving sexp_of, equal]

  (** Its first 8 characters. *)
  val short : t -> string
end

module Target : sig
  (** What is reviewed: the worktree at a directory ([None]: the session's),
      or a branch without checking it out (read-only). *)
  type t =
    | Dir of string option
    | Rev of string
  [@@deriving sexp_of, equal, compare]
end

type t =
  { root : string (** the worktree's top directory *)
  ; against : string (** [HEAD], a commit, or the empty tree's id *)
  ; diff : string (** [git diff] with untracked files as new ones *)
  ; truncated : bool
  ; branch : string option (** checked out there; [None]: detached *)
  ; base : string (** [HEAD], or the base's name ([main], [origin/main]) *)
  ; main : bool (** the repository's main checkout *)
  ; branches : string list (** local branches, to compare with instead *)
  ; remote_branches : string list (** [origin/main], ... *)
  ; tags : string list (** newest first *)
  ; commits : Commit.t list (** the last few on what is reviewed *)
  ; rev : string option (** the branch reviewed, not checked out *)
  ; direct : bool (** against the base itself, not the merge base *)
  }
[@@deriving sexp_of, equal]

(** What the session's review keys this diff's comments and approvals by:
    the worktree's [root], or [branch:REV] for a branch not checked out. *)
val review_root : t -> string

(** [git_diff]'s parameters: one [path] only, compared with [base] (its
    merge base, or itself when [direct]), of [target] (default: the
    session's worktree). *)
val params
  :  ?path:string
  -> ?base:string
  -> ?direct:bool
  -> ?target:Target.t
  -> unit
  -> (string * Json.t) list

(** [git_show]'s: [path] as it is in [rev]. *)
val show_params : rev:string -> path:string -> (string * Json.t) list

val of_json : Json.t -> t Or_error.t

(** [git_show]'s reply: the text, and whether it was cut. *)
val shown_of_json : Json.t -> (string * bool) Or_error.t

module Refs : sig
  (** [git_refs]'s reply: the repository's names, for completion. *)
  type t =
    { branches : string list (** local *)
    ; remote_branches : string list
    ; tags : string list (** newest first *)
    }
  [@@deriving sexp_of, equal]

  val empty : t
  val of_json : Json.t -> t Or_error.t
end

(** The names a diff's reply carries. *)
val refs : t -> Refs.t
