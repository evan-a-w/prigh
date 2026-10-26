open! Core

(** [git_status]'s reply (and [git_stage]'s, [git_unstage]'s): a worktree's
    branch, how it stands with its upstream, and its changed files. *)

module File : sig
  (** [index] and [worktree] are the staged and unstaged change, a letter
      each ([M], [A], [D], [R], [T], [U]; [?] in [worktree]: untracked). *)
  type t =
    { path : string
    ; orig : string option (** a rename's old path *)
    ; index : string option
    ; worktree : string option
    }
  [@@deriving sexp_of, equal]

  val staged : t -> bool

  (** Unstaged changes, untracked files included. *)
  val unstaged : t -> bool

  val untracked : t -> bool
end

type t =
  { root : string
  ; branch : string option (** [None]: detached *)
  ; head : string option (** [None] before the first commit *)
  ; upstream : string option
  ; ahead : int
  ; behind : int
  ; files : File.t list
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t

(** Files with staged changes, with unstaged changes to tracked files, and
    untracked files. *)
val counts : t -> int * int * int

(** The file at [path], or the rename that came from it. *)
val find : t -> string -> File.t option

module Commit : sig
  (** [git_commit]'s reply. *)
  type nonrec t =
    { sha : string
    ; subject : string
    ; status : t
    }
  [@@deriving sexp_of, equal]

  val of_json : Json.t -> t Or_error.t
end

module Push : sig
  (** [git_push]'s reply. *)
  type nonrec t =
    { remote : string
    ; branch : string
    ; status : t
    }
  [@@deriving sexp_of, equal]

  val of_json : Json.t -> t Or_error.t
end
