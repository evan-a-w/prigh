open! Core
open! Import

(** The review page's git bar: the reviewed worktree's branch, how it
    stands with its upstream and what is staged ([git_status]), and its
    actions: stage and unstage files (ticked in the file list; approved
    files come ticked), commit (or amend) with a message the model can
    draft ({!Generate}), push (setting the upstream), and a pull request
    for the branch. Pure: [update] and [reply] return [Out.t]s, which [App]
    turns into requests.

    A file is ticked when it is staged, or approved and not unticked
    ([excluded]); committing stages the ticked files first. *)

module Busy : sig
  type t =
    | Staging
    | Committing
    | Pushing
  [@@deriving sexp_of, equal]
end

module Tick : sig
  (** A file's box in the review's list. *)
  type t =
    | Clean (** nothing uncommitted: no box *)
    | Checked
    | Partly (** staged, with more changes not staged *)
    | Unchecked
  [@@deriving sexp_of, equal]
end

type t =
  { dir : string option (** the worktree ([None]: the session's) *)
  ; status : Git_status.t option
  ; error : string option (** [git_status] failed: no actions *)
  ; excluded : String.Set.t (** approved files unticked *)
  ; message : string
  ; amend : bool
  ; busy : Busy.t option
  ; failure : string option (** why the last action was refused *)
  ; notice : string option (** what the last action did *)
  }
[@@deriving sexp_of, equal]

val empty : t

module Reply : sig
  type t =
    | Status
    | Staged
    | Staged_to_draft (** the ticked files staged, so their diff is drafted from *)
    | Committed
    | Pushed
  [@@deriving sexp_of, equal]
end

module Out : sig
  type t =
    | Rpc of
        { method_ : Request.Method.t
        ; reply : Reply.t
        }
    | Draft_message (** start drafting the commit message *)
    | Changed (** committed: the review's diff is read again *)
    | Create_pr of { dir : string option }
  [@@deriving sexp_of, equal]
end

module Action : sig
  type t =
    | Toggle of string (** a file's box *)
    | Stage_approved
    | Message of string
    | Toggle_amend
    | Commit
    | Push
    | Create_pr
    | Draft (** Generate, for the message: the ticked files are staged first *)
  [@@deriving sexp_of, equal]
end

(** The files the review lists, with whether each is approved. *)
module Files : sig
  type t = (string * bool) list
end

(** The page reads [dir]'s changes again: its status too. Another worktree
    starts over. *)
val watch : t -> dir:string option -> t * Out.t list

val update : t -> files:Files.t -> Action.t -> t * Out.t list
val reply : t -> Reply.t -> (Json.t, string) Result.t -> t * Out.t list
val tick : t -> files:Files.t -> string -> Tick.t

(** The ticked files git does not have staged yet. *)
val pending : t -> files:Files.t -> string list

(** Staged files, and the ticked ones to stage. *)
val to_commit : t -> files:Files.t -> int

(** Whether the actions make sense: the status is in and HEAD is on a
    branch. *)
val on_branch : t -> bool
