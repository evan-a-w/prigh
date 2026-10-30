open! Core
open! Import

(** The review page's git bar: the reviewed worktree's branch, how it
    stands with its upstream and what is staged ([git_status]), and its
    actions: stage and unstage files (from the file list's menu), commit
    (or amend) with a message the model can draft ({!Generate}), push
    (setting the upstream), and a pull request for the branch. Pure:
    [update] and [reply] return [Out.t]s, which [App] turns into requests.

    The commit box shows only while composing a commit ([commit]): it
    commits the files chosen for it (staged first) and whatever git already
    has staged. *)

module Busy : sig
  type t =
    | Staging
    | Committing
    | Pushing
  [@@deriving sexp_of, equal]
end

module Stage : sig
  (** What git has of a file's changes. *)
  type t =
    | Clean (** nothing uncommitted *)
    | Unstaged
    | Partly (** staged, with more changes not staged *)
    | Staged
  [@@deriving sexp_of, equal]
end

type t =
  { dir : string option (** the worktree ([None]: the session's) *)
  ; status : Git_status.t option
  ; error : string option (** [git_status] failed: no actions *)
  ; commit : String.Set.t option
    (** composing a commit: the files chosen for it (the commit box shows) *)
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
    | Staged_to_draft
    (** the chosen files staged, so their diff is drafted from *)
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
    | Stage of string list
    | Unstage of string list
    | Compose of string list
    (** open the commit box with these files chosen (added to those chosen
        when it is open) *)
    | Leave_out of string
    (** of the commit: unchosen, and unstaged when git has it staged *)
    | Close_commit
    | Message of string
    | Toggle_amend
    | Commit
    | Push
    | Create_pr
    | Draft (** Generate, for the message: the chosen files are staged first *)
  [@@deriving sexp_of, equal]
end

(** The files the review lists, with whether each is approved. *)
module Files : sig
  type t = (string * bool) list
end

(** The page reads [dir]'s changes again: its status too. Another worktree
    starts over. *)
val watch : t -> dir:string option -> t * Out.t list

val update : t -> Action.t -> t * Out.t list
val reply : t -> Reply.t -> (Json.t, string) Result.t -> t * Out.t list
val stage : t -> string -> Stage.t

(** What the commit box opens with when nothing is chosen: the approved
    files with uncommitted changes. *)
val approved_changes : t -> files:Files.t -> string list

(** The chosen files with changes git does not have staged yet. *)
val pending : t -> string list

(** What a commit takes: the staged files and the chosen ones, sorted. *)
val to_commit : t -> string list

(** Whether the actions make sense: the status is in and HEAD is on a
    branch. *)
val on_branch : t -> bool
