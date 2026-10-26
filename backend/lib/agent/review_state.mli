open! Core
open! Import

(** A session's review of the changes: the user's comments with their threads
    ({!Review_comment}) and which changed files the user approved. Recorded in
    the session as [Change]s (see [Session.review]), read from every branch,
    so rewinding the conversation keeps them.

    A file is approved at a hash of its change (as frontends compute it from
    the diff): once the file changes again its hash differs and the approval
    no longer counts.

    Comments name their worktree's [root]; approvals too, apart from those
    recorded before worktrees were told apart (no [root]: the session's own
    repository's). A frontend reviewing a worktree shows its own only.

    Each action of the user's on a file records a {!Review_snapshot} of it
    (its text kept once per content), so that [since] can say what changed
    in it after. *)

module File_key : sig
  (** An approved file: its worktree's top directory ([None]: unknown, the
      session's repository) and its path in it. *)
  type t =
    { root : string option
    ; path : string
    }
  [@@deriving sexp, compare, equal]

  include Comparable.S with type t := t
end

module Change : sig
  type t =
    | Comment of Review_comment.t (** a comment added or changed, whole *)
    | Delete of { id : string }
    | Approve of
        { root : string option
        ; path : string
        ; hash : string
        }
    | Unapprove of
        { root : string option
        ; path : string
        }
    | Snapshot of
        { snapshot : Review_snapshot.t
        ; text : string option
          (** the file's text, the first time the review keeps it *)
        }
  [@@deriving sexp, jsonaf, equal]
end

type t =
  { comments : Review_comment.t list (** oldest first *)
  ; approvals : string File_key.Map.t (** hashes by file *)
  ; next_id : int (** of the next comment, [c<next_id>] *)
  ; snapshots : Review_snapshot.t list File_key.Map.t
    (** by file (always with a [root]), newest first *)
  ; blobs : string String.Map.t (** the snapshots' texts, by MD5 *)
  }
[@@deriving sexp_of, equal]

val empty : t
val apply : t -> Change.t -> t
val of_changes : Change.t list -> t
val find : t -> string -> Review_comment.t Or_error.t

(** Each returns the changes to record (and [apply]); errors say what to do. *)

val add
  :  t
  -> root:string
  -> path:string
  -> range:Review_comment.Range.t
  -> anchor:string
  -> text:string
  -> at:float
  -> (Review_comment.t * Change.t list) Or_error.t

(** A post in the comment's thread; the agent's is delivered, may propose
    edits and may resolve it. A user's reply reopens a resolved comment. *)
val reply
  :  t
  -> id:string
  -> author:Review_comment.Author.t
  -> text:string
  -> ?edits:Review_comment.Edit.t list
  -> ?resolve:bool
  -> at:float
  -> unit
  -> (Review_comment.t * Change.t list) Or_error.t

val set_status
  :  t
  -> id:string
  -> Review_comment.Status.t
  -> (Review_comment.t * Change.t list) Or_error.t

val delete : t -> id:string -> Change.t list Or_error.t

(** The proposal of the [post]th post (from 0) of a comment, while pending:
    the comment and the edits to apply to its file. *)
val proposal
  :  t
  -> id:string
  -> post:int
  -> (Review_comment.t * Review_comment.Edit.t list) Or_error.t

(** Settles a pending proposal ([Accepted], [Rejected] or [Failed]) and adds
    the user's undelivered post saying so, for the agent to hear with the
    next [submit]. *)
val decide
  :  t
  -> id:string
  -> post:int
  -> Review_comment.Proposal.State.t
  -> at:float
  -> (Review_comment.t * Change.t list) Or_error.t

(** The user's posts the agent has not seen, as one message (their comments
    in full, in {!Review_comment.to_prompt}'s form, after instructions on
    replying), with how many comments it covers; their posts become
    delivered. With [root], only the comments on that worktree. Fails when
    there is nothing to send. *)
val submit : ?root:string -> t -> (string * int * Change.t list) Or_error.t

val approve
  :  ?root:string
  -> path:string
  -> hash:string
  -> unit
  -> Change.t list

val unapprove : ?root:string -> t -> path:string -> Change.t list

(** Records [snapshot]; [text] is kept unless the review has it already. *)
val snapshot : t -> Review_snapshot.t -> text:string option -> Change.t list

(** [path]'s snapshots, oldest first: the history [since]'s [index] counts
    in. *)
val snapshots : t -> root:string -> path:string -> Review_snapshot.t list

val blob : t -> md5:string -> string option

module Since : sig
  (** What changed in a file since an action on it. *)
  type t =
    { path : string
    ; index : int (** of the snapshot compared with, in [snapshots] *)
    ; snapshot : Review_snapshot.t
    ; current : Review_snapshot.Content.t
    ; changed : bool
    ; diff : Text_diff.t (** snapshot to now; empty when not [changed] *)
    ; note : string option
      (** why a changed file has no diff: binary, too large *)
    }
  [@@deriving sexp_of, equal]
end

(** [path]'s snapshot [index] (default: the last) against [current], the
    file as it is now (as {!Review_snapshot.content_of} makes it). Fails
    when the file has no snapshot. *)
val since
  :  t
  -> root:string
  -> path:string
  -> ?index:int
  -> current:Review_snapshot.Content.t * string option
  -> unit
  -> Since.t Or_error.t
