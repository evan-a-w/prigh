open! Core
open! Import

(** A session's review of the changes: the user's comments with their threads
    ({!Review_comment}) and which changed files the user approved. Recorded in
    the session as [Change]s (see [Session.review]), read from every branch,
    so rewinding the conversation keeps them.

    A file is approved at a hash of its change (as frontends compute it from
    the diff): once the file changes again its hash differs and the approval
    no longer counts. *)

module Change : sig
  type t =
    | Comment of Review_comment.t (** a comment added or changed, whole *)
    | Delete of { id : string }
    | Approve of
        { path : string
        ; hash : string
        }
    | Unapprove of { path : string }
  [@@deriving sexp, jsonaf, equal]
end

type t =
  { comments : Review_comment.t list (** oldest first *)
  ; approvals : string String.Map.t (** hashes by path *)
  ; next_id : int (** of the next comment, [c<next_id>] *)
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
    delivered. Fails when there is nothing to send. *)
val submit : t -> (string * int * Change.t list) Or_error.t

val approve : path:string -> hash:string -> Change.t list
val unapprove : t -> path:string -> Change.t list
