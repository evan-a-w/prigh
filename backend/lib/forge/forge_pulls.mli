open! Core
open! Import

(** Changing pull requests, and their reviews. Conversation comments are
    {!Forge_issues.comment}s. Where the hosts differ:
    - draft/ready: GitHub's REST cannot change it, so GraphQL; Forgejo's
      drafts are a [WIP:] title prefix, added or removed;
    - merge: GitHub [PUT /merge {merge_method}] (deleting the branch is a
      second call); Forgejo [POST /merge {Do}], which deletes it itself;
    - reviews: [APPROVE] is Forgejo's [APPROVED], line comments are
      [{path, line, side}] on GitHub and [{path, new_position}] on Forgejo;
    - review comments: GitHub lists them per pull request and replies in a
      thread; Forgejo lists them per review and cannot reply in a thread. *)

val edit
  :  Forge_api.t
  -> Forge_repo.t
  -> int
  -> title:string option
  -> body:string option
  -> base:string option
  -> Pull_request.t Or_error.t

(** [state]: [open] or [closed]. *)
val set_state
  :  Forge_api.t
  -> Forge_repo.t
  -> int
  -> state:string
  -> Pull_request.t Or_error.t

(** [draft]: [true] makes it a draft, [false] ready for review. *)
val set_draft
  :  Forge_api.t
  -> Forge_repo.t
  -> int
  -> draft:bool
  -> Pull_request.t Or_error.t

module Merge_method : sig
  type t =
    | Merge
    | Squash
    | Rebase
  [@@deriving sexp_of]

  val of_string : string -> t Or_error.t
end

(** What happened to the head branch after a merge. *)
module Branch_deletion : sig
  type t =
    | Not_asked
    | Deleted
    | Kept of string (** why not *)
  [@@deriving sexp_of, equal]
end

(** Merges it at [sha] (its head: refused if it moved), then deletes the
    branch if asked: only a branch of the base repository itself (never a
    fork's, nor one whose repository was deleted), and never the
    repository's default branch or the pull request's base, which is refused
    before merging. *)
val merge
  :  Forge_api.t
  -> Forge_repo.t
  -> Pull_request.t
  -> method_:Merge_method.t
  -> title:string option
  -> message:string option
  -> delete_branch:bool
  -> Branch_deletion.t Or_error.t

module Review : sig
  type t =
    { id : int
    ; author : string
    ; state : string
      (** [APPROVED], [CHANGES_REQUESTED], [COMMENTED], [PENDING], ...
          (Forgejo's [REQUEST_CHANGES] and [COMMENT] are normalized) *)
    ; body : string
    ; submitted_at : string
    }
  [@@deriving sexp_of]

  val to_json : t -> Json.t
end

module Line_comment : sig
  type t =
    { id : int
    ; review_id : int option
    ; author : string
    ; path : string
    ; line : int option
    ; body : string
    ; in_reply_to : int option
    ; created_at : string
    ; url : string
    }
  [@@deriving sexp_of]

  val to_json : t -> Json.t
end

val reviews : Forge_api.t -> Forge_repo.t -> int -> Review.t list Or_error.t

(** All line comments, oldest first. On Forgejo they are per review:
    [reviews], when already fetched, saves fetching them again. *)
val line_comments
  :  ?reviews:Review.t list
  -> Forge_api.t
  -> Forge_repo.t
  -> int
  -> Line_comment.t list Or_error.t

module Event : sig
  type t =
    | Approve
    | Request_changes
    | Comment
  [@@deriving sexp_of]

  val of_string : string -> t Or_error.t
end

module New_comment : sig
  type t =
    { path : string
    ; line : int (** in the new version of the file *)
    ; body : string
    }

  (** [{path, line, body}] *)
  val of_json : Json.t -> t Or_error.t
end

(** Submits a review. *)
val review
  :  Forge_api.t
  -> Forge_repo.t
  -> Pull_request.t
  -> event:Event.t
  -> body:string
  -> comments:New_comment.t list
  -> Review.t Or_error.t

(** A reply in the thread of line comment [comment_id] (GitHub only). *)
val reply
  :  Forge_api.t
  -> Forge_repo.t
  -> int
  -> comment_id:int
  -> body:string
  -> Line_comment.t Or_error.t

val request_reviewers
  :  Forge_api.t
  -> Forge_repo.t
  -> int
  -> string list
  -> unit Or_error.t

(** [body] with a [Closes #N] line for each of [issues] it does not already
    close, which closes them when it is merged (on both hosts). *)
val with_closes : string -> int list -> string
