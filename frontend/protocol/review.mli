open! Core

(** The session's review ([get_review], the [review] event): the user's
    comments and the files they approved, each at the hash of its change
    (see [Prigh_ui.File_approval]), for every worktree reviewed. A frontend
    shows one worktree's at a time ({!scope}). *)

module Approval : sig
  type t =
    { root : string option
      (** the worktree's top directory; [None]: approved before worktrees
          were told apart, in the session's own *)
    ; path : string
    ; hash : string
    }
  [@@deriving sexp_of, equal]
end

module Snapshot : sig
  (** A file as it was when the user acted on it in the review, to show
      what changed in it since ([review_since], {!Review_since}). *)

  module Content : sig
    type t =
      | Text of { md5 : string }
      | Absent (** no such file *)
      | Binary of { md5 : string }
      | Too_large of { bytes : int } (** not kept *)
    [@@deriving sexp_of, equal]

    val of_json : Json.t -> t Or_error.t
  end

  type t =
    { root : string
    ; path : string
    ; action : string
      (** [approved], [unapproved], [commented], [replied], [accepted],
          [rejected] or [edited] *)
    ; by : string option (** the user who acted *)
    ; at : Time_ns.t option
    ; content : Content.t
    }
  [@@deriving sexp_of, equal]

  val of_json : Json.t -> t Or_error.t
end

type t =
  { comments : Review_comment.t list (** oldest first *)
  ; approvals : Approval.t list
  ; snapshots : Snapshot.t list
    (** by file, each file's oldest first; none from older backends *)
  }
[@@deriving sexp_of, equal]

val empty : t
val of_json : Json.t -> t Or_error.t

(** The comments, approvals and snapshots of the worktree at [root]; with
    [own] (it is the session's own worktree) also the approvals without a
    root, after its own. *)
val scope : t -> root:string -> own:bool -> t

(** [path]'s approval (in a [scope]d review): unapproving it names its
    [root]. *)
val approval : t -> string -> Approval.t option

(** The comments on [path] (relative to the repository). *)
val on : t -> string -> Review_comment.t list

(** Comments with posts the agent has not seen. *)
val unsent : t -> int

(** [path]'s snapshots (in a [scope]d review), oldest first: the history
    [review_since]'s [index] counts in. *)
val history : t -> string -> Snapshot.t list
