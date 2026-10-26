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

type t =
  { comments : Review_comment.t list (** oldest first *)
  ; approvals : Approval.t list
  }
[@@deriving sexp_of, equal]

val empty : t
val of_json : Json.t -> t Or_error.t

(** The comments and approvals of the worktree at [root]; with [own] (it is
    the session's own worktree) also the approvals without a root, after
    its own. *)
val scope : t -> root:string -> own:bool -> t

(** [path]'s approval (in a [scope]d review): unapproving it names its
    [root]. *)
val approval : t -> string -> Approval.t option

(** The comments on [path] (relative to the repository). *)
val on : t -> string -> Review_comment.t list

(** Comments with posts the agent has not seen. *)
val unsent : t -> int
