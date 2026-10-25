open! Core

(** The session's review ([get_review], the [review] event): the user's
    comments and the files they approved, each at the hash of its change
    (see [Prigh_ui.File_approval]). *)

type t =
  { comments : Review_comment.t list (** oldest first *)
  ; approvals : (string * string) list (** path, hash *)
  }
[@@deriving sexp_of, equal]

val empty : t
val of_json : Json.t -> t Or_error.t

(** The comments on [path] (relative to the repository). *)
val on : t -> string -> Review_comment.t list

(** Comments with posts the agent has not seen. *)
val unsent : t -> int
