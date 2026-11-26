open! Core

(** A range out of a session's model context ([get_state]'s [hidden], and
    what [trim_after], [delete], [untrim] and [undelete] answer): what a trim
    before or after a message, or a deletion, keeps out of it. *)

module Kind : sig
  type t =
    | Trim_before
    | Trim_after of { through : string (** the newest message it drops *) }
    | Deleted
  [@@deriving sexp_of, equal]
end

type t =
  { entry : string (** the entry that made it, to undo it ({!Request.Restore}) *)
  ; kind : Kind.t
  ; message : string (** the entry id of the message it is about *)
  ; messages : int (** the messages it keeps out of the context *)
  ; tokens : int (** their estimated tokens *)
  ; at : Time_ns.Alternate_sexp.t option [@sexp.option]
  ; divider : int option [@sexp.option]
    (** for a trim after a message: where its divider goes, as the number of
        the context's messages ([get_messages]) before it *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t

(** The [hidden] field of a state, oldest first; empty when absent. *)
val field : Json.t -> t list Or_error.t

val is_trim : t -> bool

(** The trims after a message, oldest first. *)
val trims_after : t list -> t list

(** [Message deleted], [2 messages deleted] (a reply with its tool results). *)
val deleted_label : t -> string
