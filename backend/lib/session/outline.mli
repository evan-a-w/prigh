open! Core
open! Import

(** The landmarks of a session's transcript, for frontends that load it a
    page at a time ([get_messages_page]) and still let the user jump to any
    of the user's messages ([get_outline]): small enough to fetch whole. *)

module Kind : sig
  type t =
    | User
    (** the user's prompt, or a command of theirs ([/goal], [/review], [/init]) *)
    | Compaction (** the summary standing for older messages *)
    | Error (** a reply that ended with the model's error *)
  [@@deriving sexp_of]
end

module Point : sig
  type t =
    { index : int
      (** the message's position in [Session.identified_messages], as a
          page's [older] counts *)
    ; kind : Kind.t
    ; text : string
      (** what the user wrote (or the command's title, or the error), cut to
          [text_limit] bytes *)
    ; at : float option
    }
  [@@deriving sexp_of]
end

val text_limit : int

(** [text] stripped and cut to at most [text_limit] bytes, at a
    character's first byte. *)
val cut : string -> string

val create : (string option * Timed_message.t) list -> Point.t list

(** [{points: [{index, kind, text, at?}]}], [kind] [user], [compaction] or
    [error]. *)
val to_json : Point.t list -> Json.t
