open! Core
open! Import

(** Where a session's context can be trimmed ([get_trim_points], [/trim]):
    before each of its messages that {!Message_point.trim_before} allows
    (the user's and the model's replies, not the first), with what trimming
    there would drop. *)

type t =
  { before : string (** the message's entry id, for [trim] *)
  ; index : int (** its position, as {!Outline.Point.index} counts *)
  ; role : Message_point.Role.t (** [User] or [Assistant] *)
  ; text : string (** as {!Outline.Point.text}, or {!Message_point.text} *)
  ; at : float option
  ; messages : int (** the messages before it, which trimming drops *)
  ; tokens : int (** their estimated tokens *)
  }
[@@deriving sexp_of]

(** From [Session.identified_messages], oldest first. *)
val create : (string option * Timed_message.t) list -> t list

(** [{points: [{before, index, role: user|assistant, text, at?, messages,
    tokens}]}]. *)
val to_json : t list -> Json.t
