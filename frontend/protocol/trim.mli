open! Core

(** What is trimmed from a session's model context ([get_state]'s [trim]):
    the messages before [before] are out of it, shown folded into one
    divider. *)

type t =
  { before : string (** the entry id of the message the context starts at *)
  ; messages : int (** how many messages the trims in effect dropped *)
  ; tokens : int (** their estimated tokens *)
  ; at : Time_ns.Alternate_sexp.t option [@sexp.option]
    (** when the latest trim was made *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t

(** The [trim] field of [j]; [None] when absent or [null]. *)
val field : Json.t -> t option Or_error.t

(** What [trim] or [untrim] took out or brought back ([{messages, tokens}]). *)
module Freed : sig
  type t =
    { messages : int
    ; tokens : int
    }
  [@@deriving sexp_of, equal]

  val of_json : Json.t -> t Or_error.t
end

(** A place to trim before ([get_trim_points]): one of the user's messages
    in the context but the first, with what trimming there drops. *)
module Point : sig
  type t =
    { before : string (** the message's entry id, for [trim] *)
    ; index : int (** its position, as {!Outline.Point.index} counts *)
    ; text : string (** as {!Outline.Point.text} *)
    ; at : Time_ns.Alternate_sexp.t option [@sexp.option]
    ; messages : int
    ; tokens : int
    }
  [@@deriving sexp_of, equal]

  (** [get_trim_points]' answer, oldest first. *)
  val list_of_json : Json.t -> t list Or_error.t
end

(** [~38k tokens], [~950 tokens]. *)
val tokens_label : int -> string

(** [142 messages trimmed · ~38k tokens]. *)
val label : messages:int -> tokens:int -> string

(** [get_trimmed]' answer: the messages out of the context, oldest first. *)
val trimmed_of_json : Json.t -> Message.t list Or_error.t
