open! Core

(** Where the session's status (what the agent does, how full the context
    is, the cost, the thinking level and the mode) sits, now that the chat
    has no header: a per-user preference ([/status-style], Settings →
    Appearance). *)

type t =
  | Island (** a translucent pill over the top of the transcript *)
  | Ring (** a ring of context round send; thinking and mode by the model *)
  | Whisper (** one quiet mono line under the composer *)
  | Border (** the composer's bottom edge fills; thinking and mode by the host *)
  | Hub (** the model chip: its percent, its menu has the rest *)
  | Sidebar (** a card at the sidebar's foot (else {!Whisper}) *)
[@@deriving sexp_of, equal, enumerate]

val default : t

(** ["island"], ["ring"], ["whisper"], ["border"], ["hub"], ["sidebar"]. *)
val name : t -> string

val of_name : string -> t option

(** A short title: ["Island"], ["Send ring"], ... *)
val title : t -> string

(** One line on where it puts the status. *)
val describe : t -> string
