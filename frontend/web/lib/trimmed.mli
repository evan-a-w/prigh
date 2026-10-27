open! Core
open! Import

(** What the page shows of the messages out of the session's context: those
    trimmed before a message ([State.trim]), folded into the divider at the
    transcript's start or shown above it, read-only ([get_trimmed]); those
    trimmed after a message ([State.hidden]), each range folded into a
    divider or shown below it; the messages being deleted, hidden at once;
    and the trims and undos it waits on the backend for. *)

module Shown : sig
  type t =
    | Folded
    | Loading
    | Shown of Chat.t (** oldest first, as [get_trimmed] gave them *)
  [@@deriving sexp_of]
end

type t =
  { shown : Shown.t
  ; restoring : bool (** an [untrim] is on its way *)
  ; reveal : string option
    (** this page trimmed before this message (an entry id): once the
      transcript is reloaded starting there, it shows the divider *)
  ; ranges : Shown.t String.Map.t
    (** the trims after a message, by {!Hidden.entry}: absent is folded *)
  ; restoring_ranges : String.Set.t
    (** the ranges ({!Hidden.entry}) an undo is on its way for *)
  ; deleting : Int.Set.t
    (** entries deleted from here, hidden until the transcript is reloaded
        without them *)
  ; reveal_hidden : (string * int option) option
    (** this page made the range of this {!Hidden.entry}: once the
        transcript is reloaded with it out, the page scrolls to where it
        was: its divider ([None]), or the message before a deletion (its
        index) *)
  }
[@@deriving sexp_of]

val empty : t

(** How the range of this {!Hidden.entry} shows. *)
val range : t -> string -> Shown.t
