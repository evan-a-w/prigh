open! Core
open! Import

(** The session's queued steers and follow-ups, as the backend's
    [queue_update] lists them, shown above the composer; and those that left
    the queue to be delivered, acknowledged once the transcript has them. *)

module Message = Prigh_ui.Queued_message

type t [@@deriving sexp_of]

val empty : t

(** Oldest first: steers, then follow-ups. *)
val items : t -> Message.t list

val count : t -> int

(** The backend's queues. What left them (and was not [remove]d here) is on
    its way to the agent. *)
val update : t -> steer:string list -> follow_up:string list -> t

(** Taken out at once, before the backend confirms. *)
val remove : t -> Message.t -> t

(** A user message reached the transcript: the queued message it delivers,
    if any. *)
val delivered : t -> text:string -> t * Message.t option

(** Nothing is on its way any more: the run ended (follow-ups leave the
    queue after it). *)
val forget_delivering : t -> t
