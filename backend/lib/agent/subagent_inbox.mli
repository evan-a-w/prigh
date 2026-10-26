open! Core
open! Import

(** The messages sent to a background subagent while it works: by the user
    (steers and follow-ups, like the main agent's {!Steer_queue}) or by the
    main agent ([subagent_message]). The subagent's loop takes the steers at
    each step boundary and everything when it ends its turn, so a follow-up
    makes it carry on rather than report. Once the subagent stops, the inbox
    is closed and refuses messages. *)

module From : sig
  type t =
    | User
    | Caller (** the main agent *)
  [@@deriving sexp_of, equal]
end

(** What a client is told about an agent's queue. *)
module Listing : sig
  type t =
    { agent_id : string
    ; steer : string list (** oldest first *)
    ; follow_up : string list
    }
  [@@deriving sexp_of]
end

type t

val create : unit -> t

(** Called after every change of the queue (also when the subagent takes
    messages, or when it closes with messages still queued). *)
val set_on_change : t -> (unit -> unit) -> unit

val is_open : t -> bool

(** Fails once the inbox is closed. *)
val push : t -> Steer_queue.Kind.t -> from:From.t -> string -> unit Or_error.t

(** The queued texts: steers, follow-ups. *)
val texts : t -> string list * string list

val pop_last : t -> string option

(** Like {!Steer_queue.remove}; [text], when given, must match. *)
val remove
  :  t
  -> Steer_queue.Kind.t
  -> index:int
  -> text:string option
  -> string Or_error.t

(** {2 For the subagent's loop} *)

(** The steers, as user messages with an {!Injected.Note.steer}. *)
val take_steers : t -> Message.t list

(** Steers, then follow-ups. *)
val take_all : t -> Message.t list

(** Blocks until a message is queued. *)
val await : t -> unit

(** Closes the inbox when nothing is queued; whether it did. *)
val close_if_empty : t -> bool

(** Closes the inbox, returning what was still queued (also kept for
    {!take_dropped}). *)
val close : t -> string list

(** The texts {!close} dropped, once. *)
val take_dropped : t -> string list
