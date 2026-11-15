open! Core
open! Import

(** What a connection has been sent of each streamed message, so that it
    gets each [message_update] as a delta when it has everything before it,
    and the whole message so far (a resync) when it has not: it attached to
    the session, or reconnected, while the message streamed, or asked
    ([resync]). A stream is the main agent's or a (nested) subagent's. *)

type t

module Update : sig
  type t =
    { key : string (** the stream: [""], or the subagents' ids *)
    ; stream : int
    ; seq : int
    }
  [@@deriving sexp_of]

  (** The [message_update] in [event], also nested in [subagent] ones. *)
  val of_event : Agent_event.t -> t option
end

module Send : sig
  type t =
    | Delta
    | Resync (** the whole message so far, through the update *)
    | Skip (** a resync sent already had it *)
  [@@deriving sexp_of]
end

val create : unit -> t

(** What to send the connection for [update], recording it as sent. *)
val send : t -> Update.t -> Send.t

(** Forgets what was sent: the next update of each stream is a resync. *)
val reset : t -> unit
