open! Core
open! Import

(** What a connection has to send, written out by one fiber ([run]) so that
    whoever sends (an agent's events, other clients' requests, the
    connection's own replies) never waits for the peer. A peer that stops
    reading (a suspended laptop, a phone gone from the network behind a
    proxy that keeps the socket open) must not hold anyone up: once more
    than [max_lines] lines, or more than [max_bytes] bytes in more than one
    line, wait to be written, or a write fails, the outbox fails: what waits
    is dropped, and so is everything sent later, and [failed] is determined
    so that the connection can be closed. *)

type t

(** Defaults: 10000 lines, 256 MiB. *)
val create : ?max_lines:int -> ?max_bytes:int -> unit -> t

(** Queues a line; never blocks. Ignored once the outbox is failed or
    finished. *)
val send : t -> string -> unit

module Late : sig
  type t
end

(** Queues a line made by [render] when its turn to be written comes, so
    that what it says can still change while it waits (see
    {!Update_coalescer}). [bytes] is its initial size towards [max_bytes];
    call {!resize_late} as its size changes while waiting. *)
val send_late : t -> bytes:int -> (unit -> string) -> Late.t

(** Updates the byte size of a still-waiting {!send_late} entry. Fails the
    outbox if growth pushes total waiting bytes past [max_bytes]. *)
val resize_late : t -> Late.t -> bytes:int -> unit

(** Writes the lines in order as they come, until [finish] (and the lines
    sent before it are written) or a failure. A [write] that raises fails
    the outbox. *)
val run : t -> write:(string -> unit) -> unit

(** Nothing more will be sent: [run] returns once the lines waiting are
    written. *)
val finish : t -> unit

(** Fails the outbox with [reason] (no-op when already failed). *)
val fail : t -> string -> unit

(** Determined with the reason when the outbox fails. *)
val failed : t -> string Promise.t

(** Lines waiting to be written. *)
val waiting : t -> int

(** Bytes waiting to be written. *)
val bytes : t -> int
