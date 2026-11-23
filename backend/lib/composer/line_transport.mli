open! Core
open! Import

(** A line channel to an external harness's process (one JSON frame per
    line each way): a process on the session's tool host ([Host_stream]) or,
    in tests, a scripted peer in memory. *)

module Exit : sig
  type t =
    { code : int option (** [None]: killed by a signal, or never started *)
    ; stderr_tail : string
    }
  [@@deriving sexp_of]
end

type t

(** The next line the process wrote (without its newline), waiting for it;
    [None] once its output ended. *)
val read_line : t -> string option

(** Writes [line] and a newline to the process's input; ignored once the
    process ended. *)
val write_line : t -> string -> unit

(** Ends the process's input (EOF). *)
val close_stdin : t -> unit

(** Asks the process to stop (SIGINT). *)
val interrupt : t -> unit

(** Stops the process: closes its input, then terminates it. *)
val stop : t -> unit

(** Waits until the process exited. *)
val ended : t -> Exit.t

(** Whether it exited already. *)
val is_ended : t -> bool

(** A transport from its operations, for [Host_stream] and other
    implementations. [ended] blocks until the process exited. *)
val create
  :  read_line:(unit -> string option)
  -> write_line:(string -> unit)
  -> close_stdin:(unit -> unit)
  -> interrupt:(unit -> unit)
  -> stop:(unit -> unit)
  -> ended:(unit -> Exit.t)
  -> is_ended:(unit -> bool)
  -> t

(** An in-memory process, for tests: [peer] runs in its own fiber with the
    other end (lines we write arrive on [Peer.read_line]) and exits when it
    returns ([Peer.exit] ends it sooner, with a code). *)
module Peer : sig
  type transport := t
  type t

  val read_line : t -> string option
  val write_line : t -> string -> unit
  val write_stderr : t -> string -> unit

  (** Whether our side asked to [interrupt] it, since it last looked. *)
  val take_interrupt : t -> bool

  val exit : t -> code:int -> 'a

  val spawn : sw:Switch.t -> (t -> unit) -> transport
end
