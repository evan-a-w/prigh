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

(** Stops the process: closes its input, then terminates it. *)
val stop : t -> unit

(** Waits until the process exited. *)
val ended : t -> Exit.t

(** Whether it exited already. *)
val is_ended : t -> bool

(** The MCP side channel of a process started with one: how the harness
    reaches prigh's tools ({!Injected_tools.Bridge}, the [prigh mcp-bridge]
    command to give it as a stdio MCP server), and [serve], which answers
    what comes through it with the given tools from then on. *)
module Mcp : sig
  type t =
    { bridge : Injected_tools.Bridge.t
    ; serve : Injected_tools.t -> unit
    }
end

(** Its MCP side channel, if it was started with one. *)
val mcp : t -> Mcp.t option

(** A transport from its operations, for [Host_stream] and other
    implementations. [ended] blocks until the process exited. *)
val create
  :  read_line:(unit -> string option)
  -> write_line:(string -> unit)
  -> stop:(unit -> unit)
  -> ended:(unit -> Exit.t)
  -> is_ended:(unit -> bool)
  -> ?mcp:Mcp.t
  -> unit
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

  val exit : t -> code:int -> 'a
  val spawn : sw:Switch.t -> (t -> unit) -> transport
end
