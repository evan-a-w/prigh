open! Core
open! Import

(** An RPC connection to a backend, line by line: JSON lines over TCP for a
    [HOST:PORT] address, else text messages over the WebSocket at [/ws]
    under its prefix (see {!Backend_address}). *)

type t

(** Raises like {!Backend_address.websocket}. *)
val connect : env:Env.t -> sw:Switch.t -> Backend_address.t -> t

(** [None] once the connection ended. *)
val read_line : t -> string option

(** Raises when the connection is gone. *)
val write_line : t -> string -> unit

(** Ends the connection: shuts the TCP connection down, or sends the
    WebSocket's close frame. *)
val close : t -> unit
