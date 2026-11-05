open! Core
open! Import

(** An RPC connection to a backend, line by line: JSON lines over TCP for a
    [HOST:PORT] address, else text messages over the WebSocket at [/ws]
    under its prefix (see {!Backend_address}). *)

type t

(** Raises like {!Backend_address.websocket}. *)
val connect : env:Env.t -> sw:Switch.t -> Backend_address.t -> t

(** [None] once the connection ended. Skips the answers to {!heartbeat}'s
    pings. *)
val read_line : t -> string option

(** Raises when the connection is gone. Concurrent calls write whole lines. *)
val write_line : t -> string -> unit

(** Ends the connection: shuts the TCP connection down, or sends the
    WebSocket's close frame (unless {!heartbeat} gave up on the backend). *)
val close : t -> unit

(** Notices a backend that is gone without closing the connection (a
    network dropped, a machine asleep, a frozen process): once a line was
    written ([hello] goes first), sends a [ping] request every [interval]
    seconds, and when nothing came for [timeout] seconds (or one write was
    stuck for [write_timeout]), calls [on_silent] with the seconds it waited
    and shuts the TCP connection down, so that {!read_line} returns [None]
    and writes fail. Defaults: 10, 30, 300. Returns only then. *)
val heartbeat
  :  t
  -> clock:_ Eio.Time.clock
  -> ?interval:float
  -> ?timeout:float
  -> ?write_timeout:float
  -> on_silent:(float -> unit)
  -> unit
  -> unit

(** The id of {!heartbeat}'s pings. *)
val heartbeat_id : string
