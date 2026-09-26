open! Core
open! Import

(** The browser frontend's listener: serves the static frontend from [root]
    (when given) and upgrades [GET /ws] to a WebSocket that carries the same
    JSON-lines RPC as stdio and TCP, one message per line. A connection whose
    first byte is [{] is a plain JSON-lines client (the terminal frontend's
    [-connect]) and is handed to [on_lines], so one port serves both. *)

type on_lines =
  read_line:(unit -> string option) -> write_line:(string -> unit) -> unit

(** Handles one connection: JSON lines until EOF, or one HTTP/1.1 request,
    then close (or the WebSocket until it ends). *)
val handle
  :  root:string option
  -> on_websocket:(Websocket.t -> unit)
  -> on_lines:on_lines
  -> _ Eio.Flow.two_way
  -> unit

(** URL opened for the local browser. The token is percent-encoded when one is
    required by the server. *)
val browser_url : host:string -> port:int -> token:string option -> string

(** [Rpc_server.serve_lines] over a WebSocket. *)
val serve_rpc : Rpc_server.t -> Websocket.t -> unit

(** Accepts connections until [sw] ends; returns the bound port (useful with
    port 0). *)
val listen
  :  env:Env.t
  -> sw:Switch.t
  -> addr:Eio.Net.Ipaddr.v4v6
  -> port:int
  -> root:string option
  -> on_websocket:(Websocket.t -> unit)
  -> on_lines:on_lines
  -> int

module For_testing : sig
  val safe_relative : string -> string option
  val content_type : string -> string
end
