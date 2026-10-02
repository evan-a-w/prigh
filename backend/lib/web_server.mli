open! Core
open! Import

(** The browser frontend's listener: serves the static frontend from [root]
    (when given) and upgrades [GET /ws] to a WebSocket that carries the same
    JSON-lines RPC as stdio and TCP, one message per line. A connection whose
    first byte is [{] is a plain JSON-lines client (the terminal frontend's
    [-connect]) and is handed to [on_lines], so one port serves both. *)

type on_lines =
  read_line:(unit -> string option) -> write_line:(string -> unit) -> unit

type on_websocket = query:(string * string) list -> Websocket.t -> unit

(** Handles one connection: JSON lines until EOF, or one HTTP/1.1 request,
    then close (or the WebSocket until it ends). [on_websocket] gets the
    decoded query string of the upgrade request. *)
val handle
  :  root:string option
  -> on_websocket:on_websocket
  -> on_lines:on_lines
  -> _ Eio.Flow.two_way
  -> unit

val browser_url : host:string -> port:int -> string

(** [Rpc_server.serve_lines] over a WebSocket. *)
val serve_rpc : Rpc_server.t -> on_websocket

(** Accepts connections until [sw] ends; returns the bound port (useful with
    port 0). *)
val listen
  :  env:Env.t
  -> sw:Switch.t
  -> addr:Eio.Net.Ipaddr.v4v6
  -> port:int
  -> root:string option
  -> on_websocket:on_websocket
  -> on_lines:on_lines
  -> int

module For_testing : sig
  val safe_relative : string -> string option
  val content_type : string -> string
  val parse_query : string -> (string * string) list
end
