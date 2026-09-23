open! Core
open! Import

(** The browser frontend's listener: serves the static frontend from [root]
    (when given) and upgrades [GET /ws] to a WebSocket that carries the same
    JSON-lines RPC as stdio and TCP, one message per line. *)

(** Handles one HTTP/1.1 connection: one request, then close (or the
    WebSocket until it ends). *)
val handle
  :  root:string option
  -> on_websocket:(Websocket.t -> unit)
  -> _ Eio.Flow.two_way
  -> unit

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
  -> int

module For_testing : sig
  val safe_relative : string -> string option
  val content_type : string -> string
end
