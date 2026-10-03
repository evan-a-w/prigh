open! Core
open! Import

(** The browser frontend's listener: serves the static frontend from [root]
    (when given) and upgrades WebSocket requests for the paths in
    [websockets] ([/ws] carries the same JSON-lines RPC as stdio and TCP, one
    message per line; [/terminal] a shell). A connection whose first byte is
    [{] is a plain JSON-lines client (the terminal frontend's [-connect]) and
    is handed to [on_lines], so one port serves both. *)

type on_lines =
  read_line:(unit -> string option) -> write_line:(string -> unit) -> unit

type on_websocket = query:(string * string) list -> Websocket.t -> unit

(** Handles one connection: JSON lines until EOF, or one HTTP/1.1 request,
    then close (or the WebSocket until it ends). The [websockets] handler for
    the request's path gets the decoded query string of the upgrade request. *)
val handle
  :  root:string option
  -> websockets:(string * on_websocket) list
  -> on_lines:on_lines
  -> _ Eio.Flow.two_way
  -> unit

val browser_url : host:string -> port:int -> string

(** [Rpc_server.serve_lines] over a WebSocket. *)
val serve_rpc : Rpc_server.t -> on_websocket

(** A {!Terminals} socket. The query carries [token] (checked like [hello]'s),
    [session] (the terminal's key; it starts in that session's directory on
    the backend) and the initial [cols] and [rows]. *)
val serve_terminal : Rpc_server.t -> Terminals.t -> on_websocket

(** Accepts connections until [sw] ends; returns the bound port (useful with
    port 0). *)
val listen
  :  env:Env.t
  -> sw:Switch.t
  -> addr:Eio.Net.Ipaddr.v4v6
  -> port:int
  -> root:string option
  -> websockets:(string * on_websocket) list
  -> on_lines:on_lines
  -> int

module For_testing : sig
  val safe_relative : string -> string option
  val content_type : string -> string
  val parse_query : string -> (string * string) list
end
