open! Core
open! Import

(** The browser frontend's listener: serves the static frontend from [root]
    (when given; see {!Static_site}) and upgrades WebSocket requests for the paths in
    [websockets] ([/ws] carries the same JSON-lines RPC as stdio and TCP, one
    message per line; [/terminal] a shell). A connection whose first byte is
    [{] is a plain JSON-lines client (the terminal frontend's [-connect]) and
    is handed to [on_lines], so one port serves both. *)

module Request : sig
  type t =
    { meth : string
    ; target : string (** the path and query as sent *)
    ; path : string
    ; query : (string * string) list (** decoded *)
    ; headers : (string * string) list
    }
  [@@deriving sexp_of]

  val header : t -> string -> string option
end

type on_lines =
  read_line:(unit -> string option) -> write_line:(string -> unit) -> unit

type on_websocket = query:(string * string) list -> Websocket.t -> unit
type flow = Eio.Flow.two_way_ty Eio.Resource.t

(** Answers an HTTP request read from [reader] (whose body, if any, follows
    in it) on the connection. *)
type on_http = Request.t -> reader:Eio.Buf_read.t -> flow -> unit

(** Writes the response, with [Connection: close]. *)
val respond : flow -> Static_site.Response.t -> unit

(** Handles one connection: JSON lines until EOF, or one HTTP/1.1 request,
    then close (or the WebSocket until it ends). The [websockets] handler for
    the request's path gets the decoded query string of the upgrade request.
    With [clock], a WebSocket whose peer looks gone ({!Websocket.keepalive})
    is shut down and its handler cancelled. Another request goes to
    [routes]'s handler for it, if any, else to {!File_http} or the static
    site. *)
val handle
  :  ?clock:_ Eio.Time.clock
  -> ?routes:(Request.t -> on_http option)
  -> root:string option
  -> websockets:(string * on_websocket) list
  -> on_lines:on_lines
  -> _ Eio.Flow.two_way
  -> unit

val browser_url : host:string -> port:int -> string

(** [prigh serve]'s warning for [flag] listening on [addr] when that is not
    a loopback address: prigh serves no TLS, so tokens and everything else
    go in the clear unless a proxy or tunnel protects them. *)
val plaintext_warning : flag:string -> Eio.Net.Ipaddr.v4v6 -> string option

(** Turns on TCP keepalive probes (after 30s idle, every 10s, 3 tries), so
    that a peer gone without closing its connection is noticed. *)
val keepalive : _ Eio.Net.stream_socket -> unit

(** [Rpc_router.serve_lines] over a WebSocket. *)
val serve_rpc : Rpc_router.t -> on_websocket

(** A {!Terminals} socket. The query carries [token] and optionally [user]
    and [as_user]
    (selecting the server, like [hello]'s), [session] (the terminal's key, within the namespace) and
    the initial [cols] and [rows]. The terminal runs on the session's active
    tool host, in its directory there, relayed when that is a client (see
    [Rpc_server.terminal_target]). *)
val serve_terminal : Rpc_router.t -> Terminals.t -> on_websocket

(** Accepts connections until [sw] ends; returns the bound port (useful with
    port 0). *)
val listen
  :  env:Env.t
  -> sw:Switch.t
  -> ?routes:(Request.t -> on_http option)
  -> addr:Eio.Net.Ipaddr.v4v6
  -> port:int
  -> root:string option
  -> websockets:(string * on_websocket) list
  -> on_lines:on_lines
  -> unit
  -> int

module For_testing : sig
  val parse_query : string -> (string * string) list
end
