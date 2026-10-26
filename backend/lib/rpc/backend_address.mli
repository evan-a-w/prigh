open! Core
open! Import

(** Where a backend is, as [-connect] names it:

    - [HOST:PORT]: the backend's [-listen] or [-web] port. The RPC goes over
      it as JSON lines; WebSockets and file transfers need the [-web] port.
    - a URL of its prigh-web, [ws://], [wss://], [http://] or [https://],
      optionally with the path the site is under ([wss://host/prigh/ws] or
      [https://host/prigh/]): a backend behind a (TLS-terminating) reverse
      proxy. Everything, the RPC included, goes over HTTP and WebSockets
      under that path. *)

type t =
  { tls : bool
  ; host : string
  ; port : int
  ; prefix : string
    (** the site's path without a trailing slash: [""] or e.g. ["/prigh"] *)
  ; lines : bool (** given as [HOST:PORT] *)
  }
[@@deriving sexp_of, equal]

(** The error says what is accepted. *)
val parse : string -> t Or_error.t

(** [HOST:PORT], or the WebSocket URL of the RPC ([wss://host/prigh/ws]). *)
val to_string : t -> string

(** A URL in the form {!to_string} gives (so that [https://host/] and
    [wss://host/ws] are saved and matched as one), anything else as given
    (stripped). The TUI's [Server_address.normalize] does the same. *)
val normalize : string -> string

(** The [Host] header for it. *)
val authority : t -> string

(** A connection to the server, over TLS when [tls]; raises what
    [Eio.Net.connect] and TLS raise, or [Failure] for an unknown host. *)
val connect
  :  env:Env.t
  -> sw:Switch.t
  -> t
  -> Eio.Flow.two_way_ty Eio.Resource.t

(** Raised by {!websocket} when the server does not open the WebSocket,
    with what it answered. *)
exception Not_upgraded of string

(** The WebSocket at [path] (under [prefix]) with [query]. Raises like
    {!connect}, and [Not_upgraded]. *)
val websocket
  :  env:Env.t
  -> sw:Switch.t
  -> t
  -> path:string
  -> query:(string * string) list
  -> Websocket.t
