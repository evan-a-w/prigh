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
  ; proxy_cookie : string option
    (** the [Cookie] header a sign-in proxy in front of the site wants
        ({!Browser_signin}), sent with every request to the site; never
        printed *)
  }
[@@deriving sexp_of, equal]

(** The error says what is accepted. *)
val parse : string -> t Or_error.t

(** [HOST:PORT], or the WebSocket URL of the RPC ([wss://host/prigh/ws]). *)
val to_string : t -> string

(** Its prigh-web site: [https://host/prigh/] for [wss://host/prigh/ws],
    [http://HOST:PORT/] for [HOST:PORT] (there only if it is the backend's
    [-web] port). *)
val web_url : t -> string

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

(** {!connect}, with what shuts the TCP connection down at once (also under
    TLS, without a word to the peer), so that reads and writes stuck on it
    end. *)
val connect_abortable
  :  env:Env.t
  -> sw:Switch.t
  -> t
  -> Eio.Flow.two_way_ty Eio.Resource.t * (unit -> unit)

(** With [proxy_cookie] set (only sent to a URL's site, not a [HOST:PORT]). *)
val with_proxy_cookie : t -> string option -> t

(** What to send with each request to the site: the [Cookie] header of
    [proxy_cookie]. *)
val upgrade_headers : t -> (string * string) list

(** Raised by {!websocket} when the server does not open the WebSocket,
    with what it answered. *)
exception Not_upgraded of string

(** Raised by {!websocket} instead of [Not_upgraded] when what answered
    looks like a sign-in proxy's login (see {!signin_page}): signing in
    through the browser ({!Browser_signin}) gets the cookie it wants. *)
exception Signin_required of string

(** Whether a refused upgrade is a sign-in proxy's: a redirect (other than
    to [https] on the same host, for a [ws://] address), a 401, 403 or 407,
    or a web page; what it answered, if so. *)
val signin_page : t -> Websocket.Refusal.t -> string option

(** The WebSocket at [path] (under [prefix]) with [query]. Raises like
    {!connect}, and [Not_upgraded]. *)
val websocket
  :  env:Env.t
  -> sw:Switch.t
  -> t
  -> path:string
  -> query:(string * string) list
  -> Websocket.t

(** {!websocket} over a connection [connect] made. *)
val websocket_over
  :  Eio.Flow.two_way_ty Eio.Resource.t
  -> t
  -> path:string
  -> query:(string * string) list
  -> Websocket.t
