open! Core
open! Import

(** Kept-alive HTTP/1.1 connections, by scheme, host and port, so that
    requests to the same server skip the TCP and TLS handshakes.

    The sockets belong to no Eio switch: a connection outlives the request
    that opened it, and is closed when the pool discards it (broken, not
    reusable, idle too long or over the caps) or with {!close}. *)

type t

(** [max_idle] connections in all, [max_idle_per_host] for one server, each
    kept for [idle_timeout] after its last use (expired ones are closed when
    the pool is next used). [tls] checks the servers' certificates (default:
    {!system_tls}). *)
val create
  :  ?max_idle:int
  -> ?max_idle_per_host:int
  -> ?idle_timeout:Time_ns.Span.t
  -> ?tls:Tls.Config.client Lazy.t
  -> unit
  -> t

(** The process's pool: 16 idle connections, 4 per server, 50 s each
    (servers commonly close idle ones after a minute). *)
val default : t

(** Certificates checked against the system's CAs. *)
val system_tls : Tls.Config.client Lazy.t

module Connection : sig
  type t

  val flow : t -> [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t

  (** Whether it carried an earlier request: a failure before any reply may
      only mean the server closed it meanwhile. *)
  val reused : t -> bool
end

(** An idle connection to the URI's server that still looks open, or a new
    one. Raises what connecting raises (Eio, Unix and TLS exceptions). *)
val acquire : t -> env:Env.t -> Uri.t -> Connection.t

(** Always opens a fresh connection, bypassing any idle ones in the pool. *)
val acquire_fresh : t -> env:Env.t -> Uri.t -> Connection.t

(** Returns a connection whose last response was read to its end and which
    the server keeps open. *)
val release : t -> Connection.t -> unit

(** Closes a connection that is not to be reused. *)
val close : Connection.t -> unit

(** Connections idle in the pool now. *)
val idle : t -> int
