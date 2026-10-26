open! Core
open! Async
open Prigh_client_unix

(** Servers on 127.0.0.1 for the transports' tests: each connection, over
    TLS when [tls] is given, goes to [handle]. *)

type t

val start
  :  ?tls:Tls.Config.server
  -> (Channel.t -> unit Deferred.t)
  -> t Deferred.t

val port : t -> int
val close : t -> unit Deferred.t

(** A TLS-terminating reverse proxy in front of [port] (a backend's [-web]):
    the bytes as they are. *)
val tls_proxy : Tls.Config.server -> port:int -> t Deferred.t

(** The server's side of a WebSocket. *)
module Websocket : sig
  module Request : sig
    type t =
      { line : string (** [GET /ws HTTP/1.1] *)
      ; headers : (string * string) list
      }
    [@@deriving sexp_of]

    (** The headers but the random [Sec-WebSocket-Key]. *)
    val show : t -> Sexp.t
  end

  (** Reads the upgrade request. *)
  val read_request : Channel.t -> Request.t option Deferred.t

  (** Answers [101 Switching Protocols]. *)
  val accept : Channel.t -> Request.t -> unit

  (** Answers [status] with [headers] and [body], then closes. *)
  val refuse
    :  Channel.t
    -> status:string
    -> headers:(string * string) list
    -> body:string
    -> unit

  module Frame : sig
    type t =
      { fin : bool
      ; opcode : int
      ; masked : bool
      ; payload : string
      }
    [@@deriving sexp_of]
  end

  (** Unmasked, as a server sends. *)
  val send : ?fin:bool -> Channel.t -> opcode:int -> string -> unit

  val read : Channel.t -> Frame.t option Deferred.t
end
