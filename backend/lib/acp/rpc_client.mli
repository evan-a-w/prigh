open! Core
open! Import

(** One client connection to prigh's JSON-lines RPC (see {!Rpc_server}),
    either in this process or over TCP to a [prigh serve -listen] (or
    [-web]) backend. Requests block their fiber until answered; events go to
    the connection's [on_event] in the order the backend sent them. *)

type t

(** [method_] with [params]; its [result], or its [error]. Fails at once
    when the connection is closed. *)
val call : t -> string -> (string * Json.t) list -> Json.t Or_error.t

(** Ends the connection (the backend forgets the client; its session keeps
    running). *)
val close : t -> unit

module Connector : sig
  type client := t

  (** Opens a connection. [on_event] gets every event and must not block;
      [on_close] is called with the reason when the backend ends the
      connection (not after [close]). The first request should be
      [hello]. *)
  type t =
    on_event:(Json.t -> unit) -> on_close:(string -> unit) -> client Or_error.t

  (** A client of [server], in this process. *)
  val in_process : Rpc_server.t -> t

  (** A TCP connection to [host]:[port], which lives in [sw]. *)
  val tcp : env:Env.t -> sw:Switch.t -> host:string -> port:int -> t
end
