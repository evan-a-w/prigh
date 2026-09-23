open! Core
open! Async_kernel
open Prigh_protocol

(** JSON-lines RPC client: correlates responses by id and fans out events. *)

module Incoming : sig
  type t =
    | Event of Event.t
    | Protocol_error of string
    | Stderr of string
    | Closed
  [@@deriving sexp_of]
end

type t

(** Not yet connected: call [connect]. [connect] is called again for every
    reconnection. *)
val create : connect:(unit -> Transport.t Deferred.Or_error.t) -> t

(** Opens the transport. A no-op when connected; concurrent calls share one
    attempt. When the transport later closes, pending calls fail, [Closed] is
    delivered on [incoming] and the client is disconnected again. *)
val connect : t -> unit Deferred.Or_error.t

val is_connected : t -> bool

(** Fails immediately with "not connected" while disconnected. *)
val call : t -> string -> (string * Json.t) list -> Json.t Deferred.Or_error.t

(** Stays open across reconnections; ends with [close]. *)
val incoming : t -> Incoming.t Pipe.Reader.t

(** Closes the transport (if any) and [incoming]; determined once the transport
    has shut down. *)
val close : t -> unit Deferred.t

(** Typed wrappers used by the e2e test; the UI decodes replies itself. *)
val list_models : t -> Model.t list Deferred.Or_error.t

val list_sessions : t -> Session_summary.t list Deferred.Or_error.t
