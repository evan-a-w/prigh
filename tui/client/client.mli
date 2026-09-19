open! Core
open! Async
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

val create : Transport.t -> t
val call : t -> string -> (string * Json.t) list -> Json.t Deferred.Or_error.t
val incoming : t -> Incoming.t Pipe.Reader.t
val close : t -> unit
val closed : t -> unit Deferred.t

(** Typed wrappers used by tests and tools; the UI decodes replies itself. *)
val get_state : t -> State.t Deferred.Or_error.t

val get_messages : t -> Message.t list Deferred.Or_error.t
val list_models : t -> Model.t list Deferred.Or_error.t
val list_sessions : t -> Session_summary.t list Deferred.Or_error.t
val auth_status : t -> Auth_status.t list Deferred.Or_error.t
val prompt : t -> string -> unit Deferred.Or_error.t
