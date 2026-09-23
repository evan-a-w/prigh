open! Core
open! Async
open Prigh_client

(** Runs the session's tools on this machine: spawns [prigh tool-host] lazily
    and proxies between it and the backend. Feed it the [Tool_exec] and
    [Tool_exec_cancel] events; it answers with [tool_exec_output] and
    [tool_exec_result] requests on the client. *)
type t

val create : client:Client.t -> backend:string -> t

(** Handles an event; other events are ignored. *)
val handle : t -> Prigh_protocol.Event.t -> unit

val close : t -> unit
