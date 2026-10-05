open! Core
open! Async
open Prigh_client

(** Runs the session's tools and terminals on this machine: spawns
    [prigh tool-host] lazily and proxies between it and the backend. Feed it the
    [Tool_exec], [Tool_exec_cancel] and [Terminal_*] events; it answers with
    [tool_exec_output], [tool_exec_result], [terminal_frame] and
    [terminal_closed] requests on the client. *)
type t

(** A fresh random host id ([host-] and 16 hex digits), for [hello]'s
    [host_id]: the same on every connection of this process, so the backend
    knows us again when we reconnect. *)
val new_host_id : unit -> string

(** Spawns [backend tool-host]. *)
val spawn_worker : backend:string -> unit -> Transport.t Deferred.Or_error.t

(** [spawn] starts the worker, on first use and again after it exits. *)
val create
  :  client:Client.t
  -> spawn:(unit -> Transport.t Deferred.Or_error.t)
  -> t

(** Handles an event; other events are ignored. *)
val handle : t -> Prigh_protocol.Event.t -> unit

val close : t -> unit
