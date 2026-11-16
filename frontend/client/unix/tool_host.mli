open! Core
open! Async
open Prigh_client

(** Runs the session's tools and terminals on this machine: spawns
    [prigh tool-host] ({!start}, or at the first use) and proxies between it
    and the backend. Feed it the [Tool_exec], [Tool_exec_cancel],
    [Tool_exec_input] and [Terminal_*] events; it answers with
    [tool_exec_output], [tool_exec_result], [terminal_frame] and
    [terminal_closed] requests on the client. *)
type t

(** Spawns [backend tool-host]. *)
val spawn_worker : backend:string -> unit -> Transport.t Deferred.Or_error.t

(** [spawn] starts the worker, at {!start} or the first use, and again at the
    next use after it exits. When it exits, the tool calls it was running fail
    with the end of its stderr, and (unless {!close}d) [on_exit] gets a notice
    for the user. *)
val create
  :  ?on_exit:(string -> unit)
  -> client:Client.t
  -> spawn:(unit -> Transport.t Deferred.Or_error.t)
  -> unit
  -> t

(** Starts the worker in the background unless it is running or starting, so
    that the first tool call need not wait for a process to start. A worker
    that cannot start is tried again (and its error reported) at the first
    use. *)
val start : t -> unit

(** While the backend is away ([false]), the worker's results and (some of)
    its output wait; back online, they are sent. The backend keeps the
    executions of a host that reconnects with them running
    ({!running_execs}). *)
val set_online : t -> bool -> unit

(** The executions not answered yet: running in the worker, or with a result
    waiting for the backend. Sent in [hello] as [running_execs] on
    reconnecting. *)
val running_execs : t -> string list

(** Handles an event; other events are ignored. *)
val handle : t -> Prigh_protocol.Event.t -> unit

val close : t -> unit
