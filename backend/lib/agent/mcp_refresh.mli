open! Core
open! Import

(** When a session asks its tool host for the MCP servers ([$mcp_servers]),
    so that a run's first model request does not wait for that round trip.

    A listing is current while it is for the active host and the same
    [config] (the MCP settings and sign-ins the backend sends the host). A
    run whose listing is current starts at once and refreshes it in the
    background, for the next run: an edit to a host's [mcp.json] reaches the
    run after the one it was made before. Otherwise (the first run, another
    host, changed settings or sign-ins) the run waits for a fresh one. *)

type t

val create : unit -> t

(** Whether the latest successful listing was for [host] and [config]. *)
val current : t -> host:string -> config:string -> bool

(** Before a run on [host]. [refresh] asks the host and says whether it
    answered. Refreshes run one after another, each with the settings of its
    time; one for the same host and [config] as the one under way is that
    one. Waiting ends early with [cancel]. *)
val before_run
  :  t
  -> sw:Switch.t
  -> host:string
  -> config:string
  -> cancel:Cancellation.t
  -> refresh:(cancel:Cancellation.t -> bool)
  -> unit

(** Refreshes in the background (after the refresh under way, if any). *)
val start
  :  t
  -> sw:Switch.t
  -> host:string
  -> config:string
  -> refresh:(cancel:Cancellation.t -> bool)
  -> unit

(** Cancels the background refresh, if any. *)
val cancel : t -> unit

(** Waits for the background refresh, if any. *)
val wait : t -> unit
