open! Core
open! Import

(** Serves pi's RPC protocol (what the pi web frontend speaks) on top of an
    [Rpc_server]: one connection is one prigh client whose requests are
    translated to prigh methods and whose events are translated back into
    pi's ([message_update] with the accumulated message,
    [tool_execution_start/update/end], [extension_ui_request] dialogs for
    tool confirmations and login prompts, an agents-rail widget for
    subagents, [session_reloaded] when the model or session changes).

    Beyond pi's commands it answers [list_sessions] and [switch_session], and
    the prigh-only slash commands ([/login], [/logout], [/auth], [/sessions],
    [/switch], [/host], [/help]) arrive as [prompt]s and run here. *)

(** Sends [hello] (with [token], [user], [session] and [name]) and then serves until
    [read_line] returns [None]. A failed [hello] writes
    [{"type": "prigh_hello_failed", "error": ...}] and returns. [now] is the
    wall clock in milliseconds (for the subagent widget). *)
val serve_lines
  :  Rpc_server.t
  -> ?now:(unit -> int)
  -> ?token:string
  -> ?user:string
  -> ?session:string
  -> ?name:string
  -> read_line:(unit -> string option)
  -> write_line:(string -> unit)
  -> unit
  -> unit

(** [serve_lines] over a WebSocket on the server [token] selects, taking
    [token], [user], [session] and [name] from the upgrade request's query
    string. *)
val serve_websocket : Rpc_router.t -> Web_server.on_websocket
