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
    [/switch], [/host], [/setusr], [/help]) arrive as [prompt]s and run here.
    A successful [/setusr NAME] sends [{"type": "prigh_set_user", "user":
    NAME}]: the frontend reconnects with the [as_user] query parameter.

    [get_state] also re-sends the session's live state that is not empty:
    status entries, queued messages ([queue_update]), unanswered tool
    confirmations and the agents rail widget. A frontend re-syncing (after
    connecting or switching sessions) clears these before asking. Switching
    sessions cancels the old session's confirmation dialogs.

    [watch_subagent {agentId | toolCallId}] answers with a subagent's info
    and transcript ([{subagent, messages}]) and from then on forwards its
    events as [{"type": "prigh_subagent_event", "agentId", "event"}], where
    [event] is a [message_start/update/end] or [tool_execution_*] event of
    its own conversation (timestamps continue the transcript's indices) or
    [{"type": "subagent_info", "subagent"}]. [watch_subagent {}] or a
    session change stops it. *)

(** Sends [hello] (with [token], [user], [session] and [name]; as [signed_in]
    when given, see [Rpc_server.connect]) and then serves until
    [read_line] returns [None]. A failed [hello] writes
    [{"type": "prigh_hello_failed", "error": ...}] and returns. [now] is the
    wall clock in milliseconds (for the subagent widget). *)
val serve_lines
  :  Rpc_server.t
  -> ?now:(unit -> int)
  -> ?token:string
  -> ?user:string
  -> ?signed_in:User_access.Signed_in.t
  -> ?session:string
  -> ?name:string
  -> read_line:(unit -> string option)
  -> write_line:(string -> unit)
  -> unit
  -> unit

(** [serve_lines] over a WebSocket on the server [Rpc_router.authenticate]
    selects, taking [token], [user], [as_user], [session] and [name] from the
    upgrade request's query string. *)
val serve_websocket : Rpc_router.t -> Web_server.on_websocket
