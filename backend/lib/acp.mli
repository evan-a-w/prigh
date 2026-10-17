open! Core
open! Import

(** Serves the Agent Client Protocol (ACP, JSON-RPC 2.0 over
    newline-delimited JSON) to an editor, translating it to prigh's RPC: the
    adapter is an ordinary prigh client (one per ACP session, attached to
    it, plus one that is this machine's tool host), so ACP sessions are
    normal prigh sessions, shared with the TUI and prigh-web when the
    backend is a running [prigh serve].

    Methods: [initialize], [authenticate], [session/new], [session/load]
    (replays the history), [session/resume], [session/list],
    [session/close], [session/prompt], [session/cancel] (a notification),
    [session/set_mode], [session/set_config_option] ([mode], [model],
    [thinking]). Updates: message, thought and replayed user chunks, tool
    calls (kind, title, locations, text output, diffs of edits), [plan],
    [available_commands_update], [current_mode_update],
    [config_option_update], [usage_update], [session_info_update] and, when
    the client has [session.notices], [notice]. Requests to the client:
    [session/request_permission] for tool confirmations (and single-choice
    questions), [elicitation/create] forms for [ask_user] when the client
    has [elicitation.form], [fs/read_text_file] and [fs/write_text_file] for
    the file tools when the client has them (the adapter is the session's
    tool host, see {!File_access}).

    Extensions, advertised in [initialize]'s [agentCapabilities._meta.prigh]
    (and the result's [_meta.prigh]) as [{"version": 1, "extensions":
    ["_prigh/state", "_prigh/notice"]}]: the notifications [_prigh/state]
    (prigh's status line, sent when it changes) and [_prigh/notice] (prigh's
    notices). *)

module Tool_host_config : sig
  type t =
    { host_id : string
    ; name : string
    ; cwd : string
    }
end

module Config : sig
  type t =
    { connector : Rpc_client.Connector.t
    ; token : string option
    ; user : string option
    ; tool_host : Tool_host_config.t option
      (** be this machine's tool host (and route file contents through the
          editor); [None]: tools run where prigh puts them *)
    }
end

(** Serves until [read_line] returns [None]. [log] gets what goes wrong
    that the client is not told (default: stderr). *)
val serve
  :  ?log:(string -> unit)
  -> env:Env.t
  -> Config.t
  -> read_line:(unit -> string option)
  -> write_line:(string -> unit)
  -> unit
