open! Core
open! Import

(** Any ACP agent (OpenCode, Gemini CLI, ...), driven over the Agent Client
    Protocol on its stdio ({!Acp_client_codec}), strictly by the spec: what
    the agent can do comes from what it advertises.

    - prigh's tools reach it as a stdio MCP server (the [prigh mcp-bridge]
      on its host), in [session/new]'s [mcpServers].
    - A recorded thread is continued with [session/resume], else
      [session/load] (its replay is dropped: prigh keeps its own log); an agent
      with neither starts a new session, with a notice: the session's
      handoff is what carries the conversation over then. Some agents never
      answer [session/load]: once the replay has gone quiet for
      [load_quiet] (2s), the session counts as loaded.
    - Plan mode is the agent's [plan] mode, when it has one; the model, its
      [model] config option, when it offers the session's. *)

val create
  :  ?load_quiet:Time_float.Span.t
  -> ?load_timeout:Time_float.Span.t (** 90s *)
  -> ?autonomous_quiet:Time_float.Span.t
       (** how long an autonomous turn stays open after its last text with no
           open tools or permissions (default 5s) *)
  -> Acp_agent.t
  -> Adapter.t

(** What an ACP agent can do, from what it advertises: no rewind or context
    edits; compaction through its compact command ([compact_command]: it
    lists [/compact] or [/compress]), else by prigh's summary; plan mode when
    it has a [plan] mode; images when it takes them. *)
val capabilities_of
  :  ?quirks:Acp_agent.Quirks.t (** steering mid-turn, for an agent that can *)
  -> ?compact_command:bool
  -> Acp_client_codec.Agent_capabilities.t option
  -> Acp_client_codec.Session_config.t
  -> Capabilities.t

(** Whether starting an agent failed because it is not logged in on its host
    ([-32000 Authentication required] to [session/new]). *)
val is_not_logged_in : Error.t -> bool
