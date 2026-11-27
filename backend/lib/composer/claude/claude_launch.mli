open! Core
open! Import

(** How to start [claude] for the composer: a long-lived process speaking
    stream-json on stdin and stdout, with permission prompts routed to us
    as [can_use_tool] control requests. *)

module Resume : sig
  type t =
    | New of { session_id : string option (** pin the new session's id *) }
    | Resume of
        { session_id : string
        ; fork : bool
          (** continue in a new session (its id from [system/init]) *)
        ; at : string option (** truncate after this assistant message's uuid *)
        }
  [@@deriving sexp_of]
end

type t =
  { executable : string (** ["claude"] *)
  ; model : string option
  ; permission_mode : Claude_permission_mode.t option
  ; resume : Resume.t
  ; mcp_config : Json.t option
    (** [{"mcpServers": {...}}]; see {!sdk_mcp_config} for prigh's in-band tools *)
  ; max_thinking_tokens : int option
  ; effort : string option (** ["low"] .. ["max"] *)
  ; setting_sources : string list
    (** which settings files apply ([--setting-sources]): by default only
        the user's, so a repository's [.claude/settings*.json] (its hooks,
        allow rules and MCP servers) does not run in prigh's sessions *)
  ; strict_mcp_config : bool
    (** only the [mcp_config] servers start, not the project's [.mcp.json]
        (default) *)
  ; no_session_persistence : bool
    (** [--no-session-persistence] for ephemeral one-shot completions *)
  ; tools : string option
    (** [--tools]: [Some ""] disables all built-in tools *)
  ; max_turns : int option (** [--max-turns] *)
  }
[@@deriving sexp_of]

val default : t

(** The command line. Values that could look like flags (start with [-])
    go after [--flag=]; [--input-format stream-json] is last, as the SDKs pass
    it. *)
val argv : t -> string list

(** Environment variables to remove from the inherited environment:
    [CLAUDECODE] (Claude Code thinks it is nested in itself otherwise). *)
val env_unset : string list

(** Environment variables to set: the SDK entrypoint and session-state
    reporting, and no IDE auto-connection. *)
val env_set : (string * string) list

(** An [--mcp-config] declaring in-band ("sdk") MCP servers by name: their
    calls arrive as [mcp_message] control requests. *)
val sdk_mcp_config : string list -> Json.t
