open! Core
open! Import

(** prigh's own tools offered to an external harness (Codex, Claude Code,
    an ACP agent) as an MCP server named [prigh], so a composer session keeps
    what the harness lacks: [render_ui]'s components, [memory], the other
    sessions, the forge (pull requests, issues, CI) through the user's
    sign-in, worktrees. Calls run through the runner's executor
    ([Services.execute]), so approvals and host routing stay prigh's.

    The server is a JSON-RPC handler with no I/O of its own: Claude Code
    carries its messages in-band ({!claude_reply}); Codex and ACP agents
    start [prigh mcp-bridge] on the tool host ({!Bridge}), which relays its
    stdio to the backend. *)

(** ["prigh"]: the harness names the tools [mcp__prigh__render_ui] (Claude
    Code) or reports server [prigh], tool [render_ui] (Codex, ACP). *)
val server_name : string

(** The names of the tools injected by default, in order: [render_ui],
    [memory], [custom_instructions], [sessions], [ask_session],
    [answer_session], [pull_request], [issue], [repo], [worktree],
    [reply_to_comment], and [ask_user] only when the harness has no
    questions of its own ([native_questions] false; Codex and Claude Code
    have them). Never the file and process tools ([bash], [read], [edit],
    ...): the harness has its own. *)
val default_names : native_questions:bool -> string list

(** [tools] (the session's, as [Agent] offers them) restricted to
    {!default_names}, in that order. *)
val select : native_questions:bool -> Tool.t list -> Tool.t list

(** The prigh tool an MCP call of the harness is, when it is one of ours:
    [canonical_of_mcp ~server:"prigh" ~tool:"render_ui"] is
    [Some "render_ui"], so the recorder shows (and frontends render) it as
    the prigh tool itself. [None] for other servers and unknown tools. *)
val canonical_of_mcp : server:string -> tool:string -> string option

(** The tool's MCP content: [text] (when non-empty, or when there are no
    images) then the images, with [isError]. *)
val result_to_mcp : Tool_result.t -> Json.t

type t

(** [execute tool arguments] runs a call (blocking: [ask_user] waits for
    the user). Exceptions it raises become error results. *)
val create
  :  tools:Tool.t list
  -> execute:(Tool.t -> Json.t -> Tool_result.t)
  -> t

val tools : t -> Tool.t list

(** One JSON-RPC message from the harness: the response for a request
    ([initialize], [ping], [tools/list], [tools/call]; an error for other
    methods), [None] for a notification or a response. A [tools/call] of an
    unknown tool or with non-object arguments is an [invalid params]
    error; a tool's own failure is a result with [isError]. *)
val handle : t -> Json.t -> Json.t option

(** {!handle} on one line of text: [Some line] to send back. A line that
    is not JSON gets a parse error with a null id. *)
val handle_line : t -> string -> string option

(** Claude Code's in-band MCP ([mcp_message] control requests): the line to
    write back to its stdin. Messages for another server get a JSON-RPC
    error naming the servers there are. *)
val claude_reply : t -> Claude_codec.Mcp_request.t -> string

(** How an out-of-process harness reaches the server: it starts
    [prigh mcp-bridge] (the tool host's own prigh) as a stdio MCP server,
    which relays to the Unix socket in [$PRIGH_MCP_SOCKET] that the tool
    host listens on for that harness's process ({!Mcp_bridge}). *)
module Bridge : sig
  type t =
    { command : string (** the prigh executable on the tool host *)
    ; args : string list
    ; env : (string * string) list
    }
  [@@deriving sexp_of]

  val create : prigh:string -> socket:string -> t

  (** [{command, args, env}]: the stdio server, as {!Adapter.Start.t}'s
      [mcp_servers] take it (named {!server_name}). *)
  val server_json : t -> Json.t

  (** Codex's per-thread [config] entry: [mcp_servers.prigh] =
      [{command, args, env}]. *)
  val codex_config : t -> string * Json.t

  (** An ACP [session/new] [mcpServers] element (a stdio server). *)
  val acp_server : t -> Json.t
end
