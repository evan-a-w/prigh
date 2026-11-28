open! Core
open! Import

(** Which external coding agent a composer session drives: Codex
    ([codex app-server]), Claude Code ([claude], stream-json), an agent
    speaking ACP (named by its command, [acp:<name>]), a backend-hosted
    adapter ([backend:<adapter>], running the CLI on the backend while
    routing tools to the session's tool host over MCP), or prigh's own
    harness ([prigh]). *)
type t =
  | Codex
  | Claude
  | Acp of string
  | Backend of t
  | Prigh
[@@deriving sexp, equal, compare]

(** [codex], [claude], [acp:<name>], [backend:<adapter>], [prigh]. *)
val to_string : t -> string

(** Parses an adapter name: [codex], [claude], [prigh], bare known ACP
    agent names ([antigravity], [opencode], [gemini], [grok], [goose]),
    [acp:<name>], [bridge:<adapter>] or the backwards-compatible alias
    [backend:<adapter>]. An error lists what is known. *)
val of_string : string -> t Or_error.t

(** For people: [Codex], [Claude Code], [<name> (ACP)], [prigh] (same bare
    harness name as {!harness_label}, without any mode suffix). *)
val label : t -> string

(** The bare harness name without mode suffix: [Backend (Acp "antigravity")]
    and [Acp "antigravity"] both return ["Antigravity"]. *)
val harness_label : t -> string

(** Whether the adapter's CLI runs on the backend with tools routed to the
    session's tool host. *)
val on_backend : t -> bool

(** Strips {!Backend}, returning the underlying adapter. *)
val inner : t -> t

(** Whether [t] is an external CLI harness (everything except {!Prigh}). *)
val is_external : t -> bool

(** The ACP agent name when [t] is [Acp name] or [Backend (Acp name)]. *)
val acp_name : t -> string option
