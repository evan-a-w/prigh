open! Core
open! Import

(** Claude Code's built-in tools in prigh's neutral terms: what kind of
    call each is, a one-line title, and the file changes an edit made. *)

(** [Bash] is a command, [Edit]/[MultiEdit]/[Write]/[NotebookEdit] change
    files, [Read], [Grep]/[Glob] search, [WebSearch]/[WebFetch],
    [mcp__server__tool], [TodoWrite], and [Task]/[Agent] (renamed in newer
    versions) a subagent. *)
val kind : string -> Adapter_event.Tool_kind.t

(** The salient argument, e.g. ["Bash: dune build"], ["Edit src/a.ml"]. *)
val title : name:string -> input:Json.t -> string

(** What an edit changed, from its input and, when Claude Code reports it,
    its structured result ([tool_use_result]: the patch, and whether a
    [Write] created the file). [[]] for other tools. *)
val changes
  :  name:string
  -> input:Json.t
  -> result:Json.t option
  -> Adapter_event.Change.t list

(** [TodoWrite]'s todos as a plan ([None] when there are none). *)
val plan_of_todos : Json.t -> Plan.t option
