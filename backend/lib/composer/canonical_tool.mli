open! Core
open! Import

(** An external harness's tool call as the prigh tool it amounts to, so that
    the session records it like a harness session's (the frontends' tool
    cards, approval rules, search and plans then apply): a command is
    [bash {command}], a file change [edit]/[write]/[apply_patch], a read
    [read {path}], a search [grep]/[find], a web search [web_search] or
    [web_fetch], an MCP tool [mcp__SERVER__TOOL], the harness's todo list
    [update_plan], a child agent [subagent {task}]; anything else keeps the
    harness's own name and input. *)

type t =
  { name : string
  ; arguments : Json.t
  ; native_name : string (** what the harness called it *)
  }
[@@deriving sexp_of]

val of_start
  :  kind:Adapter_event.Tool_kind.t
  -> native_name:string
  -> input:Json.t
  -> t

(** The tool result text prigh's own tool would have given: for file
    changes, the diffs the frontends render ([edit]: the unified diff;
    [apply_patch]: its [Success.] summary and diffs); otherwise [output]. *)
val result_text
  :  t
  -> ok:bool
  -> output:string
  -> changes:Adapter_event.Change.t list
  -> string

(** The [update_plan] call recording [plan]. *)
val of_plan : Plan.t -> t
