open! Core

(** Backend events (JSON) of the main agent and its subagents, for tests. *)

(** An assistant message calling a tool, [args] its arguments as JSON. *)
val call : id:string -> name:string -> string -> string

val text_reply : string -> string
val tool_start : id:string -> name:string -> string -> string
val tool_end : id:string -> name:string -> string -> string

(** [json], an event of the subagent [agent] that the call [call] started. *)
val inner : call:string -> agent:string -> string -> string

val start : call:string -> agent:string -> string -> string

val end_
  :  ?error:bool
  -> call:string
  -> agent:string
  -> turns:int
  -> string
  -> string
