open! Core
open! Import

(** Chat mode: a conversation that ignores the tool host completely. The
    model is offered only tools that need no machine (the web, the user,
    memory, other sessions, the plan, subagents, which run in chat mode
    too), [web_fetch] runs in the backend itself, and the system prompt
    describes no machine. A session with no tool host can always talk in
    chat mode. *)

(** Whether the tool named so may run in chat mode. *)
val allowed : string -> bool

(** [tools] as offered in chat mode: those {!allowed}, with [web_fetch]
    replaced by {!Tool_web_fetch.in_backend} (refusing the server's own
    network unless [allow_local]). *)
val tools : allow_local:bool -> Tool.t list -> Tool.t list

(** Why a call of the tool named so is refused in chat mode. *)
val refusal : string -> string
