open! Core
open! Import

(** The agents panel (a column right of the chat; a full-screen sheet on a
    phone): the session's subagents as a tree and its background jobs, live,
    and one of them in full: a subagent's transcript and report, a job's
    output. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** What is running, for the status line: ["2 agents · 1 job running"], or
    what has run when nothing is. [None] when nothing has. *)
val summary : Agents.t -> (string * bool) option

(** The top bar's button that opens and closes the panel. *)
val toggle : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
