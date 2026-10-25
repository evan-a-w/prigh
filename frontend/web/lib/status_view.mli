open! Core
open! Import

(** The status cluster in the middle of the top bar: what the agent is doing,
    the context (opening the session's details), tokens, cost, then what
    applies (goal, reminders, resume, queued messages to take back,
    background subagents and jobs, transfers, the tool host, verbosity and
    the plan). A narrow top bar keeps the activity and the context. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
