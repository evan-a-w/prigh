open! Core
open! Import

(** The status in the middle of the top bar: the context (opening the
    session's details), tokens and cost, then what the agent is doing, in a
    slot of its own width so that nothing moves as it changes. A narrow top
    bar keeps the context and the activity. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** What applies, after the status (before it on a phone, so that the
    status stays put as they come and go): goal, reminders, resume, queued
    messages to take back, background subagents and jobs, transfers, the
    tool host, verbosity and the plan. *)
val extras : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
