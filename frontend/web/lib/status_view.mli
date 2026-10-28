open! Core
open! Import

(** The status: the context (opening the session's details), tokens and
    cost, and what the agent is doing, in a slot of its own width so that
    nothing moves as it changes. The sidebar's session card stacks them; the
    header over the chat (sidebar closed, or a phone) keeps them on one
    line, the context and the activity when narrow. The cost is to the cent;
    its title has it exactly. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** What applies: goal, reminders, resume, queued messages to take back,
    background subagents and jobs, transfers, verbosity and the plan. In the
    header it goes before the status, so that the status stays put as they
    come and go. *)
val extras : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
