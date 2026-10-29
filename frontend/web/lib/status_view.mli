open! Core
open! Import

(** The status of the header over the chat (sidebar closed, or a phone),
    on one line: the context (opening the session's details), tokens and
    cost, and what the agent is doing, in a slot of its own width so that
    nothing moves as it changes; the context and the activity when narrow.
    The cost is to the cent; its title has it exactly. The sidebar's session
    card lays out {!activity}, {!cost} and {!meter} itself. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** What the agent is doing (or that the backend is out of reach). *)
val activity : App.Model.t -> Node.t

(** The session's cost to the cent (its title has it exactly), or spent /
    budget (opening [/budget]); none while out of reach. *)
val cost : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** The context's meter and percent (opening the session's details), its
    title the tokens in and out; none while out of reach or with no context
    window. *)
val meter : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** What applies: goal, reminders, resume, queued messages to take back,
    background subagents and jobs, transfers, the verbosity and, with
    [plan], the model's plan. In the header it goes before the status, so
    that the status stays put as they come and go. Where tools run is the
    composer's (its host chip). *)
val extras
  :  App.Model.t
  -> plan:bool
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t
