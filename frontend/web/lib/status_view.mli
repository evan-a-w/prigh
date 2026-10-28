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
    background subagents and jobs, transfers, with [host] the tool host when
    it is not the backend, verbosity and the plan. In the header it goes
    before the status, so that the status stays put as they come and go. *)
val extras
  :  App.Model.t
  -> host:bool
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t

(** Where tools run, opening the host picker ([/host]): any other host, and
    the backend too when [if_choice] and there is another to choose; [None]
    when that leaves nothing to say. *)
val host
  :  App.Model.t
  -> if_choice:bool
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t option
