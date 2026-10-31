open! Core
open! Import

(** The session's status, at the right of the header over the chat
    ({!Session_view.bar}), on one quiet line: what the agent is doing (a dot
    when idle, its words only while it works or is out of reach), a short
    thin meter of the context with its percent (its details show over it on
    hover or focus, a click opens the session's details, [/session]) and the
    cost to the cent (spent / budget with a budget, opening [/budget]). The
    tokens, the exact cost, the model and the branch are in the details. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** What the agent is doing (or that the backend is out of reach). *)
val activity : App.Model.t -> Node.t

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
