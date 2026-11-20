open! Core
open! Import

(** The session's status: what the agent is doing, how full the context is,
    the cost, and the notes on what applies (the composer's
    {!Context_ring_view} shows them). *)

(** ["12k"], ["1.2M"]. *)
val tokens : int -> string

(** How full the context is, in percent; [None] while the model's window is
    unknown. *)
val context_percent : State.t -> float option

(** [""], ["warm"] from 60%, ["hot"] from 85%. *)
val context_level : float -> string

(** The agent is working or a prompt is on its way: what turns the context
    ring. *)
val busy : App.Model.t -> bool

(** What the agent is doing, in a word or two: ["Ready"], ["Running
    tools"], ["Reconnecting…"]. *)
val activity_word : App.Model.t -> string

(** The cost to the cent; spent / budget (warm from 80%, hot once spent)
    with a budget, opening [/budget]. *)
val cost : State.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** ["warm"] from a budget's 80%, ["hot"] once spent. *)
val level_class : Prigh_ui.Loop_status.Level.t -> string

(** The width of a bar filled to [percent]. *)
val fill : float -> Css_gen.t

(** A short thin bar filled to [percent], coloured by its level. *)
val meter : float -> Node.t

(** What applies, only while it does: the goal, reminders, a resume after a
    usage limit, a Nix collection, transfers and the model's plan; each opens its command. *)
val notes : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
