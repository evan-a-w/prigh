open! Core
open! Import

(** [exit_plan_mode]: in plan mode, the model presents its plan (the [plan]
    argument, markdown) and asks to leave plan mode. The agent's approver
    handles the whole call ({!Agent}): it asks the user like a confirmation
    and, when they approve, leaves plan mode and tells the model to
    implement the plan; otherwise the model keeps planning, with the user's
    reason. Only the main agent has it. *)

val tool : Tool.t
val name : string

(** The [plan] argument. *)
val plan : Json.t -> string
