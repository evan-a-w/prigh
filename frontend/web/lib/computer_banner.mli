open! Core
open! Import

(** While the agent controls the user's screen (the user allowed it for
    this run: the state's [computer_control]), a strip above the composer
    says so and whose computer it is, with Stop (as Esc: aborts the run,
    which ends control). Nothing otherwise, also while it is still asking. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
