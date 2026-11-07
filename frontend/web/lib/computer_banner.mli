open! Core
open! Import

(** While the agent drives the user's screen (the current run called
    [computer]), a strip above the composer says so and whose computer it
    is, with Stop (as Esc: aborts the run). Nothing otherwise. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** Whether it shows. *)
val shown : App.Model.t -> bool
