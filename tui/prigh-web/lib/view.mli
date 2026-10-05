open! Core
open! Import

(** The page, from the model. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
