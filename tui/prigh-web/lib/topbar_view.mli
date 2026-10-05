open! Core
open! Import

(** The session's title (click to rename), cwd and branch, and the model and
    thinking chips. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
