open! Core
open! Import

(** The side question ([/btw]) and its streaming answer, above the composer. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
