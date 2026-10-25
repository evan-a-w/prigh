open! Core
open! Import

(** The prompt editor with pending images and the completion popup, in one box
    with, along its bottom, the model, thinking and mode (each opening its
    picker) and attach, stop and send/steer. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
