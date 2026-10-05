open! Core
open! Import

(** The prompt editor with pending images, the completion popup, send/steer and
    stop, and the status line. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
