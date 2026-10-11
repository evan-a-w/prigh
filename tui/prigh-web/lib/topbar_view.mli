open! Core
open! Import

(** The session's title (click to rename), cwd and branch; the model, the
    thinking level and the approval mode, each opening its picker (in a menu
    when the chat column is narrow, see style.css); the panel toggles. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
