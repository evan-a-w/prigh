open! Core
open! Import

(** Left, the session's title (click to rename), cwd and branch; in the
    middle, the status ([Status_view]); right, the panel toggles and the
    settings. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
