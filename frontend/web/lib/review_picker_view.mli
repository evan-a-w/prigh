open! Core
open! Import

(** A review page picker's menu ([Dialog.Review_pick]), under its button:
    a filter, the choices under their headings (the current one checked),
    and, for the base, what is typed as a last choice. *)
val view : Review_picker.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
