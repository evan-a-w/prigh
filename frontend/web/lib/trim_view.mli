open! Core
open! Import

(** At the start of a transcript trimmed of its earliest messages
    ([State.trim]), once loaded down to there: a divider saying how much is
    out of the model's context, with Show (the trimmed messages above it,
    dimmed and read-only) or Hide, and Undo. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** The class of the divider, for the page to scroll to it. *)
val divider_class : string
