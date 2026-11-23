open! Core
open! Import

(** Beside the transcript, once the user has sent two messages, but not on a
    phone: a rail of marks along the chat's left edge, one per landmark
    ([Jump.points]), each naming its message on hover or focus and jumping
    there when clicked. The jump list has no button: Ctrl+J opens it, and a
    phone's ⋯ sheet. The page marks the one in view ([aria-current]) as the
    chat scrolls. Made again only when the landmarks change, not at every
    streamed delta. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
