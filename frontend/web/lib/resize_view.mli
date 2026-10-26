open! Core
open! Import

(** A panel's resize handle: a focusable separator on its inner edge with
    its size in [aria-valuenow] and, for the page's pointer listener
    (app/resize_widget.ml), the CSS variable, the direction it
    grows in and the element that shows the size while dragging. Arrow keys
    on it go through [Keys]; a double click resets it. None on a phone. *)
val handle
  :  App.Model.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Resize.Handle.t
  -> Node.t

(** The sizes, as CSS variables on the page's root. *)
val vars : App.Model.t -> Attr.t
