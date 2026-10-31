open! Core
open! Import

(** The drawing dialog ([Dialog.Annotate]): the image with a canvas over it
    (drawn on by the page, app/annotate_widget.ml), the pen's colours and
    widths, undo, clear, and cancel or save. *)
val view : Annotate.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
