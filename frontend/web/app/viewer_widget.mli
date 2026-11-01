open! Core

(** The image viewer: a click on a [Image_view.viewer_attribute] opens its
    image in a modal dialog, the whole image (never cropped), with zoom
    (wheel, Ctrl+wheel and pinch, the buttons, + − 0 1, double-click or
    double-tap) about the pointer, pan (drag, arrows) clamped so it can't
    be lost, ← → through its [Image_view.group_attribute], Draw (for a
    [data-viewer-draw]) and download. Esc or a tap beside the image closes
    it, and focus goes back to the thumbnail. The maths is
    [Prigh_web.Zoom]. *)

val install : schedule:(Prigh_web.App.Action.t -> unit) -> unit
val is_open : unit -> bool
