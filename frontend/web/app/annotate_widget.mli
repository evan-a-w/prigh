open! Core

(** Drawing on an image ([Prigh_web.Annotate], its dialog
    [Annotate_view]): strokes made with the pointer on the canvas over the
    image, in the pen's colour and width (the canvas's [data-color] and
    [data-size]), kept at the image's own resolution. After each stroke,
    undo or clear, [Annotate_strokes] says how many there are. *)

(** A fresh canvas for the dialog just opened, sized to its image. *)
val setup : unit -> unit

val undo : unit -> unit
val clear : unit -> unit

(** The image with the strokes on it, as a PNG: [Annotated]. *)
val export : unit -> unit

(** The pointer and Ctrl+Z listeners. *)
val install : schedule:(Prigh_web.App.Action.t -> unit) -> unit
