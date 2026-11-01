open! Core
open! Import

(** A [data:] URL. *)
val src : Image.t -> string

(** [data-viewer]: a click on it opens the [img] inside (or itself) in the
    page's image viewer (app/viewer_widget.ml): the whole image, never
    cropped, with zoom and pan, Draw ([data-viewer-draw]) and download. *)
val viewer_attribute : string

(** [data-viewer-group]: the viewer steps (← →) through its [data-viewer]s. *)
val group_attribute : string

(** A thumbnail that opens the viewer, which can draw on it: the drawing is
    attached to the next prompt. *)
val thumb : Image.t -> Node.t

(** {!thumb}s in a row, one group in the viewer. *)
val thumbs : Image.t list -> Node.t
