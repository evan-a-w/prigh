open! Core
open! Import

(** A [data:] URL. *)
val src : Image.t -> string

(** A thumbnail; clicking it shows it full size until clicked again (a
    [<details>], no script), with a button to draw on it: the drawing is
    attached to the next prompt. *)
val thumb : Image.t -> Node.t

(** {!thumb}s in a row. *)
val thumbs : Image.t list -> Node.t
