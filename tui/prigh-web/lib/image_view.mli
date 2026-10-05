open! Core
open! Import

(** A [data:] URL. *)
val src : Image.t -> string

(** Thumbnails; clicking one shows it full size until clicked again (a
    [<details>], no script). *)
val thumbs : Image.t list -> Node.t
