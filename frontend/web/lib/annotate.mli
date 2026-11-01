open! Core
open! Import

(** Drawing on an image ([Dialog.Annotate]): one being attached to the
    next prompt, or one in the transcript, which the drawing then attaches.
    The strokes are the page's (app/annotate_widget.ml, on a canvas over
    the image); here are the pen and how many strokes there are to undo. *)

module Target : sig
  type t =
    | Pending of int (** the composer's image: saving replaces it *)
    | New (** a transcript's image: saving attaches the drawing *)
  [@@deriving sexp_of, equal]

  (** For the lightbox's button ([data-annotate]): [pending:N] or [new]. *)
  val to_string : t -> string

  val of_string : string -> t option
end

type t =
  { image : Image.t
  ; target : Target.t
  ; color : string (** a CSS colour from {!colors} *)
  ; size : int (** a width from {!sizes}, in screen pixels *)
  ; strokes : int
  }
[@@deriving sexp_of, equal]

val create : Image.t -> Target.t -> t

(** Name and CSS colour. *)
val colors : (string * string) list

(** Name and width. *)
val sizes : (string * int) list
