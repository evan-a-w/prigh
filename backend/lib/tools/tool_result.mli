open! Core

type t =
  { text : string
  ; is_error : bool
  ; images : Image.t list (** for the model to see, after [text] *)
  }
[@@deriving sexp_of]

val ok : ?images:Image.t list -> string -> t
val error : string -> t
