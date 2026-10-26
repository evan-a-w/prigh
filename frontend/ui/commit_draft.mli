open! Core

(** [/commit --generate]'s panel above the editor: the commit message the
    model is drafting, streamed; once whole it goes into the editor as a
    [/commit -m "..."] to edit and send. *)

type t =
  { gen_id : string (** the [generate_text] request's *)
  ; amend : bool
  ; text : string (** so far *)
  }
[@@deriving sexp_of, equal]

val create : gen_id:string -> amend:bool -> t
val add_delta : t -> string -> t

(** At most [max_rows] rows (including the frame); a long draft shows its
    tail. *)
val render : t -> width:int -> max_rows:int -> Content.t
