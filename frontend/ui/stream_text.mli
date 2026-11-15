open! Core

(** Text that streams in pieces: [append] costs the piece, not the text so
    far; [contents] joins the pieces added since it was last asked (once for
    every version sharing them). *)

type t [@@deriving sexp_of]

val of_string : string -> t
val append : t -> string -> t
val contents : t -> string
