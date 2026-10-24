open! Core

(** A titled frame around the body, [width] columns wide; body lines are
    truncated to fit. *)
val render
  :  ?border:Style.t (** default: gray *)
  -> title:string
  -> width:int
  -> Content.t
  -> Content.t

(** Columns available to body lines. *)
val inner_width : width:int -> int
