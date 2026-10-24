open! Core

(** A titled frame with rounded corners around the body, [width] columns
    wide; body lines are truncated to fit. [footer] is set in the bottom
    border, on the right (key hints, a scroll position). *)
val render
  :  ?border:Style.t (** default: gray *)
  -> ?footer:string
  -> title:string
  -> width:int
  -> Content.t
  -> Content.t

(** Columns available to body lines. *)
val inner_width : width:int -> int
