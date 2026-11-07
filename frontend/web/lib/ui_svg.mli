open! Core
open! Import

(** SVG elements for the components' drawings. *)

val el : string -> (string * string) list -> Node.t list -> Node.t

(** [n] with at most 2 decimals, for coordinates. *)
val num : float -> string

(** An [svg] whose [viewBox] is [0 0 width height], stretched to its box
    ([preserveAspectRatio="none"]: give strokes [vector-effect]) unless
    [keep_ratio]. *)
val svg
  :  ?cls:string
  -> ?keep_ratio:bool
  -> width:float
  -> height:float
  -> Node.t list
  -> Node.t

(** A line through [points] ([M x y L x y ...]); a [None] breaks it. *)
val path_of_points : (float * float) option list -> string
