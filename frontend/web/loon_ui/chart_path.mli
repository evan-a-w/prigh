(** SVG paths for {!Chart}: d3-shape's curves (as Recharts' [type] prop
    names them), and Recharts' rectangle, sector and polygon shapes. *)

open! Import

module Curve : sig
  type t =
    | Linear
    | Natural (** d3's [curveNatural]: a natural cubic spline *)
    | Monotone (** d3's [curveMonotoneX] (Y in a vertical layout) *)
    | Step
    | Step_before
    | Step_after
    | Bump (** d3's [curveBumpX] *)
    | Linear_closed
  [@@deriving enumerate, sexp_of, compare, equal]

  (** Recharts' name: ["natural"], ["monotone"], ["stepBefore"], ... *)
  val name : t -> string
end

type point = float * float

(** d3's [line().curve(curve)] over [points]. With [~vertical:true], as
    Recharts in a vertical layout ([Monotone] is then [curveMonotoneY]). *)
val line : ?vertical:bool -> Curve.t -> point list -> string

(** d3's [area().curve(curve)]: along [top], then back along [base] (given
    in the same order as [top]), closed. *)
val area : ?vertical:bool -> Curve.t -> top:point list -> base:point list -> string

(** Recharts' [Rectangle] path; [radius] is all four corners, or
    top-left, top-right, bottom-right, bottom-left. *)
val rectangle
  :  ?radius:[ `All of float | `Corners of float * float * float * float ]
  -> x:float
  -> y:float
  -> width:float
  -> height:float
  -> unit
  -> string

(** Recharts' [polarToCartesian]: angles in degrees, anticlockwise from
    3 o'clock. *)
val polar : cx:float -> cy:float -> radius:float -> angle:float -> point

(** Recharts' [Sector] path ([None] when it is empty). *)
val sector
  :  ?corner_radius:float
  -> ?force_corner_radius:bool
  -> cx:float
  -> cy:float
  -> inner_radius:float
  -> outer_radius:float
  -> start_angle:float
  -> end_angle:float
  -> unit
  -> string option

(** A closed polygon through [points] (Recharts' Radar and polar grid). *)
val polygon : point list -> string

(** Degrees to radians: [pi / 180]. *)
val radian : float

(** -1, 0 or 1. *)
val sign : float -> float

(** Recharts' [getDeltaAngle]: the signed sweep, at most 359.999°. *)
val delta_angle : float -> float -> float
