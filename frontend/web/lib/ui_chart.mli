open! Core
open! Import

(** A [chart] component, drawn as SVG in the theme's series colours
    ([--series-N]), with axes at nice ticks, a legend and tooltips on hover
    and keyboard focus; its width follows the page's.

    Text never scales with the drawing: tick labels, markers, bars and
    tooltips are HTML placed in percentages over a plot whose lines and areas
    are an SVG stretched to its box. Each label of a category chart is a
    focusable column ([.ui-chart-col]) followed by its tooltip
    ([.ui-chart-tip]), which CSS shows on hover and focus; a pie's slices
    ([.ui-chart-slice]) and a scatter's points ([.ui-chart-pt]) likewise. A
    visually hidden table has the data for screen readers. *)

val view : Ui_context.t -> path:string -> Ui_spec.Chart.t -> Node.t

(** The value axis's ticks for [values]: from zero when [zero] (bars). *)
val axis : ?zero:bool -> ?count:int -> float list -> float list

(** [v]'s position on an axis from [lo] to [hi], as a percentage. *)
val position : lo:float -> hi:float -> float -> float

(** A value with its unit: [12%], [$1,200], [340 ms]. *)
val with_unit : unit:string option -> string -> string

(** The SVG path of a pie's slice from [start] to [stop] (fractions of the
    turn, clockwise from the top) in a [100 x 100] box; a ring's when [hole]
    (the inner radius) is not zero. *)
val slice_path : hole:float -> start:float -> stop:float -> string
