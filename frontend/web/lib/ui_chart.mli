open! Core
open! Import

(** A [chart] component, drawn as SVG in the theme's series colours
    ([--series-N]), with axes at nice ticks, a legend and tooltips on hover
    and keyboard focus; its width follows the page's. *)

val view : Ui_context.t -> path:string -> Ui_spec.Chart.t -> Node.t
