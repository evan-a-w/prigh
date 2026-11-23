(** Beautiful charts. Built using Recharts. Copy and paste into your apps.
    Port of shadcn/ui's Chart.

    shadcn's [ChartContainer], [ChartTooltipContent], [ChartLegendContent]
    and [ChartStyle] wrap Recharts; here the charting is OCaml too, drawing
    the SVG Recharts draws (its [recharts-*] classes, which [container]'s
    Tailwind selectors style as shadcn's do).

    A chart is a {!Spec.t} (Recharts' [<AreaChart data>] with its children
    as {!element}s), drawn by {!container}:

    {[
      let chart = Chart.component graph in
      let%arr chart in
      Chart.container ~config ~class_:"min-h-[200px] w-full" chart
        (Chart.bar_chart ~data
           [ Chart.cartesian_grid ~vertical:false ()
           ; Chart.x_axis ~data_key:"month" ~tick_line:false ()
           ; Chart.tooltip ~content:(Chart.tooltip_content ()) ()
           ; Chart.bar ~data_key:"desktop" ~fill:"var(--color-desktop)" ~radius:(`All 4.) ()
           ])
    ]} *)

open! Import

module Curve = Chart_path.Curve

(** A data value: a row's fields are numbers or text. *)
module Value : sig
  type t =
    [ `Num of float
    | `Text of string
    ]
  [@@deriving sexp_of, equal]

  (** As JavaScript's [String(v)]. *)
  val to_string : t -> string
end

(** One datum: [[ "month", `Text "January"; "desktop", `Num 186. ]]. *)
type row = (string * Value.t) list

module Row : sig
  type t = row

  val find : t -> string -> Value.t option
  val num : t -> string -> float option
  val text : t -> string -> string option
end

(** A number as [toLocaleString()] (en-US) writes it: "1,234.5". *)
val to_locale_string : float -> string

(** shadcn's [ChartConfig]: each series' (or category's) label, icon and
    colour, by key. A colour becomes the CSS variable [--color-<key>] on the
    container. *)
module Config : sig
  type color =
    | Color of string
    | Theme of
        { light : string
        ; dark : string
        }

  type item =
    { label : string option
    ; icon : (unit -> Node.t) option
    ; color : color option
    }

  type t = (string * item) list

  (** [~theme:(light, dark)] gives a colour per theme. *)
  val item
    :  ?label:string
    -> ?icon:(unit -> Node.t)
    -> ?color:string
    -> ?theme:string * string
    -> unit
    -> item
end

module Margin : sig
  type t =
    { top : float
    ; right : float
    ; bottom : float
    ; left : float
    }

  (** Recharts' default: 5 on every side. *)
  val default : t
end

(** Recharts' [margin]; sides not given are 5. *)
val margin : ?top:float -> ?right:float -> ?bottom:float -> ?left:float -> unit -> Margin.t

module Layout : sig
  (** [Vertical] is Recharts' [layout="vertical"]: categories down the Y
      axis, bars going right. *)
  type t =
    | Horizontal
    | Vertical
  [@@deriving sexp_of, equal]
end

module Stack_offset : sig
  type t =
    | None
    | Expand (** each category's stack fills 0..1 *)
  [@@deriving sexp_of, equal]
end

module Axis_type : sig
  type t =
    | Category
    | Number
  [@@deriving sexp_of, equal]
end

module Interval : sig
  (** Which ticks an axis keeps when they would collide. *)
  type t =
    | Preserve_end
    | Preserve_start
    | Preserve_start_end
    | Every of int (** Recharts' [interval={n}]: every (n+1)th tick *)
  [@@deriving sexp_of, equal]
end

module Position : sig
  type t =
    | Top
    | Bottom
    | Left
    | Right
    | Inside
    | Outside
    | Inside_left
    | Inside_right
    | Inside_top
    | Inside_bottom
    | Inside_start
    | Inside_end
    | End
    | Center
    | Middle
  [@@deriving sexp_of, equal]
end

(** The polar frame a [label]'s content is drawn in. *)
module Polar_view_box : sig
  type t =
    { cx : float
    ; cy : float
    ; inner_radius : float
    ; outer_radius : float
    ; start_angle : float
    ; end_angle : float
    }
end

(** A tooltip or legend entry: Recharts' payload item. *)
module Item : sig
  type t =
    { data_key : string
    ; name : string
    ; value : Value.t option
    ; color : string
    ; row : row
    }
end

module Tooltip_context : sig
  type t =
    { config : Config.t
    ; label : string option
    ; payload : Item.t list
    }
end

module Indicator : sig
  type t =
    | Dot
    | Line
    | Dashed
  [@@deriving sexp_of, equal, enumerate]

  val name : t -> string
end

type tooltip_content = Tooltip_context.t -> Node.t

(** shadcn's [ChartTooltipContent]. [label_formatter label payload] and
    [formatter item index] replace the label and an item's row. *)
val tooltip_content
  :  ?class_:string
  -> ?indicator:Indicator.t
  -> ?hide_label:bool
  -> ?hide_indicator:bool
  -> ?label_formatter:(string -> Item.t list -> Node.t list)
  -> ?label_class:string
  -> ?formatter:(Item.t -> int -> Node.t list)
  -> ?color:string
  -> ?name_key:string
  -> ?label_key:string
  -> unit
  -> tooltip_content

module Legend_context : sig
  type t =
    { config : Config.t
    ; payload : Item.t list
    ; vertical_align : [ `Bottom | `Top ]
    ; class_ : string option
    }
end

type legend_content = Legend_context.t -> Node.t

(** shadcn's [ChartLegendContent]. *)
val legend_content
  :  ?class_:string
  -> ?hide_icon:bool
  -> ?name_key:string
  -> unit
  -> legend_content

(** A line's, area's or radar's dots. *)
module Dot : sig
  type props =
    { cx : float
    ; cy : float
    ; r : float
    ; index : int
    ; row : row
    ; data_key : string
    ; fill : string
    ; stroke : string
    ; stroke_width : float
    ; fill_opacity : float option
    }

  type style =
    { r : float option
    ; fill : string option
    ; fill_opacity : float option
    ; stroke : string option
    ; stroke_width : float option
    }

  type t =
    | Hide
    | Show
    | Style of style
    | Custom of (props -> Node.t)

  val style
    :  ?r:float
    -> ?fill:string
    -> ?fill_opacity:float
    -> ?stroke:string
    -> ?stroke_width:float
    -> unit
    -> t

  (** Recharts' [<Dot>]. *)
  val view : ?attrs:Attr.t list -> ?class_:string -> props -> Node.t
end

(** What a [<LabelList content>] is given. *)
module Label_props : sig
  type t =
    { x : float
    ; y : float
    ; width : float
    ; height : float
    ; value : string
    ; index : int
    ; row : row
    }
end

(** A series' children: [<LabelList>], [<Label>], [<Cell>]. *)
type child

(** Recharts' [<LabelList>]. *)
val label_list
  :  ?data_key:string
  -> ?position:Position.t
  -> ?offset:float
  -> ?class_:string
  -> ?font_size:float
  -> ?formatter:(string -> string)
  -> ?fill:string
  -> ?fill_opacity:float
  -> ?stroke:string
  -> ?content:(Label_props.t -> Node.t)
  -> unit
  -> child

(** Recharts' [<Label>] in a polar chart (as in a pie's or radial bar's
    centre text): [content] is drawn in the chart's polar frame. *)
val label
  :  ?position:Position.t
  -> ?value:string
  -> ?class_:string
  -> ?content:(Polar_view_box.t -> Node.t)
  -> unit
  -> child

(** Recharts' [<Cell fill>]: one datum's own colour. *)
val cell : fill:string -> child

module Rectangle_props : sig
  type t =
    { x : float
    ; y : float
    ; width : float
    ; height : float
    ; radius : [ `All of float | `Corners of float * float * float * float ] option
    ; fill : string
    ; index : int
    ; row : row
    }
end

(** Recharts' [<Rectangle>] (a bar). *)
val rectangle : ?attrs:Attr.t list -> ?class_:string -> Rectangle_props.t -> Node.t

module Sector_props : sig
  type t =
    { cx : float
    ; cy : float
    ; inner_radius : float
    ; outer_radius : float
    ; start_angle : float
    ; end_angle : float
    ; corner_radius : float
    ; fill : string
    ; stroke : string option
    ; index : int
    ; row : row
    }
end

(** Recharts' [<Sector>] (a pie slice). *)
val sector : ?attrs:Attr.t list -> ?class_:string -> Sector_props.t -> Node.t

(** What a custom axis tick is given. *)
module Tick_props : sig
  type t =
    { x : float
    ; y : float
    ; text_anchor : string
    ; index : int
    ; value : string
    ; row : row option
    }
end

(** What a pie's custom label is given. *)
module Pie_label_props : sig
  type t =
    { x : float
    ; y : float
    ; cx : float
    ; cy : float
    ; text_anchor : string
    ; value : float
    ; percent : float
    ; mid_angle : float
    ; inner_radius : float
    ; outer_radius : float
    ; fill : string
    ; index : int
    ; row : row
    }
end

(** A chart's child: a series, axis, grid, tooltip, legend or raw SVG. *)
type element

val cartesian_grid
  :  ?vertical:bool
  -> ?horizontal:bool
  -> ?stroke_dasharray:string
  -> unit
  -> element

val x_axis
  :  ?data_key:string
  -> ?type_:Axis_type.t
  -> ?hide:bool
  -> ?tick_line:bool
  -> ?axis_line:bool
  -> ?tick_margin:float
  -> ?tick_formatter:(string -> string)
  -> ?min_tick_gap:float
  -> ?interval:Interval.t
  -> ?tick:bool
  -> ?tick_count:int
  -> ?domain:float * float
  -> ?height:float
  -> ?orientation:[< `Bottom | `Top ]
  -> ?padding:float * float
  -> ?allow_decimals:bool
  -> ?custom_tick:(Tick_props.t -> Node.t)
  -> unit
  -> element

val y_axis
  :  ?data_key:string
  -> ?type_:Axis_type.t
  -> ?hide:bool
  -> ?tick_line:bool
  -> ?axis_line:bool
  -> ?tick_margin:float
  -> ?tick_formatter:(string -> string)
  -> ?min_tick_gap:float
  -> ?interval:Interval.t
  -> ?tick:bool
  -> ?tick_count:int
  -> ?domain:float * float
  -> ?width:float
  -> ?orientation:[< `Left | `Right ]
  -> ?padding:float * float
  -> ?allow_decimals:bool
  -> ?custom_tick:(Tick_props.t -> Node.t)
  -> unit
  -> element

val area
  :  ?type_:Curve.t
  -> ?fill:string
  -> ?fill_opacity:float
  -> ?stroke:string
  -> ?stroke_width:float
  -> ?stroke_dasharray:string
  -> ?stack_id:string
  -> ?dot:Dot.t
  -> ?active_dot:Dot.t
  -> ?name:string
  -> ?connect_nulls:bool
  -> ?children:child list
  -> data_key:string
  -> unit
  -> element

(** [active_index] and [active_bar] are Recharts' [activeIndex] and
    [activeBar]; [shape] draws every bar. *)
val bar
  :  ?fill:string
  -> ?radius:[< `All of float | `Corners of float * float * float * float ]
  -> ?stack_id:string
  -> ?stroke:string
  -> ?stroke_width:float
  -> ?active_index:int
  -> ?active_bar:(Rectangle_props.t -> Node.t)
  -> ?shape:(Rectangle_props.t -> Node.t)
  -> ?bar_size:float
  -> ?max_bar_size:float
  -> ?name:string
  -> ?layout:Layout.t
  -> ?children:child list
  -> data_key:string
  -> unit
  -> element

val line
  :  ?type_:Curve.t
  -> ?stroke:string
  -> ?stroke_width:float
  -> ?stroke_dasharray:string
  -> ?dot:Dot.t
  -> ?active_dot:Dot.t
  -> ?name:string
  -> ?connect_nulls:bool
  -> ?children:child list
  -> ?fill:string
  -> data_key:string
  -> unit
  -> element

(** Recharts' [<Tooltip>] ([ChartTooltip]): [cursor] draws the line or
    rectangle under the pointer; [default_index] shows it before any
    hover. *)
val tooltip
  :  ?cursor:bool
  -> ?content:tooltip_content
  -> ?default_index:int
  -> unit
  -> element

(** Recharts' [<Legend>] ([ChartLegend]). *)
val legend
  :  ?content:legend_content
  -> ?vertical_align:[ `Bottom | `Top ]
  -> ?class_:string
  -> unit
  -> element

val pie
  :  ?name_key:string
  -> ?inner_radius:float
  -> ?outer_radius:float
  -> ?padding_angle:float
  -> ?corner_radius:float
  -> ?stroke:string
  -> ?stroke_width:float
  -> ?label:[ `Custom of Pie_label_props.t -> Node.t | `None | `Show ]
  -> ?label_line:bool
  -> ?active_index:int
  -> ?active_shape:(Sector_props.t -> Node.t)
  -> ?start_angle:float
  -> ?end_angle:float
  -> ?fill:string
  -> ?children:child list
  -> data:row list
  -> data_key:string
  -> unit
  -> element

val radar
  :  ?fill:string
  -> ?fill_opacity:float
  -> ?stroke:string
  -> ?stroke_width:float
  -> ?dot:Dot.t
  -> ?active_dot:Dot.t
  -> data_key:string
  -> unit
  -> element

val polar_grid
  :  ?grid_type:[ `Circle | `Polygon ]
  -> ?radial_lines:bool
  -> ?polar_radius:float list
  -> ?class_:string
  -> ?stroke:string
  -> ?stroke_width:float
  -> ?fill:string
  -> unit
  -> element

val polar_angle_axis
  :  ?data_key:string
  -> ?tick:[ `Custom of Tick_props.t -> Node.t | `Hide | `Show ]
  -> ?tick_line:bool
  -> ?axis_line:bool
  -> ?class_:string
  -> unit
  -> element

val polar_radius_axis
  :  ?angle:float
  -> ?tick:bool
  -> ?tick_line:bool
  -> ?axis_line:bool
  -> ?tick_count:int
  -> ?domain:float * float
  -> ?stroke:string
  -> ?orientation:[ `Left | `Middle | `Right ]
  -> ?data_key:string
  -> ?children:child list
  -> unit
  -> element

val radial_bar
  :  ?background:bool
  -> ?corner_radius:float
  -> ?stack_id:string
  -> ?fill:string
  -> ?class_:string
  -> ?children:child list
  -> data_key:string
  -> unit
  -> element

(** SVG [<defs>] (gradients), drawn first. *)
val defs : Node.t list -> element

(** [<linearGradient>]: by default top to bottom. *)
val linear_gradient
  :  id:string
  -> ?x1:string
  -> ?y1:string
  -> ?x2:string
  -> ?y2:string
  -> Node.t list
  -> Node.t

val stop : ?opacity:float -> offset:string -> color:string -> unit -> Node.t

(** Any SVG, drawn in the chart's [<svg>]. *)
val raw : Node.t -> element

module Spec : sig
  type t
end

val area_chart
  :  ?margin:Margin.t
  -> ?layout:Layout.t
  -> ?stack_offset:Stack_offset.t
  -> data:row list
  -> element list
  -> Spec.t

val bar_chart
  :  ?margin:Margin.t
  -> ?layout:Layout.t
  -> ?stack_offset:Stack_offset.t
  -> ?bar_gap:float
  -> ?bar_category_gap:float
  -> data:row list
  -> element list
  -> Spec.t

val line_chart : ?margin:Margin.t -> ?layout:Layout.t -> data:row list -> element list -> Spec.t

(** A pie chart's data is its [pie]s'. *)
val pie_chart : ?margin:Margin.t -> element list -> Spec.t

val radar_chart
  :  ?margin:Margin.t
  -> ?cx:float
  -> ?cy:float
  -> ?inner_radius:float
  -> ?outer_radius:float
  -> data:row list
  -> element list
  -> Spec.t

(** Angles in degrees, anticlockwise from 3 o'clock (by default 0 to
    360). *)
val radial_bar_chart
  :  ?margin:Margin.t
  -> ?cx:float
  -> ?cy:float
  -> ?inner_radius:float
  -> ?outer_radius:float
  -> ?start_angle:float
  -> ?end_angle:float
  -> ?bar_gap:float
  -> ?bar_category_gap:float
  -> data:row list
  -> element list
  -> Spec.t

(** A chart's state: its measured size and where the pointer is. *)
module State : sig
  type size = float * float [@@deriving sexp_of, equal]

  type t =
    { size : size option
    ; legend : size option
    ; tooltip : size option
    ; pointer : (float * float) option
    ; key_index : int option (** the tooltip's datum, chosen by arrow keys *)
    }
  [@@deriving sexp_of, equal]

  module Action : sig
    type t =
      | Resize of size
      | Legend_size of size
      | Tooltip_size of size
      | Pointer of (float * float) option
      | Key_index of int option
    [@@deriving sexp_of, equal]
  end
end

type t =
  { state : State.t
  ; inject : State.Action.t -> unit Effect.t
  ; id : string
  }

(** A chart of a fixed size that does not respond to the pointer
    (server-side rendering, tests). *)
val static : ?width:float -> ?height:float -> ?id:string -> unit -> t

(** A chart's state in Bonsai: its size, measured by a ResizeObserver, and
    the pointer. *)
val component : local_ Bonsai.graph -> t Bonsai.t

(** shadcn's [ChartContainer]: [aspect-video] by default; its config's
    colours as [--color-<key>] variables ([ChartStyle]); the chart drawn at
    the container's size. *)
val container
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?id:string
  -> config:Config.t
  -> t
  -> Spec.t
  -> Node.t

module For_testing : sig
  (** The hook measuring the container: give it a size in tests with
      [Handle.trigger_hook]. *)
  val measure_type_id : (float * float -> unit Effect.t) Type_equal.Id.t
end
