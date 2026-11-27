open! Import
open Js_of_ocaml
module Curve = Chart_path.Curve

let num = Chart_scale.num

(* Data *)

module Value = struct
  type t =
    [ `Num of float
    | `Text of string
    ]
  [@@deriving sexp_of, equal]

  let to_string : t -> string = function
    | `Num f -> num f
    | `Text s -> s
  ;;
end

type row = (string * Value.t) list

module Row = struct
  type t = row

  let find (row : t) key = List.Assoc.find row ~equal:String.equal key

  let num row key =
    match find row key with
    | Some (`Num f) -> Some f
    | Some (`Text s) -> Float.of_string_opt s
    | None -> None
  ;;

  let text row key = Option.map (find row key) ~f:Value.to_string
end

(* Numbers as JavaScript's [toLocaleString()] writes them (en-US). *)
let to_locale_string f =
  if not (Float.is_finite f)
  then num f
  else (
    let neg = Float.(f < 0.) in
    let s = sprintf "%.3f" (Float.abs f) in
    let int_part, frac =
      match String.lsplit2 s ~on:'.' with
      | Some (i, f) -> i, String.rstrip f ~drop:(Char.equal '0')
      | None -> s, ""
    in
    let len = String.length int_part in
    let grouped =
      String.concat_map int_part ~f:(fun c -> String.make 1 c)
      |> String.to_list
      |> List.mapi ~f:(fun i c ->
        if i > 0 && (len - i) % 3 = 0 then sprintf ",%c" c else String.make 1 c)
      |> String.concat
    in
    (if neg then "-" else "") ^ grouped ^ if String.is_empty frac then "" else "." ^ frac)
;;

let value_display : Value.t -> string = function
  | `Num f -> to_locale_string f
  | `Text s -> s
;;

(* Config *)

module Config = struct
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

  let item ?label ?icon ?color ?theme () =
    let color =
      match theme, color with
      | Some (light, dark), _ -> Some (Theme { light; dark })
      | None, Some c -> Some (Color c)
      | None, None -> None
    in
    { label; icon; color }
  ;;

  let find (t : t) key = List.Assoc.find t ~equal:String.equal key
  let label t key = Option.bind (find t key) ~f:(fun i -> i.label)
end

let style_css ~id (config : Config.t) =
  let colored = List.filter config ~f:(fun (_, i) -> Option.is_some i.color) in
  if List.is_empty colored
  then None
  else
    Some
      (String.concat
         ~sep:"\n"
         (List.map
            [ `Light, ""; `Dark, ".dark" ]
            ~f:(fun (theme, prefix) ->
              let lines =
                List.filter_map colored ~f:(fun (key, item) ->
                  let color =
                    match item.color, theme with
                    | Some (Color c), _ -> Some c
                    | Some (Theme { light; _ }), `Light -> Some light
                    | Some (Theme { dark; _ }), `Dark -> Some dark
                    | None, _ -> None
                  in
                  Option.map color ~f:(fun c -> sprintf "  --color-%s: %s;" key c))
              in
              sprintf "\n%s [data-chart=%s] {\n%s\n}\n" prefix id (String.concat ~sep:"\n" lines))))
;;

let style ~id config =
  match style_css ~id config with
  | None -> Node.none
  | Some css -> Node.create "style" [ Node.text css ]
;;

(* SVG helpers *)

let svg tag ?(attrs = []) children = Node.create_svg tag ~attrs children
let a k v = Attr.create k v
let af k v = Attr.create k (num v)
let cls l = Attr.classes (List.filter l ~f:(fun s -> not (String.is_empty s)))
let g classes children = svg "g" ~attrs:[ cls classes ] children
let layer classes children = g ("recharts-layer" :: classes) children

module Svg = struct
  let defs children = svg "defs" children
end

module Anchor = struct
  let dy = function
    | `Start -> "0.71em"
    | `Middle -> "0.355em"
    | `End -> "0em"
  ;;

  let name = function
    | `Start -> "start"
    | `Middle -> "middle"
    | `End -> "end"
  ;;
end

(* Recharts' <Text>: a <text> with one <tspan>. *)
let text_node
      ?(attrs = [])
      ?(classes = [])
      ?(fill = Some "#808080")
      ?(stroke_none = true)
      ~x
      ~y
      ~anchor
      ~vertical
      content
  =
  svg
    "text"
    ~attrs:
      ((if stroke_none then [ a "stroke" "none" ] else [])
       @ [ af "x" x
         ; af "y" y
         ; cls ("recharts-text" :: classes)
         ; a "text-anchor" (Anchor.name anchor)
         ]
       @ Option.value_map fill ~default:[] ~f:(fun f -> [ a "fill" f ])
       @ attrs)
    [ svg "tspan" ~attrs:[ af "x" x; a "dy" (Anchor.dy vertical) ] [ Node.text content ] ]
;;

(* Text measurement, for hiding ticks that would overlap. *)
let measure_context = ref None

let estimate_width ~font_size s =
  let w =
    String.fold s ~init:0. ~f:(fun acc c ->
      acc
      +.
      if Char.is_uppercase c
      then 0.68
      else if Char.is_digit c
      then 0.58
      else if Char.equal c ' '
      then 0.28
      else if Char.is_lowercase c
      then 0.53
      else 0.35)
  in
  w *. font_size
;;

let text_width ~font_size s =
  let has_document =
    Js.Optdef.test (Js.Unsafe.get Js.Unsafe.global "document")
    && Js.Optdef.test (Js.Unsafe.get Js.Unsafe.global "HTMLCanvasElement")
  in
  if not has_document
  then estimate_width ~font_size s
  else (
    try
      let ctx =
        match !measure_context with
        | Some c -> c
        | None ->
          let canvas = Dom_html.createCanvas Dom_html.document in
          let c = canvas##getContext Dom_html._2d_ in
          measure_context := Some c;
          c
      in
      let family =
        Js.to_string
          (Dom_html.window##getComputedStyle Dom_html.document##.body)##.fontFamily
      in
      ctx##.font := Js.string (sprintf "%gpx %s" font_size family);
      Js.to_float (ctx##measureText (Js.string s))##.width
    with
    | _ -> estimate_width ~font_size s)
;;

(* Layout types *)

module Margin = struct
  type t =
    { top : float
    ; right : float
    ; bottom : float
    ; left : float
    }

  let default = { top = 5.; right = 5.; bottom = 5.; left = 5. }
end

let margin ?(top = 0.) ?(right = 0.) ?(bottom = 0.) ?(left = 0.) () : Margin.t =
  { top; right; bottom; left }
;;

module Layout = struct
  type t =
    | Horizontal
    | Vertical
  [@@deriving sexp_of, equal]
end

module Stack_offset = struct
  type t =
    | None
    | Expand
  [@@deriving sexp_of, equal]
end

module Axis_type = struct
  type t =
    | Category
    | Number
  [@@deriving sexp_of, equal]
end

module Interval = struct
  type t =
    | Preserve_end
    | Preserve_start
    | Preserve_start_end
    | Every of int
  [@@deriving sexp_of, equal]
end

module Position = struct
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

type box =
  { x : float
  ; y : float
  ; width : float
  ; height : float
  }

module Polar_view_box = struct
  type t =
    { cx : float
    ; cy : float
    ; inner_radius : float
    ; outer_radius : float
    ; start_angle : float
    ; end_angle : float
    }
end

(* Payloads for tooltips and legends *)

module Item = struct
  type t =
    { data_key : string
    ; name : string
    ; value : Value.t option
    ; color : string
    ; row : row
    }
end

let payload_config (config : Config.t) (item : Item.t) key =
  let field =
    match key with
    | "dataKey" -> Some item.data_key
    | "name" -> Some item.name
    | "color" -> Some item.color
    | "value" ->
      (match item.value with
       | Some (`Text s) -> Some s
       | _ -> None)
    | _ ->
      (match Row.find item.row key with
       | Some (`Text s) -> Some s
       | _ -> None)
  in
  let key' = Option.value field ~default:key in
  match Config.find config key' with
  | Some c -> Some c
  | None -> Config.find config key
;;

(* Tooltip content: shadcn's ChartTooltipContent *)

module Tooltip_context = struct
  type t =
    { config : Config.t
    ; label : string option
    ; payload : Item.t list
    }
end

module Indicator = struct
  type t =
    | Dot
    | Line
    | Dashed
  [@@deriving sexp_of, equal, enumerate]

  let name = function
    | Dot -> "dot"
    | Line -> "line"
    | Dashed -> "dashed"
  ;;
end

type tooltip_content = Tooltip_context.t -> Node.t

let tooltip_content
      ?class_
      ?(indicator = Indicator.Dot)
      ?(hide_label = false)
      ?(hide_indicator = false)
      ?label_formatter
      ?label_class
      ?formatter
      ?color
      ?name_key
      ?label_key
      ()
  : tooltip_content
  =
  fun { config; label; payload } ->
  match payload with
  | [] -> Node.none
  | first :: _ ->
    let tooltip_label =
      if hide_label
      then None
      else (
        let key = Option.value label_key ~default:first.data_key in
        let item_config = payload_config config first key in
        let value =
          match label_key, label with
          | None, Some l -> Some (Option.value (Config.label config l) ~default:l)
          | _ -> Option.bind item_config ~f:(fun c -> c.label)
        in
        let div children =
          Node.div ~attrs:[ Cn.attr [ "font-medium"; Option.value label_class ~default:"" ] ] children
        in
        match label_formatter with
        | Some f -> Some (div (f (Option.value value ~default:"") payload))
        | None ->
          (match value with
           | None | Some "" -> None
           | Some v -> Some (div [ Node.text v ])))
    in
    let nest_label = List.length payload = 1 && not (Indicator.equal indicator Dot) in
    let opt = Option.value ~default:Node.none in
    Node.div
      ~attrs:
        [ (Cn.with_class
               ?class_
               [ "border-border/50 bg-background gap-1.5 rounded-lg border px-2.5 \
                  py-1.5 text-xs shadow-xl grid min-w-32 items-start"
               ])
        ]
      [ (if nest_label then Node.none else opt tooltip_label)
      ; Node.div
          ~attrs:[ Attr.class_ "grid gap-1.5" ]
          (List.mapi payload ~f:(fun index (item : Item.t) ->
             let key = Option.value name_key ~default:item.name in
             let item_config = payload_config config item key in
             let indicator_color =
               match color with
               | Some c -> c
               | None -> Option.value (Row.text item.row "fill") ~default:item.color
             in
             Node.div
               ~attrs:
                 [ Attr.class_
                     (Cn.cn
                        [ "flex w-full flex-wrap items-stretch gap-2 [&>svg]:h-2.5 [&>svg]:w-2.5 [&>svg]:text-muted-foreground"
                        ; (if Indicator.equal indicator Dot then "items-center" else "")
                        ])
                 ]
               (match formatter, item.value with
                | Some f, Some _ -> f item index
                | _ ->
                  [ (match Option.bind item_config ~f:(fun c -> c.icon) with
                     | Some icon -> icon ()
                     | None ->
                       if hide_indicator
                       then Node.none
                       else
                         Node.div
                           ~attrs:
                             [ Attr.class_
                                 (Cn.cn
                                    [ "shrink-0 rounded-[2px] border-(--color-border) bg-(--color-bg)"
                                    ; (match indicator with
                                       | Dot -> "h-2.5 w-2.5"
                                       | Line -> "w-1"
                                       | Dashed -> "w-0 border-[1.5px] border-dashed bg-transparent")
                                    ; (if nest_label && Indicator.equal indicator Dashed
                                       then "my-0.5"
                                       else "")
                                    ])
                             ; (* [Attr.style] drops custom properties. *)
                               Attr.css_var ~name:"color-bg" indicator_color
                             ; Attr.css_var ~name:"color-border" indicator_color
                             ]
                           [])
                  ; Node.div
                      ~attrs:
                        [ Attr.class_
                            (Cn.cn
                               [ "flex flex-1 justify-between leading-none"
                               ; (if nest_label then "items-end" else "items-center")
                               ])
                        ]
                      [ Node.div
                          ~attrs:[ Attr.class_ "grid gap-1.5" ]
                          [ (if nest_label then opt tooltip_label else Node.none)
                          ; Node.span
                              ~attrs:[ Attr.class_ "text-muted-foreground" ]
                              [ Node.text
                                  (match Option.bind item_config ~f:(fun c -> c.label) with
                                   | Some l -> l
                                   | None -> item.name)
                              ]
                          ]
                      ; (match item.value with
                         | None -> Node.none
                         | Some v ->
                           Node.span
                             ~attrs:
                               [ Attr.class_
                                   "font-mono font-medium text-foreground tabular-nums"
                               ]
                             [ Node.text (value_display v) ])
                      ]
                  ])))
      ]
;;

(* Legend content: shadcn's ChartLegendContent *)

module Legend_context = struct
  type t =
    { config : Config.t
    ; payload : Item.t list
    ; vertical_align : [ `Top | `Bottom ]
    ; class_ : string option
    }
end

type legend_content = Legend_context.t -> Node.t

let legend_content ?class_ ?(hide_icon = false) ?name_key () : legend_content =
  fun { config; payload; vertical_align; class_ = legend_class } ->
  match payload with
  | [] -> Node.none
  | _ ->
    Node.div
      ~attrs:
        [ Attr.class_
            (Cn.cn
               [ "flex items-center justify-center gap-4"
               ; (match vertical_align with
                  | `Top -> "pb-3"
                  | `Bottom -> "pt-3")
               ; Option.value legend_class ~default:""
               ; Option.value class_ ~default:""
               ])
        ]
      (List.map payload ~f:(fun (item : Item.t) ->
         let key = Option.value name_key ~default:item.data_key in
         let item_config = payload_config config item key in
         Node.div
           ~attrs:
             [ Attr.class_
                 "flex items-center gap-1.5 [&>svg]:h-3 [&>svg]:w-3 [&>svg]:text-muted-foreground"
             ]
           [ (match Option.bind item_config ~f:(fun c -> c.icon) with
              | Some icon when not hide_icon -> icon ()
              | _ ->
                Node.div
                  ~attrs:
                    [ Attr.class_ "h-2 w-2 shrink-0 rounded-[2px]"
                    ; Attr.style (Css_gen.create ~field:"background-color" ~value:item.color)
                    ]
                  [])
           ; Node.text
               (Option.value (Option.bind item_config ~f:(fun c -> c.label)) ~default:"")
           ]))
;;

(* Elements *)

module Dot = struct
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

  let style ?r ?fill ?fill_opacity ?stroke ?stroke_width () =
    Style { r; fill; fill_opacity; stroke; stroke_width }
  ;;

  let view ?(attrs = []) ?(class_ = "") (p : props) =
    svg
      "circle"
      ~attrs:
        ([ af "r" p.r
         ; a "fill" p.fill
         ; a "stroke" p.stroke
         ; af "stroke-width" p.stroke_width
         ]
         @ Option.value_map p.fill_opacity ~default:[] ~f:(fun o -> [ af "fill-opacity" o ])
         @ [ af "cx" p.cx; af "cy" p.cy; cls [ "recharts-dot"; class_ ] ]
         @ attrs)
      []
  ;;

  let render t ~class_ (base : props) =
    match t with
    | Hide -> None
    | Show -> Some (view ~class_ base)
    | Style s ->
      Some
        (view
           ~class_
           { base with
             r = Option.value s.r ~default:base.r
           ; fill = Option.value s.fill ~default:base.fill
           ; fill_opacity = (match s.fill_opacity with Some o -> Some o | None -> base.fill_opacity)
           ; stroke = Option.value s.stroke ~default:base.stroke
           ; stroke_width = Option.value s.stroke_width ~default:base.stroke_width
           })
    | Custom f -> Some (f base)
  ;;
end

module Label_props = struct
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

type label_list =
  { ll_data_key : string option
  ; ll_position : Position.t option
  ; ll_offset : float
  ; ll_class : string option
  ; ll_font_size : float option
  ; ll_formatter : (string -> string) option
  ; ll_fill : string option
  ; ll_fill_opacity : float option
  ; ll_stroke : string option
  ; ll_content : (Label_props.t -> Node.t) option
  }

type label =
  { l_content : (Polar_view_box.t -> Node.t) option
  ; l_value : string option
  ; l_position : Position.t [@warning "-69"]
  ; l_class : string option
  }

type child =
  | Label_list of label_list
  | Label of label
  | Cell of string

let label_list
      ?data_key
      ?position
      ?(offset = 5.)
      ?class_
      ?font_size
      ?formatter
      ?fill
      ?fill_opacity
      ?stroke
      ?content
      ()
  =
  Label_list
    { ll_data_key = data_key
    ; ll_position = position
    ; ll_offset = offset
    ; ll_class = class_
    ; ll_font_size = font_size
    ; ll_formatter = formatter
    ; ll_fill = fill
    ; ll_fill_opacity = fill_opacity
    ; ll_stroke = stroke
    ; ll_content = content
    }
;;

let label ?(position = Position.Center) ?value ?class_ ?content () =
  Label { l_content = content; l_value = value; l_position = position; l_class = class_ }
;;

let cell ~fill = Cell fill

let label_lists children =
  List.filter_map children ~f:(function
    | Label_list l -> Some l
    | _ -> None)
;;

let labels children =
  List.filter_map children ~f:(function
    | Label l -> Some l
    | _ -> None)
;;

let cells children =
  List.filter_map children ~f:(function
    | Cell f -> Some f
    | _ -> None)
;;

module Rectangle_props = struct
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

let rectangle ?(attrs = []) ?(class_ = "") (p : Rectangle_props.t) =
  svg
    "path"
    ~attrs:
      ([ a "fill" p.fill
       ; af "x" (Chart_scale.round4 p.x)
       ; af "y" (Chart_scale.round4 p.y)
       ; af "width" (Chart_scale.round4 p.width)
       ; af "height" (Chart_scale.round4 p.height)
       ; cls [ "recharts-rectangle"; class_ ]
       ; a
           "d"
           (Chart_path.rectangle
              ?radius:p.radius
              ~x:p.x
              ~y:p.y
              ~width:p.width
              ~height:p.height
              ())
       ]
       @ attrs)
    []
;;

module Sector_props = struct
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

let sector ?(attrs = []) ?(class_ = "") (p : Sector_props.t) =
  match
    Chart_path.sector
      ~corner_radius:p.corner_radius
      ~cx:p.cx
      ~cy:p.cy
      ~inner_radius:p.inner_radius
      ~outer_radius:p.outer_radius
      ~start_angle:p.start_angle
      ~end_angle:p.end_angle
      ()
  with
  | None -> Node.none
  | Some d ->
    svg
      "path"
      ~attrs:
        ([ af "cx" p.cx; af "cy" p.cy; a "fill" p.fill ]
         @ Option.value_map p.stroke ~default:[] ~f:(fun s -> [ a "stroke" s ])
         @ [ cls [ "recharts-sector"; class_ ]; a "d" d ]
         @ attrs)
      []
;;

module Tick_props = struct
  type t =
    { x : float
    ; y : float
    ; text_anchor : string
    ; index : int
    ; value : string
    ; row : row option
    }
end

module Pie_label_props = struct
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

type axis =
  { data_key : string option
  ; type_ : Axis_type.t option
  ; hide : bool
  ; tick_line : bool
  ; axis_line : bool
  ; tick_margin : float
  ; tick_formatter : (string -> string) option
  ; min_tick_gap : float
  ; interval : Interval.t
  ; tick : bool
  ; tick_count : int
  ; domain : (float * float) option
  ; size : float option
  ; mirror_side : [ `Low | `High ]
  ; padding : float * float
  ; allow_decimals : bool
  ; custom_tick : (Tick_props.t -> Node.t) option
  }

type grid =
  { vertical : bool
  ; horizontal : bool
  ; dasharray : string option
  }

type series_kind =
  | Area of
      { curve : Curve.t
      ; fill_opacity : float option
      ; stroke_dasharray : string option
      }
  | Bar of
      { radius : [ `All of float | `Corners of float * float * float * float ] option
      ; active_index : int option
      ; active_bar : (Rectangle_props.t -> Node.t) option
      ; shape : (Rectangle_props.t -> Node.t) option
      ; bar_size : float option
      ; max_bar_size : float option
      }
  | Line of
      { curve : Curve.t
      ; stroke_dasharray : string option
      }

type series =
  { kind : series_kind
  ; s_data_key : string
  ; s_name : string option
  ; fill : string option
  ; stroke : string option
  ; stroke_width : float option
  ; stack_id : string option
  ; dot : Dot.t
  ; active_dot : Dot.t
  ; s_children : child list
  ; connect_nulls : bool
  ; s_layout : Layout.t option [@warning "-69"]
  }

type tooltip =
  { cursor : bool
  ; content : tooltip_content option
  ; default_index : int option
  }

type legend =
  { l_content : legend_content option
  ; vertical_align : [ `Top | `Bottom ]
  ; legend_class : string option
  }

type pie =
  { p_data : row list
  ; p_data_key : string
  ; p_name_key : string
  ; p_inner_radius : float option
  ; p_outer_radius : float option
  ; p_padding_angle : float
  ; p_corner_radius : float
  ; p_stroke : string option
  ; p_stroke_width : float option
  ; p_label : [ `None | `Show | `Custom of Pie_label_props.t -> Node.t ]
  ; p_label_line : bool
  ; p_active_index : int option
  ; p_active_shape : (Sector_props.t -> Node.t) option
  ; p_start_angle : float
  ; p_end_angle : float
  ; p_fill : string option
  ; p_children : child list
  }

type radar =
  { r_data_key : string
  ; r_fill : string option
  ; r_fill_opacity : float option
  ; r_stroke : string option
  ; r_stroke_width : float option
  ; r_dot : Dot.t
  ; r_active_dot : Dot.t
  }

type polar_grid =
  { grid_type : [ `Polygon | `Circle ]
  ; radial_lines : bool
  ; polar_radius : float list option
  ; pg_class : string option
  ; pg_stroke : string option
  ; pg_stroke_width : float option
  ; pg_fill : string option
  }

type angle_axis =
  { aa_data_key : string option
  ; aa_tick : [ `Show | `Hide | `Custom of Tick_props.t -> Node.t ]
  ; aa_tick_line : bool
  ; aa_axis_line : bool
  ; aa_class : string option
  }

type radius_axis =
  { ra_angle : float
  ; ra_tick : bool
  ; ra_tick_line : bool [@warning "-69"]
  ; ra_axis_line : bool
  ; ra_tick_count : int
  ; ra_domain : (float * float) option
  ; ra_stroke : string
  ; ra_orientation : [ `Right | `Left | `Middle ]
  ; ra_children : child list
  ; ra_data_key : string option
  }

type radial_bar =
  { rb_data_key : string
  ; rb_background : bool
  ; rb_corner_radius : float
  ; rb_stack_id : string option
  ; rb_fill : string option
  ; rb_class : string option
  ; rb_children : child list
  }

type element =
  | Grid of grid
  | X_axis of axis
  | Y_axis of axis
  | Series of series
  | Tooltip of tooltip
  | Legend of legend
  | Pie of pie
  | Radar of radar
  | Polar_grid of polar_grid
  | Angle_axis of angle_axis
  | Radius_axis of radius_axis
  | Radial_bar of radial_bar
  | Raw of Node.t

let cartesian_grid ?(vertical = true) ?(horizontal = true) ?stroke_dasharray () =
  Grid { vertical; horizontal; dasharray = stroke_dasharray }
;;

let axis
      ?data_key
      ?type_
      ?(hide = false)
      ?(tick_line = true)
      ?(axis_line = true)
      ?(tick_margin = 2.)
      ?tick_formatter
      ?(min_tick_gap = 5.)
      ?(interval = Interval.Preserve_end)
      ?(tick = true)
      ?(tick_count = 5)
      ?domain
      ?size
      ?(mirror_side = `Low)
      ?(padding = 0., 0.)
      ?(allow_decimals = true)
      ?custom_tick
      ()
  =
  { data_key
  ; type_
  ; hide
  ; tick_line
  ; axis_line
  ; tick_margin
  ; tick_formatter
  ; min_tick_gap
  ; interval
  ; tick
  ; tick_count
  ; domain
  ; size
  ; mirror_side
  ; padding
  ; allow_decimals
  ; custom_tick
  }
;;

let x_axis
      ?data_key
      ?type_
      ?hide
      ?tick_line
      ?axis_line
      ?tick_margin
      ?tick_formatter
      ?min_tick_gap
      ?interval
      ?tick
      ?tick_count
      ?domain
      ?height
      ?orientation
      ?padding
      ?allow_decimals
      ?custom_tick
      ()
  =
  X_axis
    (axis
       ?data_key
       ?type_
       ?hide
       ?tick_line
       ?axis_line
       ?tick_margin
       ?tick_formatter
       ?min_tick_gap
       ?interval
       ?tick
       ?tick_count
       ?domain
       ?size:height
       ?mirror_side:
         (Option.map orientation ~f:(function
            | `Bottom -> `Low
            | `Top -> `High))
       ?padding
       ?allow_decimals
       ?custom_tick
       ())
;;

let y_axis
      ?data_key
      ?type_
      ?hide
      ?tick_line
      ?axis_line
      ?tick_margin
      ?tick_formatter
      ?min_tick_gap
      ?interval
      ?tick
      ?tick_count
      ?domain
      ?width
      ?orientation
      ?padding
      ?allow_decimals
      ?custom_tick
      ()
  =
  Y_axis
    (axis
       ?data_key
       ?type_
       ?hide
       ?tick_line
       ?axis_line
       ?tick_margin
       ?tick_formatter
       ?min_tick_gap
       ?interval
       ?tick
       ?tick_count
       ?domain
       ?size:width
       ?mirror_side:
         (Option.map orientation ~f:(function
            | `Left -> `Low
            | `Right -> `High))
       ?padding
       ?allow_decimals
       ?custom_tick
       ())
;;

let series
      ~kind
      ~data_key
      ?name
      ?fill
      ?stroke
      ?stroke_width
      ?stack_id
      ~dot
      ?(active_dot = Dot.Show)
      ?(children = [])
      ?(connect_nulls = false)
      ?layout
      ()
  =
  Series
    { kind
    ; s_data_key = data_key
    ; s_name = name
    ; fill
    ; stroke
    ; stroke_width
    ; stack_id
    ; dot
    ; active_dot
    ; s_children = children
    ; connect_nulls
    ; s_layout = layout
    }
;;

let area
      ?(type_ = Curve.Linear)
      ?fill
      ?fill_opacity
      ?stroke
      ?stroke_width
      ?stroke_dasharray
      ?stack_id
      ?(dot = Dot.Hide)
      ?active_dot
      ?name
      ?connect_nulls
      ?children
      ~data_key
      ()
  =
  series
    ~kind:(Area { curve = type_; fill_opacity; stroke_dasharray })
    ~data_key
    ?name
    ?fill
    ?stroke
    ?stroke_width
    ?stack_id
    ~dot
    ?active_dot
    ?children
    ?connect_nulls
    ()
;;

let bar
      ?fill
      ?radius
      ?stack_id
      ?stroke
      ?stroke_width
      ?active_index
      ?active_bar
      ?shape
      ?bar_size
      ?max_bar_size
      ?name
      ?layout
      ?children
      ~data_key
      ()
  =
  let radius =
    Option.map radius ~f:(function
      | `All r -> `All r
      | `Corners c -> `Corners c)
  in
  series
    ~kind:(Bar { radius; active_index; active_bar; shape; bar_size; max_bar_size })
    ~data_key
    ?name
    ?fill
    ?stroke
    ?stroke_width
    ?stack_id
    ~dot:Dot.Hide
    ~active_dot:Dot.Hide
    ?children
    ?layout
    ()
;;

let line
      ?(type_ = Curve.Linear)
      ?stroke
      ?stroke_width
      ?stroke_dasharray
      ?(dot = Dot.Show)
      ?active_dot
      ?name
      ?connect_nulls
      ?children
      ?fill
      ~data_key
      ()
  =
  series
    ~kind:(Line { curve = type_; stroke_dasharray })
    ~data_key
    ?name
    ?fill
    ?stroke
    ?stroke_width
    ~dot
    ?active_dot
    ?children
    ?connect_nulls
    ()
;;

let tooltip ?(cursor = true) ?content ?default_index () =
  Tooltip { cursor; content; default_index }
;;

let legend ?content ?(vertical_align = `Bottom) ?class_ () =
  Legend { l_content = content; vertical_align; legend_class = class_ }
;;

let pie
      ?(name_key = "name")
      ?inner_radius
      ?outer_radius
      ?(padding_angle = 0.)
      ?(corner_radius = 0.)
      ?stroke
      ?stroke_width
      ?(label = `None)
      ?(label_line = true)
      ?active_index
      ?active_shape
      ?(start_angle = 0.)
      ?(end_angle = 360.)
      ?fill
      ?(children = [])
      ~data
      ~data_key
      ()
  =
  Pie
    { p_data = data
    ; p_data_key = data_key
    ; p_name_key = name_key
    ; p_inner_radius = inner_radius
    ; p_outer_radius = outer_radius
    ; p_padding_angle = padding_angle
    ; p_corner_radius = corner_radius
    ; p_stroke = stroke
    ; p_stroke_width = stroke_width
    ; p_label = label
    ; p_label_line = label_line
    ; p_active_index = active_index
    ; p_active_shape = active_shape
    ; p_start_angle = start_angle
    ; p_end_angle = end_angle
    ; p_fill = fill
    ; p_children = children
    }
;;

let radar
      ?fill
      ?fill_opacity
      ?stroke
      ?stroke_width
      ?(dot = Dot.Hide)
      ?(active_dot = Dot.Show)
      ~data_key
      ()
  =
  Radar
    { r_data_key = data_key
    ; r_fill = fill
    ; r_fill_opacity = fill_opacity
    ; r_stroke = stroke
    ; r_stroke_width = stroke_width
    ; r_dot = dot
    ; r_active_dot = active_dot
    }
;;

let polar_grid
      ?(grid_type = `Polygon)
      ?(radial_lines = true)
      ?polar_radius
      ?class_
      ?stroke
      ?stroke_width
      ?fill
      ()
  =
  Polar_grid
    { grid_type
    ; radial_lines
    ; polar_radius
    ; pg_class = class_
    ; pg_stroke = stroke
    ; pg_stroke_width = stroke_width
    ; pg_fill = fill
    }
;;

let polar_angle_axis
      ?data_key
      ?(tick = `Show)
      ?(tick_line = true)
      ?(axis_line = true)
      ?class_
      ()
  =
  Angle_axis
    { aa_data_key = data_key
    ; aa_tick = tick
    ; aa_tick_line = tick_line
    ; aa_axis_line = axis_line
    ; aa_class = class_
    }
;;

let polar_radius_axis
      ?(angle = 0.)
      ?(tick = true)
      ?(tick_line = true)
      ?(axis_line = true)
      ?(tick_count = 5)
      ?domain
      ?(stroke = "#ccc")
      ?(orientation = `Right)
      ?data_key
      ?(children = [])
      ()
  =
  Radius_axis
    { ra_angle = angle
    ; ra_tick = tick
    ; ra_tick_line = tick_line
    ; ra_axis_line = axis_line
    ; ra_tick_count = tick_count
    ; ra_domain = domain
    ; ra_stroke = stroke
    ; ra_orientation = orientation
    ; ra_children = children
    ; ra_data_key = data_key
    }
;;

let radial_bar
      ?(background = false)
      ?(corner_radius = 0.)
      ?stack_id
      ?fill
      ?class_
      ?(children = [])
      ~data_key
      ()
  =
  Radial_bar
    { rb_data_key = data_key
    ; rb_class = class_
    ; rb_background = background
    ; rb_corner_radius = corner_radius
    ; rb_stack_id = stack_id
    ; rb_fill = fill
    ; rb_children = children
    }
;;

let defs children = Raw (Svg.defs children)

let linear_gradient ~id ?(x1 = "0") ?(y1 = "0") ?(x2 = "0") ?(y2 = "1") stops =
  svg
    "linearGradient"
    ~attrs:[ a "id" id; a "x1" x1; a "y1" y1; a "x2" x2; a "y2" y2 ]
    stops
;;

let stop ?opacity ~offset ~color () =
  svg
    "stop"
    ~attrs:
      ([ a "offset" offset; a "stop-color" color ]
       @ Option.value_map opacity ~default:[] ~f:(fun o -> [ af "stop-opacity" o ]))
    []
;;
let raw node = Raw node

(* Charts *)

module Kind = struct
  type t =
    | Area
    | Bar
    | Line
    | Pie
    | Radar
    | Radial_bar
  [@@deriving sexp_of, equal]

  let class_name = function
    | Area -> "recharts-area-chart"
    | Bar -> "recharts-bar-chart"
    | Line -> "recharts-line-chart"
    | Pie -> "recharts-pie-chart"
    | Radar -> "recharts-radar-chart"
    | Radial_bar -> "recharts-radial-bar-chart"
  ;;
end

module Spec = struct
  type t =
    { kind : Kind.t
    ; data : row list
    ; margin : Margin.t
    ; layout : Layout.t
    ; stack_offset : Stack_offset.t
    ; bar_gap : float
    ; bar_category_gap : float
    ; cx : float option
    ; cy : float option
    ; inner_radius : float option
    ; outer_radius : float option
    ; start_angle : float
    ; end_angle : float
    ; elements : element list
    }
end

let make
      ~kind
      ?(margin = Margin.default)
      ?(layout = Layout.Horizontal)
      ?(stack_offset = Stack_offset.None)
      ?(bar_gap = 4.)
      ?(bar_category_gap = 0.1)
      ?cx
      ?cy
      ?inner_radius
      ?outer_radius
      ?(start_angle = 0.)
      ?(end_angle = 360.)
      ~data
      elements
  : Spec.t
  =
  { kind
  ; data
  ; margin
  ; layout
  ; stack_offset
  ; bar_gap
  ; bar_category_gap
  ; cx
  ; cy
  ; inner_radius
  ; outer_radius
  ; start_angle
  ; end_angle
  ; elements
  }
;;

let area_chart ?margin ?layout ?stack_offset ~data elements =
  make ~kind:Area ?margin ?layout ?stack_offset ~data elements
;;

let bar_chart ?margin ?layout ?stack_offset ?bar_gap ?bar_category_gap ~data elements =
  make ~kind:Bar ?margin ?layout ?stack_offset ?bar_gap ?bar_category_gap ~data elements
;;

let line_chart ?margin ?layout ~data elements = make ~kind:Line ?margin ?layout ~data elements
let pie_chart ?margin elements = make ~kind:Pie ?margin ~data:[] elements

let radar_chart ?margin ?cx ?cy ?inner_radius ?outer_radius ~data elements =
  make
    ~kind:Radar
    ?margin
    ?cx
    ?cy
    ?inner_radius
    ?outer_radius
    ~start_angle:90.
    ~end_angle:(-270.)
    ~data
    elements
;;

let radial_bar_chart
      ?margin
      ?cx
      ?cy
      ?inner_radius
      ?outer_radius
      ?start_angle
      ?end_angle
      ?bar_gap
      ?bar_category_gap
      ~data
      elements
  =
  make
    ~kind:Radial_bar
    ?margin
    ?cx
    ?cy
    ?inner_radius
    ?outer_radius
    ?start_angle
    ?end_angle
    ?bar_gap
    ?bar_category_gap
    ~data
    elements
;;

(* State *)

module State = struct
  type size = float * float [@@deriving sexp_of, equal]

  type t =
    { size : size option
    ; legend : size option
    ; tooltip : size option
    ; pointer : (float * float) option
    ; key_index : int option
    }
  [@@deriving sexp_of, equal]

  let initial = { size = None; legend = None; tooltip = None; pointer = None; key_index = None }

  module Action = struct
    type t =
      | Resize of size
      | Legend_size of size
      | Tooltip_size of size
      | Pointer of (float * float) option
      | Key_index of int option
    [@@deriving sexp_of, equal]
  end

  let apply t : Action.t -> t = function
    | Resize s -> { t with size = Some s }
    | Legend_size s -> { t with legend = Some s }
    | Tooltip_size s -> { t with tooltip = Some s }
    | Pointer p -> { t with pointer = p; key_index = (if Option.is_some p then None else t.key_index) }
    | Key_index i -> { t with key_index = i; pointer = None }
  ;;
end

type t =
  { state : State.t
  ; inject : State.Action.t -> unit Effect.t
  ; id : string
  }

let static ?(width = 320.) ?(height = 200.) ?(id = "chart") () =
  { state = { State.initial with size = Some (width, height) }
  ; inject = (fun _ -> Effect.Ignore)
  ; id
  }
;;

let component (local_ graph) =
  let open Bonsai.Let_syntax in
  let state, inject =
    Bonsai.state_machine
      ~default_model:State.initial
      ~apply_action:(fun _ model action ->
        let next = State.apply model action in
        if State.equal next model then model else next)
      graph
  in
  let id = Bonsai.path_id graph in
  let%arr state and inject and id in
  { state; inject; id }
;;

(* Measuring elements: a ResizeObserver reporting the element's size. *)
module Measure = Vdom.Attr.Hooks.Make (struct
    module State = struct
      type t =
        { mutable callback : float * float -> unit Effect.t
        ; mutable observer : Js.Unsafe.any option
        }
    end

    module Input = struct
      type t = float * float -> unit Effect.t

      let sexp_of_t _ = Sexp.Atom "<measure>"
      let combine f g s = Effect.Many [ f s; g s ]
    end

    let init callback _ = { State.callback; observer = None }

    let report (state : State.t) (el : Dom_html.element Js.t) =
      let r = el##getBoundingClientRect in
      let w = Js.to_float r##.right -. Js.to_float r##.left in
      let h = Js.to_float r##.bottom -. Js.to_float r##.top in
      if Float.(w > 0. || h > 0.) then Effect.Expert.handle_non_dom_event_exn (state.callback (w, h))
    ;;

    let on_mount =
      `Schedule_immediately_after_this_dom_patch_completes
        (fun _ (state : State.t) (el : Dom_html.element Js.t) ->
          report state el;
          if Js.Optdef.test (Js.Unsafe.get Js.Unsafe.global "ResizeObserver")
          then (
            let ctor = Js.Unsafe.get Js.Unsafe.global "ResizeObserver" in
            let observer =
              Js.Unsafe.new_obj
                ctor
                [| Js.Unsafe.inject (Js.wrap_callback (fun _ _ -> report state el)) |]
            in
            ignore (Js.Unsafe.meth_call observer "observe" [| Js.Unsafe.inject el |] : unit);
            state.observer <- Some observer))
    ;;

    let update ~old_input:_ ~new_input (state : State.t) _ = state.callback <- new_input

    let destroy _ (state : State.t) _ =
      Option.iter state.observer ~f:(fun o ->
        ignore (Js.Unsafe.meth_call o "disconnect" [||] : unit));
      state.observer <- None
    ;;
  end)

let measure name callback = Vdom.Attr.create_hook name (Measure.create callback)

(* Pointer position relative to the element the handler is on. *)
let local_point (ev : Dom_html.mouseEvent Js.t) =
  let x = Js.to_float (Js.Unsafe.coerce ev)##.clientX in
  let y = Js.to_float (Js.Unsafe.coerce ev)##.clientY in
  (* Tests' fake events have no [currentTarget] (reading it raises). *)
  match
    Option.try_with (fun () ->
      let target : Dom_html.element Js.t Js.Opt.t Js.Optdef.t =
        Js.Unsafe.get ev "currentTarget"
      in
      Option.bind (Js.Optdef.to_option target) ~f:Js.Opt.to_option)
    |> Option.join
  with
  | Some t ->
    let r = t##getBoundingClientRect in
    x -. Js.to_float r##.left, y -. Js.to_float r##.top
  | None -> x, y
;;

(* Rendering *)

type render_ctx =
  { spec : Spec.t
  ; width : float
  ; height : float
  ; config : Config.t [@warning "-69"]
  ; chart : t
  }

type active =
  { index : int
  ; coordinate : float * float
  ; tooltip_label : string option
  ; payload : Item.t list
  }

type rendered =
  { svg_children : Node.t list
  ; active : active option
  ; legend_payload : Item.t list
  ; plot : box
  ; nav_count : int
  }

let find_map_elements (spec : Spec.t) ~f = List.find_map spec.elements ~f
let find_tooltip spec = find_map_elements spec ~f:(function Tooltip t -> Some t | _ -> None)
let find_legend spec = find_map_elements spec ~f:(function Legend l -> Some l | _ -> None)

let legend_height (ctx : render_ctx) =
  match find_legend ctx.spec with
  | None -> 0.
  | Some _ ->
    (match ctx.chart.state.legend with
     | Some (_, h) -> h
     | None -> 28.)
;;

let offset_box (ctx : render_ctx) ~left ~right ~top ~bottom =
  let m = ctx.spec.margin in
  let lh = legend_height ctx in
  let legend_top, legend_bottom =
    match find_legend ctx.spec with
    | Some { vertical_align = `Top; _ } -> lh, 0.
    | _ -> 0., lh
  in
  let x = m.left +. left in
  let y = m.top +. top +. legend_top in
  { x
  ; y
  ; width = Float.max 0. (ctx.width -. x -. m.right -. right)
  ; height = Float.max 0. (ctx.height -. y -. m.bottom -. bottom -. legend_bottom)
  }
;;

(* Recharts' getTicksEnd/getTicksStart: which ticks to show so their labels
   do not overlap. *)
let visible_ticks ~interval ~min_tick_gap ~start ~end_ ~size (ticks : (float * 'a) list) =
  let ticks = Array.of_list ticks in
  let len = Array.length ticks in
  match (interval : Interval.t) with
  | Every n ->
    Array.to_list ticks |> List.filteri ~f:(fun i _ -> i % (n + 1) = 0)
  | _ when len = 0 -> []
  | _ ->
    let sgn =
      if len >= 2 && Float.(fst ticks.(1) < fst ticks.(0)) then -1. else 1.
    in
    let start, end_ = if Float.(sgn > 0.) then start, end_ else end_, start in
    let is_visible pos sz start end_ =
      if Float.(sgn *. pos < sgn *. start || sgn *. pos > sgn *. end_)
      then false
      else
        Float.(sgn *. (pos -. (sgn *. sz /. 2.) -. start) >= 0.)
        && Float.(sgn *. (pos +. (sgn *. sz /. 2.) -. end_) <= 0.)
    in
    let shown = Array.create ~len None in
    (match interval with
     | Preserve_end | Every _ ->
       let end_ = ref end_ in
       for i = len - 1 downto 0 do
         let coord, v = ticks.(i) in
         let sz = size i v in
         let tick_coord =
           if i = len - 1
           then (
             let gap = sgn *. (coord +. (sgn *. sz /. 2.) -. !end_) in
             if Float.(gap > 0.) then coord -. (gap *. sgn) else coord)
           else coord
         in
         if is_visible tick_coord sz start !end_
         then (
           end_ := tick_coord -. (sgn *. ((sz /. 2.) +. min_tick_gap));
           shown.(i) <- Some (tick_coord, v))
       done
     | Preserve_start | Preserve_start_end ->
       let start = ref start in
       let end_ = ref end_ in
       let preserve_end = Interval.equal interval Preserve_start_end in
       if preserve_end
       then (
         let coord, v = ticks.(len - 1) in
         let sz = size (len - 1) v in
         let gap = sgn *. (coord +. (sgn *. sz /. 2.) -. !end_) in
         let tick_coord = if Float.(gap > 0.) then coord -. (gap *. sgn) else coord in
         if is_visible tick_coord sz !start !end_
         then (
           end_ := tick_coord -. (sgn *. ((sz /. 2.) +. min_tick_gap));
           shown.(len - 1) <- Some (tick_coord, v)));
       let count = if preserve_end then len - 1 else len in
       for i = 0 to count - 1 do
         let coord, v = ticks.(i) in
         let sz = size i v in
         let tick_coord =
           if i = 0
           then (
             let gap = sgn *. (coord -. (sgn *. sz /. 2.) -. !start) in
             if Float.(gap < 0.) then coord -. (gap *. sgn) else coord)
           else coord
         in
         if is_visible tick_coord sz !start !end_
         then (
           start := tick_coord +. (sgn *. ((sz /. 2.) +. min_tick_gap));
           shown.(i) <- Some (tick_coord, v))
       done);
    Array.to_list shown |> List.filter_opt
;;

(* Stacking: for each series, each row's (base, top). *)
let stack_values ~(offset : Stack_offset.t) (data : row list) (keys : string list) =
  let rows = Array.of_list data in
  let values =
    List.map keys ~f:(fun k ->
      Array.map rows ~f:(fun r -> Option.value (Row.num r k) ~default:0.))
  in
  let values =
    match offset with
    | None -> values
    | Expand ->
      let n = Array.length rows in
      let sums =
        Array.init n ~f:(fun i -> List.fold values ~init:0. ~f:(fun acc v -> acc +. v.(i)))
      in
      List.map values ~f:(fun v ->
        Array.mapi v ~f:(fun i x -> if Float.(sums.(i) = 0.) then 0. else x /. sums.(i)))
  in
  let acc = Array.create ~len:(Array.length rows) 0. in
  List.map values ~f:(fun v ->
    Array.mapi v ~f:(fun i x ->
      let base = acc.(i) in
      acc.(i) <- base +. x;
      base, base +. x))
;;

let category_values (data : row list) key =
  List.mapi data ~f:(fun i r ->
    match key with
    | Some k -> Option.value (Row.text r k) ~default:(Int.to_string i)
    | None -> Int.to_string i)
;;

let format_tick (axis : axis) s =
  match axis.tick_formatter with
  | Some f -> f s
  | None -> s
;;

let bar_positions
      ?max_bar_size
      ~bar_gap
      ~bar_category_gap
      ~band_size
      (sizes : float option list)
  =
  let len = List.length sizes in
  if len = 0
  then []
  else (
    let real_gap = ref bar_gap in
    match sizes with
    | Some _ :: _ ->
      let sum = ref (List.fold sizes ~init:0. ~f:(fun acc s -> acc +. Option.value s ~default:0.)) in
      sum := !sum +. (Float.of_int (len - 1) *. !real_gap);
      if Float.(!sum >= band_size)
      then (
        sum := !sum -. (Float.of_int (len - 1) *. !real_gap);
        real_gap := 0.);
      let full = ref (band_size /. Float.of_int len) in
      let use_full = Float.(!sum >= band_size && !full > 0.) in
      if use_full
      then (
        full := !full *. 0.9;
        sum := Float.of_int len *. !full);
      let offset = Float.of_int (Float.to_int ((band_size -. !sum) /. 2.)) in
      let prev = ref (offset -. !real_gap, 0.) in
      List.map sizes ~f:(fun s ->
        let po, ps = !prev in
        let size = if use_full then !full else Option.value s ~default:0. in
        let pos = po +. ps +. !real_gap, size in
        prev := pos;
        pos)
    | _ ->
      let offset = band_size *. bar_category_gap in
      let gaps = Float.of_int (len - 1) *. !real_gap in
      if Float.(band_size -. (2. *. offset) -. gaps <= 0.)
      then real_gap := 0.;
      let original =
        (band_size -. (2. *. offset) -. (Float.of_int (len - 1) *. !real_gap))
        /. Float.of_int len
      in
      let original =
        if Float.(original > 1.) then Float.of_int (Float.to_int original) else original
      in
      let size =
        Option.value_map max_bar_size ~default:original ~f:(Float.min original)
      in
      List.mapi sizes ~f:(fun i _ ->
        ( offset +. ((original +. !real_gap) *. Float.of_int i) +. ((original -. size) /. 2.)
        , size )))
;;

let color_of_series (s : series) =
  match s.kind with
  | Bar _ -> Option.value s.fill ~default:"#3182bd"
  | Area _ -> Option.value s.stroke ~default:"#3182bd"
  | Line _ -> Option.value s.stroke ~default:"#3182bd"
;;

let series_name (s : series) = Option.value s.s_name ~default:s.s_data_key

let text_classes = [ "recharts-label" ]

(* LabelList positions in a cartesian box (Recharts' getCartesianPosition). *)
let cartesian_label_position (position : Position.t) ~offset (b : box) =
  let vsign = if Float.(b.height >= 0.) then 1. else -1. in
  let voff = vsign *. offset in
  let vend = if Float.(vsign > 0.) then `End else `Start in
  let vstart = if Float.(vsign > 0.) then `Start else `End in
  let hsign = if Float.(b.width >= 0.) then 1. else -1. in
  let hoff = hsign *. offset in
  let hend = if Float.(hsign > 0.) then `End else `Start in
  let hstart = if Float.(hsign > 0.) then `Start else `End in
  match position with
  | Top -> b.x +. (b.width /. 2.), b.y -. voff, `Middle, vend
  | Bottom -> b.x +. (b.width /. 2.), b.y +. b.height +. voff, `Middle, vstart
  | Left -> b.x -. hoff, b.y +. (b.height /. 2.), hend, `Middle
  | Right -> b.x +. b.width +. hoff, b.y +. (b.height /. 2.), hstart, `Middle
  | Inside_left -> b.x +. hoff, b.y +. (b.height /. 2.), hstart, `Middle
  | Inside_right -> b.x +. b.width -. hoff, b.y +. (b.height /. 2.), hend, `Middle
  | Inside_top -> b.x +. (b.width /. 2.), b.y +. voff, `Middle, vstart
  | Inside_bottom -> b.x +. (b.width /. 2.), b.y +. b.height -. voff, `Middle, vend
  | _ -> b.x +. (b.width /. 2.), b.y +. (b.height /. 2.), `Middle, `Middle
;;

let render_label_list
      (ll : label_list)
      ~default_key
      ~default_position
      (entries : (box * int * row * Value.t option) list)
  =
  layer
    [ "recharts-label-list" ]
    (List.map entries ~f:(fun (b, index, row, value) ->
       let value =
         match ll.ll_data_key with
         | Some k -> Row.find row k
         | None -> (match value with Some v -> Some v | None -> Row.find row default_key)
       in
       let text = Option.value_map value ~default:"" ~f:Value.to_string in
       let text = match ll.ll_formatter with Some f -> f text | None -> text in
       let position = Option.value ll.ll_position ~default:default_position in
       let x, y, anchor, vertical = cartesian_label_position position ~offset:ll.ll_offset b in
       match ll.ll_content with
       | Some f ->
         f { x = b.x; y = b.y; width = b.width; height = b.height; value = text; index; row }
       | None ->
         text_node
           ~classes:(text_classes @ Option.to_list ll.ll_class)
           ~fill:(Some (Option.value ll.ll_fill ~default:"#808080"))
           ~stroke_none:(Option.is_none ll.ll_stroke)
           ~attrs:
             (Option.value_map ll.ll_font_size ~default:[] ~f:(fun s -> [ af "font-size" s ])
              @ Option.value_map ll.ll_fill_opacity ~default:[] ~f:(fun o ->
                [ af "fill-opacity" o ])
              @ Option.value_map ll.ll_stroke ~default:[] ~f:(fun s -> [ a "stroke" s ])
              @ [ af "offset" ll.ll_offset ])
           ~x
           ~y
           ~anchor
           ~vertical
           text))
;;

let render_cartesian (ctx : render_ctx) : rendered =
  let spec = ctx.spec in
  let data = spec.data in
  let n = List.length data in
  let rows = Array.of_list data in
  let x_axis =
    find_map_elements spec ~f:(function X_axis a -> Some a | _ -> None)
  in
  let y_axis =
    find_map_elements spec ~f:(function Y_axis a -> Some a | _ -> None)
  in
  let implicit = axis ~hide:true () in
  let x_axis_present = Option.is_some x_axis in
  let y_axis_present = Option.is_some y_axis in
  let x_axis = Option.value x_axis ~default:implicit in
  let y_axis = Option.value y_axis ~default:implicit in
  let vertical = Layout.equal spec.layout Vertical in
  let x_type =
    Option.value x_axis.type_ ~default:(if vertical then Axis_type.Number else Category)
  in
  let y_type =
    Option.value y_axis.type_ ~default:(if vertical then Axis_type.Category else Number)
  in
  let series =
    List.filter_map spec.elements ~f:(function Series s -> Some s | _ -> None)
  in
  let x_size = Option.value x_axis.size ~default:30. in
  let y_size = Option.value y_axis.size ~default:60. in
  let shows (axis : axis) present = present && not axis.hide in
  let plot =
    offset_box
      ctx
      ~left:
        (if shows y_axis y_axis_present && Poly.equal y_axis.mirror_side `Low then y_size else 0.)
      ~right:
        (if shows y_axis y_axis_present && Poly.equal y_axis.mirror_side `High then y_size else 0.)
      ~top:
        (if shows x_axis x_axis_present && Poly.equal x_axis.mirror_side `High then x_size else 0.)
      ~bottom:
        (if shows x_axis x_axis_present && Poly.equal x_axis.mirror_side `Low then x_size else 0.)
  in
  (* Stacks *)
  let stack_groups =
    List.filter_map series ~f:(fun s -> s.stack_id)
    |> List.dedup_and_sort ~compare:String.compare
  in
  let stacked =
    List.concat_map stack_groups ~f:(fun id ->
      let members = List.filter series ~f:(fun s -> Option.equal String.equal s.stack_id (Some id)) in
      let keys = List.map members ~f:(fun s -> s.s_data_key) in
      List.zip_exn keys (stack_values ~offset:spec.stack_offset data keys))
  in
  let series_values (s : series) =
    match s.stack_id with
    | Some _ ->
      let v = List.Assoc.find_exn stacked ~equal:String.equal s.s_data_key in
      Array.map v ~f:(fun (b, t) -> Some (b, t))
    | None -> Array.map rows ~f:(fun r -> Option.map (Row.num r s.s_data_key) ~f:(fun v -> Float.nan, v))
  in
  let values = List.map series ~f:(fun s -> s, series_values s) in
  (* Number domain *)
  let number_axis = if vertical then x_axis else y_axis in
  let domain_values =
    List.concat_map values ~f:(fun (_, v) ->
      Array.to_list v
      |> List.concat_map ~f:(function
        | None -> []
        | Some (b, t) -> if Float.is_nan b then [ t ] else [ b; t ]))
  in
  let data_min = List.fold domain_values ~init:0. ~f:Float.min in
  let data_max = List.fold domain_values ~init:Float.neg_infinity ~f:Float.max in
  let data_max = if Float.is_finite data_max then data_max else 0. in
  let ticks, domain =
    match number_axis.domain with
    | Some (lo, hi) ->
      ( Chart_scale.fixed_domain_ticks
          ~allow_decimals:number_axis.allow_decimals
          ~count:number_axis.tick_count
          (lo, hi)
      , (lo, hi) )
    | None ->
      let ticks =
        Chart_scale.nice_ticks
          ~allow_decimals:number_axis.allow_decimals
          ~count:number_axis.tick_count
          (data_min, data_max)
      in
      (match ticks with
       | [] -> [], (data_min, data_max)
       | first :: _ -> ticks, (first, List.last_exn ticks))
  in
  let lo, hi = domain in
  let base_value = if Float.(lo >= 0.) then lo else if Float.(hi <= 0.) then hi else 0. in
  let num_range =
    if vertical then plot.x, plot.x +. plot.width else plot.y +. plot.height, plot.y
  in
  let num_scale : Chart_scale.Linear.t = { domain; range = num_range } in
  let nscale v = Chart_scale.Linear.apply num_scale v in
  (* Category scale *)
  let cat_axis = if vertical then y_axis else x_axis in
  let categories = category_values data cat_axis.data_key in
  let pad_lo, pad_hi = cat_axis.padding in
  let cat_range =
    if vertical
    then plot.y +. pad_lo, plot.y +. plot.height -. pad_hi
    else plot.x +. pad_lo, plot.x +. plot.width -. pad_hi
  in
  let is_band = Kind.equal spec.kind Bar in
  let band = Chart_scale.Band.create ~point:(not is_band) ~n ~range:cat_range () in
  let bandwidth = Chart_scale.Band.bandwidth band in
  let cat_start i = Chart_scale.Band.apply band i in
  let cat_centre i = cat_start i +. (bandwidth /. 2.) in
  ignore (x_type, y_type);
  (* Active index *)
  let tooltip = find_tooltip spec in
  let in_plot (x, y) =
    Float.(x >= plot.x && x <= plot.x +. plot.width && y >= plot.y && y <= plot.y +. plot.height)
  in
  let active_index =
    match tooltip with
    | None -> None
    | Some tip ->
      (match ctx.chart.state.pointer, ctx.chart.state.key_index with
       | Some p, _ when in_plot p ->
         Chart_scale.Band.index_at band (if vertical then snd p else fst p)
       | Some _, _ -> None
       | None, Some i -> Some i
       | None, None -> tip.default_index)
  in
  let active_index = Option.filter active_index ~f:(fun i -> i >= 0 && i < n) in
  (* Grid *)
  let grid =
    find_map_elements spec ~f:(function Grid g -> Some g | _ -> None)
  in
  let num_tick_coords = List.map ticks ~f:nscale in
  let cat_tick_coords = List.init n ~f:cat_centre in
  let h_coords = if vertical then cat_tick_coords else num_tick_coords in
  let v_coords = if vertical then num_tick_coords else cat_tick_coords in
  let grid_line ~x1 ~y1 ~x2 ~y2 dash =
    svg
      "line"
      ~attrs:
        ([ a "stroke" "#ccc"; a "fill" "none" ]
         @ Option.value_map dash ~default:[] ~f:(fun d -> [ a "stroke-dasharray" d ])
         @ [ af "x" plot.x
           ; af "y" plot.y
           ; af "width" plot.width
           ; af "height" plot.height
           ; af "x1" x1
           ; af "y1" y1
           ; af "x2" x2
           ; af "y2" y2
           ])
      []
  in
  let grid_node =
    match grid with
    | None -> Node.none
    | Some gr ->
      g
        [ "recharts-cartesian-grid" ]
        [ (if gr.horizontal
           then
             g
               [ "recharts-cartesian-grid-horizontal" ]
               (List.map h_coords ~f:(fun y ->
                  grid_line ~x1:plot.x ~y1:y ~x2:(plot.x +. plot.width) ~y2:y gr.dasharray))
           else Node.none)
        ; (if gr.vertical
           then
             g
               [ "recharts-cartesian-grid-vertical" ]
               (List.map v_coords ~f:(fun x ->
                  grid_line ~x1:x ~y1:plot.y ~x2:x ~y2:(plot.y +. plot.height) gr.dasharray))
           else Node.none)
        ]
  in
  (* Bars' positions *)
  let bar_series = List.filter series ~f:(fun s -> match s.kind with Bar _ -> true | _ -> false) in
  let bar_groups =
    List.fold bar_series ~init:[] ~f:(fun acc s ->
      match s.stack_id with
      | Some id when List.exists acc ~f:(fun (gid, _) -> Option.equal String.equal gid (Some id)) -> acc
      | _ ->
        acc @ [ s.stack_id, (match s.kind with Bar b -> b.bar_size | _ -> None) ])
  in
  let positions =
    bar_positions
      ?max_bar_size:
        (List.find_map bar_series ~f:(fun s ->
           match s.kind with
           | Bar b -> b.max_bar_size
           | _ -> None))
      ~bar_gap:spec.bar_gap
      ~bar_category_gap:spec.bar_category_gap
      ~band_size:bandwidth
      (List.map bar_groups ~f:snd)
  in
  let bar_position (s : series) =
    let idx =
      match s.stack_id with
      | Some id ->
        List.findi bar_groups ~f:(fun _ (gid, _) -> Option.equal String.equal gid (Some id))
        |> Option.map ~f:fst
      | None ->
        let unstacked_before =
          List.take_while bar_series ~f:(fun s' -> not (phys_equal s' s))
        in
        let groups_before =
          List.fold unstacked_before ~init:[] ~f:(fun acc s' ->
            match s'.stack_id with
            | Some id when List.mem acc (Some id) ~equal:(Option.equal String.equal) -> acc
            | sid -> sid :: acc)
        in
        Some (List.length groups_before)
    in
    Option.bind idx ~f:(List.nth positions)
  in
  let point_of i v = if vertical then nscale v, cat_centre i else cat_centre i, nscale v in
  (* Series; their labels go above the axes, as in Recharts. *)
  let label_nodes = ref [] in
  let add_labels l = label_nodes := !label_nodes @ l in
  let render_series (s, (vals : (float * float) option array)) =
    let stroke_width = Option.value s.stroke_width ~default:1. in
    let top_points () =
      Array.to_list
        (Array.mapi vals ~f:(fun i v -> Option.map v ~f:(fun (_, t) -> point_of i t)))
    in
    let segments pts =
      if s.connect_nulls
      then [ List.filter_opt pts ]
      else
        List.group pts ~break:(fun a b -> Bool.(Option.is_some a <> Option.is_some b))
        |> List.filter_map ~f:(fun seg ->
          match seg with
          | Some _ :: _ -> Some (List.filter_opt seg)
          | _ -> None)
    in
    let dots ~cls_name ~fill ~stroke =
      match s.dot with
      | Dot.Hide -> Node.none
      | dot ->
        layer
          [ cls_name ^ "s" ]
          (Array.to_list vals
           |> List.filter_mapi ~f:(fun i v ->
             Option.bind v ~f:(fun (_, t) ->
               let cx, cy = point_of i t in
               Dot.render
                 dot
                 ~class_:cls_name
                 { cx
                 ; cy
                 ; r = 3.
                 ; index = i
                 ; row = rows.(i)
                 ; data_key = s.s_data_key
                 ; fill
                 ; stroke
                 ; stroke_width
                 ; fill_opacity = None
                 })))
    in
    let point_labels ~default_position =
      List.map (label_lists s.s_children) ~f:(fun ll ->
        render_label_list
          ll
          ~default_key:s.s_data_key
          ~default_position
          (Array.to_list vals
           |> List.filter_mapi ~f:(fun i v ->
             Option.map v ~f:(fun (_, t) ->
               let x, y = point_of i t in
               ( { x; y; width = 0.; height = 0. }
               , i
               , rows.(i)
               , Row.find rows.(i) s.s_data_key )))))
    in
    match s.kind with
    | Area { curve; fill_opacity; stroke_dasharray } ->
      let fill = Option.value s.fill ~default:"#3182bd" in
      let stroke = Option.value s.stroke ~default:"#3182bd" in
      let base_of i (b, _) =
        let b = if Float.is_nan b then base_value else b in
        point_of i b
      in
      let area_paths =
        let pts =
          Array.to_list (Array.mapi vals ~f:(fun i v -> Option.map v ~f:(fun bt -> i, bt)))
        in
        segments pts
        |> List.map ~f:(fun seg ->
          ( Chart_path.area
              ~vertical
              curve
              ~top:(List.map seg ~f:(fun (i, (_, t)) -> point_of i t))
              ~base:(List.map seg ~f:(fun (i, bt) -> base_of i bt))
          , Chart_path.line ~vertical curve (List.map seg ~f:(fun (i, (_, t)) -> point_of i t)) ))
      in
      let fo = Option.value_map fill_opacity ~default:[] ~f:(fun o -> [ af "fill-opacity" o ]) in
      layer
        [ "recharts-area" ]
        ([ layer
             []
             [ svg
                 "path"
                 ~attrs:
                   ([ af "stroke-width" stroke_width; a "fill" fill ]
                    @ fo
                    @ [ a "stroke" "none"
                      ; cls [ "recharts-curve"; "recharts-area-area" ]
                      ; a "d" (String.concat (List.map area_paths ~f:fst))
                      ])
                 []
             ; svg
                 "path"
                 ~attrs:
                   ([ af "stroke-width" stroke_width; a "fill" "none" ]
                    @ fo
                    @ [ a "stroke" stroke ]
                    @ Option.value_map stroke_dasharray ~default:[] ~f:(fun d ->
                      [ a "stroke-dasharray" d ])
                    @ [ cls [ "recharts-curve"; "recharts-area-curve" ]
                      ; a "d" (String.concat (List.map area_paths ~f:snd))
                      ])
                 []
             ]
         ; dots ~cls_name:"recharts-area-dot" ~fill ~stroke
         ]
         @ (add_labels (point_labels ~default_position:Top);
            []))
    | Line { curve; stroke_dasharray } ->
      let stroke = Option.value s.stroke ~default:"#3182bd" in
      let d =
        segments (top_points ())
        |> List.map ~f:(fun seg -> Chart_path.line ~vertical curve seg)
        |> String.concat
      in
      layer
        [ "recharts-line" ]
        ([ svg
             "path"
             ~attrs:
               ([ a "stroke" stroke; af "stroke-width" stroke_width; a "fill" "none" ]
                @ Option.value_map stroke_dasharray ~default:[] ~f:(fun d ->
                  [ a "stroke-dasharray" d ])
                @ [ cls [ "recharts-curve"; "recharts-line-curve" ]; a "d" d ])
             []
         ; dots ~cls_name:"recharts-line-dot" ~fill:(Option.value s.fill ~default:"#fff") ~stroke
         ]
         @ (add_labels (point_labels ~default_position:Top);
            []))
    | Bar { radius; active_index = bar_active; active_bar; shape; bar_size = _; max_bar_size = _ } ->
      let fill = Option.value s.fill ~default:"#808080" in
      let cells = Array.of_list (cells s.s_children) in
      (match bar_position s with
       | None -> Node.none
       | Some (offset, size) ->
         let rects =
           Array.to_list vals
           |> List.filter_mapi ~f:(fun i v ->
             Option.map v ~f:(fun (b, t) ->
               let b = if Float.is_nan b then base_value else b in
               let box =
                 if vertical
                 then (
                   let x = nscale b in
                   { x; y = cat_start i +. offset; width = nscale t -. x; height = size })
                 else (
                   let y = nscale t in
                   { x = cat_start i +. offset; y; width = size; height = nscale b -. y })
               in
               i, box))
         in
         let rect_props i (b : box) : Rectangle_props.t =
           let row = rows.(i) in
           let fill =
             if i < Array.length cells
             then cells.(i)
             else Option.value (Row.text row "fill") ~default:fill
           in
           { x = b.x; y = b.y; width = b.width; height = b.height; radius; fill; index = i; row }
         in
         let stroke_attrs =
           Option.value_map s.stroke ~default:[] ~f:(fun st -> [ a "stroke" st ])
           @ Option.value_map s.stroke_width ~default:[] ~f:(fun w -> [ af "stroke-width" w ])
         in
         let is_active i =
           match bar_active with
           | Some a -> a = i
           | None -> false
         in
         layer
           [ "recharts-bar" ]
           ([ layer
                [ "recharts-bar-rectangles" ]
                (List.map rects ~f:(fun (i, b) ->
                   let p = rect_props i b in
                   layer
                     [ "recharts-bar-rectangle" ]
                     [ (match shape, active_bar with
                        | _, Some f when is_active i -> f p
                        | Some f, _ -> f p
                        | None, _ -> rectangle ~attrs:stroke_attrs p)
                     ]))
            ]
            @ (add_labels
                 (List.map (label_lists s.s_children) ~f:(fun ll ->
                    render_label_list
                      ll
                      ~default_key:s.s_data_key
                      ~default_position:Center
                      (List.map rects ~f:(fun (i, b) ->
                         b, i, rows.(i), Row.find rows.(i) s.s_data_key))));
               [])))
  in
  let series_nodes = List.map values ~f:render_series in
  (* Axes *)
  let render_axis (axis : axis) ~is_x ~present ~(type_ : Axis_type.t) =
    if not (shows axis present)
    then Node.none
    else (
      let low = Poly.equal axis.mirror_side `Low in
      let ticks =
        match type_ with
        | Category -> List.mapi categories ~f:(fun i c -> cat_centre i, c)
        | Number -> List.map ticks ~f:(fun t -> nscale t, num t)
      in
      let font_size = 12. in
      let size i v =
        let s = format_tick axis v in
        ignore i;
        if is_x then text_width ~font_size s else font_size *. 1.25
      in
      let shown =
        if axis.tick
        then
          visible_ticks
            ~interval:axis.interval
            ~min_tick_gap:axis.min_tick_gap
            ~start:0.
            ~end_:(if is_x then ctx.width else ctx.height)
            ~size
            ticks
        else []
      in
      let tick_size = 6. in
      let line_pos =
        if is_x
        then if low then plot.y +. plot.height else plot.y
        else if low
        then plot.x
        else plot.x +. plot.width
      in
      let sgn = if (is_x && low) || ((not is_x) && not low) then 1. else -1. in
      let axis_name = if is_x then "xAxis" else "yAxis" in
      let orientation =
        match is_x, low with
        | true, true -> "bottom"
        | true, false -> "top"
        | false, true -> "left"
        | false, false -> "right"
      in
      let common =
        [ a "orientation" orientation
        ; af "width" (if is_x then plot.width else y_size)
        ; af "height" (if is_x then x_size else plot.height)
        ]
      in
      layer
        [ "recharts-cartesian-axis"; "recharts-" ^ axis_name; axis_name ]
        [ (if axis.axis_line
           then (
             let x1, y1, x2, y2 =
               if is_x
               then plot.x, line_pos, plot.x +. plot.width, line_pos
               else line_pos, plot.y, line_pos, plot.y +. plot.height
             in
             svg
               "line"
               ~attrs:
                 (common
                  @ [ cls [ "recharts-cartesian-axis-line" ]
                    ; a "stroke" "#666"
                    ; a "fill" "none"
                    ; af "x1" x1
                    ; af "y1" y1
                    ; af "x2" x2
                    ; af "y2" y2
                    ])
               [])
           else Node.none)
        (* Recharts 3: the labels and the lines in two groups. *)
        ; g
            [ "recharts-cartesian-axis-ticks"; "recharts-" ^ axis_name ^ "-ticks" ]
            [ (if List.is_empty shown
               then Node.none
               else
                 g
                   [ "recharts-cartesian-axis-tick-labels"
                   ; "recharts-" ^ axis_name ^ "-tick-labels"
                   ]
                   (List.mapi shown ~f:(fun index (coord, v) ->
                      let label = format_tick axis v in
                      let text_pos =
                        line_pos +. (sgn *. (tick_size +. axis.tick_margin))
                      in
                      let x, y = if is_x then coord, text_pos else text_pos, coord in
                      let anchor, vertical_anchor =
                        if is_x
                        then `Middle, if low then `Start else `End
                        else (if low then `End else `Start), `Middle
                      in
                      layer
                        [ "recharts-cartesian-axis-tick-label" ]
                        [ (match axis.custom_tick with
                           | Some f ->
                             f
                               { x
                               ; y
                               ; text_anchor = Anchor.name anchor
                               ; index
                               ; value = v
                               ; row = None
                               }
                           | None ->
                             text_node
                               ~attrs:common
                               ~classes:[ "recharts-cartesian-axis-tick-value" ]
                               ~fill:(Some "#666")
                               ~x
                               ~y
                               ~anchor
                               ~vertical:vertical_anchor
                               label)
                        ])))
            ; (if List.is_empty shown
               then Node.none
               else
                 g
                   [ "recharts-cartesian-axis-tick-lines"
                   ; "recharts-" ^ axis_name ^ "-tick-lines"
                   ]
                   (List.map shown ~f:(fun (coord, _) ->
                      layer
                        [ "recharts-cartesian-axis-tick" ]
                        [ (if axis.tick_line
                           then (
                             let a1 = line_pos +. (sgn *. tick_size) in
                             let x1, y1, x2, y2 =
                               if is_x
                               then coord, a1, coord, line_pos
                               else a1, coord, line_pos, coord
                             in
                             svg
                               "line"
                               ~attrs:
                                 (common
                                  @ [ cls [ "recharts-cartesian-axis-tick-line" ]
                                    ; a "stroke" "#666"
                                    ; a "fill" "none"
                                    ; af "x1" x1
                                    ; af "y1" y1
                                    ; af "x2" x2
                                    ; af "y2" y2
                                    ])
                               [])
                           else Node.none)
                        ])))
            ]
        ])
  in
  let axes =
    [ render_axis x_axis ~is_x:true ~present:x_axis_present ~type_:x_type
    ; render_axis y_axis ~is_x:false ~present:y_axis_present ~type_:y_type
    ]
  in
  (* Tooltip parts *)
  let active =
    Option.map active_index ~f:(fun i ->
      let payload =
        List.filter_map values ~f:(fun (s, _) ->
          let value = Row.find rows.(i) s.s_data_key in
          Some
            { Item.data_key = s.s_data_key
            ; name = series_name s
            ; value
            ; color = color_of_series s
            ; row = rows.(i)
            })
      in
      let coordinate =
        match ctx.chart.state.pointer with
        | Some p -> p
        | None ->
          if vertical
          then plot.x +. (plot.width /. 2.), cat_centre i
          else cat_centre i, plot.y +. (plot.height /. 2.)
      in
      { index = i
      ; coordinate
      ; tooltip_label = Some (List.nth_exn categories i)
      ; payload
      })
  in
  let cursor_node =
    match active, tooltip with
    | Some act, Some { cursor = true; _ } ->
      let i = act.index in
      if is_band
      then
        rectangle
          ~attrs:[ a "stroke" "none"; a "pointer-events" "none" ]
          ~class_:"recharts-tooltip-cursor"
          { x = (if vertical then plot.x else cat_start i)
          ; y = (if vertical then cat_start i else plot.y +. 0.5)
          ; width = (if vertical then plot.width else bandwidth)
          ; height = (if vertical then bandwidth else plot.height -. 1.)
          ; radius = None
          ; fill = "#ccc"
          ; index = i
          ; row = rows.(i)
          }
      else (
        let c = cat_centre i in
        let d =
          if vertical
          then sprintf "M%s,%sL%s,%s" (num plot.x) (num c) (num (plot.x +. plot.width)) (num c)
          else sprintf "M%s,%sL%s,%s" (num c) (num plot.y) (num c) (num (plot.y +. plot.height))
        in
        svg
          "path"
          ~attrs:
            [ a "stroke" "#ccc"
            ; a "pointer-events" "none"
            ; cls [ "recharts-curve"; "recharts-tooltip-cursor" ]
            ; a "d" d
            ]
          [])
    | _ -> Node.none
  in
  let active_dots =
    match active with
    | None -> []
    | Some act ->
      List.filter_map values ~f:(fun (s, vals) ->
        match s.kind with
        | Bar _ -> None
        | Area _ | Line _ ->
          Option.bind vals.(act.index) ~f:(fun (_, t) ->
            let cx, cy = point_of act.index t in
            let color = color_of_series s in
            Dot.render
              s.active_dot
              ~class_:""
              { cx
              ; cy
              ; r = 4.
              ; index = act.index
              ; row = rows.(act.index)
              ; data_key = s.s_data_key
              ; fill = color
              ; stroke = "#fff"
              ; stroke_width = 2.
              ; fill_opacity = None
              }
            |> Option.map ~f:(fun d -> layer [ "recharts-active-dot" ] [ d ])))
  in
  let raws = List.filter_map spec.elements ~f:(function Raw n -> Some n | _ -> None) in
  let legend_payload =
    List.map series ~f:(fun s ->
      { Item.data_key = s.s_data_key
      ; name = series_name s
      ; value = Some (`Text (series_name s))
      ; color = color_of_series s
      ; row = []
      })
  in
  let bar_cursor, line_cursor = if is_band then cursor_node, Node.none else Node.none, cursor_node in
  { svg_children =
      raws
      @ [ grid_node; bar_cursor ]
      @ series_nodes
      @ axes
      @ !label_nodes
      @ [ line_cursor ]
      @ active_dots
  ; active
  ; legend_payload
  ; plot
  ; nav_count = n
  }
;;

(* Polar charts *)

let polar_frame (ctx : render_ctx) =
  let plot = offset_box ctx ~left:0. ~right:0. ~top:0. ~bottom:0. in
  let max_radius = Float.min plot.width plot.height /. 2. in
  let cx = Option.value ctx.spec.cx ~default:(ctx.width /. 2.) in
  let cy = Option.value ctx.spec.cy ~default:(ctx.height /. 2.) in
  let inner = Option.value ctx.spec.inner_radius ~default:0. in
  let outer = Option.value ctx.spec.outer_radius ~default:(max_radius *. 0.8) in
  plot, max_radius, cx, cy, inner, outer
;;

let pointer_polar (ctx : render_ctx) ~cx ~cy =
  Option.map ctx.chart.state.pointer ~f:(fun (x, y) ->
    let dx = x -. cx
    and dy = y -. cy in
    let r = Float.sqrt ((dx *. dx) +. (dy *. dy)) in
    let angle = Float.atan2 (-.dy) dx /. Chart_path.radian in
    r, angle)
;;

(* Is [angle] within the arc from [start_angle] to [end_angle] (either way)? *)
let angle_in_range angle ~start_angle ~end_angle =
  let lo = Float.min start_angle end_angle
  and hi = Float.max start_angle end_angle in
  let rec norm a = if Float.(a < lo) then norm (a +. 360.) else a in
  let a = norm angle in
  let a = if Float.(a > hi) then a -. (360. *. Float.round_down ((a -. lo) /. 360.)) else a in
  Float.(a >= lo && a <= hi)
;;

let text_anchor_of ~x ~cx = if Float.(x > cx) then "start" else if Float.(x < cx) then "end" else "middle"

let render_polar_label (l : label) (vb : Polar_view_box.t) =
  match l.l_content with
  | Some f -> f vb
  | None ->
    (match l.l_value with
     | None -> Node.none
     | Some v ->
       text_node
         ~classes:(Option.to_list l.l_class)
         ~x:vb.cx
         ~y:vb.cy
         ~anchor:`Middle
         ~vertical:`Middle
         v)
;;

let render_pie (ctx : render_ctx) : rendered =
  let plot, _, _, _, _, _ = polar_frame ctx in
  let pies = List.filter_map ctx.spec.elements ~f:(function Pie p -> Some p | _ -> None) in
  let max_pie_radius = Float.min plot.width plot.height /. 2. in
  let cx = plot.x +. (plot.width /. 2.) in
  let cy = plot.y +. (plot.height /. 2.) in
  let tooltip = find_tooltip ctx.spec in
  let polar = pointer_polar ctx ~cx ~cy in
  let actives = ref None in
  let legend = ref [] in
  let nodes =
    List.mapi pies ~f:(fun pie_index p ->
      let inner = Option.value p.p_inner_radius ~default:0. in
      let outer = Option.value p.p_outer_radius ~default:(max_pie_radius *. 0.8) in
      let rows = Array.of_list p.p_data in
      let vals = Array.map rows ~f:(fun r -> Option.value (Row.num r p.p_data_key) ~default:0.) in
      let cells = Array.of_list (cells p.p_children) in
      let fill_of i =
        if i < Array.length cells
        then cells.(i)
        else (
          match Row.text rows.(i) "fill" with
          | Some f -> f
          | None -> Option.value p.p_fill ~default:"#808080")
      in
      let delta = Chart_path.delta_angle p.p_start_angle p.p_end_angle in
      let sgn = Chart_path.sign delta in
      let abs_delta = Float.abs delta in
      let not_zero = Array.count vals ~f:(fun v -> Float.(v <> 0.)) in
      let total_padding =
        Float.of_int (if Float.(abs_delta >= 360.) then not_zero else not_zero - 1)
        *. p.p_padding_angle
      in
      let real_total = abs_delta -. total_padding in
      let sum = Array.fold vals ~init:0. ~f:(fun acc v -> acc +. Float.abs v) in
      let prev_end = ref p.p_start_angle in
      let sectors =
        Array.mapi vals ~f:(fun i v ->
          let percent = if Float.(sum = 0.) then 0. else Float.abs v /. sum in
          let start_angle =
            if i = 0
            then p.p_start_angle
            else !prev_end +. (sgn *. p.p_padding_angle *. if Float.(v <> 0.) then 1. else 0.)
          in
          let end_angle = start_angle +. (sgn *. percent *. real_total) in
          prev_end := end_angle;
          let props : Sector_props.t =
            { cx
            ; cy
            ; inner_radius = inner
            ; outer_radius = outer
            ; start_angle
            ; end_angle
            ; corner_radius = p.p_corner_radius
            ; fill = fill_of i
            ; stroke = Some (Option.value p.p_stroke ~default:"#fff")
            ; index = i
            ; row = rows.(i)
            }
          in
          props, percent, v)
      in
      let hovered =
        match tooltip, polar with
        | Some _, Some (r, angle) when Float.(r >= inner && r <= outer) ->
          Array.findi sectors ~f:(fun _ ((s : Sector_props.t), _, _) ->
            angle_in_range angle ~start_angle:s.start_angle ~end_angle:s.end_angle)
          |> Option.map ~f:fst
        | Some { default_index = Some d; _ }, None
          when Option.is_none ctx.chart.state.pointer && pie_index = 0 ->
          Some d
        | _ -> None
      in
      let hovered =
        match hovered, ctx.chart.state.key_index with
        | None, Some k when pie_index = 0 -> Some k
        | h, _ -> h
      in
      let hovered = Option.filter hovered ~f:(fun i -> i >= 0 && i < Array.length rows) in
      (* A fixed [active_index] (shadcn's [shape] drawing one sector larger)
         stays put; otherwise the active shape follows the tooltip, as
         Recharts' [activeShape]. *)
      let active_index =
        match p.p_active_index with
        | Some i -> Some i
        | None -> hovered
      in
      let name_of i = Option.value (Row.text rows.(i) p.p_name_key) ~default:(Int.to_string i) in
      Option.iter hovered ~f:(fun i ->
        let s, _, _ = sectors.(i) in
        let mid = (s.start_angle +. s.end_angle) /. 2. in
        let coordinate =
          match ctx.chart.state.pointer with
          | Some pt -> pt
          | None -> Chart_path.polar ~cx ~cy ~radius:((inner +. outer) /. 2.) ~angle:mid
        in
        actives
        := Some
             { index = i
             ; coordinate
             ; tooltip_label = None
             ; payload =
                 [ { Item.data_key = p.p_data_key
                   ; name = name_of i
                   ; value = Row.find rows.(i) p.p_data_key
                   ; color = s.fill
                   ; row = rows.(i)
                   }
                 ]
             });
      legend
      := !legend
         @ Array.to_list
             (Array.mapi sectors ~f:(fun i ((s : Sector_props.t), _, _) ->
                { Item.data_key = p.p_data_key
                ; name = name_of i
                ; value = Some (`Text (name_of i))
                ; color = s.fill
                ; row = rows.(i)
                }));
      let stroke_width_attr =
        Option.value_map p.p_stroke_width ~default:[] ~f:(fun w -> [ af "stroke-width" w ])
      in
      let sector_nodes =
        Array.to_list sectors
        |> List.map ~f:(fun ((s : Sector_props.t), _, _) ->
          let is_active = Option.equal Int.equal active_index (Some s.index) in
          layer
            [ "recharts-pie-sector" ]
            [ (match p.p_active_shape with
               | Some f when is_active -> layer [ "recharts-active-shape" ] [ f s ]
               | _ ->
                 sector
                   ~attrs:(stroke_width_attr @ [ a "name" (name_of s.index) ])
                   s)
            ])
      in
      let label_nodes =
        match p.p_label with
        | `None -> Node.none
        | label ->
          layer
            [ "recharts-pie-labels" ]
            (Array.to_list sectors
             |> List.map ~f:(fun ((s : Sector_props.t), percent, v) ->
               let mid = (s.start_angle +. s.end_angle) /. 2. in
               let x, y = Chart_path.polar ~cx ~cy ~radius:(outer +. 20.) ~angle:mid in
               let x0, y0 = Chart_path.polar ~cx ~cy ~radius:outer ~angle:mid in
               let anchor = text_anchor_of ~x ~cx in
               layer
                 []
                 [ (if p.p_label_line
                    then
                      svg
                        "path"
                        ~attrs:
                          [ a "stroke" s.fill
                          ; a "fill" "none"
                          ; cls [ "recharts-curve"; "recharts-pie-label-line" ]
                          ; a "d" (sprintf "M%s,%sL%s,%s" (num x0) (num y0) (num x) (num y))
                          ]
                        []
                    else Node.none)
                 ; (match label with
                    | `Custom f ->
                      f
                        { x
                        ; y
                        ; cx
                        ; cy
                        ; text_anchor = anchor
                        ; value = v
                        ; percent
                        ; mid_angle = mid
                        ; inner_radius = inner
                        ; outer_radius = outer
                        ; fill = s.fill
                        ; index = s.index
                        ; row = s.row
                        }
                    | _ ->
                      svg
                        "text"
                        ~attrs:
                          [ a "stroke" "none"
                          ; af "x" x
                          ; af "y" y
                          ; cls [ "recharts-text"; "recharts-pie-label-text" ]
                          ; a "text-anchor" anchor
                          ; a "dominant-baseline" "central"
                          ; a "fill" s.fill
                          ]
                        [ svg "tspan" ~attrs:[ af "x" x; a "dy" "0em" ] [ Node.text (num v) ] ])
                 ]))
      in
      let vb : Polar_view_box.t =
        { cx
        ; cy
        ; inner_radius = inner
        ; outer_radius = outer
        ; start_angle = p.p_start_angle
        ; end_angle = p.p_end_angle
        }
      in
      let label_list_nodes =
        List.map (label_lists p.p_children) ~f:(fun ll ->
          layer
            [ "recharts-label-list" ]
            (Array.to_list sectors
             |> List.map ~f:(fun ((s : Sector_props.t), _, _) ->
               let mid = (s.start_angle +. s.end_angle) /. 2. in
               let x, y, anchor =
                 match ll.ll_position with
                 | Some Outside ->
                   let x, y = Chart_path.polar ~cx ~cy ~radius:(outer +. ll.ll_offset) ~angle:mid in
                   x, y, if Float.(x >= cx) then `Start else `End
                 | _ ->
                   let x, y = Chart_path.polar ~cx ~cy ~radius:((inner +. outer) /. 2.) ~angle:mid in
                   x, y, `Middle
               in
               let value =
                 match ll.ll_data_key with
                 | Some k -> Row.text s.row k
                 | None -> Row.text s.row p.p_data_key
               in
               let text = Option.value value ~default:"" in
               let text = match ll.ll_formatter with Some f -> f text | None -> text in
               text_node
                 ~classes:(text_classes @ Option.to_list ll.ll_class)
                 ~fill:(Some (Option.value ll.ll_fill ~default:"#808080"))
                 ~stroke_none:(Option.is_none ll.ll_stroke)
                 ~attrs:
                   (Option.value_map ll.ll_font_size ~default:[] ~f:(fun s -> [ af "font-size" s ])
                    @ Option.value_map ll.ll_stroke ~default:[] ~f:(fun s -> [ a "stroke" s ]))
                 ~x
                 ~y
                 ~anchor
                 ~vertical:`Middle
                 text)))
      in
      layer
        [ "recharts-pie" ]
        ([ layer [] sector_nodes; label_nodes ]
         @ label_list_nodes
         @ List.map (labels p.p_children) ~f:(fun l -> render_polar_label l vb)))
  in
  { svg_children = nodes
  ; active = !actives
  ; legend_payload = !legend
  ; plot
  ; nav_count = (match pies with p :: _ -> List.length p.p_data | [] -> 0)
  }
;;

let polar_text_anchor angle =
  let c = Float.cos (-.angle *. Chart_path.radian) in
  if Float.(c > 1e-5) then `Start else if Float.(c < -1e-5) then `End else `Middle
;;

let polar_vertical_anchor angle =
  let s = Float.sin (-.angle *. Chart_path.radian) in
  if Float.(s > 1e-5) then `Start else if Float.(s < -1e-5) then `End else `Middle
;;

let render_polar_grid (pg : polar_grid) ~cx ~cy ~inner ~outer ~angles ~radii =
  let radii = Option.value pg.polar_radius ~default:radii in
  let stroke = Option.value pg.pg_stroke ~default:"#ccc" in
  let common =
    [ a "stroke" stroke ]
    @ Option.value_map pg.pg_stroke_width ~default:[] ~f:(fun w -> [ af "stroke-width" w ])
  in
  let cls_extra = Option.to_list pg.pg_class in
  let grid_polygon r =
    String.concat
      (List.mapi angles ~f:(fun i angle ->
         let x, y = Chart_path.polar ~cx ~cy ~radius:r ~angle in
         sprintf "%s %s,%s" (if i = 0 then "M" else "L") (num x) (num y)))
    ^ "Z"
  in
  let concentric fill r =
    match pg.grid_type with
    | `Circle ->
      svg
        "circle"
        ~attrs:
          (common
           @ [ a "fill" fill
             ; af "cx" cx
             ; af "cy" cy
             ; af "r" r
             ; cls ("recharts-polar-grid-concentric-circle" :: cls_extra)
             ])
        []
    | `Polygon ->
      svg
        "path"
        ~attrs:
          (common
           @ [ a "fill" fill
             ; af "cx" cx
             ; af "cy" cy
             ; af "radius" r
             ; cls ("recharts-polar-grid-concentric-polygon" :: cls_extra)
             ; a "d" (grid_polygon r)
             ])
        []
  in
  let background =
    match pg.pg_fill with
    | Some f when not (String.equal f "none") ->
      [ concentric f (List.fold radii ~init:0. ~f:Float.max) ]
    | _ -> []
  in
  g
    [ "recharts-polar-grid" ]
    [ g [ "recharts-polar-grid-concentric" ] (background @ List.map radii ~f:(concentric "none"))
    ; (if pg.radial_lines && not (List.is_empty angles)
       then
         g
           [ "recharts-polar-grid-angle" ]
           (List.map angles ~f:(fun angle ->
              let x1, y1 = Chart_path.polar ~cx ~cy ~radius:inner ~angle in
              let x2, y2 = Chart_path.polar ~cx ~cy ~radius:outer ~angle in
              svg
                "line"
                ~attrs:
                  (common
                   @ [ af "cx" cx
                     ; af "cy" cy
                     ; af "x1" x1
                     ; af "y1" y1
                     ; af "x2" x2
                     ; af "y2" y2
                     ])
                []))
       else Node.none)
    ]
;;

let render_radius_axis
      (ra : radius_axis)
      ~cx
      ~cy
      ~(scale : float -> float)
      ~(ticks : (float * string) list)
      ~(vb : Polar_view_box.t)
  =
  let outer = vb.outer_radius in
  let angle = ra.ra_angle in
  let pt r = Chart_path.polar ~cx ~cy ~radius:r ~angle in
  let anchor =
    match ra.ra_orientation with
    | `Left -> `End
    | `Right -> `Start
    | `Middle -> `Middle
  in
  layer
    [ "recharts-polar-radius-axis"; "radiusAxis" ]
    ([ (if ra.ra_axis_line
        then (
          let x1, y1 = pt vb.inner_radius in
          let x2, y2 = pt outer in
          svg
            "line"
            ~attrs:
              [ cls [ "recharts-polar-radius-axis-line" ]
              ; a "stroke" ra.ra_stroke
              ; a "fill" "none"
              ; af "x1" x1
              ; af "y1" y1
              ; af "x2" x2
              ; af "y2" y2
              ]
            [])
        else Node.none)
     ; (if ra.ra_tick
        then
          layer
            [ "recharts-polar-radius-axis-ticks" ]
            (List.map ticks ~f:(fun (v, label) ->
               let x, y = pt (scale v) in
               layer
                 [ "recharts-polar-radius-axis-tick" ]
                 [ text_node
                     ~attrs:
                       [ a
                           "transform"
                           (sprintf "rotate(%s, %s, %s)" (num (90. -. angle)) (num x) (num y))
                       ]
                     ~classes:[ "recharts-polar-radius-axis-tick-value" ]
                     ~fill:(Some ra.ra_stroke)
                     ~x
                     ~y
                     ~anchor
                     ~vertical:`End
                     label
                 ]))
        else Node.none)
     ]
     @ List.map (labels ra.ra_children) ~f:(fun l -> render_polar_label l vb))
;;

let render_radar (ctx : render_ctx) : rendered =
  let plot, _, cx, cy, inner, outer = polar_frame ctx in
  let data = ctx.spec.data in
  let rows = Array.of_list data in
  let n = Array.length rows in
  let radars = List.filter_map ctx.spec.elements ~f:(function Radar r -> Some r | _ -> None) in
  let angle_axis = List.find_map ctx.spec.elements ~f:(function Angle_axis x -> Some x | _ -> None) in
  let radius_axis = List.find_map ctx.spec.elements ~f:(function Radius_axis x -> Some x | _ -> None) in
  let angle_of i =
    ctx.spec.start_angle
    +. ((ctx.spec.end_angle -. ctx.spec.start_angle) *. Float.of_int i /. Float.of_int (Int.max n 1))
  in
  let angles = List.init n ~f:angle_of in
  let values =
    List.concat_map radars ~f:(fun r ->
      Array.to_list rows |> List.filter_map ~f:(fun row -> Row.num row r.r_data_key))
  in
  let data_max = List.fold values ~init:0. ~f:Float.max in
  let tick_count = Option.value_map radius_axis ~default:5 ~f:(fun r -> r.ra_tick_count) in
  let ticks, domain =
    match Option.bind radius_axis ~f:(fun r -> r.ra_domain) with
    | Some (lo, hi) ->
      Chart_scale.fixed_domain_ticks ~allow_decimals:false ~count:tick_count (lo, hi), (lo, hi)
    | None ->
      let t = Chart_scale.nice_ticks ~allow_decimals:false ~count:tick_count (0., data_max) in
      t, (List.hd_exn t, List.last_exn t)
  in
  let rscale : Chart_scale.Linear.t = { domain; range = inner, outer } in
  let rs v = Chart_scale.Linear.apply rscale v in
  let tooltip = find_tooltip ctx.spec in
  let active_index =
    match tooltip, pointer_polar ctx ~cx ~cy with
    | Some _, Some (r, angle) when Float.(r <= outer) && n > 0 ->
      let step = 360. /. Float.of_int n in
      let a = Float.mod_float (ctx.spec.start_angle -. angle +. 720. +. (step /. 2.)) 360. in
      Some (Float.to_int (a /. step) % n)
    | Some _, None ->
      (match ctx.chart.state.key_index with
       | Some k -> Some k
       | None -> Option.bind tooltip ~f:(fun t -> t.default_index))
    | _ -> None
  in
  let active_index = Option.filter active_index ~f:(fun i -> i >= 0 && i < n) in
  let categories = category_values data (Option.bind angle_axis ~f:(fun a -> a.aa_data_key)) in
  let grid =
    List.find_map ctx.spec.elements ~f:(function
      | Polar_grid pg -> Some (render_polar_grid pg ~cx ~cy ~inner ~outer ~angles ~radii:(List.map ticks ~f:rs))
      | _ -> None)
  in
  let color (r : radar) = Option.value r.r_stroke ~default:(Option.value r.r_fill ~default:"#3182bd") in
  let radar_nodes =
    List.map radars ~f:(fun r ->
      let pts =
        List.mapi angles ~f:(fun i angle ->
          let v = Option.value (Row.num rows.(i) r.r_data_key) ~default:0. in
          Chart_path.polar ~cx ~cy ~radius:(rs v) ~angle, i)
      in
      let fill = Option.value r.r_fill ~default:"none" in
      let polygon_pts = List.map pts ~f:fst in
      let polygon_pts = match polygon_pts with [] -> [] | p :: _ -> polygon_pts @ [ p ] in
      layer
        [ "recharts-radar" ]
        [ layer
            [ "recharts-radar-polygon" ]
            [ svg
                "path"
                ~attrs:
                  ([ a "fill" fill ]
                   @ Option.value_map r.r_fill_opacity ~default:[] ~f:(fun o -> [ af "fill-opacity" o ])
                   @ Option.value_map r.r_stroke ~default:[] ~f:(fun s -> [ a "stroke" s ])
                   @ Option.value_map r.r_stroke_width ~default:[] ~f:(fun w -> [ af "stroke-width" w ])
                   @ [ cls [ "recharts-polygon" ]; a "d" (Chart_path.polygon polygon_pts) ])
                []
            ]
        ; (match r.r_dot with
           | Dot.Hide -> Node.none
           | dot ->
             layer
               [ "recharts-radar-dots" ]
               (List.filter_map pts ~f:(fun ((x, y), i) ->
                  Dot.render
                    dot
                    ~class_:"recharts-radar-dot"
                    { cx = x
                    ; cy = y
                    ; r = 3.
                    ; index = i
                    ; row = rows.(i)
                    ; data_key = r.r_data_key
                    ; fill
                    ; stroke = Option.value r.r_stroke ~default:"none"
                    ; stroke_width = Option.value r.r_stroke_width ~default:1.
                    ; fill_opacity = None
                    })))
        ])
  in
  let angle_axis_node =
    match angle_axis with
    | None -> Node.none
    | Some aa ->
      let outline =
        match angles with
        | [] -> []
        | first :: _ ->
          List.map (angles @ [ first ]) ~f:(fun angle -> Chart_path.polar ~cx ~cy ~radius:outer ~angle)
      in
      layer
        [ "recharts-polar-angle-axis"; "angleAxis" ]
        [ (if aa.aa_axis_line
           then
             svg
               "path"
               ~attrs:
                 [ a "fill" "none"
                 ; cls [ "recharts-polygon"; "recharts-polar-angle-axis-line" ]
                 ; a "d" (Chart_path.polygon outline)
                 ]
               []
           else Node.none)
        ; layer
            [ "recharts-polar-angle-axis-ticks" ]
            (List.mapi angles ~f:(fun i angle ->
               let x1, y1 = Chart_path.polar ~cx ~cy ~radius:outer ~angle in
               let x, y = Chart_path.polar ~cx ~cy ~radius:(outer +. 8.) ~angle in
               let anchor = polar_text_anchor angle in
               layer
                 [ "recharts-polar-angle-axis-tick" ]
                 [ (if aa.aa_tick_line
                    then
                      svg
                        "line"
                        ~attrs:
                          [ cls [ "recharts-polar-angle-axis-tick-line" ]
                          ; a "fill" "none"
                          ; af "x1" x1
                          ; af "y1" y1
                          ; af "x2" x
                          ; af "y2" y
                          ]
                        []
                    else Node.none)
                 ; (match aa.aa_tick with
                    | `Hide -> Node.none
                    | `Custom f ->
                      f
                        { x
                        ; y
                        ; text_anchor = Anchor.name anchor
                        ; index = i
                        ; value = List.nth_exn categories i
                        ; row = Some rows.(i)
                        }
                    | `Show ->
                      text_node
                        ~classes:("recharts-polar-angle-axis-tick-value" :: Option.to_list aa.aa_class)
                        ~x
                        ~y
                        ~anchor
                        ~vertical:(polar_vertical_anchor angle)
                        (List.nth_exn categories i))
                 ]))
        ]
  in
  let vb : Polar_view_box.t =
    { cx
    ; cy
    ; inner_radius = inner
    ; outer_radius = outer
    ; start_angle = ctx.spec.start_angle
    ; end_angle = ctx.spec.end_angle
    }
  in
  let radius_axis_node =
    match radius_axis with
    | None -> Node.none
    | Some ra -> render_radius_axis ra ~cx ~cy ~scale:rs ~ticks:(List.map ticks ~f:(fun t -> t, num t)) ~vb
  in
  let active =
    Option.map active_index ~f:(fun i ->
      { index = i
      ; coordinate =
          (match ctx.chart.state.pointer with
           | Some p -> p
           | None -> Chart_path.polar ~cx ~cy ~radius:outer ~angle:(angle_of i))
      ; tooltip_label = Some (List.nth_exn categories i)
      ; payload =
          List.map radars ~f:(fun r ->
            { Item.data_key = r.r_data_key
            ; name = r.r_data_key
            ; value = Row.find rows.(i) r.r_data_key
            ; color = color r
            ; row = rows.(i)
            })
      })
  in
  let active_dots =
    match active_index with
    | None -> []
    | Some i ->
      List.filter_map radars ~f:(fun r ->
        let v = Option.value (Row.num rows.(i) r.r_data_key) ~default:0. in
        let x, y = Chart_path.polar ~cx ~cy ~radius:(rs v) ~angle:(angle_of i) in
        Dot.render
          r.r_active_dot
          ~class_:""
          { cx = x
          ; cy = y
          ; r = 4.
          ; index = i
          ; row = rows.(i)
          ; data_key = r.r_data_key
          ; fill = color r
          ; stroke = "#fff"
          ; stroke_width = 2.
          ; fill_opacity = None
          }
        |> Option.map ~f:(fun d -> layer [ "recharts-active-dot" ] [ d ]))
  in
  let cursor =
    match active_index, tooltip with
    | Some i, Some { cursor = true; _ } ->
      let x, y = Chart_path.polar ~cx ~cy ~radius:outer ~angle:(angle_of i) in
      svg
        "path"
        ~attrs:
          [ a "stroke" "#ccc"
          ; a "fill" "none"
          ; a "pointer-events" "none"
          ; cls [ "recharts-curve"; "recharts-tooltip-cursor" ]
          ; a "d" (sprintf "M%s,%sL%s,%s" (num cx) (num cy) (num x) (num y))
          ]
        []
    | _ -> Node.none
  in
  let raws = List.filter_map ctx.spec.elements ~f:(function Raw n -> Some n | _ -> None) in
  { svg_children =
      raws
      @ [ Option.value grid ~default:Node.none ]
      @ radar_nodes
      @ [ angle_axis_node; radius_axis_node; cursor ]
      @ active_dots
  ; active
  ; legend_payload =
      List.map radars ~f:(fun r ->
        { Item.data_key = r.r_data_key
        ; name = r.r_data_key
        ; value = Some (`Text r.r_data_key)
        ; color = color r
        ; row = []
        })
  ; plot
  ; nav_count = n
  }
;;

let radial_label (ll : label_list) ~text (vb : Polar_view_box.t) ~id =
  let radius = (vb.inner_radius +. vb.outer_radius) /. 2. in
  let delta = Chart_path.delta_angle vb.start_angle vb.end_angle in
  let sgn = if Float.(delta >= 0.) then 1. else -1. in
  let clockwise = false in
  let label_angle, direction =
    match ll.ll_position with
    | Some Inside_end -> vb.end_angle -. (sgn *. ll.ll_offset), not clockwise
    | Some End -> vb.end_angle +. (sgn *. ll.ll_offset), clockwise
    | _ -> vb.start_angle +. (sgn *. ll.ll_offset), clockwise
  in
  let direction = if Float.(delta <= 0.) then direction else not direction in
  let sx, sy = Chart_path.polar ~cx:vb.cx ~cy:vb.cy ~radius ~angle:label_angle in
  let ex, ey =
    Chart_path.polar
      ~cx:vb.cx
      ~cy:vb.cy
      ~radius
      ~angle:(label_angle +. ((if direction then 1. else -1.) *. 359.))
  in
  let path =
    sprintf
      "M%s,%s\n    A%s,%s,0,1,%s,\n    %s,%s"
      (num sx)
      (num sy)
      (num radius)
      (num radius)
      (if direction then "0" else "1")
      (num ex)
      (num ey)
  in
  svg
    "text"
    ~attrs:
      ((if Option.is_none ll.ll_stroke then [ a "stroke" "none" ] else [])
       @ [ a "fill" (Option.value ll.ll_fill ~default:"#808080") ]
       @ Option.value_map ll.ll_font_size ~default:[] ~f:(fun s -> [ af "font-size" s ])
       @ Option.value_map ll.ll_stroke ~default:[] ~f:(fun s -> [ a "stroke" s ])
       @ [ a "dominant-baseline" "central"
         ; cls ([ "recharts-radial-bar-label" ] @ Option.to_list ll.ll_class)
         ])
    [ svg "defs" [ svg "path" ~attrs:[ a "id" id; a "d" path ] [] ]
    ; svg "textPath" ~attrs:[ a "href" ("#" ^ id); a "xlink:href" ("#" ^ id) ] [ Node.text text ]
    ]
;;

let render_radial_bar (ctx : render_ctx) : rendered =
  let plot, max_radius, cx, cy, inner, outer = polar_frame ctx in
  ignore max_radius;
  let data = ctx.spec.data in
  let rows = Array.of_list data in
  let n = Array.length rows in
  let bars = List.filter_map ctx.spec.elements ~f:(function Radial_bar r -> Some r | _ -> None) in
  let radius_axis = List.find_map ctx.spec.elements ~f:(function Radius_axis x -> Some x | _ -> None) in
  let start_angle = ctx.spec.start_angle
  and end_angle = ctx.spec.end_angle in
  (* Stacks along the angle *)
  let groups =
    List.fold bars ~init:[] ~f:(fun acc b ->
      match b.rb_stack_id with
      | Some id when List.exists acc ~f:(fun g -> Option.equal String.equal g (Some id)) -> acc
      | sid -> acc @ [ sid ])
  in
  let stacked =
    List.concat_map groups ~f:(fun gid ->
      let members =
        match gid with
        | Some id -> List.filter bars ~f:(fun b -> Option.equal String.equal b.rb_stack_id (Some id))
        | None -> []
      in
      let keys = List.map members ~f:(fun b -> b.rb_data_key) in
      List.zip_exn keys (stack_values ~offset:None data keys))
  in
  let values_of (b : radial_bar) =
    match b.rb_stack_id with
    | Some _ -> List.Assoc.find_exn stacked ~equal:String.equal b.rb_data_key
    | None -> Array.map rows ~f:(fun r -> 0., Option.value (Row.num r b.rb_data_key) ~default:0.)
  in
  let all = List.map bars ~f:(fun b -> b, values_of b) in
  (* Recharts 3 takes the angle axis's domain from the bars' own values, not
     their stacked totals, and clips a stack at the end angle. *)
  let data_max =
    List.fold bars ~init:0. ~f:(fun acc b ->
      Array.fold rows ~init:acc ~f:(fun acc r ->
        Float.max acc (Option.value (Row.num r b.rb_data_key) ~default:0.)))
  in
  let ascale : Chart_scale.Linear.t = { domain = 0., data_max; range = start_angle, end_angle } in
  let ang v =
    let a = Chart_scale.Linear.apply ascale v in
    if not (Float.is_finite a && Float.is_finite start_angle && Float.is_finite end_angle)
    then start_angle
    else if Float.(end_angle >= start_angle)
    then Float.clamp_exn a ~min:start_angle ~max:end_angle
    else Float.clamp_exn a ~min:end_angle ~max:start_angle
  in
  let band = Chart_scale.Band.create ~n ~range:(inner, outer) () in
  let bandwidth = Chart_scale.Band.bandwidth band in
  let group_index (b : radial_bar) =
    match b.rb_stack_id with
    | Some id -> List.findi groups ~f:(fun _ g -> Option.equal String.equal g (Some id)) |> Option.map ~f:fst
    | None ->
      List.findi groups ~f:(fun _ g -> Option.is_none g)
      |> Option.map ~f:(fun _ ->
        let before = List.take_while bars ~f:(fun b' -> not (phys_equal b' b)) in
        let gs =
          List.fold before ~init:[] ~f:(fun acc b' ->
            match b'.rb_stack_id with
            | Some id when List.mem acc (Some id) ~equal:(Option.equal String.equal) -> acc
            | sid -> sid :: acc)
        in
        List.length gs)
  in
  let positions =
    bar_positions
      ~bar_gap:ctx.spec.bar_gap
      ~bar_category_gap:ctx.spec.bar_category_gap
      ~band_size:bandwidth
      (List.map groups ~f:(fun _ -> None))
  in
  let tooltip = find_tooltip ctx.spec in
  let active_index =
    match tooltip, pointer_polar ctx ~cx ~cy with
    | Some _, Some (r, _) when Float.(r >= inner && r <= outer) ->
      Chart_scale.Band.index_at band r
    | Some t, None ->
      (match ctx.chart.state.key_index with Some k -> Some k | None -> t.default_index)
    | _ -> None
  in
  let active_index = Option.filter active_index ~f:(fun i -> i >= 0 && i < n) in
  let sectors_of (b : radial_bar) vals =
    match Option.bind (group_index b) ~f:(List.nth positions) with
    | None -> []
    | Some (offset, size) ->
      Array.to_list vals
      |> List.mapi ~f:(fun i (base, top) ->
        let r0 = Chart_scale.Band.apply band i +. offset in
        let fill =
          match Row.text rows.(i) "fill" with
          | Some f -> f
          | None -> Option.value b.rb_fill ~default:"#808080"
        in
        ({ cx
         ; cy
         ; inner_radius = r0
         ; outer_radius = r0 +. size
         ; start_angle = ang base
         ; end_angle = ang top
         ; corner_radius = b.rb_corner_radius
         ; fill
         ; stroke = None
         ; index = i
         ; row = rows.(i)
         }
         : Sector_props.t))
  in
  let nodes =
    List.mapi all ~f:(fun bi (b, vals) ->
      let sectors = sectors_of b vals in
      layer
        ("recharts-area" :: Option.to_list b.rb_class)
        ([ (if b.rb_background
            then
              layer
                [ "recharts-radial-bar-background" ]
                (List.map sectors ~f:(fun s ->
                   sector
                     ~class_:"recharts-radial-bar-background-sector"
                     { s with start_angle; end_angle; fill = "#eee"; corner_radius = b.rb_corner_radius }))
            else Node.none)
         ; layer
             [ "recharts-radial-bar-sectors" ]
             (List.map sectors ~f:(fun s -> sector ~class_:"recharts-radial-bar-sector" s))
         ]
         @ List.mapi (label_lists b.rb_children) ~f:(fun li ll ->
           layer
             [ "recharts-label-list" ]
             (List.map sectors ~f:(fun (s : Sector_props.t) ->
                let text =
                  Option.value
                    (Row.text s.row (Option.value ll.ll_data_key ~default:b.rb_data_key))
                    ~default:""
                in
                let text = match ll.ll_formatter with Some f -> f text | None -> text in
                let vb : Polar_view_box.t =
                  { cx
                  ; cy
                  ; inner_radius = s.inner_radius
                  ; outer_radius = s.outer_radius
                  ; start_angle = s.start_angle
                  ; end_angle = s.end_angle
                  }
                in
                match ll.ll_position with
                | Some (Inside_start | Inside_end | End) ->
                  radial_label
                    ll
                    ~text
                    vb
                    ~id:(sprintf "%s-radial-%d-%d-%d" ctx.chart.id bi li s.index)
                | _ ->
                  let mid = (s.start_angle +. s.end_angle) /. 2. in
                  let x, y =
                    Chart_path.polar ~cx ~cy ~radius:((s.inner_radius +. s.outer_radius) /. 2.) ~angle:mid
                  in
                  text_node
                    ~classes:(text_classes @ Option.to_list ll.ll_class)
                    ~x
                    ~y
                    ~anchor:`Middle
                    ~vertical:`Middle
                    text)))))
  in
  let grid =
    List.find_map ctx.spec.elements ~f:(function
      | Polar_grid pg ->
        Some
          (render_polar_grid
             pg
             ~cx
             ~cy
             ~inner
             ~outer
             ~angles:[]
             ~radii:(List.init n ~f:(fun i -> Chart_scale.Band.apply band i +. (bandwidth /. 2.))))
      | _ -> None)
  in
  let vb : Polar_view_box.t =
    { cx; cy; inner_radius = inner; outer_radius = outer; start_angle; end_angle }
  in
  let radius_axis_node =
    match radius_axis with
    | None -> Node.none
    | Some ra ->
      let categories = category_values data ra.ra_data_key in
      render_radius_axis
        ra
        ~cx
        ~cy
        ~scale:(fun i -> Chart_scale.Band.apply band (Float.to_int i) +. (bandwidth /. 2.))
        ~ticks:(List.mapi categories ~f:(fun i c -> Float.of_int i, c))
        ~vb
  in
  let active =
    Option.map active_index ~f:(fun i ->
      { index = i
      ; coordinate =
          (match ctx.chart.state.pointer with
           | Some p -> p
           | None -> Chart_path.polar ~cx ~cy ~radius:(Chart_scale.Band.apply band i) ~angle:start_angle)
      ; tooltip_label = Option.bind radius_axis ~f:(fun ra -> Option.bind ra.ra_data_key ~f:(Row.text rows.(i)))
      ; payload =
          List.map all ~f:(fun (b, _) ->
            let fill =
              match Row.text rows.(i) "fill" with
              | Some f -> f
              | None -> Option.value b.rb_fill ~default:"#808080"
            in
            { Item.data_key = b.rb_data_key
            ; name = b.rb_data_key
            ; value = Row.find rows.(i) b.rb_data_key
            ; color = fill
            ; row = rows.(i)
            })
      })
  in
  let raws = List.filter_map ctx.spec.elements ~f:(function Raw n -> Some n | _ -> None) in
  { svg_children = raws @ [ Option.value grid ~default:Node.none ] @ nodes @ [ radius_axis_node ]
  ; active
  ; legend_payload =
      Array.to_list rows
      |> List.mapi ~f:(fun i row ->
        let b = List.hd bars in
        let key = Option.value_map b ~default:"value" ~f:(fun b -> b.rb_data_key) in
        { Item.data_key = key
        ; name = Int.to_string i
        ; value = Row.find row key
        ; color = Option.value (Row.text row "fill") ~default:"#808080"
        ; row
        })
  ; plot
  ; nav_count = n
  }
;;

let container_classes =
  "flex aspect-video justify-center text-xs \
   [&_.recharts-cartesian-axis-tick_text]:fill-muted-foreground \
   [&_.recharts-cartesian-grid_line[stroke='#ccc']]:stroke-border/50 \
   [&_.recharts-curve.recharts-tooltip-cursor]:stroke-border \
   [&_.recharts-dot[stroke='#fff']]:stroke-transparent \
   [&_.recharts-layer]:outline-hidden \
   [&_.recharts-polar-grid_[stroke='#ccc']]:stroke-border \
   [&_.recharts-radial-bar-background-sector]:fill-muted \
   [&_.recharts-rectangle.recharts-tooltip-cursor]:fill-muted \
   [&_.recharts-reference-line_[stroke='#ccc']]:stroke-border \
   [&_.recharts-sector]:outline-hidden \
   [&_.recharts-sector[stroke='#fff']]:stroke-transparent \
   [&_.recharts-surface]:outline-hidden"
;;

let px f = sprintf "%spx" (num f)

let container ?(attrs = []) ?class_ ?id ~config (chart : t) (spec : Spec.t) =
  let chart_id = "chart-" ^ Option.value id ~default:chart.id in
  let width, height = Option.value chart.state.size ~default:(320., 200.) in
  let ctx = { spec; width; height; config; chart } in
  let rendered =
    match spec.kind with
    | Area | Bar | Line -> render_cartesian ctx
    | Pie -> render_pie ctx
    | Radar -> render_radar ctx
    | Radial_bar -> render_radial_bar ctx
  in
  let tooltip = find_tooltip spec in
  let tooltip_node =
    match tooltip, rendered.active with
    | Some tip, Some act ->
      let content = Option.value tip.content ~default:(tooltip_content ()) in
      let tw, th = Option.value chart.state.tooltip ~default:(128., 60.) in
      let plot = rendered.plot in
      let place coord size lo extent =
        let positive = coord +. 10. in
        if Float.(positive +. size > lo +. extent)
        then Float.max (coord -. size -. 10.) lo
        else positive
      in
      let x = place (fst act.coordinate) tw plot.x plot.width in
      let y = place (snd act.coordinate) th plot.y plot.height in
      Node.div
        ~attrs:
          [ Attr.tabindex (-1)
          ; Attr.class_ "recharts-tooltip-wrapper"
          ; measure "loon-chart-tooltip" (fun s -> chart.inject (Tooltip_size s))
          ; Attr.style
              (Css_gen.concat
                 [ Css_gen.create ~field:"visibility" ~value:"visible"
                 ; Css_gen.create ~field:"pointer-events" ~value:"none"
                 ; Css_gen.create ~field:"position" ~value:"absolute"
                 ; Css_gen.create ~field:"top" ~value:"0px"
                 ; Css_gen.create ~field:"left" ~value:"0px"
                 ; Css_gen.create ~field:"transition" ~value:"transform 400ms ease"
                 ; Css_gen.create
                     ~field:"transform"
                     ~value:(sprintf "translate(%s, %s)" (px x) (px y))
                 ])
          ]
        [ content { config; label = act.tooltip_label; payload = act.payload } ]
    | _ -> Node.none
  in
  let legend_node =
    match find_legend spec with
    | None -> Node.none
    | Some l ->
      let content = Option.value l.l_content ~default:(legend_content ()) in
      let m = spec.margin in
      Node.div
        ~attrs:
          [ Attr.class_ "recharts-legend-wrapper"
          ; measure "loon-chart-legend" (fun s -> chart.inject (Legend_size s))
          ; Attr.style
              (Css_gen.concat
                 [ Css_gen.create ~field:"position" ~value:"absolute"
                 ; Css_gen.create ~field:"width" ~value:(px (width -. m.left -. m.right))
                 ; Css_gen.create ~field:"height" ~value:"auto"
                 ; Css_gen.create ~field:"left" ~value:(px m.left)
                 ; (match l.vertical_align with
                    | `Bottom -> Css_gen.create ~field:"bottom" ~value:(px m.bottom)
                    | `Top -> Css_gen.create ~field:"top" ~value:(px m.top))
                 ])
          ]
        [ content
            { config
            ; payload = rendered.legend_payload
            ; vertical_align = l.vertical_align
            ; class_ = l.legend_class
            }
        ]
  in
  let on_key ev =
    let count = rendered.nav_count in
    if Option.is_none tooltip || count = 0
    then Effect.Ignore
    else (
      let current = Option.map rendered.active ~f:(fun a -> a.index) in
      let move d =
        let next =
          match current with
          | None -> if d > 0 then 0 else count - 1
          | Some i -> Int.clamp_exn (i + d) ~min:0 ~max:(count - 1)
        in
        Effect.Many [ Dom_util.prevent_default ev; chart.inject (Key_index (Some next)) ]
      in
      match Dom_util.key ev with
      | ArrowRight -> move 1
      | ArrowLeft -> move (-1)
      | Escape -> chart.inject (Key_index None)
      | _ -> Effect.Ignore)
  in
  Node.div
    ~attrs:
      ([ data "slot" "chart"
       ; data "chart" chart_id
       ; (Cn.with_class ?class_ [ container_classes ])
       ]
       @ attrs)
    [ style ~id:chart_id config
    ; Node.div
        ~attrs:
          [ Attr.class_ "recharts-responsive-container"
          ; measure "loon-chart-size" (fun s -> chart.inject (Resize s))
          ; Attr.style
              (Css_gen.concat
                 [ Css_gen.create ~field:"width" ~value:"100%"
                 ; Css_gen.create ~field:"height" ~value:"100%"
                 ; Css_gen.create ~field:"min-width" ~value:"0"
                 ])
          ]
        [ (* Recharts 3: a zero-size box, so that the chart does not size its
             container. *)
          Node.div
            ~attrs:
              [ Attr.style
                  (Css_gen.concat
                     [ Css_gen.create ~field:"width" ~value:"0"
                     ; Css_gen.create ~field:"height" ~value:"0"
                     ; Css_gen.create ~field:"overflow" ~value:"visible"
                     ])
              ]
            [ Node.div
            ~attrs:
              [ Attr.classes [ "recharts-wrapper"; Kind.class_name spec.kind ]
              ; Attr.style
                  (Css_gen.concat
                     [ Css_gen.create ~field:"position" ~value:"relative"
                     ; Css_gen.create ~field:"cursor" ~value:"default"
                     ; Css_gen.create ~field:"width" ~value:(px width)
                     ; Css_gen.create ~field:"height" ~value:(px height)
                     ])
              ; Attr.on_mousemove (fun ev -> chart.inject (Pointer (Some (local_point ev))))
              ; Attr.on_mouseleave (fun _ -> chart.inject (Pointer None))
              ]
            [ svg
                "svg"
                ~attrs:
                  [ a "role" "application"
                  ; Attr.tabindex 0
                  ; Attr.class_ "recharts-surface"
                  ; af "width" width
                  ; af "height" height
                  ; a "viewBox" (sprintf "0 0 %s %s" (num width) (num height))
                  ; Attr.style
                      (Css_gen.concat
                         [ Css_gen.create ~field:"width" ~value:"100%"
                         ; Css_gen.create ~field:"height" ~value:"100%"
                         ; Css_gen.create ~field:"display" ~value:"block"
                         ])
                  ; Attr.on_keydown on_key
                  ; Attr.on_blur (fun _ -> chart.inject (Key_index None))
                  ]
                rendered.svg_children
            ; legend_node
            ; tooltip_node
            ]
          ]
        ]
    ]
;;
module For_testing = struct
  let measure_type_id = Measure.For_testing.type_id
end
