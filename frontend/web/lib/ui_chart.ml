open! Core
open! Import
module C = Ui_spec.Chart

let pct f = Ui_svg.num f ^ "%"
let style = Ui_style.attr

let div ?(attrs = []) cls children =
  Node.div ~attrs:(Attr.class_ cls :: attrs) children
;;

let span ?(attrs = []) cls children =
  Node.span ~attrs:(Attr.class_ cls :: attrs) children
;;

let text = Node.text
let series_class k = sprintf "s%d" ((k mod 8) + 1)
let hidden = Attr.create "aria-hidden" "true"
let finite = List.filter ~f:Float.is_finite

let axis ?(zero = false) ?(count = 5) values =
  let values = finite values in
  let values = if zero then 0. :: values else values in
  match
    ( List.min_elt values ~compare:Float.compare
    , List.max_elt values ~compare:Float.compare )
  with
  | Some lo, Some hi -> Ui_format.ticks ~count lo hi
  | _ -> Ui_format.ticks ~count 0. 1.
;;

let position ~lo ~hi v =
  if Float.(hi -. lo <= 0.) then 0. else (v -. lo) /. (hi -. lo) *. 100.
;;

let currency_symbols = [ "$"; "€"; "£"; "¥"; "₹"; "₩" ]
let is_currency u = List.mem currency_symbols u ~equal:String.equal

let with_unit ~unit s =
  match unit with
  | None | Some "" -> s
  | Some "%" -> s ^ "%"
  | Some u when is_currency u ->
    (match
       String.chop_prefix s ~prefix:"-", String.chop_prefix s ~prefix:"−"
     with
     | Some s, _ -> "-" ^ u ^ s
     | None, Some s -> "−" ^ u ^ s
     | None, None -> u ^ s)
  | Some u -> s ^ " " ^ u
;;

(* Units short enough for every tick; the others are named once, by the
   axis. *)
let tick_unit = function
  | Some u when String.equal u "%" || is_currency u -> Some u
  | _ -> None
;;

let value_text ~unit v = with_unit ~unit (Ui_format.number v)
let tick_text ~unit v = with_unit ~unit:(tick_unit unit) (Ui_format.compact v)

let slice_path ~hole ~start ~stop =
  let r = 48. in
  let sweep = Float.min (stop -. start) 0.99999 in
  let stop = start +. sweep in
  let at radius f =
    let a = f *. 2. *. Float.pi in
    50. +. (radius *. Float.sin a), 50. -. (radius *. Float.cos a)
  in
  let p (x, y) = sprintf "%s %s" (Ui_svg.num x) (Ui_svg.num y) in
  let large = if Float.(sweep > 0.5) then 1 else 0 in
  let outer =
    sprintf
      "M %s A %s %s 0 %d 1 %s"
      (p (at r start))
      (Ui_svg.num r)
      (Ui_svg.num r)
      large
      (p (at r stop))
  in
  if Float.(hole <= 0.)
  then outer ^ " L 50 50 Z"
  else
    sprintf
      "%s L %s A %s %s 0 %d 0 %s Z"
      outer
      (p (at hole stop))
      (Ui_svg.num hole)
      (Ui_svg.num hole)
      large
      (p (at hole start))
;;

let kind_name (kind : C.Kind.t) =
  let s = String.tr ~target:'_' ~replacement:' ' (C.Kind.to_string kind) in
  String.capitalize s ^ " chart"
;;

let tick_count : C.Height.t -> int = function
  | Small -> 3
  | Medium -> 5
  | Large -> 7
;;

let height_class : C.Height.t -> string = function
  | Small -> "h-small"
  | Medium -> "h-medium"
  | Large -> "h-large"
;;

(* ---- pieces every kind has *)

let figure (chart : C.t) ?(cls = "") children =
  let label =
    match chart.title with
    | Some t -> sprintf "%s: %s" (kind_name chart.kind) t
    | None -> kind_name chart.kind
  in
  Node.create
    "figure"
    ~attrs:
      [ Attr.class_
          (String.concat
             ~sep:" "
             (List.filter
                [ "ui-chart"; height_class chart.height; cls ]
                ~f:(Fn.non String.is_empty)))
      ; Attr.create "data-kind" (C.Kind.to_string chart.kind)
      ; Attr.role "figure"
      ; Attr.create "aria-label" label
      ]
    ([ Option.value_map chart.title ~default:Node.none ~f:(fun t ->
         div "ui-title" [ text t ])
     ; Option.value_map chart.subtitle ~default:Node.none ~f:(fun t ->
         div "ui-chart-sub" [ text t ])
     ]
     @ children)
;;

let axis_name ~cls name ~unit =
  let unit = if Option.is_some (tick_unit unit) then None else unit in
  match name, unit with
  | None, None -> Node.none
  | Some n, None -> div ~attrs:[ hidden ] cls [ text n ]
  | None, Some u -> div ~attrs:[ hidden ] cls [ text u ]
  | Some n, Some u -> div ~attrs:[ hidden ] cls [ text (sprintf "%s (%s)" n u) ]
;;

let empty chart =
  figure chart [ div "ui-chart-empty" [ text "No data to chart" ] ]
;;

let legend items =
  Node.ul
    ~attrs:[ Attr.class_ "ui-chart-legend"; hidden ]
    (List.map items ~f:(fun (cls, name, extra) ->
       Node.li
         ~attrs:[ Attr.class_ cls ]
         ([ span "ui-chart-swatch" []; span "ui-chart-name" [ text name ] ]
          @ extra)))
;;

let data_table ~caption ~head rows =
  let cells tag = List.map ~f:(fun c -> Node.create tag [ text c ]) in
  Node.create
    "table"
    ~attrs:[ Attr.class_ "sr-only" ]
    [ Node.create "caption" [ text caption ]
    ; Node.create "thead" [ Node.create "tr" (cells "th" head) ]
    ; Node.create
        "tbody"
        (List.map rows ~f:(fun row -> Node.create "tr" (cells "td" row)))
    ]
;;

let tip_row ?(cls = "") name value =
  div
    (String.strip ("ui-chart-tip-row " ^ cls))
    [ span "ui-chart-swatch" []
    ; span "ui-chart-name" [ text name ]
    ; span "ui-chart-value" [ text value ]
    ]
;;

let tip ?(attrs = []) cls ~head rows =
  div
    ~attrs:(hidden :: attrs)
    ("ui-chart-tip " ^ cls)
    (div "ui-chart-tip-head" [ text head ] :: rows)
;;

let label_width labels =
  List.fold labels ~init:1 ~f:(fun acc l -> Int.max acc (String.length l))
;;

(* Along the foot, every other label hides only where they would collide:
   [thin-B] hides them while the axis is narrower than B pixels (a container
   query), B being the first of [breakpoints] at least the width [gaps]
   gaps of the [longest] label take (11px monospace, 7px a character, and
   room between). *)
let breakpoints = [ 160; 200; 240; 280; 320; 360; 400; 480; 560; 640; 720; 800 ]

let thin_class ~gaps ~longest =
  let needed = gaps * ((longest * 7) + 8) in
  match List.find breakpoints ~f:(fun b -> b >= needed) with
  | Some b -> sprintf " thin-%d" b
  | None -> " thin"
;;

let ticks_nodes ~cls ~side ticks ~lo ~hi ~unit =
  let cls =
    if String.equal side "left"
    then
      cls
      ^ thin_class
          ~gaps:(List.length ticks - 1)
          ~longest:(label_width (List.map ticks ~f:(tick_text ~unit)))
    else cls
  in
  div
    ~attrs:[ hidden ]
    cls
    (List.mapi ticks ~f:(fun i v ->
       let minor =
         String.equal side "left" && List.length ticks > 4 && i % 2 = 1
       in
       span
         ~attrs:[ style [ side, pct (position ~lo ~hi v) ] ]
         (if minor then "ui-chart-tick minor" else "ui-chart-tick")
         [ text (tick_text ~unit v) ]))
;;

let grid ~side ticks ~lo ~hi =
  div
    ~attrs:[ hidden ]
    "ui-chart-grid"
    (List.map ticks ~f:(fun v ->
       span
         ~attrs:[ style [ side, pct (position ~lo ~hi v) ] ]
         (if Float.(v = 0.)
          then "ui-chart-gridline zero"
          else "ui-chart-gridline")
         []))
;;

(* ---- line, area and the bars: one value per label and series *)

let segments points =
  List.group points ~break:(fun a b ->
    Bool.( <> ) (Option.is_some a) (Option.is_some b))
  |> List.filter_map ~f:(fun group ->
    match Option.all group with
    | Some (_ :: _ as pts) -> Some pts
    | Some [] | None -> None)
;;

let categories (chart : C.t) ~labels ~(series : C.Series.t list) =
  let n = List.length labels in
  let labels = Array.of_list labels in
  let values =
    Array.of_list_map series ~f:(fun (s : C.Series.t) ->
      let v = Array.of_list s.values in
      Array.init n ~f:(fun i ->
        if i < Array.length v
        then Option.filter v.(i) ~f:Float.is_finite
        else None))
  in
  let names = Array.of_list_map series ~f:(fun (s : C.Series.t) -> s.name) in
  let at i = Array.to_list (Array.map values ~f:(fun v -> v.(i))) in
  let unit = chart.unit in
  let stacked = C.Kind.equal chart.kind Stacked_bar in
  let horizontal = C.Kind.equal chart.kind Horizontal_bar in
  let bars =
    match chart.kind with
    | Bar | Stacked_bar | Horizontal_bar -> true
    | Line | Area | Pie | Donut | Scatter -> false
  in
  let all_values =
    if stacked
    then
      List.concat
        (List.init n ~f:(fun i ->
           let vs = List.filter_opt (at i) in
           [ List.sum
               (module Float)
               (List.filter vs ~f:Float.is_positive)
               ~f:Fn.id
           ; List.sum
               (module Float)
               (List.filter vs ~f:Float.is_negative)
               ~f:Fn.id
           ]))
    else
      List.concat_map (Array.to_list values) ~f:(fun v ->
        List.filter_opt (Array.to_list v))
  in
  if
    n = 0
    || Array.is_empty values
    || List.is_empty
         (List.filter_opt (List.concat_map (List.init n ~f:Fn.id) ~f:at))
  then empty chart
  else (
    let ticks =
      axis
        ~zero:(bars || C.Kind.equal chart.kind Area)
        ~count:(tick_count chart.height)
        all_values
    in
    let lo = List.hd_exn ticks in
    let hi = List.last_exn ticks in
    let pos v = position ~lo ~hi v in
    let zero = pos (Float.clamp_exn 0. ~min:lo ~max:hi) in
    let band = 100. /. Float.of_int n in
    let start i = Float.of_int i *. band in
    let centre i = start i +. (band /. 2.) in
    let bar ?(cls = "") k a b =
      let a, b = Float.min a b, Float.max a b in
      let place =
        if horizontal
        then [ "left", pct a; "width", pct (b -. a) ]
        else [ "bottom", pct a; "height", pct (b -. a) ]
      in
      span
        ~attrs:[ style place ]
        (String.strip (sprintf "ui-chart-bar %s %s" (series_class k) cls))
        []
    in
    let slot children = span "ui-chart-slot" children in
    let marks i =
      let vs = at i in
      if stacked
      then (
        let pos_ends =
          List.filter_mapi vs ~f:(fun k v ->
            Option.bind v ~f:(fun v -> Option.some_if Float.(v > 0.) k))
          |> List.last
        in
        let neg_ends =
          List.filter_mapi vs ~f:(fun k v ->
            Option.bind v ~f:(fun v -> Option.some_if Float.(v < 0.) k))
          |> List.last
        in
        let _, _, segs =
          List.foldi vs ~init:(0., 0., []) ~f:(fun k (up, down, acc) v ->
            match v with
            | None -> up, down, acc
            | Some v when Float.(v >= 0.) ->
              let cls =
                if Option.equal Int.equal pos_ends (Some k) then "end" else ""
              in
              up +. v, down, bar ~cls k (pos up) (pos (up +. v)) :: acc
            | Some v ->
              let cls =
                if Option.equal Int.equal neg_ends (Some k)
                then "neg end"
                else "neg"
              in
              up, down +. v, bar ~cls k (pos down) (pos (down +. v)) :: acc)
        in
        [ span "ui-chart-group" [ slot (List.rev segs) ] ])
      else if bars
      then
        [ span
            "ui-chart-group"
            (List.mapi vs ~f:(fun k v ->
               slot
                 (match v with
                  | None -> []
                  | Some v ->
                    [ bar
                        ~cls:(if Float.(v < 0.) then "neg end" else "end")
                        k
                        zero
                        (pos v)
                    ])))
        ]
      else (
        let lone k =
          let near j = j >= 0 && j < n && Option.is_some values.(k).(j) in
          not (near (i - 1) || near (i + 1))
        in
        span "ui-chart-guide" []
        :: List.filter_mapi vs ~f:(fun k v ->
          Option.map v ~f:(fun v ->
            span
              ~attrs:[ style [ "bottom", pct (pos v) ] ]
              (sprintf
                 "ui-chart-dot %s%s"
                 (series_class k)
                 (if lone k then " lone" else ""))
              [])))
    in
    let summary i =
      let parts =
        List.mapi (at i) ~f:(fun k v ->
          let v =
            Option.value_map v ~default:"no value" ~f:(value_text ~unit)
          in
          if Array.length names = 1 then v else sprintf "%s %s" names.(k) v)
      in
      sprintf "%s: %s" labels.(i) (String.concat ~sep:", " parts)
    in
    let tip_rows i =
      let vs = at i in
      List.mapi vs ~f:(fun k v ->
        tip_row
          ~cls:(series_class k)
          names.(k)
          (Option.value_map v ~default:"—" ~f:(value_text ~unit)))
      @
      if stacked && List.length vs > 1
      then
        [ tip_row
            ~cls:"total"
            "Total"
            (value_text
               ~unit
               (List.sum (module Float) (List.filter_opt vs) ~f:Fn.id))
        ]
      else []
    in
    let columns =
      List.concat_map (List.init n ~f:Fn.id) ~f:(fun i ->
        let place, tip_place, tip_cls =
          if horizontal
          then (
            let below = Float.(centre i < 50.) in
            ( [ "top", pct (start i); "height", pct band ]
            , [ "--y", pct (if below then start i +. band else start i) ]
            , if below then "below" else "above" ))
          else (
            let right = Float.(centre i < 50.) in
            ( [ "left", pct (start i); "width", pct band ]
            , [ "--x", pct (centre i) ]
            , if right then "right" else "left" ))
        in
        [ div
            ~attrs:
              [ style place
              ; Attr.tabindex 0
              ; Attr.role "img"
              ; Attr.create "aria-label" (summary i)
              ]
            "ui-chart-col"
            (marks i)
        ; tip ~attrs:[ style tip_place ] tip_cls ~head:labels.(i) (tip_rows i)
        ])
    in
    let lines =
      if bars
      then Node.none
      else (
        let pts k =
          List.init n ~f:(fun i ->
            Option.map values.(k).(i) ~f:(fun v -> centre i, 100. -. pos v))
        in
        let base = 100. -. zero in
        let areas =
          if C.Kind.equal chart.kind Area
          then (
            (* The highest series' fill at the back: each fill that
               overlaps another is over it, keeping its own tint. *)
            let mean k =
              let vs = Array.filter_map values.(k) ~f:Fn.id in
              if Array.is_empty vs
              then 0.
              else
                Array.sum (module Float) vs ~f:Fn.id
                /. Float.of_int (Array.length vs)
            in
            List.concat_map
              (List.stable_sort
                 (List.init (Array.length values) ~f:Fn.id)
                 ~compare:(fun a b -> Float.compare (mean b) (mean a)))
              ~f:(fun k ->
                List.map
                  (segments (pts k))
                  ~f:(fun seg ->
                    let x0, _ = List.hd_exn seg in
                    let x1, _ = List.last_exn seg in
                    Ui_svg.el
                      "path"
                      [ "class", "ui-chart-fill " ^ series_class k
                      ; ( "d"
                        , sprintf
                            "M %s %s L %s L %s %s Z"
                            (Ui_svg.num x0)
                            (Ui_svg.num base)
                            (String.drop_prefix
                               (Ui_svg.path_of_points
                                  (List.map seg ~f:Option.some))
                               1)
                            (Ui_svg.num x1)
                            (Ui_svg.num base) )
                      ]
                      [])))
          else []
        in
        let strokes =
          List.init (Array.length values) ~f:(fun k ->
            Ui_svg.el
              "path"
              [ "class", "ui-chart-line " ^ series_class k
              ; "d", Ui_svg.path_of_points (pts k)
              ]
              [])
        in
        Ui_svg.svg ~cls:"ui-chart-svg" ~width:100. ~height:100. (areas @ strokes))
    in
    let plot =
      div
        "ui-chart-plot"
        ([ grid ~side:(if horizontal then "left" else "bottom") ticks ~lo ~hi
         ; lines
         ]
         @ columns)
    in
    let value_axis =
      ticks_nodes
        ~cls:
          (if horizontal
           then "ui-chart-xaxis ui-chart-values"
           else "ui-chart-yaxis")
        ~side:(if horizontal then "left" else "bottom")
        ticks
        ~lo
        ~hi
        ~unit
    in
    let label_axis =
      if horizontal
      then
        div
          ~attrs:[ hidden ]
          "ui-chart-yaxis ui-chart-cats"
          (List.init n ~f:(fun i ->
             span
               ~attrs:[ style [ "top", pct (centre i) ] ]
               "ui-chart-tick"
               [ text labels.(i) ]))
      else (
        let stride = Int.max 1 ((n + 7) / 8) in
        let shown =
          List.filter (List.init n ~f:Fn.id) ~f:(fun i -> i mod stride = 0)
        in
        div
          ~attrs:[ hidden ]
          ("ui-chart-xaxis"
           ^ thin_class
               ~gaps:(List.length shown)
               ~longest:(label_width (List.map shown ~f:(fun i -> labels.(i))))
          )
          (List.filter_map (List.init n ~f:Fn.id) ~f:(fun i ->
             Option.some_if
               (i mod stride = 0)
               (span
                  ~attrs:
                    [ style
                        [ "left", pct (centre i)
                        ; "--w", pct (Float.of_int stride *. band)
                        ]
                    ]
                  (if i / stride mod 2 = 1
                   then "ui-chart-tick minor"
                   else "ui-chart-tick")
                  [ text labels.(i) ]))))
    in
    let gutter =
      if horizontal
      then Int.min 16 (label_width (Array.to_list labels)) + 1
      else label_width (List.map ticks ~f:(tick_text ~unit)) + 1
    in
    let y_name, x_name =
      if horizontal
      then
        ( axis_name ~cls:"ui-chart-yname" chart.x_label ~unit:None
        , axis_name ~cls:"ui-chart-xname" chart.y_label ~unit )
      else
        ( axis_name ~cls:"ui-chart-yname" chart.y_label ~unit
        , axis_name ~cls:"ui-chart-xname" chart.x_label ~unit:None )
    in
    figure
      chart
      ~cls:
        (if C.Kind.equal chart.kind Area || ((not bars) && n > 40)
         then "dense"
         else "")
      [ y_name
      ; div
          ~attrs:
            [ style
                [ "--gutter", sprintf "%dch" gutter; "--rows", Int.to_string n ]
            ]
          "ui-chart-frame"
          (if horizontal
           then [ label_axis; plot; value_axis ]
           else [ value_axis; plot; label_axis ])
      ; x_name
      ; (if Array.length names > 1
         then
           legend
             (List.mapi (Array.to_list names) ~f:(fun k name ->
                series_class k, name, []))
         else Node.none)
      ; data_table
          ~caption:(Option.value chart.title ~default:(kind_name chart.kind))
          ~head:(Option.value chart.x_label ~default:"" :: Array.to_list names)
          (List.init n ~f:(fun i ->
             labels.(i)
             :: List.map
                  (at i)
                  ~f:(Option.value_map ~default:"" ~f:(value_text ~unit))))
      ])
;;

(* ---- pie and donut *)

let pie (chart : C.t) ~(slices : C.Slice.t list) ~hole =
  let slices =
    List.map slices ~f:(fun s ->
      { s with
        value = (if Float.is_finite s.value then Float.max 0. s.value else 0.)
      })
  in
  let total = List.sum (module Float) slices ~f:(fun s -> s.value) in
  if Float.(total <= 0.)
  then empty chart
  else (
    let unit = chart.unit in
    let cls k =
      sprintf
        "%s n%d%s"
        (series_class k)
        (k + 1)
        (if k >= 8 then " alt" else "")
    in
    let share v = Ui_format.percent ~decimals:1 (v /. total) in
    let _, paths =
      List.foldi slices ~init:(0., []) ~f:(fun k (from, acc) (s : C.Slice.t) ->
        let upto = from +. (s.value /. total) in
        ( upto
        , if Float.(s.value <= 0.)
          then acc
          else
            Ui_svg.el
              "g"
              [ "class", "ui-chart-slice " ^ cls k
              ; "tabindex", "0"
              ; "role", "img"
              ; ( "aria-label"
                , sprintf
                    "%s: %s (%s)"
                    s.label
                    (value_text ~unit s.value)
                    (share s.value) )
              ]
              [ Ui_svg.el
                  "path"
                  [ "d", slice_path ~hole ~start:from ~stop:upto ]
                  []
              ]
            :: acc ))
    in
    let centre =
      div
        ~attrs:[ hidden ]
        "ui-chart-centre"
        ((if Float.(hole > 0.)
          then
            [ div
                "ui-chart-total"
                [ span "ui-chart-value" [ text (value_text ~unit total) ]
                ; span "ui-chart-name" [ text "Total" ]
                ]
            ]
          else [])
         @ List.mapi slices ~f:(fun k (s : C.Slice.t) ->
           div
             ("ui-chart-centre-item " ^ cls k)
             [ span "ui-chart-name" [ text s.label ]
             ; span "ui-chart-value" [ text (value_text ~unit s.value) ]
             ; span "ui-chart-share" [ text (share s.value) ]
             ]))
    in
    figure
      chart
      [ div
          "ui-chart-round"
          [ div
              "ui-chart-disc"
              [ Ui_svg.el
                  "svg"
                  [ "class", "ui-chart-svg"
                  ; "viewBox", "0 0 100 100"
                  ; "role", "group"
                  ]
                  (List.rev paths)
              ; centre
              ]
          ; legend
              (List.mapi slices ~f:(fun k (s : C.Slice.t) ->
                 ( cls k
                 , s.label
                 , [ span "ui-chart-value" [ text (value_text ~unit s.value) ]
                   ; span "ui-chart-share" [ text (share s.value) ]
                   ] )))
          ]
      ; data_table
          ~caption:(Option.value chart.title ~default:(kind_name chart.kind))
          ~head:[ ""; "Value"; "Share" ]
          (List.map slices ~f:(fun s ->
             [ s.label; value_text ~unit s.value; share s.value ]))
      ])
;;

(* ---- scatter *)

let focusable_points = 300

let scatter (chart : C.t) ~(sets : C.Points.t list) =
  let sets =
    List.map sets ~f:(fun (s : C.Points.t) ->
      { s with
        points =
          List.filter s.points ~f:(fun (x, y) ->
            Float.is_finite x && Float.is_finite y)
      })
  in
  let all = List.concat_map sets ~f:(fun s -> s.points) in
  if List.is_empty all
  then empty chart
  else (
    let count = tick_count chart.height in
    let xticks = axis ~count (List.map all ~f:fst) in
    let yticks = axis ~count (List.map all ~f:snd) in
    let xlo, xhi = List.hd_exn xticks, List.last_exn xticks in
    let ylo, yhi = List.hd_exn yticks, List.last_exn yticks in
    let focusable = List.length all <= focusable_points in
    let named name v =
      match name with
      | Some n -> sprintf "%s %s" n v
      | None -> v
    in
    let unit = chart.unit in
    let several = List.length sets > 1 in
    let points =
      List.concat_mapi sets ~f:(fun k (s : C.Points.t) ->
        List.map s.points ~f:(fun (x, y) ->
          let px = position ~lo:xlo ~hi:xhi x in
          let py = position ~lo:ylo ~hi:yhi y in
          let xs = Ui_format.number x in
          let ys = value_text ~unit y in
          let place = [ "left", pct px; "bottom", pct py ] in
          if focusable
          then
            span
              ~attrs:
                [ style place
                ; Attr.tabindex 0
                ; Attr.role "img"
                ; Attr.create
                    "aria-label"
                    (sprintf
                       "%s%s, %s"
                       (if several then s.name ^ ": " else "")
                       (named chart.x_label xs)
                       (named chart.y_label ys))
                ]
              ("ui-chart-pt " ^ series_class k)
              [ tip
                  (sprintf
                     "%s %s"
                     (if Float.(px < 50.) then "right" else "left")
                     (if Float.(py > 50.) then "below" else "above"))
                  ~head:s.name
                  [ tip_row
                      ~cls:"plain"
                      (Option.value chart.x_label ~default:"x")
                      xs
                  ; tip_row
                      ~cls:"plain"
                      (Option.value chart.y_label ~default:"y")
                      ys
                  ]
              ]
          else span ~attrs:[ style place ] ("ui-chart-pt " ^ series_class k) []))
    in
    let gutter = label_width (List.map yticks ~f:(tick_text ~unit)) + 1 in
    figure
      chart
      [ axis_name ~cls:"ui-chart-yname" chart.y_label ~unit
      ; div
          ~attrs:[ style [ "--gutter", sprintf "%dch" gutter ] ]
          "ui-chart-frame"
          [ ticks_nodes
              ~cls:"ui-chart-yaxis"
              ~side:"bottom"
              yticks
              ~lo:ylo
              ~hi:yhi
              ~unit
          ; div
              "ui-chart-plot"
              (grid ~side:"bottom" yticks ~lo:ylo ~hi:yhi
               :: div
                    ~attrs:[ hidden ]
                    "ui-chart-vgrid"
                    [ grid ~side:"left" xticks ~lo:xlo ~hi:xhi ]
               :: points)
          ; ticks_nodes
              ~cls:"ui-chart-xaxis ui-chart-values"
              ~side:"left"
              xticks
              ~lo:xlo
              ~hi:xhi
              ~unit:None
          ]
      ; axis_name ~cls:"ui-chart-xname" chart.x_label ~unit:None
      ; (if several
         then
           legend
             (List.mapi sets ~f:(fun k (s : C.Points.t) ->
                series_class k, s.name, []))
         else Node.none)
      ; data_table
          ~caption:(Option.value chart.title ~default:(kind_name chart.kind))
          ~head:
            [ ""
            ; Option.value chart.x_label ~default:"x"
            ; Option.value chart.y_label ~default:"y"
            ]
          (List.concat_map sets ~f:(fun (s : C.Points.t) ->
             List.map (List.take s.points focusable_points) ~f:(fun (x, y) ->
               [ s.name; Ui_format.number x; value_text ~unit y ])))
      ])
;;

let view (_ : Ui_context.t) ~path:_ (chart : C.t) =
  match chart.data with
  | Categories { labels; series } -> categories chart ~labels ~series
  | Slices slices ->
    pie chart ~slices ~hole:(if C.Kind.equal chart.kind Donut then 30. else 0.)
  | Points sets -> scatter chart ~sets
;;
