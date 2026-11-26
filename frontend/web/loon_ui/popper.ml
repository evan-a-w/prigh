open! Import
open Js_of_ocaml

module Side = struct
  type t =
    | Top
    | Right
    | Bottom
    | Left
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Top -> "top"
    | Right -> "right"
    | Bottom -> "bottom"
    | Left -> "left"
  ;;

  let opposite = function
    | Top -> Bottom
    | Bottom -> Top
    | Left -> Right
    | Right -> Left
  ;;
end

module Align = struct
  type t =
    | Start
    | Center
    | End
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Start -> "start"
    | Center -> "center"
    | End -> "end"
  ;;
end

module Anchor = struct
  type t =
    | Element of string
    | Point of
        { x : float
        ; y : float
        }
  [@@deriving sexp_of, equal]
end

type rect =
  { left : float
  ; top : float
  ; width : float
  ; height : float
  }

let rect_of (el : Dom_html.element Js.t) =
  let r = el##getBoundingClientRect in
  let left = Js.to_float r##.left
  and top = Js.to_float r##.top in
  { left
  ; top
  ; width = Js.to_float r##.right -. left
  ; height = Js.to_float r##.bottom -. top
  }
;;

let by_id id = Js.Opt.to_option (Dom_html.document##getElementById (Js.string id))

let anchor_rect : Anchor.t -> rect option = function
  | Element id -> Option.map (by_id id) ~f:rect_of
  | Point { x; y } -> Some { left = x; top = y; width = 0.; height = 0. }
;;

let viewport () =
  let d = Dom_html.document##.documentElement in
  Float.of_int d##.clientWidth, Float.of_int d##.clientHeight
;;

let computed (el : Dom_html.element Js.t) prop =
  let style = Dom_html.window##getComputedStyle el in
  Js.to_string ((Js.Unsafe.coerce style)##getPropertyValue (Js.string prop))
;;

(* Whether [el] is the containing block of fixed descendants (a transform,
   a filter, containment...), so that their [left] and [top] are relative
   to it rather than to the viewport. *)
let contains_fixed el =
  let set prop =
    let v = computed el prop in
    not (String.is_empty v || String.equal v "none")
  in
  let has prop words =
    let v = computed el prop in
    List.exists words ~f:(fun w -> String.is_substring v ~substring:w)
  in
  set "transform"
  || set "translate"
  || set "scale"
  || set "rotate"
  || set "perspective"
  || set "filter"
  || set "backdrop-filter"
  || has "contain" [ "paint"; "layout"; "strict"; "content" ]
  || has "will-change" [ "transform"; "filter"; "perspective" ]
  || has "container-type" [ "size"; "inline-size" ]
;;

(* Where (0, 0) of a fixed element inside [el] is, in the viewport. *)
let fixed_origin (el : Dom_html.element Js.t) =
  let rec go (node : Dom.node Js.t Js.opt) =
    match Js.Opt.to_option (Js.Opt.bind node Dom_html.CoerceTo.element) with
    | None -> 0., 0.
    | Some p when phys_equal p Dom_html.document##.documentElement -> 0., 0.
    | Some p ->
      if contains_fixed p
      then (
        let r = rect_of p in
        ( r.left +. Float.of_int p##.clientLeft -. Js.to_float p##.scrollLeft
        , r.top +. Float.of_int p##.clientTop -. Js.to_float p##.scrollTop ))
      else go p##.parentNode
  in
  if Portal.is_shown el then 0., 0. else go el##.parentNode
;;

let dpr () =
  let d = Js.to_float (Js.Unsafe.coerce Dom_html.window)##.devicePixelRatio in
  if Float.( > ) d 0. then d else 1.
;;

let round v =
  let d = dpr () in
  Float.round_nearest (v *. d) /. d
;;

let px v = sprintf "%gpx" (round v)

let set_style (el : Dom_html.element Js.t) name value =
  let style = Js.Unsafe.coerce el##.style in
  let old = Js.to_string (style##getPropertyValue (Js.string name)) in
  if not (String.equal old value)
  then style##setProperty (Js.string name) (Js.string value) |> ignore
;;

let set_attr (el : Dom_html.element Js.t) name value =
  let old = Js.Opt.case (el##getAttribute (Js.string name)) (fun () -> "") Js.to_string in
  if not (String.equal old value) then el##setAttribute (Js.string name) (Js.string value)
;;

let first_child (el : Dom_html.element Js.t) : Dom_html.element Js.t option =
  Js.Opt.to_option (Js.Unsafe.coerce el)##.firstElementChild
;;

let query (el : Dom_html.element Js.t) selector =
  Js.Opt.to_option (el##querySelector (Js.string selector))
;;

module Placement = struct
  type t =
    { side : Side.t
    ; side_offset : float
    ; align : Align.t
    ; align_offset : float
    ; avoid_collisions : bool
    ; collision_padding : float
    ; anchor : Anchor.t
    ; name : string
    }
  [@@deriving sexp_of]
end

let arrow_width = 10.
let arrow_height = 5.

let position (p : Placement.t) (wrapper : Dom_html.element Js.t) =
  match anchor_rect p.anchor, first_child wrapper with
  | None, _ | _, None -> ()
  | Some a, Some content ->
    let vw, vh = viewport () in
    let pad = p.collision_padding in
    let arrow = query content "[data-popper-arrow]" in
    (* Radix measures its arrow's wrapper: shadcn's [size-2.5] arrow is
       10px high, not the SVG's 5. *)
    let arrow_w, arrow_h =
      match arrow with
      | None -> arrow_width, 0.
      | Some el ->
        let w = Float.of_int el##.offsetWidth
        and h = Float.of_int el##.offsetHeight in
        if Float.(h > 0.) then w, h else arrow_width, arrow_height
    in
    let offset = p.side_offset +. arrow_h in
    let coords side ~w ~h =
      let cross = if Align.equal p.align End then -.p.align_offset else p.align_offset in
      let along start length size =
        (match p.align with
         | Start -> start
         | Center -> start +. ((length -. size) /. 2.)
         | End -> start +. length -. size)
        +. cross
      in
      match (side : Side.t) with
      | Bottom -> along a.left a.width w, a.top +. a.height +. offset
      | Top -> along a.left a.width w, a.top -. h -. offset
      | Right -> a.left +. a.width +. offset, along a.top a.height h
      | Left -> a.left -. w -. offset, along a.top a.height h
    in
    let overflow side ~w ~h =
      let x, y = coords side ~w ~h in
      match (side : Side.t) with
      | Bottom -> y +. h -. (vh -. pad)
      | Top -> pad -. y
      | Right -> x +. w -. (vw -. pad)
      | Left -> pad -. x
    in
    let w = Float.of_int wrapper##.offsetWidth
    and h = Float.of_int wrapper##.offsetHeight in
    let side =
      if not p.avoid_collisions
      then p.side
      else (
        let here = overflow p.side ~w ~h in
        let there = overflow (Side.opposite p.side) ~w ~h in
        if Float.(here <= 0.) || Float.(there >= here)
        then p.side
        else Side.opposite p.side)
    in
    let available_height, available_width =
      match side with
      | Bottom -> vh -. pad -. (a.top +. a.height +. offset), vw -. (2. *. pad)
      | Top -> a.top -. offset -. pad, vw -. (2. *. pad)
      | Right -> vh -. (2. *. pad), vw -. pad -. (a.left +. a.width +. offset)
      | Left -> vh -. (2. *. pad), a.left -. offset -. pad
    in
    let var suffix value = set_style wrapper (sprintf "--radix-%s-%s" p.name suffix) value in
    var "content-available-height" (px (Float.max 0. available_height));
    var "content-available-width" (px (Float.max 0. available_width));
    var "trigger-width" (px a.width);
    var "trigger-height" (px a.height);
    (* The size may have changed with the available height. *)
    let w = Float.of_int wrapper##.offsetWidth
    and h = Float.of_int wrapper##.offsetHeight in
    let x, y = coords side ~w ~h in
    let clamp v ~lo ~hi = if Float.(hi < lo) then lo else Float.max lo (Float.min hi v) in
    let x, y =
      if not p.avoid_collisions
      then x, y
      else (
        match side with
        | Top | Bottom ->
          let x = clamp x ~lo:pad ~hi:(vw -. pad -. w) in
          (* Radix's sticky "partial": never detached from the anchor. *)
          clamp x ~lo:(a.left -. w) ~hi:(a.left +. a.width), y
        | Left | Right ->
          let y = clamp y ~lo:pad ~hi:(vh -. pad -. h) in
          x, clamp y ~lo:(a.top -. h) ~hi:(a.top +. a.height))
    in
    let ox, oy = fixed_origin wrapper in
    set_style wrapper "left" (px (x -. ox));
    set_style wrapper "top" (px (y -. oy));
    let along =
      match p.align with
      | Start -> "0%"
      | Center -> "50%"
      | End -> "100%"
    in
    var
      "content-transform-origin"
      (match side with
       | Bottom -> sprintf "%s %s" along (px (-.arrow_h))
       | Top -> sprintf "%s %s" along (px (h +. arrow_h))
       | Right -> sprintf "%s %s" (px (-.arrow_h)) along
       | Left -> sprintf "%s %s" (px (w +. arrow_h)) along);
    set_attr content "data-side" (Side.name side);
    set_attr content "data-align" (Align.name p.align);
    Option.iter arrow ~f:(fun arrow ->
      let base = Side.name (Side.opposite side) in
      List.iter [ "top"; "bottom"; "left"; "right" ] ~f:(fun edge ->
        if not (String.equal edge base) then set_style arrow edge "");
      set_style arrow base "0";
      (match side with
       | Top | Bottom ->
         let ax = clamp (a.left +. (a.width /. 2.) -. x -. (arrow_w /. 2.)) ~lo:0. ~hi:(w -. arrow_w) in
         set_style arrow "left" (px ax)
       | Left | Right ->
         let ay =
           clamp (a.top +. (a.height /. 2.) -. y -. (arrow_w /. 2.)) ~lo:0. ~hi:(h -. arrow_w)
         in
         set_style arrow "top" (px ay));
      set_style
        arrow
        "transform-origin"
        (match side with
         | Top -> ""
         | Right -> "0 0"
         | Bottom -> "center 0"
         | Left -> "100% 0");
      set_style
        arrow
        "transform"
        (match side with
         | Top -> "translateY(100%)"
         | Right -> "translateY(50%) rotate(90deg) translateX(-50%)"
         | Bottom -> "rotate(180deg)"
         | Left -> "translateY(50%) rotate(-90deg) translateX(50%)"))
;;

(* Radix Select's item-aligned position: the selected item (else the first)
   over the trigger's value. The wrapper runs from the item's top part down
   to the bottom margin of the window (or from the top margin, scrolling the
   list, when the items above it do not fit); scrolling the list then grows
   it, up to the window's height ([expand_on_scroll]). *)
let margin = 10.

type item_aligned =
  { mutable anchored_bottom : bool
  ; mutable min_height : float
  ; mutable prev_scroll : float
  ; mutable expand : bool
  }

let float_style (el : Dom_html.element Js.t) name =
  let v =
    Js.to_string
      ((Dom_html.window##getComputedStyle el)##getPropertyValue (Js.string name))
  in
  Option.value (Float.of_string_opt (String.chop_suffix_if_exists v ~suffix:"px")) ~default:0.
;;

let position_item_aligned (st : item_aligned) ~anchor (wrapper : Dom_html.element Js.t) =
  match by_id anchor, first_child wrapper with
  | None, _ | _, None -> ()
  | Some trigger, Some content ->
    let vw, vh = viewport () in
    let available = vh -. (2. *. margin) in
    let t = rect_of trigger in
    let value =
      Option.value_map (query trigger "[data-slot=select-value]") ~default:t ~f:rect_of
    in
    let items = Dom.list_of_nodeList (content##querySelectorAll (Js.string "[role=option]")) in
    let item =
      match query content "[role=option][data-state=checked]" with
      | Some item -> Some item
      | None -> List.hd items
    in
    set_style content "max-height" "100%";
    set_style wrapper "max-height" (px available);
    set_style wrapper "--radix-select-content-available-height" (px available);
    let c = rect_of content in
    (* Horizontally: the item's text over the trigger's value. *)
    let text_left =
      match item with
      | Some item ->
        (match Js.Opt.to_option (Js.Unsafe.coerce item)##.lastElementChild with
         | Some (text : Dom_html.element Js.t) -> (rect_of text).left
         | None -> (rect_of item).left)
      | None -> c.left
    in
    let left = value.left -. (text_left -. c.left) in
    let min_width = t.width +. (t.left -. left) in
    set_style wrapper "min-width" (px min_width);
    let width = Float.max min_width c.width in
    let left = Float.max margin (Float.min left (vw -. margin -. width)) in
    let ox, oy = fixed_origin wrapper in
    set_style wrapper "left" (px (left -. ox));
    (match item, query content "[data-radix-select-viewport]" with
     | Some item, Some vp ->
       let border_top = float_style content "border-top-width" in
       let border_bottom = float_style content "border-bottom-width" in
       let padding_top = float_style content "padding-top" in
       let padding_bottom = float_style content "padding-bottom" in
       let vp_padding_top = float_style vp "padding-top" in
       let vp_padding_bottom = float_style vp "padding-bottom" in
       let items_height = Float.of_int vp##.scrollHeight in
       let full =
         border_top +. padding_top +. items_height +. padding_bottom +. border_bottom
       in
       let item_height = Float.of_int item##.offsetHeight in
       let min_height = Float.min (item_height *. 5.) full in
       let top_to_trigger_middle = t.top +. (t.height /. 2.) -. margin in
       let trigger_middle_to_bottom = available -. top_to_trigger_middle in
       let half = item_height /. 2. in
       let item_middle = Float.of_int item##.offsetTop +. half in
       let content_top_to_item_middle = border_top +. padding_top +. item_middle in
       let item_middle_to_content_bottom = full -. content_top_to_item_middle in
       let vp_top = Float.of_int vp##.offsetTop in
       let height, top, scroll =
         if Float.(content_top_to_item_middle <= top_to_trigger_middle)
         then (
           let is_last = Option.equal phys_equal (List.last items) (Some item) in
           let vp_offset_bottom =
             Float.of_int (content##.clientHeight - vp##.offsetTop - vp##.offsetHeight)
           in
           let below =
             Float.max
               trigger_middle_to_bottom
               (half
                +. (if is_last then vp_padding_bottom else 0.)
                +. vp_offset_bottom
                +. border_bottom)
           in
           let height = Float.min available (content_top_to_item_middle +. below) in
           st.anchored_bottom <- true;
           height, margin +. available -. height, 0.)
         else (
           let is_first = Option.equal phys_equal (List.hd items) (Some item) in
           let above =
             Float.max
               top_to_trigger_middle
               (border_top +. vp_top +. (if is_first then vp_padding_top else 0.) +. half)
           in
           let height = Float.min available (above +. item_middle_to_content_bottom) in
           st.anchored_bottom <- false;
           height, margin, content_top_to_item_middle -. top_to_trigger_middle +. vp_top)
       in
       let height = Float.max height min_height in
       st.min_height <- min_height;
       set_style wrapper "min-height" (px min_height);
       set_style wrapper "height" (px height);
       set_style wrapper "top" (px (top -. oy));
       vp##.scrollTop := Js.float scroll;
       st.prev_scroll <- Js.to_float vp##.scrollTop
     | _ -> set_style wrapper "top" (px (t.top -. oy)))
;;

(* Shows the select's scroll buttons while the viewport can scroll their way
   (Radix renders them only then); [true] when that changed one. *)
let sync_scroll_buttons (content : Dom_html.element Js.t) =
  match query content "[data-radix-select-viewport]" with
  | None -> false
  | Some vp ->
    let top = Js.to_float vp##.scrollTop in
    let max_top = Float.of_int (vp##.scrollHeight - vp##.clientHeight) in
    let set selector hidden =
      match query content selector with
      | None -> false
      | Some b ->
        let was = Js.to_bool (b##hasAttribute (Js.string "hidden")) in
        if hidden
        then b##setAttribute (Js.string "hidden") (Js.string "")
        else b##removeAttribute (Js.string "hidden");
        Bool.( <> ) was hidden
    in
    let up = set "[data-slot=select-scroll-up-button]" Float.(top <= 0.) in
    let down =
      set "[data-slot=select-scroll-down-button]" Float.(round_up top >= max_top)
    in
    up || down
;;

(* Radix's [SelectViewport] onScroll: while the wrapper is shorter than the
   window allows, scrolling grows it instead (upwards when it is anchored at
   the bottom). *)
let expand_on_scroll (st : item_aligned) (wrapper : Dom_html.element Js.t) (vp : Dom_html.element Js.t)
  =
  let scroll = Js.to_float vp##.scrollTop in
  let delta = Float.abs (st.prev_scroll -. scroll) in
  (if st.expand && Float.(delta > 0.)
   then (
     let _, vh = viewport () in
     let available = vh -. (2. *. margin) in
     let height = (rect_of wrapper).height in
     if Float.(height < available)
     then (
       let next = Float.max st.min_height height +. delta in
       let clamped = Float.min available next in
       let diff = next -. clamped in
       set_style wrapper "height" (px clamped);
       if st.anchored_bottom
       then (
         let top = (rect_of wrapper).top -. (clamped -. height) in
         let _, oy = fixed_origin wrapper in
         set_style wrapper "top" (px (top -. oy));
         vp##.scrollTop := Js.float (if Float.(diff > 0.) then diff else 0.)))));
  st.prev_scroll <- Js.to_float vp##.scrollTop
;;

module Mode = struct
  type t =
    | Popper of Placement.t
    | Item_aligned of string
  [@@deriving sexp_of]
end

module Hook = Vdom.Attr.Hooks.Make (struct
    module State = struct
      type t =
        { mutable mode : Mode.t
        ; mutable frame : Dom_html.animation_frame_request_id option
        ; mutable frames : int
        ; item_aligned : item_aligned
        ; mutable listener : Dom.event_listener_id option
        }
    end

    module Input = struct
      type t = Mode.t [@@deriving sexp_of]

      let combine _ second = second
    end

    let init mode _ =
      { State.mode
      ; frame = None
      ; frames = 0
      ; item_aligned =
          { anchored_bottom = false; min_height = 0.; prev_scroll = 0.; expand = false }
      ; listener = None
      }
    ;;

    let rec tick (state : State.t) el () =
      state.frames <- state.frames + 1;
      (match state.mode with
       | Popper p -> position p el
       | Item_aligned anchor ->
         if state.frames <= 2
         then (
           (* As Radix: placed, then placed again once the scroll buttons
              that placement calls for are shown, so they cover no item. *)
           position_item_aligned state.item_aligned ~anchor el;
           Option.iter (first_child el) ~f:(fun content ->
             if sync_scroll_buttons content
             then (
               position_item_aligned state.item_aligned ~anchor el;
               ignore (sync_scroll_buttons content : bool))))
         else if state.frames = 3
         then (
           state.item_aligned.expand <- true;
           match
             Option.bind (first_child el) ~f:(fun c ->
               query c "[data-radix-select-viewport]")
           with
           | None -> ()
           | Some vp ->
             state.item_aligned.prev_scroll <- Js.to_float vp##.scrollTop;
             state.listener
             <- Some
                  (Dom_html.addEventListener
                     vp
                     Dom_html.Event.scroll
                     (Dom_html.handler (fun _ ->
                        expand_on_scroll state.item_aligned el vp;
                        (* After the select's own listener: this may have
                           scrolled the viewport again. *)
                        Option.iter (first_child el) ~f:(fun c ->
                          ignore (sync_scroll_buttons c : bool));
                        Js._true))
                     Js._false)));
      state.frame
      <- Some
           (Dom_html.window##requestAnimationFrame
              (Js.wrap_callback (fun (_ : Js.number_t) -> tick state el ())))
    ;;

    let on_mount =
      `Schedule_immediately_after_this_dom_patch_completes
        (fun _ (state : State.t) el ->
          Portal.show el;
          set_style el "position" "fixed";
          set_style el "left" "0";
          set_style el "top" "0";
          set_style el "min-width" "max-content";
          set_style el "z-index" "50";
          (match state.mode with
           | Popper _ -> ()
           | Item_aligned _ ->
             set_style el "display" "flex";
             set_style el "flex-direction" "column");
          tick state el ())
    ;;

    let update ~old_input:_ ~new_input (state : State.t) _ = state.mode <- new_input

    let destroy _ (state : State.t) _ =
      Option.iter state.frame ~f:(fun id -> Dom_html.window##cancelAnimationFrame id);
      Option.iter state.listener ~f:Dom_html.removeEventListener;
      state.frame <- None
    ;;
  end)

let hook mode = Vdom.Attr.create_hook "loon-popper" (Hook.create mode)

let wrapper
  ?(side = Side.Bottom)
  ?(side_offset = 0.)
  ?(align = Align.Center)
  ?(align_offset = 0.)
  ?(avoid_collisions = true)
  ?(collision_padding = 0.)
  ?(attrs = [])
  ~name
  ~anchor
  content
  =
  Node.div
    ~attrs:
      ([ Attr.create "data-radix-popper-content-wrapper" "" ]
       @ Portal.positioned
       @ [ hook
           (Popper
              { side
              ; side_offset
              ; align
              ; align_offset
              ; avoid_collisions
              ; collision_padding
              ; anchor
              ; name
              })
         ]
       @ attrs)
    [ content ]
;;

let item_aligned_wrapper ?(attrs = []) ~name:_ ~anchor content =
  Node.div
    ~attrs:
      ([ Attr.create "data-radix-popper-content-wrapper" "" ]
       @ Portal.positioned
       @ [ hook (Item_aligned anchor) ]
       @ attrs)
    [ content ]
;;

let content_attrs ?(side = Side.Bottom) ?(align = Align.Center) () =
  [ data "side" (Side.name side); data "align" (Align.name align) ]
;;

let arrow ?class_ () =
  Node.span
    ~attrs:[ Attr.create "data-popper-arrow" ""; Attr.style (Css_gen.position `Absolute) ]
    [ Node.create_svg
        "svg"
        ~attrs:
          ([ Attr.create "width" "10"
           ; Attr.create "height" "5"
           ; Attr.create "viewBox" "0 0 30 10"
           ; Attr.create "preserveAspectRatio" "none"
           ; Attr.style (Css_gen.display `Block)
           ]
           @ Option.value_map class_ ~default:[] ~f:(fun c -> [ Cn.attr [ c ] ]))
        [ Node.create_svg "polygon" ~attrs:[ Attr.create "points" "0,0 30,0 15,10" ] [] ]
    ]
;;
