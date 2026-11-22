open! Import
open Js_of_ocaml

module Orientation = struct
  type t =
    | Horizontal
    | Vertical
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Horizontal -> "horizontal"
    | Vertical -> "vertical"
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

module State = struct
  type t =
    { selected : int
    ; count : int
    }
  [@@deriving sexp_of, compare, equal]
end

type t =
  { id : string
  ; orientation : Orientation.t
  ; align : Align.t
  ; loop : bool
  ; autoplay : int option
  ; state : State.t
  ; on_change : State.t -> unit Effect.t
  }

let create
      ?(orientation = Orientation.Horizontal)
      ?(align = Align.Center)
      ?(loop = false)
      ?autoplay
      ~id
      ~state
      ~on_change
      ()
  =
  { id; orientation; align; loop; autoplay; state; on_change }
;;

let component ?orientation ?align ?loop ?autoplay (local_ graph) =
  let open Bonsai.Let_syntax in
  let state, set_state =
    Bonsai.state
      { State.selected = 0; count = 0 }
      graph
      ~equal:[%equal: State.t]
  in
  let id = Bonsai.path_id graph in
  let%arr state
  and set_state
  and id in
  create ?orientation ?align ?loop ?autoplay ~id ~state ~on_change:set_state ()
;;

let selected t = t.state.selected
let count t = t.state.count
let horizontal t = Orientation.equal t.orientation Horizontal
let can_scroll_prev t = t.state.count > 1 && (t.loop || t.state.selected > 0)

let can_scroll_next t =
  t.state.count > 1 && (t.loop || t.state.selected < t.state.count - 1)
;;

let scroll_to t i =
  let n = t.state.count in
  if n = 0
  then Effect.Ignore
  else (
    let i = if t.loop then i % n else Int.clamp_exn i ~min:0 ~max:(n - 1) in
    if i = t.state.selected
    then Effect.Ignore
    else t.on_change { t.state with selected = i })
;;

let scroll_prev t =
  if can_scroll_prev t
  then scroll_to t (t.state.selected - 1)
  else Effect.Ignore
;;

let scroll_next t =
  if can_scroll_next t
  then scroll_to t (t.state.selected + 1)
  else Effect.Ignore
;;

(* What a carousel keeps in the page, by id: the measured snap points (the
   track's offsets, positive), the slide it shows, a drag in progress and
   the autoplay's state. *)
type drag =
  { pointer_id : int
  ; start : float
  ; cross_start : float
  ; start_translate : float
  ; mutable dragging : bool
  ; mutable samples : (float * float) list (* (position, time), latest first *)
  }

type dom =
  { mutable latest : t option
  ; mutable snaps : float array
  ; mutable applied : int option
  ; mutable translate : float
  ; mutable drag : drag option
  ; mutable suppress_click : bool
  ; mutable hovered : bool
  ; mutable stopped : bool
  ; mutable timer_for : int option
  ; mutable observed : bool
  }

let doms : dom String.Table.t = String.Table.create ()

let dom t =
  Hashtbl.find_or_add doms t.id ~default:(fun () ->
    { latest = None
    ; snaps = [||]
    ; applied = None
    ; translate = 0.
    ; drag = None
    ; suppress_click = false
    ; hovered = false
    ; stopped = false
    ; timer_for = None
    ; observed = false
    })
;;

let track (viewport : Dom_html.element Js.t) : Dom_html.element Js.t option =
  Js.Opt.to_option (Js.Unsafe.coerce viewport)##.firstElementChild
;;

(* Embla's snap points with [containScroll: "trimSnaps"]: where each
   slide's content (inside the padding that spaces the slides) starts, less
   the room [align] leaves before it; then (but for [loop]) kept within the
   track's scrollable length, without repeats. Positions are in the
   viewport, as if the track were not moved: the slides' rects within the
   track's, plus the track's offset (which leaves its transform out). *)
let measure t (viewport : Dom_html.element Js.t) =
  match track viewport with
  | None -> [||]
  | Some track ->
    let h = horizontal t in
    let slides : Dom_html.element Js.t list =
      Dom.list_of_nodeList (Js.Unsafe.coerce track##.children)
    in
    let offset (el : Dom_html.element Js.t) =
      if h then el##.offsetLeft else el##.offsetTop
    in
    let start (el : Dom_html.element Js.t) =
      let r = el##getBoundingClientRect in
      Js.to_float (if h then r##.left else r##.top)
    in
    let track_start = start track in
    let size (el : Dom_html.element Js.t) =
      if h then el##.offsetWidth else el##.offsetHeight
    in
    let padding (el : Dom_html.element Js.t) =
      let style = Dom_html.window##getComputedStyle el in
      Float.of_string_opt
        (String.chop_suffix_if_exists
           ~suffix:"px"
           (Js.to_string
              (if h then style##.paddingLeft else style##.paddingTop)))
      |> Option.value ~default:0.
    in
    let origin = Float.of_int (offset track - offset viewport) in
    let view =
      Float.of_int
        (if h then viewport##.clientWidth else viewport##.clientHeight)
    in
    let spans =
      List.map slides ~f:(fun slide ->
        let pad = padding slide in
        ( origin +. (start slide -. track_start) +. pad
        , Float.of_int (size slide) -. pad ))
    in
    (match spans with
     | [] -> [||]
     | (first, _) :: _ ->
       let last_end =
         List.fold spans ~init:first ~f:(fun acc (s, size) ->
           Float.max acc (s +. size))
       in
       let raw =
         List.map spans ~f:(fun (s, size) ->
           let before =
             match t.align with
             | Start -> 0.
             | Center -> (view -. size) /. 2.
             | End -> view -. size
           in
           s -. before)
       in
       if t.loop
       then Array.of_list raw
       else if Float.(last_end -. first <= view)
       then [| Float.min first 0. |]
       else (
         let lo = Float.min first 0.
         and hi = last_end -. view in
         let clamped =
           List.map raw ~f:(fun s -> Float.clamp_exn s ~min:lo ~max:hi)
         in
         List.fold clamped ~init:[] ~f:(fun acc s ->
           match acc with
           | prev :: _ when Float.(abs (s -. prev) < 1.) -> acc
           | _ -> s :: acc)
         |> List.rev
         |> Array.of_list))
;;

let set_transform t (viewport : Dom_html.element Js.t) ~translate ~animate =
  Option.iter (track viewport) ~f:(fun track ->
    let px = sprintf "%.3fpx" translate in
    let style = (Js.Unsafe.coerce track)##.style in
    let set name value =
      (style##setProperty (Js.string name) (Js.string value) : unit)
    in
    set
      "transition"
      (if animate
       then "transform 450ms cubic-bezier(0.25, 1, 0.5, 1)"
       else "none");
    set
      "transform"
      (if horizontal t
       then sprintf "translate3d(%s, 0px, 0px)" px
       else sprintf "translate3d(0px, %s, 0px)" px))
;;

let fire eff = Effect.Expert.handle_non_dom_event_exn eff

let rec schedule_autoplay t =
  let st = dom t in
  match t.autoplay with
  | None -> ()
  | Some delay ->
    let key = t.id ^ "-autoplay" in
    if st.stopped || st.hovered || t.state.count < 2
    then (
      st.timer_for <- None;
      fire (After_render.cancel key))
    else if not (Option.equal Int.equal st.timer_for (Some t.state.selected))
    then (
      st.timer_for <- Some t.state.selected;
      fire
        (After_render.schedule
           key
           delay
           (Effect.of_thunk (fun () ->
              st.timer_for <- None;
              Option.iter st.latest ~f:(fun t ->
                fire
                  (if can_scroll_next t then scroll_next t else scroll_to t 0);
                schedule_autoplay t)))))

(* After each render: measure the snap points, keep the state's count in
   step with them, and move the track to the selected one (animated when
   the selection changed). *)
and update t (viewport : Dom_html.element Js.t) =
  let st = dom t in
  st.latest <- Some t;
  let snaps = measure t viewport in
  st.snaps <- snaps;
  let count = Array.length snaps in
  let selected =
    Int.clamp_exn t.state.selected ~min:0 ~max:(Int.max 0 (count - 1))
  in
  if count <> t.state.count || selected <> t.state.selected
  then fire (t.on_change { selected; count });
  if count > 0 && Option.is_none st.drag
  then (
    let animate =
      match st.applied with
      | Some prev -> prev <> selected
      | None -> false
    in
    st.applied <- Some selected;
    st.translate <- -.snaps.(selected);
    set_transform t viewport ~translate:st.translate ~animate);
  if not st.observed
  then (
    st.observed <- true;
    let observer = Js.Unsafe.global##.ResizeObserver in
    if Js.Optdef.test observer
    then (
      let callback =
        Js.wrap_callback (fun _ _ ->
          match st.latest with
          | Some t ->
            st.applied <- None;
            update t viewport
          | None -> ())
      in
      let o = new%js observer callback in
      o##observe viewport));
  schedule_autoplay t
;;

let root ?(attrs = []) ?class_ t children =
  let on_keydown ev =
    match Dom_util.key ev with
    | ArrowLeft -> Effect.Many [ Dom_util.prevent_default ev; scroll_prev t ]
    | ArrowRight -> Effect.Many [ Dom_util.prevent_default ev; scroll_next t ]
    | _ -> Effect.Ignore
  in
  let hover =
    match t.autoplay with
    | None -> []
    | Some _ ->
      let set hovered =
        Effect.of_thunk (fun () ->
          let st = dom t in
          st.hovered <- hovered;
          schedule_autoplay t)
      in
      [ Attr.on_mouseenter (fun _ -> set true)
      ; Attr.on_mouseleave (fun _ -> set false)
      ]
  in
  Node.div
    ~attrs:
      ([ Attr.on_keydown on_keydown
       ; Cn.with_class ?class_ [ "relative" ]
       ; Attr.role "region"
       ; aria "roledescription" "carousel"
       ; data "slot" "carousel"
       ]
       @ hover
       @ attrs)
    children
;;

let current_target (ev : #Dom_html.event Js.t) : Dom_html.element Js.t option =
  Js.Opt.to_option (Js.Unsafe.coerce ev)##.currentTarget
;;

let now () = Js.to_float Js.Unsafe.global##.performance##now

let on_pointerdown t (ev : Dom_html.pointerEvent Js.t) =
  Effect.of_thunk (fun () ->
    let st = dom t in
    st.suppress_click <- false;
    let mouse = String.equal (Js.to_string ev##.pointerType) "mouse" in
    if (not mouse) || ev##.button = 0
    then (
      let x = Js.to_float ev##.clientX
      and y = Js.to_float ev##.clientY in
      let along, across = if horizontal t then x, y else y, x in
      st.drag
      <- Some
           { pointer_id = ev##.pointerId
           ; start = along
           ; cross_start = across
           ; start_translate = st.translate
           ; dragging = false
           ; samples = [ along, now () ]
           }))
;;

let bounds st =
  let n = Array.length st.snaps in
  if n = 0 then 0., 0. else -.st.snaps.(n - 1), -.st.snaps.(0)
;;

let on_pointermove t (ev : Dom_html.pointerEvent Js.t) =
  Effect.of_thunk (fun () ->
    let st = dom t in
    match st.drag, current_target ev with
    | Some drag, Some viewport when drag.pointer_id = ev##.pointerId ->
      let x = Js.to_float ev##.clientX
      and y = Js.to_float ev##.clientY in
      let along, across = if horizontal t then x, y else y, x in
      let delta = along -. drag.start in
      if
        (not drag.dragging)
        && Float.(abs delta > 4.)
        && Float.(abs delta > abs (across -. drag.cross_start))
      then (
        drag.dragging <- true;
        st.stopped <- true;
        schedule_autoplay t;
        try (Js.Unsafe.coerce viewport)##setPointerCapture ev##.pointerId with
        | _ -> ());
      if drag.dragging
      then (
        drag.samples <- List.take ((along, now ()) :: drag.samples) 4;
        let lo, hi = bounds st in
        let raw = drag.start_translate +. delta in
        (* Past the ends, the track follows the pointer less and less. *)
        let translate =
          if t.loop
          then raw
          else if Float.(raw > hi)
          then hi +. ((raw -. hi) *. 0.3)
          else if Float.(raw < lo)
          then lo +. ((raw -. lo) *. 0.3)
          else raw
        in
        st.translate <- translate;
        set_transform t viewport ~translate ~animate:false)
    | _ -> ())
;;

let on_pointerup t (ev : Dom_html.pointerEvent Js.t) =
  Effect.of_thunk (fun () ->
    let st = dom t in
    match st.drag, current_target ev with
    | Some drag, Some viewport when drag.pointer_id = ev##.pointerId ->
      st.drag <- None;
      if drag.dragging
      then (
        st.suppress_click <- true;
        (* Where the track would come to rest, going on at the pointer's
           speed for a moment; the nearest snap point to that, but at least
           one slide on when the pointer moved far enough. *)
        let velocity =
          match drag.samples, List.last drag.samples with
          | (p1, t1) :: _, Some (p0, t0) when Float.(t1 -. t0 > 0.) ->
            (p1 -. p0) /. (t1 -. t0)
          | _ -> 0.
        in
        let projected = st.translate +. (velocity *. 150.) in
        let nearest =
          Array.foldi
            st.snaps
            ~init:(0, Float.infinity)
            ~f:(fun i (best, dist) snap ->
              let d = Float.abs (-.snap -. projected) in
              if Float.(d < dist) then i, d else best, dist)
          |> fst
        in
        let current = Option.value st.applied ~default:0 in
        let moved = st.translate -. drag.start_translate in
        let target =
          if nearest = current && Float.(abs moved > 40.)
          then if Float.(moved < 0.) then current + 1 else current - 1
          else nearest
        in
        let n = Array.length st.snaps in
        let target =
          if t.loop then target % n else Int.clamp_exn target ~min:0 ~max:(n - 1)
        in
        match st.latest with
        | Some t when target <> t.state.selected -> fire (scroll_to t target)
        | _ ->
          (* Back to the slide it was on. *)
          st.translate <- -.st.snaps.(current);
          set_transform t viewport ~translate:st.translate ~animate:true)
    | _ -> ())
;;

let content ?(attrs = []) ?class_ t children =
  let st_click =
    Attr.Global_listeners.click ~phase:Capture ~f:(fun ev ->
      let st = dom t in
      if st.suppress_click
      then (
        st.suppress_click <- false;
        Dom_util.stop ev)
      else Effect.Ignore)
  in
  Node.div
    ~attrs:
      [ Attr.class_ "overflow-hidden"
      ; data "slot" "carousel-content"
      ; Attr.style
          (Css_gen.create
             ~field:"touch-action"
             ~value:
               (if horizontal t then "pan-y pinch-zoom" else "pan-x pinch-zoom"))
      ; After_render.attr ~name:"carousel" (update t)
      ; Attr.on_pointerdown (on_pointerdown t)
      ; Attr.on_pointermove (on_pointermove t)
      ; Attr.on_pointerup (on_pointerup t)
      ; st_click
      ]
    [ Node.div
        ~attrs:
          ([ Cn.with_class
               ?class_
               [ "flex"; (if horizontal t then "-ml-4" else "-mt-4 flex-col") ]
           ]
           @ attrs)
        children
    ]
;;

let item ?(attrs = []) ?class_ t children =
  Node.div
    ~attrs:
      ([ Attr.role "group"
       ; aria "roledescription" "slide"
       ; data "slot" "carousel-item"
       ; Cn.with_class
           ?class_
           [ "min-w-0 shrink-0 grow-0 basis-full"
           ; (if horizontal t then "pl-4" else "pt-4")
           ]
       ]
       @ attrs)
    children
;;

(* shadcn's Button with its [data-slot] replaced. *)
let button ~attrs ~slot ~variant ~size ~class_ ~disabled ~on_click children =
  Node.button
    ~attrs:
      ([ Attr.type_ "button"
       ; data "slot" slot
       ; data "variant" (Button.Variant.name variant)
       ; data "size" (Button.Size.name size)
       ; Attr.class_ (Button.classes ~variant ~size ~class_ ())
       ]
       @ (if disabled
          then [ Attr.disabled ]
          else [ Attr.on_click (fun _ -> on_click) ])
       @ attrs)
    children
;;

let previous
      ?(attrs = [])
      ?class_
      ?(variant = Button.Variant.Outline)
      ?(size = Button.Size.Icon_sm)
      t
  =
  button
    ~attrs
    ~slot:"carousel-previous"
    ~variant
    ~size
    ~class_:
      (Cn.cn
         [ "rounded-full absolute touch-manipulation"
         ; (if horizontal t
            then "inset-y-0 -left-12 my-auto"
            else "-top-12 left-1/2 -translate-x-1/2 rotate-90")
         ; Option.value class_ ~default:""
         ])
    ~disabled:(not (can_scroll_prev t))
    ~on_click:(scroll_prev t)
    [ Icon.view ~class_:"" Chevron_left
    ; Node.span ~attrs:[ Attr.class_ "sr-only" ] [ Node.text "Previous slide" ]
    ]
;;

let next
      ?(attrs = [])
      ?class_
      ?(variant = Button.Variant.Outline)
      ?(size = Button.Size.Icon_sm)
      t
  =
  button
    ~attrs
    ~slot:"carousel-next"
    ~variant
    ~size
    ~class_:
      (Cn.cn
         [ "rounded-full absolute touch-manipulation"
         ; (if horizontal t
            then "inset-y-0 -right-12 my-auto"
            else "-bottom-12 left-1/2 -translate-x-1/2 rotate-90")
         ; Option.value class_ ~default:""
         ])
    ~disabled:(not (can_scroll_next t))
    ~on_click:(scroll_next t)
    [ Icon.view ~class_:"" Chevron_right
    ; Node.span ~attrs:[ Attr.class_ "sr-only" ] [ Node.text "Next slide" ]
    ]
;;
