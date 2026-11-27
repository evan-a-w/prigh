open! Import
open Js_of_ocaml

module Type = struct
  type t =
    | Auto
    | Always
    | Scroll
    | Hover
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Auto -> "auto"
    | Always -> "always"
    | Scroll -> "scroll"
    | Hover -> "hover"
  ;;
end

module Orientation = struct
  type t =
    | Vertical
    | Horizontal
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Vertical -> "vertical"
    | Horizontal -> "horizontal"
  ;;
end

(* The scrollbars, thumbs and viewport are drawn by the virtual DOM once;
   from then on, as Radix does through refs, this controller keeps their
   state, sizes and positions in the DOM directly, from the viewport's
   scroll metrics. *)
module Controller = struct
  let float (o : _ Js.t) name : float = Js.to_float (Js.Unsafe.get o name)
  let str s = Js.string s

  let set_attr (el : Dom_html.element Js.t) name v =
    el##setAttribute (str name) (str v)
  ;;

  let set_style (el : Dom_html.element Js.t) name v =
    ignore
      (Js.Unsafe.meth_call
         el##.style
         "setProperty"
         [| Js.Unsafe.inject (str name); Js.Unsafe.inject (str v) |]
       : unit)
  ;;

  let attr (el : Dom_html.element Js.t) name =
    Js.Opt.case (el##getAttribute (str name)) (fun () -> "") Js.to_string
  ;;

  let closest (el : Dom_html.element Js.t) selector
    : Dom_html.element Js.t option
    =
    Js.Opt.to_option
      (Js.Unsafe.meth_call el "closest" [| Js.Unsafe.inject (str selector) |])
  ;;

  let padding el ~start ~end_ =
    let style = Dom_html.window##getComputedStyle el in
    let strip v =
      String.chop_suffix v ~suffix:"px" |> Option.value ~default:v
    in
    let px name =
      Js.Unsafe.get style name
      |> Js.Optdef.to_option
      |> Option.value ~default:(Js.string "0")
      |> Js.to_string
      |> strip
      |> Float.of_string_opt
      |> Option.value ~default:0.
    in
    px start +. px end_
  ;;

  let thumb_min = 18.

  type t =
    { root : Dom_html.element Js.t
    ; mutable type_ : Type.t
    ; mutable delay_ms : float
    ; mutable hovering : bool
    ; mutable scrolling : bool
    ; mutable hide_timer : Dom_html.timeout_id_safe option
    ; mutable dragging : (Dom_html.element Js.t * Orientation.t * float) option
    ; mutable cleanup : (unit -> unit) list
    }

  let viewport t : Dom_html.element Js.t option =
    Js.Opt.to_option
      (t.root##querySelector (str ":scope > [data-radix-scroll-area-viewport]"))
  ;;

  let own_scrollbar t (el : Dom_html.element Js.t) =
    match closest el "[data-slot=scroll-area]" with
    | Some root -> phys_equal root t.root
    | None -> false
  ;;

  let scrollbars t =
    Dom.list_of_nodeList
      (t.root##querySelectorAll (str "[data-slot=scroll-area-scrollbar]"))
    |> List.filter ~f:(own_scrollbar t)
  ;;

  let orientation sb : Orientation.t =
    if String.equal (attr sb "data-orientation") "horizontal"
    then Horizontal
    else Vertical
  ;;

  let thumb (sb : Dom_html.element Js.t) : Dom_html.element Js.t option =
    Js.Opt.to_option (Js.Unsafe.get sb "firstElementChild")
  ;;

  (* viewport size, content size, scroll position along an axis *)
  let metrics (vp : Dom_html.element Js.t)
    : Orientation.t -> float * float * float
    = function
    | Vertical ->
      float vp "clientHeight", float vp "scrollHeight", float vp "scrollTop"
    | Horizontal ->
      float vp "clientWidth", float vp "scrollWidth", float vp "scrollLeft"
  ;;

  let overflows vp o =
    let viewport, content, _ = metrics vp o in
    Float.(content - viewport >= 1.)
  ;;

  let track (sb : Dom_html.element Js.t) : Orientation.t -> float * float
    = function
    | Vertical ->
      ( float sb "clientHeight"
      , padding sb ~start:"paddingTop" ~end_:"paddingBottom" )
    | Horizontal ->
      ( float sb "clientWidth"
      , padding sb ~start:"paddingLeft" ~end_:"paddingRight" )
  ;;

  let thumb_size vp sb o =
    let viewport, content, _ = metrics vp o in
    let size, pad = track sb o in
    let ratio = if Float.(content > 0.) then viewport /. content else 1. in
    Float.max ((size -. pad) *. ratio) thumb_min
  ;;

  let update t =
    match viewport t with
    | None -> ()
    | Some vp ->
      let sbs = scrollbars t in
      let has o =
        List.exists sbs ~f:(fun sb -> Orientation.equal (orientation sb) o)
      in
      set_style vp "overflow-x" (if has Horizontal then "scroll" else "hidden");
      set_style vp "overflow-y" (if has Vertical then "scroll" else "hidden");
      let visible =
        List.map sbs ~f:(fun sb ->
          let o = orientation sb in
          let overflow = overflows vp o in
          let visible =
            match t.type_ with
            | Always -> true
            | Auto -> overflow
            | Hover -> overflow && (t.hovering || Option.is_some t.dragging)
            | Scroll -> overflow && (t.scrolling || Option.is_some t.dragging)
          in
          set_attr sb "data-state" (if visible then "visible" else "hidden");
          set_style sb "display" (if visible then "" else "none");
          Option.iter (thumb sb) ~f:(fun th ->
            set_attr th "data-state" (if overflow then "visible" else "hidden"));
          sb, o, visible)
      in
      let shown o =
        List.find_map visible ~f:(fun (sb, o', v) ->
          if v && Orientation.equal o o' then Some sb else None)
      in
      (match shown Vertical, shown Horizontal with
       | Some v, Some h when not (Type.equal t.type_ Scroll) ->
         set_style
           t.root
           "--radix-scroll-area-corner-width"
           (sprintf "%.0fpx" (float v "offsetWidth"));
         set_style
           t.root
           "--radix-scroll-area-corner-height"
           (sprintf "%.0fpx" (float h "offsetHeight"))
       | _ ->
         set_style t.root "--radix-scroll-area-corner-width" "0px";
         set_style t.root "--radix-scroll-area-corner-height" "0px");
      List.iter visible ~f:(fun (sb, o, v) ->
        if v
        then (
          let viewport, content, scroll = metrics vp o in
          let size, pad = track sb o in
          let thumb_size = thumb_size vp sb o in
          let max_scroll = content -. viewport in
          let offset =
            if Float.(max_scroll > 0.)
            then
              Float.clamp_exn (scroll /. max_scroll) ~min:0. ~max:1.
              *. (size -. pad -. thumb_size)
            else 0.
          in
          match o with
          | Vertical ->
            set_style
              sb
              "--radix-scroll-area-thumb-height"
              (sprintf "%fpx" thumb_size);
            Option.iter (thumb sb) ~f:(fun th ->
              set_style
                th
                "transform"
                (sprintf "translate3d(0, %fpx, 0)" offset))
          | Horizontal ->
            set_style
              sb
              "--radix-scroll-area-thumb-width"
              (sprintf "%fpx" thumb_size);
            Option.iter (thumb sb) ~f:(fun th ->
              set_style
                th
                "transform"
                (sprintf "translate3d(%fpx, 0, 0)" offset))))
  ;;

  let clear_timer t =
    Option.iter t.hide_timer ~f:Dom_html.clearTimeout;
    t.hide_timer <- None
  ;;

  let later t f =
    clear_timer t;
    t.hide_timer
    <- Some
         (Dom_html.setTimeout
            (fun () ->
               t.hide_timer <- None;
               f ();
               update t)
            t.delay_ms)
  ;;

  (* Scrolls so that the thumb's point [offset] from its start is under the
     pointer. *)
  let scroll_to_pointer t sb o ~offset ev =
    Option.iter (viewport t) ~f:(fun vp ->
      let rect = sb##getBoundingClientRect in
      let pointer, start =
        match (o : Orientation.t) with
        | Vertical -> float ev "clientY", Js.to_float rect##.top
        | Horizontal -> float ev "clientX", Js.to_float rect##.left
      in
      let pad_start =
        match o with
        | Vertical -> padding sb ~start:"paddingTop" ~end_:"unset"
        | Horizontal -> padding sb ~start:"paddingLeft" ~end_:"unset"
      in
      let viewport, content, _ = metrics vp o in
      let size, pad = track sb o in
      let thumb_size = thumb_size vp sb o in
      let room = size -. pad -. thumb_size in
      if Float.(room > 0.)
      then (
        let fraction = (pointer -. start -. pad_start -. offset) /. room in
        let scroll =
          Float.clamp_exn fraction ~min:0. ~max:1. *. (content -. viewport)
        in
        match o with
        | Vertical -> Js.Unsafe.set vp "scrollTop" (Js.number_of_float scroll)
        | Horizontal ->
          Js.Unsafe.set vp "scrollLeft" (Js.number_of_float scroll)))
  ;;

  let listen (target : _ Js.t) name f =
    let id =
      Dom.addEventListener
        target
        (Dom.Event.make name)
        (Dom.handler (fun ev ->
           f ev;
           Js._true))
        Js._false
    in
    fun () -> Dom.removeEventListener id
  ;;

  let target_element ev : Dom_html.element Js.t option =
    Js.Opt.to_option (Js.Unsafe.get ev "target")
    |> Option.bind ~f:(fun (node : Dom.node Js.t) ->
      Js.Opt.to_option (Dom_html.CoerceTo.element node))
  ;;

  let scrollbar_of_event t ev =
    Option.bind (target_element ev) ~f:(fun el ->
      match closest el "[data-slot=scroll-area-scrollbar]" with
      | Some sb when own_scrollbar t sb -> Some sb
      | _ -> None)
  ;;

  let mount t =
    let root = t.root in
    let on_pointerdown ev =
      match scrollbar_of_event t ev with
      | Some sb when Int.equal (Js.Unsafe.get ev "button") 0 ->
        let o = orientation sb in
        let on_thumb =
          match target_element ev, thumb sb with
          | Some el, Some th -> phys_equal el th
          | _ -> false
        in
        let offset =
          match on_thumb, thumb sb, viewport t with
          | true, Some th, _ ->
            let r = th##getBoundingClientRect in
            (match o with
             | Vertical -> float ev "clientY" -. Js.to_float r##.top
             | Horizontal -> float ev "clientX" -. Js.to_float r##.left)
          | _, _, Some vp -> thumb_size vp sb o /. 2.
          | _ -> 0.
        in
        (try
           ignore
             (Js.Unsafe.meth_call
                sb
                "setPointerCapture"
                [| Js.Unsafe.get ev "pointerId" |]
              : unit)
         with
         | _ -> ());
        Dom.preventDefault ev;
        t.dragging <- Some (sb, o, offset);
        scroll_to_pointer t sb o ~offset ev
      | _ -> ()
    in
    let on_pointermove ev =
      match t.dragging with
      | Some (sb, o, offset) -> scroll_to_pointer t sb o ~offset ev
      | None -> ()
    in
    let on_pointerup ev =
      match t.dragging with
      | Some (sb, _, _) ->
        t.dragging <- None;
        (try
           ignore
             (Js.Unsafe.meth_call
                sb
                "releasePointerCapture"
                [| Js.Unsafe.get ev "pointerId" |]
              : unit)
         with
         | _ -> ());
        if not t.hovering then later t (fun () -> ()) else update t
      | None -> ()
    in
    let on_wheel ev =
      match scrollbar_of_event t ev, viewport t with
      | Some _, Some vp ->
        Js.Unsafe.set
          vp
          "scrollTop"
          (Js.number_of_float (float vp "scrollTop" +. float ev "deltaY"));
        Js.Unsafe.set
          vp
          "scrollLeft"
          (Js.number_of_float (float vp "scrollLeft" +. float ev "deltaX"));
        Dom.preventDefault ev
      | _ -> ()
    in
    let on_enter _ =
      t.hovering <- true;
      clear_timer t;
      update t
    in
    let on_leave _ = later t (fun () -> t.hovering <- false) in
    let on_scroll _ =
      if Type.equal t.type_ Scroll
      then (
        t.scrolling <- true;
        later t (fun () -> t.scrolling <- false));
      update t
    in
    let observers =
      match viewport t with
      | None -> []
      | Some vp ->
        let resize =
          ResizeObserver.observe ~node:vp ~f:(fun _ _ -> update t) ()
        in
        Option.iter
          (Js.Opt.to_option (Js.Unsafe.get vp "firstElementChild"))
          ~f:(fun (content : Dom_html.element Js.t) -> resize##observe content);
        let mutation =
          MutationObserver.observe
            ~node:root
            ~f:(fun _ _ -> update t)
            ~child_list:true
            ~subtree:true
            ()
        in
        [ listen vp "scroll" on_scroll
        ; (fun () -> resize##disconnect)
        ; (fun () -> mutation##disconnect)
        ]
    in
    t.cleanup
    <- [ listen root "pointerdown" on_pointerdown
       ; listen root "pointermove" on_pointermove
       ; listen root "pointerup" on_pointerup
       ; listen root "pointercancel" on_pointerup
       ; listen root "wheel" on_wheel
       ; listen root "pointerenter" on_enter
       ; listen root "pointerleave" on_leave
       ]
       @ observers;
    update t
  ;;

  let destroy t =
    clear_timer t;
    List.iter t.cleanup ~f:(fun f -> f ());
    t.cleanup <- []
  ;;
end

module Hook = Vdom.Attr.Hooks.Make (struct
    module State = struct
      type t = Controller.t option ref
    end

    module Input = struct
      type t = Type.t * Time_ns.Span.t [@@deriving sexp_of]

      let combine _ second = second
    end

    let init _ _ = ref None

    let on_mount =
      `Schedule_immediately_after_this_dom_patch_completes
        (fun (type_, delay) (state : State.t) root ->
          let t =
            { Controller.root
            ; type_
            ; delay_ms = Time_ns.Span.to_ms delay
            ; hovering = false
            ; scrolling = false
            ; hide_timer = None
            ; dragging = None
            ; cleanup = []
            }
          in
          state := Some t;
          Controller.mount t)
    ;;

    let update ~old_input:_ ~new_input:(type_, delay) (state : State.t) _ =
      Option.iter !state ~f:(fun (t : Controller.t) ->
        t.type_ <- type_;
        t.delay_ms <- Time_ns.Span.to_ms delay;
        Controller.update t)
    ;;

    let destroy _ (state : State.t) _ =
      Option.iter !state ~f:Controller.destroy;
      state := None
    ;;
  end)

(* Radix's viewport hides the native scrollbars with this stylesheet. *)
let hide_native_scrollbars =
  "[data-radix-scroll-area-viewport]{scrollbar-width:none;-ms-overflow-style:none;-webkit-overflow-scrolling:touch;}[data-radix-scroll-area-viewport]::-webkit-scrollbar{display:none}"
;;

let scroll_bar ?(attrs = []) ?class_ ?(orientation = Orientation.Vertical) () =
  Node.div
    ~attrs:
      ([ data "state" "hidden"
       ; data "orientation" (Orientation.name orientation)
       ; data "slot" "scroll-area-scrollbar"
       ; Cn.with_class
           ?class_
           [ "data-horizontal:h-2.5 data-horizontal:flex-col \
              data-horizontal:border-t data-horizontal:border-t-transparent \
              data-vertical:h-full data-vertical:w-2.5 data-vertical:border-l \
              data-vertical:border-l-transparent flex touch-none p-px \
              transition-colors select-none"
           ]
       ; Attr.create
           "style"
           (match orientation with
            | Vertical ->
              "position: absolute; top: 0px; right: 0px; bottom: \
               var(--radix-scroll-area-corner-height); display: none;"
            | Horizontal ->
              "position: absolute; bottom: 0px; left: 0px; right: \
               var(--radix-scroll-area-corner-width); display: none;")
       ]
       @ attrs)
    [ Node.div
        ~attrs:
          [ data "state" "hidden"
          ; data "slot" "scroll-area-thumb"
          ; Attr.class_ "rounded-full relative flex-1 bg-border"
          ; Attr.create
              "style"
              "width: var(--radix-scroll-area-thumb-width); height: \
               var(--radix-scroll-area-thumb-height);"
          ]
        []
    ]
;;

let root
      ?(attrs = [])
      ?class_
      ?(type_ = Type.Hover)
      ?(scroll_hide_delay = Time_ns.Span.of_int_ms 600)
      children
  =
  Node.div
    ~attrs:
      ([ Attr.create "dir" "ltr"
       ; data "slot" "scroll-area"
       ; Cn.with_class ?class_ [ "relative" ]
       ; Attr.create
           "style"
           "position: relative; --radix-scroll-area-corner-width: 0px; \
            --radix-scroll-area-corner-height: 0px;"
       ; Vdom.Attr.create_hook
           "loon-scroll-area"
           (Hook.create (type_, scroll_hide_delay))
       ]
       @ attrs)
    [ Node.create "style" [ Node.text hide_native_scrollbars ]
    ; Node.div
        ~attrs:
          [ Attr.create "data-radix-scroll-area-viewport" ""
          ; data "slot" "scroll-area-viewport"
          ; Attr.class_
              "size-full rounded-[inherit] transition-[color,box-shadow] \
               outline-none focus-visible:ring-[3px] \
               focus-visible:ring-ring/50 focus-visible:outline-1"
          ; Attr.create "style" "overflow: hidden;"
          ]
        [ Node.div
            ~attrs:[ Attr.create "style" "min-width: 100%; display: table;" ]
            children
        ]
    ; scroll_bar ()
    ]
;;
