open! Import
open Js_of_ocaml

module Direction = struct
  type t =
    | Top
    | Bottom
    | Left
    | Right
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Top -> "top"
    | Bottom -> "bottom"
    | Left -> "left"
    | Right -> "right"
  ;;

  (* vaul's own stylesheet slides the panel; these are tw-animate's
     equivalents. *)
  let animation = function
    | Top -> "data-[state=open]:slide-in-from-top data-[state=closed]:slide-out-to-top"
    | Bottom ->
      "data-[state=open]:slide-in-from-bottom data-[state=closed]:slide-out-to-bottom"
    | Left -> "data-[state=open]:slide-in-from-left data-[state=closed]:slide-out-to-left"
    | Right ->
      "data-[state=open]:slide-in-from-right data-[state=closed]:slide-out-to-right"
  ;;
end

type t =
  { modal : Modal.t
  ; direction : Direction.t
  }

let view ?id ?trigger_id ?(direction = Direction.Bottom) ~open_ ~on_open_change f =
  Modal.view ?id ?trigger_id ~open_ ~on_open_change (fun modal -> f { modal; direction })
;;

let component ?(default_open = false) ?direction f (local_ graph) =
  let open Bonsai.Let_syntax in
  let open_, set_open = Bonsai.state default_open graph in
  let id = Bonsai.path_id graph in
  let%arr open_ and set_open and id and f in
  view ~id ?direction ~open_ ~on_open_change:set_open f
;;

let trigger ?attrs t node = Modal.trigger ~slot:"drawer-trigger" ?attrs t.modal node
let close ?attrs t node = Modal.close ~slot:"drawer-close" ?attrs t.modal node

let overlay_class =
  "bg-black/10 supports-backdrop-filter:backdrop-blur-xs fixed inset-0 z-50 \
   data-open:animate-in data-open:fade-in-0 data-closed:animate-out \
   data-closed:fade-out-0"
;;

let content_class =
  [ "bg-popover text-popover-foreground flex h-auto flex-col text-sm \
     data-[vaul-drawer-direction=bottom]:rounded-t-xl \
     data-[vaul-drawer-direction=bottom]:border-t \
     data-[vaul-drawer-direction=left]:rounded-r-xl \
     data-[vaul-drawer-direction=left]:border-r \
     data-[vaul-drawer-direction=right]:rounded-l-xl \
     data-[vaul-drawer-direction=right]:border-l \
     data-[vaul-drawer-direction=top]:rounded-b-xl \
     data-[vaul-drawer-direction=top]:border-b group/drawer-content fixed z-50 \
     data-[vaul-drawer-direction=bottom]:inset-x-0 \
     data-[vaul-drawer-direction=bottom]:bottom-0 \
     data-[vaul-drawer-direction=bottom]:mt-24 \
     data-[vaul-drawer-direction=bottom]:max-h-[min(80dvh,calc(var(--vvh,100dvh)-var(--safe-t)-24px))] \
     data-[vaul-drawer-direction=left]:inset-y-0 \
     data-[vaul-drawer-direction=left]:left-0 data-[vaul-drawer-direction=left]:w-3/4 \
     data-[vaul-drawer-direction=right]:inset-y-0 \
     data-[vaul-drawer-direction=right]:right-0 \
     data-[vaul-drawer-direction=right]:w-3/4 \
     data-[vaul-drawer-direction=top]:inset-x-0 data-[vaul-drawer-direction=top]:top-0 \
     data-[vaul-drawer-direction=top]:mb-24 \
     data-[vaul-drawer-direction=top]:max-h-[min(80dvh,calc(var(--vvh,100dvh)-var(--safe-t)-24px))] \
     data-[vaul-drawer-direction=left]:sm:max-w-sm \
     data-[vaul-drawer-direction=right]:sm:max-w-sm"
  ]
;;

let handle_class =
  "bg-muted mt-4 h-1 w-[100px] rounded-full mx-auto hidden shrink-0 touch-none \
   group-data-[vaul-drawer-direction=bottom]/drawer-content:block"
;;

module Drag_rules = struct
  type release =
    | Close
    | Reset
  [@@deriving sexp_of]

  (* vaul's [dampenValue]. *)
  let dampen v = 8. *. (Float.log (v +. 1.) -. 2.)

  let follow ~height ~dragged_up =
    if Float.(dragged_up > 0.)
    then Float.min (-.dampen dragged_up) 0., None
    else (
      let down = Float.abs dragged_up in
      down, Some (1. -. (down /. height)))
  ;;

  let release ~height ~swipe ~dragged_up ~ms =
    let velocity = Float.abs dragged_up /. Float.max ms 1. in
    if Float.(dragged_up > 0.)
    then Reset
    else if Float.(velocity > 0.4 || swipe >= height *. 0.25)
    then Close
    else Reset
  ;;
end

(* Dragging a bottom drawer, as vaul's: the panel follows the pointer down
   (the overlay fading), barely up, and closes when let go a quarter of its
   height down or flicked down, else springs back. As vaul's [shouldDrag],
   nothing drags in the first 500ms, from a [select] or [data-vaul-no-drag],
   with text selected, or from inside a scrolled element; a drag that starts
   upwards does not move it. *)
module Drag = Vdom.Attr.Hooks.Make (struct
    module Input = struct
      type t = unit Effect.t

      let sexp_of_t _ = Sexp.Atom "<close>"
      let combine _ second = second
    end

    module State = struct
      type t =
        { mutable close : Input.t
        ; mutable listeners : Dom.event_listener_id list
        }
    end

    let init close _ = { State.close; listeners = [] }
    let now () = Js.to_float (new%js Js.date_now)##getTime
    let transition = "0.5s cubic-bezier(0.32, 0.72, 0, 1)"

    let no_drag (target : Dom_html.element Js.t) =
      String.equal (Js.to_string target##.tagName) "SELECT"
      || Js.Opt.test
           ((Js.Unsafe.coerce target)##closest (Js.string "[data-vaul-no-drag]")
            : Dom_html.element Js.t Js.opt)
    ;;

    let selection () =
      try
        Js.to_string
          (Js.Unsafe.meth_call
             (Js.Unsafe.meth_call Dom_html.window "getSelection" [||])
             "toString"
             [||])
      with
      | _ -> ""
    ;;

    let on_handle_or_header (target : Dom_html.element Js.t) =
      Js.Opt.test
        ((Js.Unsafe.coerce target)##closest
           (Js.string "[data-slot=drawer-handle], [data-slot=drawer-header]")
         : Dom_html.element Js.t Js.opt)
    ;;

    let at_scroll_top (target : Dom_html.element Js.t) (root : Dom_html.element Js.t) =
      let rec climb (el : Dom_html.element Js.t) =
        if el##.scrollHeight > el##.clientHeight && Float.(Js.to_float el##.scrollTop > 0.)
        then false
        else if phys_equal el root
        then true
        else if
          Js.Opt.case
            (el##getAttribute (Js.string "role"))
            (fun () -> false)
            (fun r -> String.equal (Js.to_string r) "dialog")
        then true
        else (
          match Js.Opt.to_option (Js.Opt.bind el##.parentNode Dom_html.CoerceTo.element) with
          | Some parent -> climb parent
          | None -> true)
      in
      climb target
    ;;

    let drag_slop = 6.

    (* vaul's [shouldDrag], for a bottom drawer not yet moved. *)
    let should_drag (target : Dom_html.element Js.t) (root : Dom_html.element Js.t) ~opened ~dragged_up =
      if Float.(now () -. opened < 500.)
      then false
      else if not (String.is_empty (selection ()))
      then false
      else if Float.(dragged_up > 0.)
      then false
      else at_scroll_top target root
    ;;

    let on_mount =
      `Schedule_immediately_after_this_dom_patch_completes
        (fun _ (state : State.t) (el : Dom_html.element Js.t) ->
          let opened = now () in
          (* The press: pointer id, y, time, target and whether it started on the handle/header. *)
          let press = ref None in
          let allowed = ref false in
          let swipe = ref 0. in
          let overlay () =
            Js.Opt.to_option
              (Js.Opt.bind el##.parentNode (fun parent ->
                 (Js.Unsafe.coerce parent)##querySelector
                   (Js.string "[data-slot=drawer-overlay]")))
          in
          let set_y ~animate y =
            swipe := y;
            el##.style##.transition
            := Js.string (if animate then "transform " ^ transition else "none");
            el##.style##.transform := Js.string (sprintf "translate3d(0, %gpx, 0)" y)
          in
          let set_opacity ~animate o =
            Option.iter (overlay ()) ~f:(fun (ov : Dom_html.element Js.t) ->
              ov##.style##.transition
              := Js.string (if animate then "opacity " ^ transition else "none");
              ov##.style##.opacity := Js.string (sprintf "%g" o))
          in
          let clear_styles () =
            swipe := 0.;
            el##.style##.transition := Js.string "";
            el##.style##.transform := Js.string "";
            Option.iter (overlay ()) ~f:(fun (ov : Dom_html.element Js.t) ->
              ov##.style##.transition := Js.string "";
              ov##.style##.opacity := Js.string "")
          in
          let reset () =
            set_y ~animate:true 0.;
            set_opacity ~animate:true 1.
          in
          let down =
            Dom_html.addEventListener
              el
              Dom_html.Event.pointerdown
              (Dom_html.handler (fun (ev : Dom_html.pointerEvent Js.t) ->
                 Js.Opt.iter ev##.target (fun target ->
                   if ev##.button = 0 && not (no_drag target)
                   then (
                     let on_handle = on_handle_or_header target in
                     if on_handle || at_scroll_top target el
                     then (
                       press := Some (ev##.pointerId, Js.to_float ev##.clientY, now (), target, on_handle);
                       allowed := false)));
                 Js._true))
              Js._false
          in
          let move =
            Dom_html.addEventListener
              el
              Dom_html.Event.pointermove
              (Dom_html.handler (fun (ev : Dom_html.pointerEvent Js.t) ->
                 (match !press with
                  | Some (pid, y0, _, target, on_handle) when pid = ev##.pointerId ->
                    let dy = Js.to_float ev##.clientY -. y0 in
                    let dragged_up = -.dy in
                    if not !allowed
                    then (
                      if Float.(dy < -.drag_slop) && not on_handle
                      then press := None
                      else if
                        Float.(dy > drag_slop)
                        && (on_handle || should_drag target el ~opened ~dragged_up)
                      then (
                        allowed := true;
                        (try (Js.Unsafe.coerce el)##setPointerCapture ev##.pointerId with
                         | _ -> ());
                        let height = Float.of_int el##.offsetHeight in
                        let y, opacity = Drag_rules.follow ~height ~dragged_up in
                        set_y ~animate:false y;
                        Option.iter opacity ~f:(set_opacity ~animate:false)))
                    else (
                      let height = Float.of_int el##.offsetHeight in
                      let y, opacity = Drag_rules.follow ~height ~dragged_up in
                      set_y ~animate:false y;
                      Option.iter opacity ~f:(set_opacity ~animate:false))
                  | _ -> ());
                 Js._true))
              Js._false
          in
          let finish ~cancel (ev : Dom_html.pointerEvent Js.t) =
            match !press with
            | Some (pid, y0, t0, _, _) when pid = ev##.pointerId ->
              press := None;
              let was_allowed = !allowed in
              allowed := false;
              if was_allowed
              then (
                try (Js.Unsafe.coerce el)##releasePointerCapture ev##.pointerId with
                | _ -> ());
              if cancel
              then (if was_allowed || Float.(!swipe <> 0.) then reset ())
              else if was_allowed && Float.(!swipe <> 0.)
              then (
                let height = Float.of_int el##.offsetHeight in
                match
                  Drag_rules.release
                    ~height
                    ~swipe:!swipe
                    ~dragged_up:(y0 -. Js.to_float ev##.clientY)
                    ~ms:(now () -. t0)
                with
                | Close ->
                  clear_styles ();
                  Effect.Expert.handle_non_dom_event_exn state.close
                | Reset -> reset ())
            | _ -> ()
          in
          let up =
            Dom_html.addEventListener
              el
              Dom_html.Event.pointerup
              (Dom_html.handler (fun ev ->
                 finish ~cancel:false ev;
                 Js._true))
              Js._false
          in
          let cancel =
            Dom_html.addEventListener
              el
              (Dom_html.Event.make "pointercancel")
              (Dom_html.handler (fun ev ->
                 finish ~cancel:true ev;
                 Js._true))
              Js._false
          in
          state.listeners <- [ down; move; up; cancel ])
    ;;

    let update ~old_input:_ ~new_input (state : State.t) _ = state.close <- new_input

    let destroy _ (state : State.t) _ =
      List.iter state.listeners ~f:Dom_html.removeEventListener
    ;;
  end)

let content ?(attrs = []) ?class_ t children =
  let m = t.modal in
  Modal.content
    ~auto_focus:false
    ~overlay:(Modal.overlay ~slot:"drawer-overlay" ~class_:overlay_class m)
    ~content_attrs:
      ([ data "slot" "drawer-content"
       ; data "vaul-drawer-direction" (Direction.name t.direction)
       ; Cn.with_class
           ?class_
           (content_class
            @ [ "data-[state=open]:animate-in data-[state=closed]:animate-out \
                 duration-500 ease-[cubic-bezier(0.32,0.72,0,1)]"
              ; Direction.animation t.direction
              ; (* vaul's stylesheet: no text selected by a drag. *)
                "[@media(hover:hover)_and_(pointer:fine)]:select-none"
              ])
       ]
       @ (match t.direction with
          | Bottom ->
            [ Vdom.Attr.create_hook "loon-drawer-drag" (Drag.create (m.on_open_change false))
            ; Attr.style (Css_gen.create ~field:"touch-action" ~value:"pan-y")
            ]
          | Top | Left | Right -> [])
       @ attrs)
    m
    (Node.div ~attrs:[ data "slot" "drawer-handle"; Attr.class_ handle_class ] [] :: children)
;;

let header ?(attrs = []) ?class_ children =
  Node.div
    ~attrs:
      ([ data "slot" "drawer-header"
       ; Cn.with_class
           ?class_
           [ "gap-0.5 p-4 md:gap-0.5 md:text-left flex flex-col touch-none \
              group-data-[vaul-drawer-direction=bottom]/drawer-content:text-center \
              group-data-[vaul-drawer-direction=top]/drawer-content:text-center"
           ]
       ]
       @ attrs)
    children
;;

let footer ?(attrs = []) ?class_ children =
  Node.div
    ~attrs:
      ([ data "slot" "drawer-footer"
       ; Cn.with_class ?class_ [ "gap-2 p-4 mt-auto flex flex-col" ]
       ]
       @ attrs)
    children
;;

let title ?attrs ?class_ t children =
  Modal.title
    ~slot:"drawer-title"
    ~class_:(Cn.cn [ "text-foreground text-base font-medium font-heading"; Option.value class_ ~default:"" ])
    ?attrs
    t.modal
    children
;;

let description ?attrs ?class_ t children =
  Modal.description
    ~slot:"drawer-description"
    ~class_:(Cn.cn [ "text-muted-foreground text-sm"; Option.value class_ ~default:"" ])
    ?attrs
    t.modal
    children
;;
