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

type panel =
  { attrs : Attr.t list
  ; class_ : string option
  ; id : string option
  ; default_size : float option
  ; min_size : float
  ; max_size : float
  ; children : Node.t list
  }

type handle =
  { attrs : Attr.t list
  ; class_ : string option
  ; with_handle : bool
  ; disabled : bool
  }

type child =
  | Panel of panel
  | Handle of handle

let panel
      ?(attrs = [])
      ?class_
      ?id
      ?default_size
      ?(min_size = 0.)
      ?(max_size = 100.)
      children
  =
  Panel { attrs; class_; id; default_size; min_size; max_size; children }
;;

let handle ?(attrs = []) ?class_ ?(with_handle = false) ?(disabled = false) () =
  Handle { attrs; class_; with_handle; disabled }
;;

let panels children =
  List.filter_map children ~f:(function
    | Panel p -> Some p
    | Handle _ -> None)
;;

let clamp (p : panel) size =
  Float.clamp_exn size ~min:p.min_size ~max:p.max_size
;;

let default_layout children =
  let panels = panels children in
  let given =
    List.sum
      (module Float)
      panels
      ~f:(fun p -> Option.value p.default_size ~default:0.)
  in
  let unsized = List.count panels ~f:(fun p -> Option.is_none p.default_size) in
  let share =
    if unsized = 0
    then 0.
    else Float.max 0. (100. -. given) /. Float.of_int unsized
  in
  let sizes =
    List.map panels ~f:(fun p -> Option.value p.default_size ~default:share)
  in
  let total = List.sum (module Float) sizes ~f:Fn.id in
  let sizes =
    if unsized = 0 && Float.(total > 0.)
    then List.map sizes ~f:(fun s -> s *. 100. /. total)
    else sizes
  in
  (* Fitted within the limits, what that frees or takes goes to the others
     in order, as far as their limits allow. *)
  let sizes = Array.of_list (List.map2_exn panels sizes ~f:clamp) in
  let panels = Array.of_list panels in
  let (_ : float) =
    Array.foldi
      sizes
      ~init:(100. -. Array.sum (module Float) sizes ~f:Fn.id)
      ~f:(fun i left size ->
        let next = clamp panels.(i) (size +. left) in
        sizes.(i) <- next;
        left -. (next -. size))
  in
  Array.to_list sizes
;;

let layout_for children layout =
  if List.length layout = List.length (panels children)
  then layout
  else default_layout children
;;

let adjust children ~layout ~pivot ~delta =
  let panels = Array.of_list (panels children) in
  let sizes = Array.of_list (layout_for children layout) in
  let n = Array.length sizes in
  if pivot < 0 || pivot + 1 >= n || Float.(delta = 0.)
  then Array.to_list sizes
  else (
    (* The panel that grows, and those that shrink in turn, nearest first. *)
    let grow, shrink =
      if Float.(delta > 0.)
      then pivot, List.range (pivot + 1) n
      else pivot + 1, List.range ~stride:(-1) pivot (-1) ~stop:`exclusive
    in
    let room = panels.(grow).max_size -. sizes.(grow) in
    let give =
      List.sum
        (module Float)
        shrink
        ~f:(fun i -> sizes.(i) -. panels.(i).min_size)
    in
    let amount =
      Float.max 0. (Float.min (Float.abs delta) (Float.min room give))
    in
    sizes.(grow) <- sizes.(grow) +. amount;
    let (_ : float) =
      List.fold shrink ~init:amount ~f:(fun left i ->
        let take = Float.min left (sizes.(i) -. panels.(i).min_size) in
        sizes.(i) <- sizes.(i) -. take;
        left -. take)
    in
    Array.to_list sizes)
;;

let number x = sprintf "%g" (Float.round_decimal x ~decimal_digits:3)

(* A drag in progress, by group id: where it started. *)
type drag =
  { pivot : int
  ; start : float
  ; start_layout : float list
  ; size : float
  }

let drags : drag String.Table.t = String.Table.create ()

(* By group id: the layout rendered when a key last moved a handle, and the
   layout that asked for. *)
let pending : (float list * float list) String.Table.t = String.Table.create ()

let set_state (el : Dom_html.element Js.t) state =
  el##setAttribute (Js.string "data-separator") (Js.string state)
;;

let current_target (ev : #Dom_html.event Js.t) : Dom_html.element Js.t option =
  Js.Opt.to_option (Js.Unsafe.coerce ev)##.currentTarget
;;

let body_style field value =
  let style = (Js.Unsafe.coerce Dom_html.document##.body)##.style in
  (style##setProperty (Js.string field) (Js.string value) : unit)
;;

let in_browser () = Js.Optdef.test (Js.Unsafe.global##.document : _ Js.Optdef.t)

let view
      ?(attrs = [])
      ?class_
      ?(id = "resizable")
      ~orientation
      ~layout
      ~on_layout_change
      children
  =
  let layout = layout_for children layout in
  let horizontal = Orientation.equal orientation Horizontal in
  let panel_list = Array.of_list (panels children) in
  let panel_id k =
    match panel_list.(k).id with
    | Some id -> id
    | None -> sprintf "%s-panel-%d" id k
  in
  let pointer_pos (ev : Dom_html.pointerEvent Js.t) =
    if horizontal then Js.to_float ev##.clientX else Js.to_float ev##.clientY
  in
  let render_handle (h : handle) ~index ~pivot =
    let size = List.nth layout pivot in
    let aria_values =
      match size with
      | None -> []
      | Some now ->
        let p = panel_list.(pivot) in
        let at target =
          List.nth_exn
            (adjust children ~layout ~pivot ~delta:(target -. now))
            pivot
        in
        [ aria "controls" (panel_id pivot)
        ; aria "valuemax" (number (at p.max_size))
        ; aria "valuemin" (number (at p.min_size))
        ; aria "valuenow" (number now)
        ]
    in
    let move delta =
      Effect.lazy_
        (lazy
          (let base =
             (* Keys pressed before the last change shows build on it. *)
             match Hashtbl.find pending id with
             | Some (rendered, latest) when phys_equal rendered layout -> latest
             | _ -> layout
           in
           let next = adjust children ~layout:base ~pivot ~delta in
           Hashtbl.set pending ~key:id ~data:(layout, next);
           on_layout_change next))
    in
    let on_keydown ev =
      let step d = Effect.Many [ Dom_util.prevent_default ev; move d ] in
      match Dom_util.key ev with
      | ArrowRight when horizontal -> step 5.
      | ArrowLeft when horizontal -> step (-5.)
      | ArrowDown when not horizontal -> step 5.
      | ArrowUp when not horizontal -> step (-5.)
      | ArrowLeft | ArrowRight | ArrowUp | ArrowDown ->
        Dom_util.prevent_default ev
      | Home -> step (-100.)
      | End -> step 100.
      | F6 ->
        (* Cycles focus through the group's handles. *)
        Effect.Many
          [ Dom_util.prevent_default ev
          ; Effect.of_thunk (fun () ->
              match current_target ev with
              | None -> ()
              | Some el ->
                Js.Opt.iter el##.parentNode (fun group ->
                  let handles =
                    Dom.list_of_nodeList
                      ((Js.Unsafe.coerce group)##querySelectorAll
                         (Js.string ":scope > [role=separator][tabindex]"))
                  in
                  let n = List.length handles in
                  match
                    List.findi handles ~f:(fun _ h ->
                      phys_equal (h : Dom_html.element Js.t) el)
                  with
                  | None -> ()
                  | Some (i, _) ->
                    let shift = Js.to_bool ev##.shiftKey in
                    let j = (i + if shift then n - 1 else 1) % n in
                    (List.nth_exn handles j)##focus))
          ]
      | _ -> Effect.Ignore
    in
    let on_pointerdown (ev : Dom_html.pointerEvent Js.t) =
      if ev##.button <> 0
      then Effect.Ignore
      else (
        match current_target ev with
        | None -> Effect.Ignore
        | Some el ->
          Effect.Many
            [ Dom_util.prevent_default ev
            ; Effect.of_thunk (fun () ->
                Js.Opt.iter el##.parentNode (fun group ->
                  let rect = (Js.Unsafe.coerce group)##getBoundingClientRect in
                  let size =
                    Js.to_float
                      (if horizontal then rect##.width else rect##.height)
                  in
                  Hashtbl.set
                    drags
                    ~key:id
                    ~data:
                      { pivot
                      ; start = pointer_pos ev
                      ; start_layout = layout
                      ; size
                      });
                (try
                   (Js.Unsafe.coerce el)##setPointerCapture ev##.pointerId
                 with
                 | _ -> ());
                el##focus;
                set_state el "active";
                body_style
                  "cursor"
                  (if horizontal then "ew-resize" else "ns-resize");
                body_style "user-select" "none")
            ])
    in
    let on_pointermove (ev : Dom_html.pointerEvent Js.t) =
      match Hashtbl.find drags id with
      | Some drag when drag.pivot = pivot && Float.(drag.size > 0.) ->
        let delta = (pointer_pos ev -. drag.start) /. drag.size *. 100. in
        on_layout_change
          (adjust children ~layout:drag.start_layout ~pivot ~delta)
      | _ -> Effect.Ignore
    in
    let on_pointerup (ev : Dom_html.pointerEvent Js.t) =
      Effect.of_thunk (fun () ->
        if Hashtbl.mem drags id
        then (
          Hashtbl.remove drags id;
          body_style "cursor" "";
          body_style "user-select" "";
          (* As react-resizable-panels: the handle lets go of the focus
             it took on the pointer down. *)
          Option.iter (current_target ev) ~f:(fun el ->
            el##blur;
            set_state el "inactive")))
    in
    let cursor = if horizontal then "ew-resize" else "ns-resize" in
    (* Hovering a handle sets the page's cursor, as react-resizable-panels'
       global cursor style. *)
    let hover_cursor value (_ : Dom_html.pointerEvent Js.t) =
      Effect.of_thunk (fun () ->
        if in_browser () && not (Hashtbl.mem drags id) then body_style "cursor" value)
    in
    let dom_state state =
      fun (ev : #Dom_html.event Js.t) ->
      Effect.of_thunk (fun () ->
        if not (Hashtbl.mem drags id)
        then
          Option.iter (current_target ev) ~f:(fun el -> set_state el (state el)))
    in
    Node.div
      ~attrs:
        ([ Attr.role "separator"
         ; Attr.id (sprintf "%s-handle-%d" id index)
         ; aria "orientation" (if horizontal then "vertical" else "horizontal")
         ; data "slot" "resizable-handle"
         ; data "separator" (if h.disabled then "disabled" else "inactive")
         ; Cn.with_class
             ?class_:h.class_
             [ "relative flex w-px items-center justify-center bg-border \
                ring-offset-background after:absolute after:inset-y-0 after:left-1/2 \
                after:w-1 after:-translate-x-1/2 focus-visible:ring-1 \
                focus-visible:ring-ring focus-visible:outline-hidden \
                aria-[orientation=horizontal]:h-px aria-[orientation=horizontal]:w-full \
                aria-[orientation=horizontal]:after:left-0 \
                aria-[orientation=horizontal]:after:h-1 \
                aria-[orientation=horizontal]:after:w-full \
                aria-[orientation=horizontal]:after:translate-x-0 \
                aria-[orientation=horizontal]:after:-translate-y-1/2 \
                [&[aria-orientation=horizontal]>div]:rotate-90"
             ]
         ; Attr.style
             (Css_gen.concat
                [ Css_gen.create ~field:"flex-basis" ~value:"auto"
                ; Css_gen.create
                    ~field:"cursor"
                    ~value:
                      (if h.disabled
                       then "not-allowed"
                       else if horizontal
                       then "ew-resize"
                       else "ns-resize")
                ; Css_gen.create ~field:"flex-grow" ~value:"0"
                ; Css_gen.create ~field:"flex-shrink" ~value:"0"
                ; Css_gen.create ~field:"touch-action" ~value:"none"
                ])
         ]
         @ aria_values
         @ (if h.disabled
            then [ aria "disabled" "true" ]
            else
              [ Attr.tabindex 0
              ; Attr.on_keydown on_keydown
              ; Attr.on_pointerdown on_pointerdown
              ; Attr.on_pointermove on_pointermove
              ; Attr.on_pointerup on_pointerup
              ; Attr.on_pointerenter (dom_state (fun _ -> "hover"))
              ; Attr.on_pointerenter (hover_cursor cursor)
              ; Attr.on_pointerleave (hover_cursor "")
              ; Attr.on_pointerleave
                  (dom_state (fun el ->
                     if
                       in_browser ()
                       && Js.to_bool
                            ((Js.Unsafe.coerce el)##matches
                               (Js.string ":focus"))
                     then "focus"
                     else "inactive"))
              ; Attr.on_focus (dom_state (fun _ -> "focus"))
              ; Attr.on_blur (dom_state (fun _ -> "inactive"))
              ])
         @ h.attrs)
      (if h.with_handle
       then
         [ Node.div ~attrs:[ Attr.class_ "bg-border h-6 w-1 rounded-lg z-10 flex \
                                          shrink-0" ] [] ]
       else [])
  in
  let render_panel (p : panel) ~k =
    let size = Option.value (List.nth layout k) ~default:0. in
    Node.div
      ~attrs:
        ([ data "slot" "resizable-panel"
         ; data "panel" "true"
         ; Attr.id (panel_id k)
         ; Attr.style
             (Css_gen.concat
                (* As react-resizable-panels: without [min-*: 0] a flex item
                   does not shrink below its content. *)
                [ Css_gen.create ~field:"min-height" ~value:"0"
                ; Css_gen.create ~field:"max-height" ~value:"100%"
                ; Css_gen.create ~field:"height" ~value:"auto"
                ; Css_gen.create ~field:"min-width" ~value:"0"
                ; Css_gen.create ~field:"max-width" ~value:"100%"
                ; Css_gen.create ~field:"width" ~value:"auto"
                ; Css_gen.create ~field:"border" ~value:"0"
                ; Css_gen.create ~field:"padding" ~value:"0"
                ; Css_gen.create ~field:"margin" ~value:"0"
                ; Css_gen.create ~field:"display" ~value:"flex"
                ; Css_gen.create ~field:"flex-basis" ~value:"0"
                ; Css_gen.create ~field:"flex-shrink" ~value:"1"
                ; Css_gen.create ~field:"overflow" ~value:"visible"
                ; Css_gen.create ~field:"flex-grow" ~value:(number size)
                ])
         ]
         @ p.attrs)
      [ Node.div
          ~attrs:
            ([ Attr.style
                 (Css_gen.concat
                    [ Css_gen.create ~field:"max-height" ~value:"100%"
                    ; Css_gen.create ~field:"max-width" ~value:"100%"
                    ; Css_gen.create ~field:"flex-grow" ~value:"1"
                    ; Css_gen.create ~field:"overflow" ~value:"auto"
                    ; Css_gen.create
                        ~field:"touch-action"
                        ~value:(if horizontal then "pan-y" else "pan-x")
                    ])
             ]
             @ Option.to_list (Option.map p.class_ ~f:Attr.class_))
          p.children
      ]
  in
  let _, _, nodes =
    List.fold children ~init:(0, 0, []) ~f:(fun (k, index, acc) child ->
      match child with
      | Panel p -> k + 1, index, render_panel p ~k :: acc
      | Handle h -> k, index + 1, render_handle h ~index ~pivot:(k - 1) :: acc)
  in
  Node.div
    ~attrs:
      ([ data "slot" "resizable-panel-group"
       ; data "group" ""
       ; aria "orientation" (Orientation.name orientation)
       ; Attr.id id
       ; Cn.with_class
           ?class_
           [ "flex h-full w-full aria-[orientation=vertical]:flex-col" ]
       ; Attr.style
           (Css_gen.concat
              [ Css_gen.create ~field:"height" ~value:"100%"
              ; Css_gen.create ~field:"width" ~value:"100%"
              ; Css_gen.create ~field:"overflow" ~value:"hidden"
              ; Css_gen.create ~field:"display" ~value:"flex"
              ; Css_gen.create
                  ~field:"flex-direction"
                  ~value:(if horizontal then "row" else "column")
              ; Css_gen.create ~field:"flex-wrap" ~value:"nowrap"
              ; Css_gen.create
                  ~field:"touch-action"
                  ~value:(if horizontal then "pan-y" else "pan-x")
              ])
       ]
       @ attrs)
    (List.rev nodes)
;;

let component ?attrs ?class_ ~orientation children (local_ graph) =
  let open Bonsai.Let_syntax in
  let layout, set_layout = Bonsai.state [] graph ~equal:[%equal: float list] in
  let id = Bonsai.path_id graph in
  let%arr layout
  and set_layout
  and id
  and children in
  view
    ?attrs
    ?class_
    ~id
    ~orientation
    ~layout
    ~on_layout_change:set_layout
    children
;;
