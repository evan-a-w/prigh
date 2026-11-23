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

let decimals step =
  match String.lsplit2 (Float.to_string step) ~on:'.' with
  | None -> 0
  | Some (_, frac) ->
    (match String.lsplit2 frac ~on:'e' with
     | Some _ -> 10
     | None -> String.length frac)
;;

let snap ~min ~max ~step v =
  let v = (Float.round_nearest ((v -. min) /. step) *. step) +. min in
  Float.clamp_exn (Float.round_decimal v ~decimal_digits:(decimals step)) ~min ~max
;;

let update values ~index ~value ~min ~max ~step ~min_steps_between_thumbs =
  let value = snap ~min ~max ~step value in
  let next =
    List.mapi values ~f:(fun i v -> if i = index then value else v)
    |> List.sort ~compare:Float.compare
  in
  let min_gap = Float.of_int min_steps_between_thumbs *. step in
  let rec far_enough = function
    | a :: (b :: _ as rest) -> Float.(b -. a >= min_gap -. 1e-9) && far_enough rest
    | _ -> true
  in
  if far_enough next
  then (
    let index =
      List.findi next ~f:(fun _ v -> Float.equal v value) |> Option.value_map ~default:index ~f:fst
    in
    Some (next, index))
  else None
;;

let fmt x = Float.to_string_hum ~decimals:6 ~strip_zero:true ~delimiter:' ' x
let percent ~min ~max v = if Float.equal max min then 0. else (v -. min) /. (max -. min) *. 100.

(* Radix keeps a thumb inside the track: offset by half its width (16px) at
   the ends. *)
let in_bounds_offset p = 8. -. (p *. 8. /. 50.)

let label ~index ~count =
  if count > 2
  then Some (sprintf "Value %d of %d" (index + 1) count)
  else if count = 2
  then Some (if index = 0 then "Minimum" else "Maximum")
  else None
;;

(* Pointer dragging: the root captures the pointer and remembers which
   thumb it moves. *)
let thumbs (root : Dom_html.element Js.t) =
  Dom.list_of_nodeList (root##querySelectorAll (Js.string "[role=slider]"))
;;

let focus_thumb root index =
  Effect.of_thunk (fun () ->
    Option.iter (List.nth (thumbs root) index) ~f:(fun el -> el##focus))
;;

let view
  ?(attrs = [])
  ?class_
  ?(min = 0.)
  ?(max = 100.)
  ?(step = 1.)
  ?(min_steps_between_thumbs = 0)
  ?(orientation = Orientation.Horizontal)
  ?(disabled = false)
  ~value:values
  ~on_change
  ()
  =
  let orientation_name = Orientation.name orientation in
  let o = data "orientation" orientation_name in
  let disabled_attrs = if disabled then [ data "disabled" "" ] else [] in
  let count = List.length values in
  let move ~index v =
    match update values ~index ~value:v ~min ~max ~step ~min_steps_between_thumbs with
    | None -> None
    | Some (next, index) ->
      Some ((if [%equal: float list] next values then Effect.Ignore else on_change next), index)
  in
  let value_at (root : Dom_html.element Js.t) (ev : Dom_html.pointerEvent Js.t) =
    let r = root##getBoundingClientRect in
    let p =
      match orientation with
      | Horizontal ->
        (Js.to_float ev##.clientX -. Js.to_float r##.left)
        /. Float.max 1. (Js.to_float r##.right -. Js.to_float r##.left)
      | Vertical ->
        (Js.to_float r##.bottom -. Js.to_float ev##.clientY)
        /. Float.max 1. (Js.to_float r##.bottom -. Js.to_float r##.top)
    in
    min +. (Float.clamp_exn p ~min:0. ~max:1. *. (max -. min))
  in
  let current_root (ev : Dom_html.pointerEvent Js.t) : Dom_html.element Js.t =
    Js.Unsafe.get ev "currentTarget"
  in
  let on_pointerdown (ev : Dom_html.pointerEvent Js.t) =
    if disabled || count = 0 || ev##.button <> 0
    then Effect.Ignore
    else (
      let root = current_root ev in
      let v = value_at root ev in
      let index =
        List.mapi values ~f:(fun i x -> i, Float.abs (x -. v))
        |> List.min_elt ~compare:(fun (_, a) (_, b) -> Float.compare a b)
        |> Option.value_map ~default:0 ~f:fst
      in
      let pointer_id : int = Js.Unsafe.get ev "pointerId" in
      Js.Unsafe.set root "loonSliderThumb" index;
      ignore (Js.Unsafe.meth_call root "setPointerCapture" [| Js.Unsafe.inject pointer_id |] : unit);
      match move ~index v with
      | None -> Effect.Many [ Dom_util.prevent_default ev; focus_thumb root index ]
      | Some (eff, index) ->
        Js.Unsafe.set root "loonSliderThumb" index;
        Effect.Many [ Dom_util.prevent_default ev; eff; focus_thumb root index ])
  in
  let on_pointermove (ev : Dom_html.pointerEvent Js.t) =
    let root = current_root ev in
    let pointer_id : int = Js.Unsafe.get ev "pointerId" in
    if disabled
       || not
            (Js.to_bool
               (Js.Unsafe.meth_call root "hasPointerCapture" [| Js.Unsafe.inject pointer_id |]))
    then Effect.Ignore
    else (
      let index : int = Js.Unsafe.get root "loonSliderThumb" in
      match move ~index (value_at root ev) with
      | None -> Effect.Ignore
      | Some (eff, next_index) ->
        Js.Unsafe.set root "loonSliderThumb" next_index;
        if next_index = index then eff else Effect.Many [ eff; focus_thumb root next_index ])
  in
  let on_pointerup (ev : Dom_html.pointerEvent Js.t) =
    let root = current_root ev in
    let pointer_id : int = Js.Unsafe.get ev "pointerId" in
    Effect.of_thunk (fun () ->
      if Js.to_bool
           (Js.Unsafe.meth_call root "hasPointerCapture" [| Js.Unsafe.inject pointer_id |])
      then
        ignore
          (Js.Unsafe.meth_call root "releasePointerCapture" [| Js.Unsafe.inject pointer_id |]
           : unit))
  in
  let percents = List.map values ~f:(percent ~min ~max) in
  let start =
    if count > 1
    then Option.value (List.min_elt percents ~compare:Float.compare) ~default:0.
    else 0.
  in
  let end_ =
    100. -. Option.value (List.max_elt percents ~compare:Float.compare) ~default:0.
  in
  let range_style =
    match orientation with
    | Horizontal ->
      Css_gen.concat
        [ Css_gen.create ~field:"left" ~value:(fmt start ^ "%")
        ; Css_gen.create ~field:"right" ~value:(fmt end_ ^ "%")
        ]
    | Vertical ->
      Css_gen.concat
        [ Css_gen.create ~field:"bottom" ~value:(fmt start ^ "%")
        ; Css_gen.create ~field:"top" ~value:(fmt end_ ^ "%")
        ]
  in
  let thumb index v =
    let p = percent ~min ~max v in
    let on_keydown ev =
      let big = step *. 10. in
      let shift = Js.to_bool ev##.shiftKey in
      let by d = Some (v +. if shift then d *. 10. else d) in
      let target =
        match Dom_util.key ev with
        | Home -> Some min
        | End -> Some max
        | PageUp -> Some (v +. big)
        | PageDown -> Some (v -. big)
        | ArrowRight | ArrowUp -> by step
        | ArrowLeft | ArrowDown -> by (-.step)
        | _ -> None
      in
      match target with
      | None -> Effect.Ignore
      | Some target ->
        (match move ~index target with
         | None -> Dom_util.prevent_default ev
         | Some (eff, next_index) ->
           let refocus =
             if next_index = index
             then Effect.Ignore
             else
               (* The thumb that moved has a new index: focus follows it.
                  (Tests' fake events have no [currentTarget].) *)
               Effect.of_thunk (fun () ->
                 match (Js.Unsafe.get ev "currentTarget" : Dom_html.element Js.t) with
                 | exception _ -> ()
                 | el ->
                   Js.Opt.iter
                     (Js.Unsafe.meth_call
                        el
                        "closest"
                        [| Js.Unsafe.inject (Js.string "[data-slot=slider]") |])
                     (fun root ->
                       Option.iter (List.nth (thumbs root) next_index) ~f:(fun el ->
                         el##focus)))
           in
           Effect.Many [ Dom_util.prevent_default ev; eff; refocus ])
    in
    Node.span
      ~attrs:
        [ Attr.style
            (Css_gen.concat
               [ Css_gen.create ~field:"transform" ~value:"var(--radix-slider-thumb-transform)"
               ; Css_gen.create ~field:"position" ~value:"absolute"
               ; Css_gen.create
                   ~field:
                     (match orientation with
                      | Horizontal -> "left"
                      | Vertical -> "bottom")
                   ~value:(sprintf "calc(%s%% + %spx)" (fmt p) (fmt (in_bounds_offset p)))
               ])
        ]
      [ Node.span
          ~attrs:
            ([ Attr.role "slider"
             ; aria "valuemin" (fmt min)
             ; aria "valuemax" (fmt max)
             ; aria "orientation" orientation_name
             ; o
             ; data "slot" "slider-thumb"
             ; aria "valuenow" (fmt v)
             ; Attr.class_
                 "border-ring ring-ring/50 relative size-3 rounded-full border bg-white \
                  transition-[color,box-shadow] after:absolute after:-inset-2 \
                  hover:ring-3 focus-visible:ring-3 focus-visible:outline-hidden \
                  active:ring-3 block shrink-0 select-none disabled:pointer-events-none \
                  disabled:opacity-50"
             ]
             @ Option.value_map (label ~index ~count) ~default:[] ~f:(fun l ->
               [ aria "label" l ])
             @ disabled_attrs
             @ if disabled then [] else [ Attr.tabindex 0; Attr.on_keydown on_keydown ])
          []
      ]
  in
  Node.span
    ~attrs:
      ([ Attr.create "dir" "ltr"
       ; o
       ; aria_bool "disabled" disabled
       ]
       @ disabled_attrs
       @ [ data "slot" "slider"
         ; Cn.with_class
             ?class_
             [ "data-vertical:min-h-40 relative flex w-full touch-none items-center \
                select-none data-disabled:opacity-50 data-vertical:h-full \
                data-vertical:w-auto data-vertical:flex-col"
             ]
         ; Attr.css_var
             ~name:"radix-slider-thumb-transform"
             (match orientation with
              | Horizontal -> "translateX(-50%)"
              | Vertical -> "translateY(50%)")
         ; Attr.on_pointerdown on_pointerdown
         ; Attr.on_pointermove on_pointermove
         ; Attr.on_pointerup on_pointerup
         ]
       @ attrs)
    (Node.span
       ~attrs:
         ([ o; data "slot" "slider-track" ]
          @ disabled_attrs
          @ [ Attr.class_
                "bg-muted rounded-full data-horizontal:h-1 data-vertical:w-1 relative \
                 grow overflow-hidden data-horizontal:w-full data-vertical:h-full"
            ])
       [ Node.span
           ~attrs:
             ([ o ]
              @ disabled_attrs
              @ [ Attr.style range_style
                ; data "slot" "slider-range"
                ; Attr.class_
                    "bg-primary absolute select-none data-horizontal:h-full \
                     data-vertical:w-full"
                ])
           []
       ]
     :: List.mapi values ~f:thumb)
;;

let component
  ?attrs
  ?class_
  ?(min = 0.)
  ?(max = 100.)
  ?step
  ?min_steps_between_thumbs
  ?orientation
  ?disabled
  ?default_value
  (local_ graph)
  =
  let default_value = Option.value default_value ~default:[ min; max ] in
  Control.make ~equal:[%equal: float list] default_value graph ~view:(fun value set ->
    view
      ?attrs
      ?class_
      ~min
      ~max
      ?step
      ?min_steps_between_thumbs
      ?orientation
      ?disabled
      ~value
      ~on_change:set
      ())
;;
