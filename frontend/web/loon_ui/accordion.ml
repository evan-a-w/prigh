open! Import
open Js_of_ocaml

module Type = struct
  type t =
    | Single
    | Multiple
  [@@deriving enumerate, sexp_of, compare, equal]
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

type t =
  { type_ : Type.t
  ; collapsible : bool
  ; disabled : bool
  ; orientation : Orientation.t
  ; id : string
  ; value : string list
  ; on_change : string list -> unit Effect.t
  }

type accordion = t

module Item = struct
  type t =
    { root : accordion
    ; value : string
    ; open_ : bool
    ; disabled : bool
    }
end

module Part = struct
  type t = Item.t -> Node.t
end

let toggled (type_ : Type.t) ~collapsible ~open_ value =
  let is_open = List.mem open_ value ~equal:String.equal in
  match type_ with
  | Multiple ->
    if is_open
    then List.filter open_ ~f:(fun o -> not (String.equal o value))
    else open_ @ [ value ]
  | Single -> if is_open then if collapsible then [] else open_ else [ value ]
;;

let trigger_id t value = t.id ^ "-trigger-" ^ value
let content_id t value = t.id ^ "-content-" ^ value
let state open_ = if open_ then "open" else "closed"
let orientation_attr t = data "orientation" (Orientation.name t.orientation)

let view
      ?(attrs = [])
      ?class_
      ?(collapsible = false)
      ?(disabled = false)
      ?(orientation = Orientation.Vertical)
      ?(id = "accordion")
      ~type_
      ~value
      ~on_change
      children
  =
  let t = { type_; collapsible; disabled; orientation; id; value; on_change } in
  Node.div
    ~attrs:
      ([ orientation_attr t
       ; data "slot" "accordion"
       ; Cn.with_class ?class_ [ "flex w-full flex-col" ]
       ]
       @ attrs)
    (children t)
;;

let component
      ?attrs
      ?class_
      ?collapsible
      ?disabled
      ?orientation
      ?(default_value = [])
      ~type_
      children
      (local_ graph)
  =
  let open Bonsai.Let_syntax in
  let value, set_value = Bonsai.state default_value graph in
  let id = Bonsai.path_id graph in
  let%arr value
  and set_value
  and id
  and children in
  view
    ?attrs
    ?class_
    ?collapsible
    ?disabled
    ?orientation
    ~id
    ~type_
    ~value
    ~on_change:set_value
    children
;;

let item ?(attrs = []) ?class_ ?(disabled = false) t ~value parts =
  let open_ = List.mem t.value value ~equal:String.equal in
  let disabled = disabled || t.disabled in
  let item = { Item.root = t; value; open_; disabled } in
  Node.div
    ~key:value
    ~attrs:
      ([ data "state" (state open_) ]
       @ (if disabled then [ data "disabled" "" ] else [])
       @ [ orientation_attr t
         ; data "slot" "accordion-item"
         ; Cn.with_class ?class_ [ "not-last:border-b" ]
         ]
       @ attrs)
    (List.map parts ~f:(fun part -> part item))
;;

(* Radix moves focus between the accordion's enabled triggers, found in the
   DOM: those of its own items (not of nested accordions), wrapping round. *)
let move_focus ~from key =
  Effect.of_thunk (fun () ->
    Js.Opt.iter
      (Dom_html.document##getElementById (Js.string from))
      (fun trigger ->
         Js.Opt.iter
           (trigger##closest (Js.string "[data-slot=accordion]"))
           (fun root ->
              let triggers =
                root##querySelectorAll
                  (Js.string
                     ":scope > [data-slot=accordion-item] > h3 > \
                      [data-slot=accordion-trigger]:not([disabled])")
                |> Dom.list_of_nodeList
              in
              let n = List.length triggers in
              let index =
                List.findi triggers ~f:(fun _ el ->
                  String.equal (Js.to_string el##.id) from)
                |> Option.map ~f:fst
              in
              let target =
                match key, index with
                | `First, _ -> Some 0
                | `Last, _ -> Some (n - 1)
                | `Next, Some i -> Some ((i + 1) % n)
                | `Prev, Some i -> Some ((i - 1 + n) % n)
                | (`Next | `Prev), None -> None
              in
              Option.iter target ~f:(fun i ->
                match List.nth triggers i with
                | Some el ->
                  (Js.Unsafe.coerce el : Dom_html.element Js.t)##focus
                | None -> ()))))
;;

let in_browser () = Js.Optdef.test (Js.Unsafe.global##.document : _ Js.Optdef.t)

let trigger ?(attrs = []) ?class_ children (item : Item.t) =
  let t = item.root in
  let locked =
    match t.type_ with
    | Single -> item.open_ && not t.collapsible
    | Multiple -> false
  in
  let on_keydown ev =
    let key =
      match t.orientation, (Dom_util.key ev : Dom_html.Keyboard_code.t) with
      | Vertical, ArrowDown | Horizontal, ArrowRight -> Some `Next
      | Vertical, ArrowUp | Horizontal, ArrowLeft -> Some `Prev
      | _, Home -> Some `First
      | _, End -> Some `Last
      | _ -> None
    in
    match key with
    | None -> Effect.Ignore
    | Some key ->
      Effect.Many
        [ Dom_util.prevent_default ev
        ; (if in_browser ()
           then move_focus ~from:(trigger_id t item.value) key
           else Effect.Ignore)
        ]
  in
  Node.h3
    ~attrs:
      ([ orientation_attr t; data "state" (state item.open_) ]
       @ (if item.disabled then [ data "disabled" "" ] else [])
       @ [ Attr.class_ "flex" ])
    [ Node.button
        ~attrs:
          ([ Attr.type_ "button"
           ; aria "controls" (content_id t item.value)
           ; aria_bool "expanded" item.open_
           ; data "state" (state item.open_)
           ]
           @ (if locked then [ aria "disabled" "true" ] else [])
           @ (if item.disabled
              then [ data "disabled" ""; Attr.disabled ]
              else [])
           @ [ orientation_attr t
             ; Attr.id (trigger_id t item.value)
             ; data "slot" "accordion-trigger"
             ; Cn.with_class
                 ?class_
                 [ "focus-visible:ring-ring/50 focus-visible:border-ring \
                    focus-visible:after:border-ring \
                    **:data-[slot=accordion-trigger-icon]:text-muted-foreground \
                    rounded-lg py-2.5 text-left text-sm font-medium hover:underline \
                    focus-visible:ring-3 **:data-[slot=accordion-trigger-icon]:ml-auto \
                    **:data-[slot=accordion-trigger-icon]:size-4 \
                    group/accordion-trigger relative flex flex-1 items-start \
                    justify-between border border-transparent transition-all \
                    outline-none disabled:pointer-events-none disabled:opacity-50"
                 ]
             ; Attr.on_keydown on_keydown
             ]
           @ (if item.disabled
              then []
              else
                [ Attr.on_click (fun _ ->
                    t.on_change
                      (toggled
                         t.type_
                         ~collapsible:t.collapsible
                         ~open_:t.value
                         item.value))
                ])
           @ attrs)
        (children
         @ [ Icon.view
               ~attrs:[ data "slot" "accordion-trigger-icon" ]
               ~class_:
                 "pointer-events-none shrink-0 \
                  group-aria-expanded/accordion-trigger:hidden"
               Chevron_down
           ; Icon.view
               ~attrs:[ data "slot" "accordion-trigger-icon" ]
               ~class_:
                 "pointer-events-none hidden shrink-0 \
                  group-aria-expanded/accordion-trigger:inline"
               Chevron_up
           ])
    ]
;;

let content ?(attrs = []) ?class_ children (item : Item.t) =
  let t = item.root in
  Node.div
    ~attrs:
      ([ data "state" (state item.open_); Attr.id (content_id t item.value) ]
       @ (if item.open_ then [] else [ Attr.hidden ])
       @ (if item.disabled then [ data "disabled" "" ] else [])
       @ [ Attr.role "region"
         ; aria "labelledby" (trigger_id t item.value)
         ; orientation_attr t
         ; data "slot" "accordion-content"
         ; Attr.class_ "data-open:animate-accordion-down \
                        data-closed:animate-accordion-up text-sm overflow-hidden"
         ; Collapsible.presence ~accordion:true ~open_:item.open_ ()
         ]
       @ attrs)
    [ Node.div
        ~attrs:
          [ Cn.with_class
              ?class_
              [ "pt-0 pb-2.5 h-(--radix-accordion-content-height) [&_a]:underline \
                 [&_a]:underline-offset-3 [&_a]:hover:text-foreground \
                 [&_p:not(:last-child)]:mb-4"
              ]
          ]
        children
    ]
;;
