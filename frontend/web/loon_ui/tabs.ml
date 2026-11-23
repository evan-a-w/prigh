open! Import

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

module List_variant = struct
  type t =
    | Default
    | Line
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Default -> "default"
    | Line -> "line"
  ;;

  let classes = function
    | Default -> "bg-muted"
    | Line -> "gap-1 bg-transparent"
  ;;
end

type t =
  { value : string
  ; on_change : string -> unit Effect.t
  ; id : string
  ; orientation : Orientation.t
  }

module Trigger = struct
  type t =
    { attrs : Attr.t list
    ; class_ : string option
    ; disabled : bool
    ; value : string
    ; children : Node.t list
    }
end

let trigger_id t value = t.id ^ "-trigger-" ^ value
let content_id t value = t.id ^ "-content-" ^ value

let view ?(attrs = []) ?class_ ?(orientation = Orientation.Horizontal) ?(id = "tabs") ~value
  ~on_change children
  =
  let t = { value; on_change; id; orientation } in
  Node.div
    ~attrs:
      ([ data "slot" "tabs"
       ; Attr.create "dir" "ltr"
       ; data "orientation" (Orientation.name orientation)
       ; Cn.with_class
           ?class_
           [ "gap-2 group/tabs flex data-horizontal:flex-col" ]
       ]
       @ attrs)
    (children t)
;;

let component ?attrs ?class_ ?orientation ~default_value children (local_ graph) =
  let open Bonsai.Let_syntax in
  let value, set_value = Bonsai.state default_value graph in
  let id = Bonsai.path_id graph in
  let%arr value and set_value and id and children in
  view ?attrs ?class_ ?orientation ~id ~value ~on_change:set_value children
;;

(* The trigger an arrow key (or Home, End) moves to from [value]'s,
   wrapping round and skipping disabled ones. *)
let moved t (triggers : Trigger.t list) ~value ~(key : Js_of_ocaml.Dom_html.Keyboard_code.t) =
  let enabled = List.filter triggers ~f:(fun tr -> not tr.disabled) in
  let n = List.length enabled in
  if n = 0
  then None
  else (
    let index =
      List.findi enabled ~f:(fun _ tr -> String.equal tr.value value)
      |> Option.map ~f:fst
    in
    let step d =
      Some
        (match index with
         | None -> if d > 0 then 0 else n - 1
         | Some i -> (i + d + n) % n)
    in
    let next =
      match t.orientation, key with
      | Horizontal, ArrowRight | Vertical, ArrowDown -> step 1
      | Horizontal, ArrowLeft | Vertical, ArrowUp -> step (-1)
      | _, Home -> Some 0
      | _, End -> Some (n - 1)
      | _ -> None
    in
    Option.map next ~f:(fun i -> (List.nth_exn enabled i).value))
;;

let list ?(attrs = []) ?class_ ?(variant = List_variant.Default) t triggers =
  let trigger (tr : Trigger.t) =
    let active = String.equal tr.value t.value in
    Node.button
      ~attrs:
        ([ Attr.type_ "button"
         ; Attr.role "tab"
         ; aria_bool "selected" active
         ; aria "controls" (content_id t tr.value)
         ; data "state" (if active then "active" else "inactive")
         ; Attr.id (trigger_id t tr.value)
         ; data "orientation" (Orientation.name t.orientation)
         ; Attr.tabindex (if active then 0 else -1)
         ; data "slot" "tabs-trigger"
         ; Cn.with_class
             ?class_:tr.class_
             [ "gap-1.5 rounded-md border border-transparent px-1.5 py-0.5 text-sm \
                font-medium \
                group-data-[variant=default]/tabs-list:data-active:shadow-sm \
                group-data-[variant=line]/tabs-list:data-active:shadow-none \
                [&_svg:not([class*='size-'])]:size-4 has-data-[icon=inline-end]:pr-1 \
                has-data-[icon=inline-start]:pl-1 relative inline-flex \
                h-[calc(100%-1px)] flex-1 items-center justify-center whitespace-nowrap \
                text-foreground/60 transition-all group-data-vertical/tabs:w-full \
                group-data-vertical/tabs:justify-start hover:text-foreground \
                focus-visible:border-ring focus-visible:ring-[3px] \
                focus-visible:ring-ring/50 focus-visible:outline-1 \
                focus-visible:outline-ring disabled:pointer-events-none \
                disabled:opacity-50 dark:text-muted-foreground \
                dark:hover:text-foreground [&_svg]:pointer-events-none [&_svg]:shrink-0"
             ; "group-data-[variant=line]/tabs-list:bg-transparent \
                group-data-[variant=line]/tabs-list:data-active:bg-transparent \
                dark:group-data-[variant=line]/tabs-list:data-active:border-transparent \
                dark:group-data-[variant=line]/tabs-list:data-active:bg-transparent"
             ; "data-active:bg-background data-active:text-foreground \
                dark:data-active:border-input dark:data-active:bg-input/30 \
                dark:data-active:text-foreground"
             ; "after:absolute after:bg-foreground after:opacity-0 \
                after:transition-opacity group-data-horizontal/tabs:after:inset-x-0 \
                group-data-horizontal/tabs:after:bottom-[-5px] \
                group-data-horizontal/tabs:after:h-0.5 \
                group-data-vertical/tabs:after:inset-y-0 \
                group-data-vertical/tabs:after:-right-1 \
                group-data-vertical/tabs:after:w-0.5 \
                group-data-[variant=line]/tabs-list:data-active:after:opacity-100"
             ]
         ]
         @ (if tr.disabled
            then [ Attr.disabled; data "disabled" "" ]
            else [ Attr.on_click (fun _ -> t.on_change tr.value) ])
         @ tr.attrs)
      tr.children
  in
  let on_keydown ev =
    match moved t triggers ~value:t.value ~key:(Dom_util.key ev) with
    | None -> Effect.Ignore
    | Some value ->
      Effect.Many
        [ Dom_util.prevent_default ev
        ; t.on_change value
        ; Dom_util.focus_selector (sprintf "[id=%S]" (trigger_id t value))
        ]
  in
  Node.div
    ~attrs:
      ([ Attr.role "tablist"
       ; aria "orientation" (Orientation.name t.orientation)
       ; data "slot" "tabs-list"
       ; data "variant" (List_variant.name variant)
       ; Cn.with_class
           ?class_
           [ "rounded-lg p-[3px] group-data-horizontal/tabs:h-8 \
              data-[variant=line]:rounded-none group/tabs-list inline-flex w-fit \
              items-center justify-center text-muted-foreground \
              group-data-vertical/tabs:h-fit group-data-vertical/tabs:flex-col"
           ; List_variant.classes variant
           ]
       ; Attr.on_keydown on_keydown
       ]
       @ attrs)
    (List.map triggers ~f:trigger)
;;

let trigger ?(attrs = []) ?class_ ?(disabled = false) ~value children =
  { Trigger.attrs; class_; disabled; value; children }
;;

let content ?(attrs = []) ?class_ t ~value children =
  let active = String.equal value t.value in
  Node.div
    ~attrs:
      ([ data "state" (if active then "active" else "inactive")
       ; data "orientation" (Orientation.name t.orientation)
       ; Attr.role "tabpanel"
       ; aria "labelledby" (trigger_id t value)
       ; Attr.id (content_id t value)
       ; Attr.tabindex 0
       ; data "slot" "tabs-content"
       ; Cn.with_class ?class_ [ "text-sm flex-1 outline-none" ]
       ]
       @ (if active then [] else [ Attr.hidden ])
       @ attrs)
    (if active then children else [])
;;
