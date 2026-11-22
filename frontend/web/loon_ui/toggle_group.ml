open! Import

type kind =
  | Single of
      { value : string option
      ; on_change : string option -> unit Effect.t
      }
  | Multiple of
      { value : string list
      ; on_change : string list -> unit Effect.t
      }

type t =
  { kind : kind
  ; variant : Toggle.Variant.t option
  ; size : Toggle.Size.t option
  ; spacing : int
  ; disabled : bool
  ; (* The items, in the order they are created (for tests, which have no
       DOM; OCaml evaluates a list literal last first, so tests make them with
       [List.map]). *)
    mutable items : (string * bool) list
  }

let pressed t value =
  match t.kind with
  | Single { value = v; _ } -> [%equal: string option] v (Some value)
  | Multiple { value = vs; _ } -> List.mem vs value ~equal:String.equal
;;

let group_selector = "[data-slot=toggle-group]"
let item_selector = "[data-slot=toggle-group-item]"

let root ?(attrs = []) ?class_ ?variant ?size ?(spacing = 2) ?(orientation = Roving.Orientation.Horizontal) ?(disabled = false)
  kind children
  =
  let t = { kind; variant; size; spacing; disabled; items = [] } in
  let children = children t in
  Node.div
    ~attrs:
      ([ Attr.role
           (match kind with
            | Single _ -> "radiogroup"
            | Multiple _ -> "toolbar")
       ; Attr.create "dir" "ltr"
       ; data "slot" "toggle-group"
       ]
       @ Roving.Tab_stop.group_attrs
           ~item:item_selector
           ~active:"[data-state=on]"
           ~focusable:(not disabled)
       @ Option.value_map variant ~default:[] ~f:(fun v ->
         [ data "variant" (Toggle.Variant.name v) ])
       @ Option.value_map size ~default:[] ~f:(fun s -> [ data "size" (Toggle.Size.name s) ])
       @ [ data "spacing" (Int.to_string spacing)
         ; Attr.css_var ~name:"gap" (Int.to_string spacing)
         ; Cn.with_class
             ?class_
             [ "rounded-lg data-[size=sm]:rounded-[min(var(--radius-md),10px)] \
                group/toggle-group flex w-fit flex-row items-center \
                gap-[--spacing(var(--gap))] data-vertical:flex-col \
                data-vertical:items-stretch"
             ]
         ]
       @ [ data "orientation" (Roving.Orientation.name orientation) ]
       @ attrs)
    children
;;

let single ?attrs ?class_ ?variant ?size ?spacing ?orientation ?disabled ~value ~on_change
  children
  =
  root
    ?attrs
    ?class_
    ?variant
    ?size
    ?spacing
    ?orientation
    ?disabled
    (Single { value; on_change })
    children
;;

let multiple ?attrs ?class_ ?variant ?size ?spacing ?orientation ?disabled ~value
  ~on_change children
  =
  root
    ?attrs
    ?class_
    ?variant
    ?size
    ?spacing
    ?orientation
    ?disabled
    (Multiple { value; on_change })
    children
;;

let single_component
  ?attrs
  ?class_
  ?variant
  ?size
  ?spacing
  ?orientation
  ?disabled
  ?default_value
  children
  (local_ graph)
  =
  let open Bonsai.Let_syntax in
  let value, set = Bonsai.state default_value ~equal:[%equal: string option] graph in
  let%arr value and set and children in
  { Control.value
  ; set
  ; view =
      single
        ?attrs
        ?class_
        ?variant
        ?size
        ?spacing
        ?orientation
        ?disabled
        ~value
        ~on_change:set
        children
  }
;;

let multiple_component
  ?attrs
  ?class_
  ?variant
  ?size
  ?spacing
  ?orientation
  ?disabled
  ?(default_value = [])
  children
  (local_ graph)
  =
  let open Bonsai.Let_syntax in
  let value, set = Bonsai.state default_value ~equal:[%equal: string list] graph in
  let%arr value and set and children in
  { Control.value
  ; set
  ; view =
      multiple
        ?attrs
        ?class_
        ?variant
        ?size
        ?spacing
        ?orientation
        ?disabled
        ~value
        ~on_change:set
        children
  }
;;

let item ?(attrs = []) ?class_ ?variant ?size ?(disabled = false) t ~value children =
  let disabled = disabled || t.disabled in
  t.items <- t.items @ [ value, not disabled ];
  let on = pressed t value in
  let variant = Option.first_some t.variant variant in
  let size = Option.first_some t.size size in
  let toggle () =
    match t.kind with
    | Single { on_change; _ } -> on_change (if on then None else Some value)
    | Multiple { value = vs; on_change } ->
      on_change
        (if on
         then List.filter vs ~f:(fun v -> not (String.equal v value))
         else vs @ [ value ])
  in
  Node.button
    ~attrs:
      ([ Attr.type_ "button"; data "state" (if on then "on" else "off") ]
       @ (match t.kind with
          | Single _ -> [ Attr.role "radio"; aria_bool "checked" on ]
          | Multiple _ -> [ aria_bool "pressed" on ])
       @ Roving.Tab_stop.item_attrs ~group:group_selector ~item:item_selector
       @ [ data "slot" "toggle-group-item" ]
       @ [ data "variant" (Toggle.Variant.name (Option.value variant ~default:Default))
         ; data "size" (Toggle.Size.name (Option.value size ~default:Default))
         ; data "spacing" (Int.to_string t.spacing)
         ; Cn.attr
             ([ "group-data-[spacing=0]/toggle-group:rounded-none \
                 group-data-[spacing=0]/toggle-group:px-2 \
                 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 \
                 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 \
                 group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg \
                 group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg \
                 group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg \
                 group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg \
                 shrink-0 focus:z-10 focus-visible:z-10 \
                 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 \
                 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 \
                 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l \
                 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t"
              ; Toggle.classes ?variant ?size ()
              ]
              @ Option.to_list class_)
         ; Attr.on_keydown (fun ev ->
             match
               Roving.next
                 ev
                 ~group:group_selector
                 ~item:item_selector
                 ~fallback:t.items
                 ~current:value
             with
             | None -> Effect.Ignore
             | Some (_, focus) -> Effect.Many [ Dom_util.prevent_default ev; focus ])
         ]
       @ (if disabled
          then [ data "disabled" ""; Attr.disabled ]
          else [ Attr.on_click (fun _ -> toggle ()) ])
       @ attrs)
    children
;;
