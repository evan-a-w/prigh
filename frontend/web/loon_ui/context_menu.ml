open! Import

module State = struct
  type t =
    | Closed
    | Open of
        { x : float
        ; y : float
        ; menu : Menu.State.t
        }
  [@@deriving sexp_of, equal]

  let is_open = function
    | Closed -> false
    | Open _ -> true
  ;;
end

let classes : Menu.Classes.t =
  { slot = "context-menu"
  ; item =
      "focus:bg-accent focus:text-accent-foreground \
       data-[variant=destructive]:text-destructive \
       data-[variant=destructive]:focus:bg-destructive/10 \
       dark:data-[variant=destructive]:focus:bg-destructive/20 \
       data-[variant=destructive]:focus:text-destructive \
       data-[variant=destructive]:*:[svg]:text-destructive \
       focus:*:[svg]:text-accent-foreground gap-1.5 rounded-md px-1.5 py-1 text-sm \
       data-inset:pl-7 [&_svg:not([class*='size-'])]:size-4 group/context-menu-item \
       relative flex cursor-default items-center outline-hidden select-none \
       data-disabled:pointer-events-none data-disabled:opacity-50 \
       [&_svg]:pointer-events-none [&_svg]:shrink-0"
  ; checkbox_item =
      "focus:bg-accent focus:text-accent-foreground gap-1.5 rounded-md py-1 pr-8 pl-1.5 \
       text-sm data-inset:pl-7 [&_svg:not([class*='size-'])]:size-4 relative flex \
       cursor-default items-center outline-hidden select-none \
       data-disabled:pointer-events-none data-disabled:opacity-50 \
       [&_svg]:pointer-events-none [&_svg]:shrink-0"
  ; radio_item =
      "focus:bg-accent focus:text-accent-foreground gap-1.5 rounded-md py-1 pr-8 pl-1.5 \
       text-sm data-inset:pl-7 [&_svg:not([class*='size-'])]:size-4 relative flex \
       cursor-default items-center outline-hidden select-none \
       data-disabled:pointer-events-none data-disabled:opacity-50 \
       [&_svg]:pointer-events-none [&_svg]:shrink-0"
  ; checkbox_indicator = "absolute right-2 pointer-events-none"
  ; radio_indicator = "absolute right-2 pointer-events-none"
  ; indicator_slots = false
  ; label = "text-muted-foreground px-1.5 py-1 text-xs font-medium data-inset:pl-7"
  ; separator = "bg-border -mx-1 my-1 h-px"
  ; shortcut = "text-muted-foreground \
                group-focus/context-menu-item:text-accent-foreground ml-auto text-xs \
                tracking-widest"
  ; sub_trigger =
      "focus:bg-accent focus:text-accent-foreground data-open:bg-accent \
       data-open:text-accent-foreground gap-1.5 rounded-md px-1.5 py-1 text-sm \
       data-inset:pl-7 [&_svg:not([class*='size-'])]:size-4 flex cursor-default \
       items-center outline-hidden select-none [&_svg]:pointer-events-none \
       [&_svg]:shrink-0"
  ; sub_trigger_icon = "ml-auto"
  ; sub_content =
      "data-open:animate-in data-closed:animate-out data-closed:fade-out-0 \
       data-open:fade-in-0 data-closed:zoom-out-95 data-open:zoom-in-95 \
       data-[side=bottom]:slide-in-from-top-2 data-[side=left]:slide-in-from-right-2 \
       data-[side=right]:slide-in-from-left-2 data-[side=top]:slide-in-from-bottom-2 \
       bg-popover text-popover-foreground min-w-32 rounded-lg border p-1 shadow-lg \
       duration-100 cn-menu-target z-50 \
       origin-(--radix-context-menu-content-transform-origin) overflow-hidden"
  }
;;

let content_class =
  "data-open:animate-in data-closed:animate-out data-closed:fade-out-0 \
   data-open:fade-in-0 data-closed:zoom-out-95 data-open:zoom-in-95 \
   data-[side=bottom]:slide-in-from-top-2 data-[side=left]:slide-in-from-right-2 \
   data-[side=right]:slide-in-from-left-2 data-[side=top]:slide-in-from-bottom-2 \
   ring-foreground/10 bg-popover text-popover-foreground min-w-36 rounded-lg p-1 \
   shadow-md ring-1 duration-100 cn-menu-target z-50 \
   max-h-(--radix-context-menu-content-available-height) \
   origin-(--radix-context-menu-content-transform-origin) overflow-x-hidden \
   overflow-y-auto"
;;

type t =
  { id : string
  ; modal : bool
  ; state : State.t
  ; on_change : State.t -> unit Effect.t
  }

let view ?(id = "context-menu") ?(modal = true) ~state ~on_change f =
  Node.fragment (f { id; modal; state; on_change })
;;

let component ?modal f (local_ graph) =
  let open Bonsai.Let_syntax in
  let state, set_state = Bonsai.state State.Closed graph in
  let id = Bonsai.path_id graph in
  let%arr state and set_state and id and f in
  view ~id ?modal ~state ~on_change:set_state f
;;

let trigger_id t = Menu.trigger_id t.id

let trigger ?(attrs = []) ?class_ ?(disabled = false) t children =
  Node.span
    ~attrs:
      ([ data "slot" "context-menu-trigger"
       ; Attr.id (trigger_id t)
       ; data "state" (if State.is_open t.state then "open" else "closed")
       ]
       @ [ Cn.with_class ?class_ [ "select-none" ] ]
       @ (if disabled
          then [ data "disabled" "" ]
          else
            [ Attr.on_contextmenu (fun ev ->
                let x = Js_of_ocaml.Js.to_float ev##.clientX
                and y = Js_of_ocaml.Js.to_float ev##.clientY in
                Effect.Many
                  [ Dom_util.prevent_default ev
                  ; t.on_change (Open { x; y; menu = Menu.State.initial })
                  ])
            ; Attr.style (Css_gen.create ~field:"-webkit-touch-callout" ~value:"none")
            ])
       @ attrs)
    children
;;

let content ?attrs ?class_ ?avoid_collisions t items =
  match t.state with
  | Closed -> Node.none
  | Open { x; y; menu = s } ->
    let menu : Menu.t =
      { id = t.id
      ; classes
      ; state = s
      ; set_state = (fun menu -> t.on_change (Open { x; y; menu }))
      ; close = t.on_change Closed
      ; on_edge = None
      }
    in
    Menu.content
      ?attrs
      ?class_
      ~side:Right
      ~side_offset:2.
      ~align:Start
      ?avoid_collisions
      ~modal:t.modal
      ~labelled_by:false
      ~base_class:content_class
      ~anchor:(Point { x; y })
      menu
      items
;;

let group = Menu.group
let label = Menu.label
let item = Menu.item
let checkbox_item = Menu.checkbox_item
let radio_group = Menu.radio_group
let radio_item = Menu.radio_item ?indicator:None
let separator = Menu.separator
let shortcut = Menu.shortcut classes
let sub = Menu.sub
let sub_trigger = Menu.sub_trigger
let sub_content = Menu.sub_content
