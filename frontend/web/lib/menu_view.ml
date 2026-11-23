open! Core
open! Import
open Html

let slot name = Attr.create "data-slot" name

(* loon's DropdownMenu (Nova): its content and an item. The item under the
   keys has [data-highlighted]; a disabled one stays dimmed, its hint saying
   why. *)
let surface_look =
  "bg-popover text-popover-foreground min-w-32 overflow-x-hidden \
   overflow-y-auto rounded-lg p-1 shadow-md ring-1 ring-foreground/10 \
   animate-in fade-in-0 zoom-in-95 duration-100 cn-menu-target \
   narrow:rounded-t-xl narrow:rounded-b-none narrow:border-t \
   narrow:border-border narrow:pt-0 narrow:pr-[max(8px,var(--safe-r))] \
   narrow:pb-[max(8px,var(--safe-b))] narrow:pl-[max(8px,var(--safe-l))] \
   narrow:overscroll-contain narrow:ring-0 narrow:z-(--z-menu-surface) \
   narrow:shadow-[0_-12px_40px_-12px_var(--edge-shadow)] \
   narrow:animate-[sheet-up_0.18s_ease-out] motion-reduce:animate-none"
;;

let item_look =
  "group/dropdown-menu-item relative flex cursor-default items-center gap-1.5 \
   rounded-md px-1.5 py-1 text-sm outline-hidden select-none hover:bg-accent \
   hover:text-accent-foreground data-highlighted:bg-accent \
   data-highlighted:text-accent-foreground \
   data-[variant=destructive]:text-destructive \
   data-[variant=destructive]:hover:bg-destructive/10 \
   data-[variant=destructive]:hover:text-destructive \
   data-[variant=destructive]:data-highlighted:bg-destructive/10 \
   data-[variant=destructive]:data-highlighted:text-destructive \
   data-[variant=destructive]:[&>.icon]:text-destructive \
   aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit \
   aria-disabled:*:opacity-50 [&>.icon]:size-4 [&>.icon]:shrink-0 \
   [&>.icon]:text-muted-foreground hover:[&>.icon]:text-accent-foreground \
   data-highlighted:[&>.icon]:text-accent-foreground narrow:min-h-11"
;;

let label_look = "menu-label truncate"

let backdrop ?(cls = "") ~on_close children =
  let outside (ev : Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t) =
    Js_of_ocaml.Js.Unsafe.equals ev##.target ev##.currentTarget
  in
  Node.div
    ~key:"menu"
    ~attrs:
      [ Attr.class_ (String.strip ("menu-backdrop " ^ cls))
      ; Attr.on_click (fun ev -> if outside ev then on_close else Effect.Ignore)
      ; Attr.on_contextmenu (fun ev ->
          Js_of_ocaml.Dom.preventDefault ev;
          if outside ev then on_close else Effect.Ignore)
      ]
    children
;;

let surface ?(cls = "") ?(attrs = []) ~label children =
  Node.div
    ~attrs:
      ([ Attr.class_
           (String.concat ~sep:" " [ "loon"; cls; "menu-surface"; surface_look ])
       ; slot "dropdown-menu-content"
       ; Attr.role "menu"
       ; Attr.create "aria-label" label
       ; Attr.tabindex (-1)
       ]
       @ attrs)
    children
;;

let section ?(cls = "") children =
  Node.div
    ~attrs:
      [ Attr.class_
          (String.concat
             ~sep:" "
             [ "menu-section flex flex-col [&+&]:-mx-1 [&+&]:mt-1 \
                [&+&]:border-t [&+&]:border-border [&+&]:px-1 [&+&]:pt-1"
             ; cls
             ])
      ; slot "dropdown-menu-group"
      ; Attr.role "group"
      ]
    children
;;

let heading text =
  Node.div
    ~attrs:
      [ Attr.class_
          "menu-heading px-1.5 py-1 text-xs font-medium text-muted-foreground"
      ; slot "dropdown-menu-label"
      ]
    [ Node.text text ]
;;

let item
      ?(cls = "")
      ?(selected = false)
      ?(danger = false)
      ?(disabled = false)
      ?icon:glyph
      ?hint
      ?shortcut
      ?(attrs = [])
      label
  =
  let label_node =
    match hint with
    | Some hint when not (String.is_empty hint) ->
      Node.span
        ~attrs:[ Attr.class_ "menu-text flex min-w-0 flex-col leading-tight" ]
        [ span ~cls:label_look label
        ; span
            ~cls:"menu-hint text-xs whitespace-normal text-muted-foreground"
            hint
        ]
    | _ -> span ~cls:label_look label
  in
  Node.div
    ~attrs:
      ([ classes
           ("menu-item"
            :: item_look
            :: (if String.is_empty cls then [] else [ cls ]))
           [ "selected", selected; "danger", danger; "disabled", disabled ]
       ; slot "dropdown-menu-item"
       ; (if selected then Attr.create "data-highlighted" "" else Attr.empty)
       ; Attr.role "menuitem"
       ; (if danger
          then Attr.create "data-variant" "destructive"
          else Attr.empty)
       ; (if disabled then Attr.create "aria-disabled" "true" else Attr.empty)
       ]
       @ attrs)
    [ Option.value_map glyph ~default:Node.none ~f:(fun g -> icon g)
    ; label_node
    ; Option.value_map shortcut ~default:Node.none ~f:(fun key ->
        Node.kbd
          ~attrs:
            [ Attr.class_
                "menu-command ml-auto min-w-0 truncate pl-3 font-mono text-xs \
                 text-muted-foreground \
                 group-data-highlighted/dropdown-menu-item:text-accent-foreground \
                 group-hover/dropdown-menu-item:text-accent-foreground"
            ; slot "dropdown-menu-shortcut"
            ]
          [ Node.text key ])
    ]
;;
