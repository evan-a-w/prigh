open! Import

type t = Modal.t

let view ?id ~open_ ~on_open_change f = Modal.view ?id ~open_ ~on_open_change f
let component = Modal.component
let trigger ?attrs t node = Modal.trigger ~slot:"dialog-trigger" ?attrs t node
let close ?attrs t node = Modal.close ~slot:"dialog-close" ?attrs t node

let overlay_class =
  "data-open:animate-in data-closed:animate-out data-closed:fade-out-0 \
   data-open:fade-in-0 bg-black/10 duration-100 \
   supports-backdrop-filter:backdrop-blur-xs fixed inset-0 isolate z-50"
;;

let content_class =
  "bg-popover text-popover-foreground data-open:animate-in \
   data-closed:animate-out data-closed:fade-out-0 data-open:fade-in-0 \
   data-closed:zoom-out-95 data-open:zoom-in-95 ring-foreground/10 grid \
   max-w-[calc(100%-2rem)] gap-4 rounded-xl p-4 text-sm ring-1 duration-100 \
   sm:max-w-sm fixed top-1/2 left-1/2 z-50 w-full -translate-x-1/2 \
   -translate-y-1/2 outline-none"
;;

let content
      ?(attrs = [])
      ?class_
      ?(show_close_button = true)
      ?initial_focus
      t
      children
  =
  Modal.content
    ?initial_focus
    ~overlay:(Modal.overlay ~slot:"dialog-overlay" ~class_:overlay_class t)
    ~content_attrs:
      ([ data "slot" "dialog-content"; Cn.with_class ?class_ [ content_class ] ]
       @ attrs)
    t
    (children
     @
     if show_close_button
     then
       [ close
           t
           (Button.view
              ~variant:Ghost
              ~size:Icon_sm
              ~class_:"absolute top-2 right-2"
              [ Icon.view X
              ; Node.span ~attrs:[ Attr.class_ "sr-only" ] [ Node.text "Close" ]
              ])
       ]
     else [])
;;

let header ?(attrs = []) ?class_ children =
  Node.div
    ~attrs:
      ([ data "slot" "dialog-header"
       ; Cn.with_class ?class_ [ "gap-2 flex flex-col" ]
       ]
       @ attrs)
    children
;;

let footer ?(attrs = []) ?class_ ?show_close_button children =
  Node.div
    ~attrs:
      ([ data "slot" "dialog-footer"
       ; Cn.with_class
           ?class_
           [ "bg-muted/50 -mx-4 -mb-4 rounded-b-xl border-t p-4 flex \
              flex-col-reverse gap-2 sm:flex-row sm:justify-end"
           ]
       ]
       @ attrs)
    (children
     @
     match show_close_button with
     | None -> []
     | Some t ->
       [ close t (Button.view ~variant:Outline [ Node.text "Close" ]) ])
;;

let title ?attrs ?class_ t children =
  Modal.title
    ~slot:"dialog-title"
    ~class_:
      (Cn.cn
         [ "text-base leading-none font-medium font-heading"
         ; Option.value class_ ~default:""
         ])
    ?attrs
    t
    children
;;

let description ?attrs ?class_ t children =
  Modal.description
    ~slot:"dialog-description"
    ~class_:
      (Cn.cn
         [ "text-muted-foreground *:[a]:hover:text-foreground text-sm \
            *:[a]:underline *:[a]:underline-offset-3"
         ; Option.value class_ ~default:""
         ])
    ?attrs
    t
    children
;;
