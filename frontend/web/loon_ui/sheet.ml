open! Import

type t = Modal.t

module Side = struct
  type t =
    | Top
    | Right
    | Bottom
    | Left
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Top -> "top"
    | Right -> "right"
    | Bottom -> "bottom"
    | Left -> "left"
  ;;
end

let view ?id ~open_ ~on_open_change f = Modal.view ?id ~open_ ~on_open_change f
let component = Modal.component
let trigger ?attrs t node = Modal.trigger ~slot:"sheet-trigger" ?attrs t node
let close ?attrs t node = Modal.close ~slot:"sheet-close" ?attrs t node

let overlay_class =
  "bg-black/10 supports-backdrop-filter:backdrop-blur-xs fixed inset-0 z-50 \
   duration-100 data-open:animate-in data-open:fade-in-0 \
   data-closed:animate-out data-closed:fade-out-0"
;;

let content_class =
  "bg-popover text-popover-foreground fixed z-50 flex flex-col gap-4 \
   bg-clip-padding text-sm shadow-lg transition duration-200 ease-in-out \
   data-[side=bottom]:inset-x-0 data-[side=bottom]:bottom-0 \
   data-[side=bottom]:h-auto data-[side=bottom]:border-t \
   data-[side=left]:inset-y-0 data-[side=left]:left-0 data-[side=left]:h-full \
   data-[side=left]:w-3/4 data-[side=left]:border-r \
   data-[side=right]:inset-y-0 data-[side=right]:right-0 \
   data-[side=right]:h-full data-[side=right]:w-3/4 data-[side=right]:border-l \
   data-[side=top]:inset-x-0 data-[side=top]:top-0 data-[side=top]:h-auto \
   data-[side=top]:border-b data-[side=left]:sm:max-w-sm \
   data-[side=right]:sm:max-w-sm data-open:animate-in data-open:fade-in-0 \
   data-[side=bottom]:data-open:slide-in-from-bottom-10 \
   data-[side=left]:data-open:slide-in-from-left-10 \
   data-[side=right]:data-open:slide-in-from-right-10 \
   data-[side=top]:data-open:slide-in-from-top-10 data-closed:animate-out \
   data-closed:fade-out-0 \
   data-[side=bottom]:data-closed:slide-out-to-bottom-10 \
   data-[side=left]:data-closed:slide-out-to-left-10 \
   data-[side=right]:data-closed:slide-out-to-right-10 \
   data-[side=top]:data-closed:slide-out-to-top-10"
;;

let content
      ?(attrs = [])
      ?class_
      ?(side = Side.Right)
      ?(show_close_button = true)
      (t : t)
      children
  =
  Modal.content
    ~overlay:(Modal.overlay ~slot:"sheet-overlay" ~class_:overlay_class t)
    ~content_attrs:
      ([ data "slot" "sheet-content"
       ; data "side" (Side.name side)
       ; Cn.with_class ?class_ [ content_class ]
       ]
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
              ~class_:"absolute top-3 right-3"
              [ Icon.view X
              ; Node.span ~attrs:[ Attr.class_ "sr-only" ] [ Node.text "Close" ]
              ])
       ]
     else [])
;;

let header ?(attrs = []) ?class_ children =
  Node.div
    ~attrs:
      ([ data "slot" "sheet-header"
       ; Cn.with_class ?class_ [ "gap-0.5 p-4 flex flex-col" ]
       ]
       @ attrs)
    children
;;

let footer ?(attrs = []) ?class_ children =
  Node.div
    ~attrs:
      ([ data "slot" "sheet-footer"
       ; Cn.with_class ?class_ [ "gap-2 p-4 mt-auto flex flex-col" ]
       ]
       @ attrs)
    children
;;

let title ?attrs ?class_ t children =
  Modal.title
    ~slot:"sheet-title"
    ~class_:
      (Cn.cn
         [ "text-foreground text-base font-medium font-heading"
         ; Option.value class_ ~default:""
         ])
    ?attrs
    t
    children
;;

let description ?attrs ?class_ t children =
  Modal.description
    ~slot:"sheet-description"
    ~class_:
      (Cn.cn
         [ "text-muted-foreground text-sm"; Option.value class_ ~default:"" ])
    ?attrs
    t
    children
;;
