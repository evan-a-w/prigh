open! Import

type t = Modal.t

module Size = struct
  type t =
    | Default
    | Sm
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Default -> "default"
    | Sm -> "sm"
  ;;
end

let view ?id ~open_ ~on_open_change f = Modal.view ?id ~open_ ~on_open_change f
let component = Modal.component

let trigger ?attrs t node =
  Modal.trigger ~slot:"alert-dialog-trigger" ?attrs t node
;;

let overlay_class =
  "data-open:animate-in data-closed:animate-out data-closed:fade-out-0 \
   data-open:fade-in-0 bg-black/10 duration-100 \
   supports-backdrop-filter:backdrop-blur-xs fixed inset-0 z-50"
;;

let content_class =
  "data-open:animate-in data-closed:animate-out data-closed:fade-out-0 \
   data-open:fade-in-0 data-closed:zoom-out-95 data-open:zoom-in-95 bg-popover \
   text-popover-foreground ring-foreground/10 gap-4 rounded-xl p-4 ring-1 \
   duration-100 data-[size=default]:max-w-xs data-[size=sm]:max-w-xs \
   data-[size=default]:sm:max-w-sm group/alert-dialog-content fixed top-1/2 \
   left-1/2 z-50 grid w-full -translate-x-1/2 -translate-y-1/2 outline-none"
;;

let content ?(attrs = []) ?class_ ?(size = Size.Default) (t : t) children =
  Modal.content
    ~dismissible:false
    ~role:"alertdialog"
    ~initial_focus:"[data-slot=alert-dialog-cancel]"
    ~overlay:
      (Modal.overlay ~slot:"alert-dialog-overlay" ~class_:overlay_class t)
    ~content_attrs:
      ([ data "slot" "alert-dialog-content"
       ; data "size" (Size.name size)
       ; Cn.with_class ?class_ [ content_class ]
       ]
       @ attrs)
    t
    children
;;

let header ?(attrs = []) ?class_ children =
  Node.div
    ~attrs:
      ([ data "slot" "alert-dialog-header"
       ; Cn.with_class
           ?class_
           [ "grid grid-rows-[auto_1fr] place-items-center gap-1.5 text-center \
              has-data-[slot=alert-dialog-media]:grid-rows-[auto_auto_1fr] \
              has-data-[slot=alert-dialog-media]:gap-x-4 \
              sm:group-data-[size=default]/alert-dialog-content:place-items-start \
              sm:group-data-[size=default]/alert-dialog-content:text-left \
              sm:group-data-[size=default]/alert-dialog-content:has-data-[slot=alert-dialog-media]:grid-rows-[auto_1fr]"
           ]
       ]
       @ attrs)
    children
;;

let footer ?(attrs = []) ?class_ children =
  Node.div
    ~attrs:
      ([ data "slot" "alert-dialog-footer"
       ; Cn.with_class
           ?class_
           [ "bg-muted/50 -mx-4 -mb-4 rounded-b-xl border-t p-4 flex \
              flex-col-reverse gap-2 \
              group-data-[size=sm]/alert-dialog-content:grid \
              group-data-[size=sm]/alert-dialog-content:grid-cols-2 \
              sm:flex-row sm:justify-end"
           ]
       ]
       @ attrs)
    children
;;

let title ?attrs ?class_ t children =
  Modal.title
    ~slot:"alert-dialog-title"
    ~class_:
      (Cn.cn
         [ "text-base font-medium \
            sm:group-data-[size=default]/alert-dialog-content:group-has-data-[slot=alert-dialog-media]/alert-dialog-content:col-start-2 \
            font-heading"
         ; Option.value class_ ~default:""
         ])
    ?attrs
    t
    children
;;

let description ?attrs ?class_ t children =
  Modal.description
    ~slot:"alert-dialog-description"
    ~class_:
      (Cn.cn
         [ "text-muted-foreground *:[a]:hover:text-foreground text-sm \
            text-balance md:text-pretty *:[a]:underline \
            *:[a]:underline-offset-3"
         ; Option.value class_ ~default:""
         ])
    ?attrs
    t
    children
;;

let media ?(attrs = []) ?class_ children =
  Node.div
    ~attrs:
      ([ data "slot" "alert-dialog-media"
       ; Cn.with_class
           ?class_
           [ "bg-muted mb-2 inline-flex size-10 items-center justify-center \
              rounded-md \
              sm:group-data-[size=default]/alert-dialog-content:row-span-2 \
              *:[svg:not([class*='size-'])]:size-6"
           ]
       ]
       @ attrs)
    children
;;

(* shadcn's [<Button asChild><AlertDialogPrimitive.Action>]: the button's
   classes and data attributes, the action's slot. *)
let button
      ~slot
      ~hook
      ?(attrs = [])
      ?class_
      ?(variant = Button.Variant.Default)
      ?(size = Button.Size.Default)
      ~on_click
      children
  =
  Node.button
    ~attrs:
      ([ Attr.type_ "button"
       ; data "slot" slot
       ; data "variant" (Button.Variant.name variant)
       ; data "size" (Button.Size.name size)
       ; Attr.class_
           (Button.classes
              ~variant
              ~size
              ~class_:(Cn.cn (hook :: Option.to_list class_))
              ())
       ; Attr.on_click (fun _ -> on_click)
       ]
       @ attrs)
    children
;;

let action ?attrs ?class_ ?variant ?size ?on_click (t : t) children =
  button
    ~slot:"alert-dialog-action"
    ~hook:""
    ?attrs
    ?class_
    ?variant
    ?size
    ~on_click:
      (Effect.Many
         [ Option.value on_click ~default:Effect.Ignore
         ; t.on_open_change false
         ])
    children
;;

let cancel
      ?attrs
      ?class_
      ?(variant = Button.Variant.Outline)
      ?size
      (t : t)
      children
  =
  button
    ~slot:"alert-dialog-cancel"
    ~hook:""
    ?attrs
    ?class_
    ~variant
    ?size
    ~on_click:(t.on_open_change false)
    children
;;
