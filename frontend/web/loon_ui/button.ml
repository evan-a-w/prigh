open! Import

module Variant = struct
  type t =
    | Default
    | Destructive
    | Outline
    | Secondary
    | Ghost
    | Link
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Default -> "default"
    | Destructive -> "destructive"
    | Outline -> "outline"
    | Secondary -> "secondary"
    | Ghost -> "ghost"
    | Link -> "link"
  ;;

  let classes = function
    | Default -> "bg-primary text-primary-foreground hover:bg-primary/80"
    | Outline -> "border-border bg-background hover:bg-muted hover:text-foreground \
                  dark:bg-input/30 dark:border-input dark:hover:bg-input/50 \
                  aria-expanded:bg-muted aria-expanded:text-foreground"
    | Secondary -> "bg-secondary text-secondary-foreground \
                    hover:bg-[color-mix(in_oklch,var(--secondary),var(--foreground)_5%)] \
                    aria-expanded:bg-secondary aria-expanded:text-secondary-foreground"
    | Ghost -> "hover:bg-muted hover:text-foreground dark:hover:bg-muted/50 \
                aria-expanded:bg-muted aria-expanded:text-foreground"
    | Destructive -> "bg-destructive/10 hover:bg-destructive/20 \
                      focus-visible:ring-destructive/20 \
                      dark:focus-visible:ring-destructive/40 dark:bg-destructive/20 \
                      text-destructive focus-visible:border-destructive/40 \
                      dark:hover:bg-destructive/30"
    | Link -> "text-primary underline-offset-4 hover:underline"
  ;;
end

module Size = struct
  type t =
    | Default
    | Xs
    | Sm
    | Lg
    | Icon
    | Icon_xs
    | Icon_sm
    | Icon_lg
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Default -> "default"
    | Xs -> "xs"
    | Sm -> "sm"
    | Lg -> "lg"
    | Icon -> "icon"
    | Icon_xs -> "icon-xs"
    | Icon_sm -> "icon-sm"
    | Icon_lg -> "icon-lg"
  ;;

  let classes = function
    | Default -> "h-8 gap-1.5 px-2.5 has-data-[icon=inline-end]:pr-2 \
                  has-data-[icon=inline-start]:pl-2"
    | Xs -> "h-6 gap-1 rounded-[min(var(--radius-md),10px)] px-2 text-xs \
             in-data-[slot=button-group]:rounded-lg has-data-[icon=inline-end]:pr-1.5 \
             has-data-[icon=inline-start]:pl-1.5 [&_svg:not([class*='size-'])]:size-3"
    | Sm -> "h-7 gap-1 rounded-[min(var(--radius-md),12px)] px-2.5 text-[0.8rem] \
             in-data-[slot=button-group]:rounded-lg has-data-[icon=inline-end]:pr-1.5 \
             has-data-[icon=inline-start]:pl-1.5 [&_svg:not([class*='size-'])]:size-3.5"
    | Lg -> "h-9 gap-1.5 px-2.5 has-data-[icon=inline-end]:pr-2 \
             has-data-[icon=inline-start]:pl-2"
    | Icon -> "size-8"
    | Icon_xs -> "size-6 rounded-[min(var(--radius-md),10px)] \
                  in-data-[slot=button-group]:rounded-lg \
                  [&_svg:not([class*='size-'])]:size-3"
    | Icon_sm -> "size-7 rounded-[min(var(--radius-md),12px)] \
                  in-data-[slot=button-group]:rounded-lg"
    | Icon_lg -> "size-9"
  ;;
end

let base =
  "focus-visible:border-ring focus-visible:ring-ring/50 \
   aria-invalid:ring-destructive/20 dark:aria-invalid:ring-destructive/40 \
   aria-invalid:border-destructive dark:aria-invalid:border-destructive/50 rounded-lg \
   border border-transparent bg-clip-padding text-sm font-medium focus-visible:ring-3 \
   aria-invalid:ring-3 active:not-aria-[haspopup]:translate-y-px \
   [&_svg:not([class*='size-'])]:size-4 group/button inline-flex shrink-0 items-center \
   justify-center whitespace-nowrap transition-all outline-none select-none \
   disabled:pointer-events-none disabled:opacity-50 [&_svg]:pointer-events-none \
   [&_svg]:shrink-0"
;;

let class_list ?(variant = Variant.Default) ?(size = Size.Default) ?class_ () =
  [ base; Variant.classes variant; Size.classes size ] @ Option.to_list class_
;;

let classes ?variant ?size ?class_ () = Cn.cn (class_list ?variant ?size ?class_ ())

let slot_attrs ?(variant = Variant.Default) ?(size = Size.Default) () =
  [ data "slot" "button"; data "variant" (Variant.name variant); data "size" (Size.name size) ]
;;

let view
  ?(attrs = [])
  ?class_
  ?variant
  ?size
  ?(disabled = false)
  ?(type_ = `Button)
  ?on_click
  children
  =
  Node.button
    ~attrs:
      ([ Attr.type_
           (match type_ with
            | `Button -> "button"
            | `Submit -> "submit"
            | `Reset -> "reset")
       ]
       @ slot_attrs ?variant ?size ()
       @ [ Cn.attr (class_list ?variant ?size ?class_ ()) ]
       @ (if disabled then [ Attr.disabled ] else [])
       @ Option.value_map on_click ~default:[] ~f:(fun effect ->
         [ Attr.on_click (fun _ -> effect) ])
       @ attrs)
    children
;;

let link ?(attrs = []) ?class_ ?variant ?size ~href children =
  Node.a
    ~attrs:
      (slot_attrs ?variant ?size ()
       @ [ Attr.href href; Cn.attr (class_list ?variant ?size ?class_ ()) ]
       @ attrs)
    children
;;
