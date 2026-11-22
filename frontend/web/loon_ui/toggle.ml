open! Import

module Variant = struct
  type t =
    | Default
    | Outline
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Default -> "default"
    | Outline -> "outline"
  ;;

  let classes = function
    | Default -> "bg-transparent"
    | Outline ->
      "border-input hover:bg-muted border bg-transparent"
  ;;
end

module Size = struct
  type t =
    | Default
    | Sm
    | Lg
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Default -> "default"
    | Sm -> "sm"
    | Lg -> "lg"
  ;;

  let classes = function
    | Default -> "h-8 min-w-8 px-2.5 has-data-[icon=inline-end]:pr-2 \
                  has-data-[icon=inline-start]:pl-2"
    | Sm -> "h-7 min-w-7 rounded-[min(var(--radius-md),12px)] px-2.5 text-[0.8rem] \
             has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 \
             [&_svg:not([class*='size-'])]:size-3.5"
    | Lg -> "h-9 min-w-9 px-2.5 has-data-[icon=inline-end]:pr-2 \
             has-data-[icon=inline-start]:pl-2"
  ;;
end

let base =
  "hover:text-foreground aria-pressed:bg-muted focus-visible:border-ring \
   focus-visible:ring-ring/50 aria-invalid:ring-destructive/20 \
   dark:aria-invalid:ring-destructive/40 aria-invalid:border-destructive \
   data-[state=on]:bg-muted gap-1 rounded-lg text-sm font-medium transition-all \
   [&_svg:not([class*='size-'])]:size-4 group/toggle inline-flex items-center \
   justify-center whitespace-nowrap outline-none hover:bg-muted \
   focus-visible:ring-[3px] disabled:pointer-events-none disabled:opacity-50 \
   [&_svg]:pointer-events-none [&_svg]:shrink-0"
;;

let class_list ?(variant = Variant.Default) ?(size = Size.Default) ?class_ () =
  [ base; Variant.classes variant; Size.classes size ] @ Option.to_list class_
;;

let classes ?variant ?size ?class_ () = Cn.cn (class_list ?variant ?size ?class_ ())

let view ?(attrs = []) ?class_ ?variant ?size ?(disabled = false) ~pressed ~on_change children
  =
  Node.button
    ~attrs:
      ([ Attr.type_ "button"
       ; aria_bool "pressed" pressed
       ; data "state" (if pressed then "on" else "off")
       ; data "slot" "toggle"
       ; Cn.attr (class_list ?variant ?size ?class_ ())
       ]
       @ (if disabled
          then [ data "disabled" ""; Attr.disabled ]
          else [ Attr.on_click (fun _ -> on_change (not pressed)) ])
       @ attrs)
    children
;;

let component
  ?attrs
  ?class_
  ?variant
  ?size
  ?disabled
  ?(default_pressed = false)
  children
  (local_ graph)
  =
  Control.make ~equal:Bool.equal default_pressed graph ~view:(fun pressed set ->
    view ?attrs ?class_ ?variant ?size ?disabled ~pressed ~on_change:set children)
;;
