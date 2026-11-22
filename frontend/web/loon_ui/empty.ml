open! Import

module Media_variant = struct
  type t =
    | Default
    | Icon
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Default -> "default"
    | Icon -> "icon"
  ;;

  let classes = function
    | Default -> "bg-transparent"
    | Icon ->
      "bg-muted text-foreground flex size-8 shrink-0 items-center justify-center \
       rounded-lg [&_svg:not([class*='size-'])]:size-4"
  ;;
end

let div slot classes ?(attrs = []) ?class_ children =
  Node.div ~attrs:([ data "slot" slot; Cn.with_class ?class_ [ classes ] ] @ attrs) children
;;

let root =
  div
    "empty"
    "gap-4 rounded-xl border-dashed p-6 flex w-full min-w-0 flex-1 flex-col \
     items-center justify-center text-center text-balance"
;;

let header = div "empty-header" "gap-2 flex max-w-sm flex-col items-center"

let media ?(attrs = []) ?class_ ?(variant = Media_variant.Default) children =
  Node.div
    ~attrs:
      ([ data "slot" "empty-icon"
       ; data "variant" (Media_variant.name variant)
       ; Cn.with_class
           ?class_
           [ "mb-2 flex shrink-0 items-center justify-center \
              [&_svg]:pointer-events-none [&_svg]:shrink-0"
           ; Media_variant.classes variant
           ]
       ]
       @ attrs)
    children
;;

let title = div "empty-title" "text-sm font-medium tracking-tight font-heading"

let description =
  div
    "empty-description"
    "text-sm/relaxed text-muted-foreground [&>a]:underline [&>a]:underline-offset-4 \
     [&>a:hover]:text-primary"
;;

let content =
  div
    "empty-content"
    "gap-2.5 text-sm flex w-full max-w-sm min-w-0 flex-col items-center text-balance"
;;
