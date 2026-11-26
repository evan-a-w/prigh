open! Import

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

let div slot classes ?(attrs = []) ?class_ children =
  Node.div
    ~attrs:([ data "slot" slot; Cn.with_class ?class_ [ classes ] ] @ attrs)
    children
;;

let root ?(attrs = []) ?class_ ?(size = Size.Default) children =
  div
    "card"
    "ring-foreground/10 bg-card text-card-foreground gap-(--card-spacing) \
     overflow-hidden rounded-xl py-(--card-spacing) text-sm ring-1 \
     [--card-spacing:--spacing(4)] has-data-[slot=card-footer]:pb-0 \
     has-[>img:first-child]:pt-0 data-[size=sm]:[--card-spacing:--spacing(3)] \
     data-[size=sm]:has-data-[slot=card-footer]:pb-0 \
     *:[img:first-child]:rounded-t-xl *:[img:last-child]:rounded-b-xl \
     group/card flex flex-col"
    ~attrs:(data "size" (Size.name size) :: attrs)
    ?class_
    children
;;

let header =
  div
    "card-header"
    "gap-1 rounded-t-xl px-(--card-spacing) [.border-b]:pb-(--card-spacing) \
     group/card-header @container/card-header grid auto-rows-min items-start \
     has-data-[slot=card-action]:grid-cols-[1fr_auto] \
     has-data-[slot=card-description]:grid-rows-[auto_auto]"
;;

let title =
  div
    "card-title"
    "text-base leading-snug font-medium group-data-[size=sm]/card:text-sm \
     font-heading"
;;

let description = div "card-description" "text-muted-foreground text-sm"

let action =
  div
    "card-action"
    "col-start-2 row-span-2 row-start-1 self-start justify-self-end"
;;

let content = div "card-content" "px-(--card-spacing)"

let footer =
  div
    "card-footer"
    "bg-muted/50 rounded-b-xl border-t p-(--card-spacing) flex items-center"
;;
