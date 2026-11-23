open! Import

module Variant = struct
  type t =
    | Default
    | Destructive
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Default -> "default"
    | Destructive -> "destructive"
  ;;

  let classes = function
    | Default -> "bg-card text-card-foreground"
    | Destructive -> "text-destructive bg-card \
                      *:data-[slot=alert-description]:text-destructive/90 \
                      *:[svg]:text-current"
  ;;
end

let base =
  "grid gap-0.5 rounded-lg border px-2.5 py-2 text-left text-sm \
   has-data-[slot=alert-action]:relative has-data-[slot=alert-action]:pr-18 \
   has-[>svg]:grid-cols-[auto_1fr] has-[>svg]:gap-x-2 *:[svg]:row-span-2 \
   *:[svg]:translate-y-0.5 *:[svg]:text-current *:[svg:not([class*='size-'])]:size-4 \
   group/alert relative w-full"
;;

let class_list ?(variant = Variant.Default) ?class_ () =
  [ base; Variant.classes variant ] @ Option.to_list class_
;;

let classes ?variant ?class_ () = Cn.cn (class_list ?variant ?class_ ())

let root ?(attrs = []) ?class_ ?variant children =
  Node.div
    ~attrs:
      ([ data "slot" "alert"; Attr.role "alert"; Cn.attr (class_list ?variant ?class_ ()) ]
       @ attrs)
    children
;;

let div slot classes ?(attrs = []) ?class_ children =
  Node.div ~attrs:([ data "slot" slot; Cn.with_class ?class_ [ classes ] ] @ attrs) children
;;

let title =
  div
    "alert-title"
    "font-medium group-has-[>svg]/alert:col-start-2 [&_a]:underline \
     [&_a]:underline-offset-3 [&_a]:hover:text-foreground"
;;

let description =
  div
    "alert-description"
    "text-muted-foreground text-sm text-balance md:text-pretty \
     [&_p:not(:last-child)]:mb-4 [&_a]:underline [&_a]:underline-offset-3 \
     [&_a]:hover:text-foreground"
;;

let action = div "alert-action" "absolute top-2 right-2"
