open! Import

module Variant = struct
  type t =
    | Default
    | Secondary
    | Destructive
    | Outline
    | Ghost
    | Link
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Default -> "default"
    | Secondary -> "secondary"
    | Destructive -> "destructive"
    | Outline -> "outline"
    | Ghost -> "ghost"
    | Link -> "link"
  ;;

  let classes = function
    | Default -> "bg-primary text-primary-foreground [a]:hover:bg-primary/80"
    | Secondary ->
      "bg-secondary text-secondary-foreground [a]:hover:bg-secondary/80"
    | Destructive ->
      "bg-destructive/10 [a]:hover:bg-destructive/20 \
       focus-visible:ring-destructive/20 \
       dark:focus-visible:ring-destructive/40 text-destructive \
       dark:bg-destructive/20"
    | Outline ->
      "border-border text-foreground [a]:hover:bg-muted \
       [a]:hover:text-muted-foreground"
    | Ghost ->
      "hover:bg-muted hover:text-muted-foreground dark:hover:bg-muted/50"
    | Link -> "text-primary underline-offset-4 hover:underline"
  ;;
end

let base =
  "h-5 gap-1 rounded-4xl border border-transparent px-2 py-0.5 text-xs \
   font-medium transition-all has-data-[icon=inline-end]:pr-1.5 \
   has-data-[icon=inline-start]:pl-1.5 [&>svg]:size-3! group/badge inline-flex \
   w-fit shrink-0 items-center justify-center overflow-hidden \
   whitespace-nowrap focus-visible:border-ring focus-visible:ring-[3px] \
   focus-visible:ring-ring/50 aria-invalid:border-destructive \
   aria-invalid:ring-destructive/20 dark:aria-invalid:ring-destructive/40 \
   [&>svg]:pointer-events-none"
;;

let class_list ?(variant = Variant.Default) ?class_ () =
  [ base; Variant.classes variant ] @ Option.to_list class_
;;

let classes ?variant ?class_ () = Cn.cn (class_list ?variant ?class_ ())

let attrs_of ?(variant = Variant.Default) ?class_ attrs =
  [ data "slot" "badge"
  ; data "variant" (Variant.name variant)
  ; Cn.attr (class_list ~variant ?class_ ())
  ]
  @ attrs
;;

let view ?(attrs = []) ?class_ ?variant children =
  Node.span ~attrs:(attrs_of ?variant ?class_ attrs) children
;;

let link ?(attrs = []) ?class_ ?variant ~href children =
  Node.a ~attrs:(Attr.href href :: attrs_of ?variant ?class_ attrs) children
;;
