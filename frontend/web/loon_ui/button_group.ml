open! Import

module Orientation = struct
  type t =
    | Horizontal
    | Vertical
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Horizontal -> "horizontal"
    | Vertical -> "vertical"
  ;;

  let classes = function
    | Horizontal ->
      "[&>[data-slot]:not(:has(~[data-slot]))]:rounded-r-lg! \
       [&>*:not(:first-child)]:rounded-l-none \
       [&>*:not(:first-child)]:border-l-0 \
       [&>*:not(:last-child)]:rounded-r-none"
    | Vertical ->
      "[&>[data-slot]:not(:has(~[data-slot]))]:rounded-b-lg! flex-col \
       [&>*:not(:first-child)]:rounded-t-none \
       [&>*:not(:first-child)]:border-t-0 \
       [&>*:not(:last-child)]:rounded-b-none"
  ;;
end

let class_list ?(orientation = Orientation.Horizontal) ?class_ () =
  [ "has-[>[data-slot=button-group]]:gap-2 \
     has-[select[aria-hidden=true]:last-child]:[&>[data-slot=select-trigger]:last-of-type]:rounded-r-lg \
     group/button-group flex w-fit items-stretch *:focus-visible:relative \
     *:focus-visible:z-10 \
     [&>[data-slot=select-trigger]:not([class*='w-'])]:w-fit [&>input]:flex-1"
  ; Orientation.classes orientation
  ]
  @ Option.to_list class_
;;

let classes ?orientation ?class_ () = Cn.cn (class_list ?orientation ?class_ ())

let root ?(attrs = []) ?class_ ?orientation children =
  Node.div
    ~attrs:
      ([ Attr.role "group"; data "slot" "button-group" ]
       @ Option.value_map orientation ~default:[] ~f:(fun o ->
         [ data "orientation" (Orientation.name o) ])
       @ [ Cn.attr (class_list ?orientation ?class_ ()) ]
       @ attrs)
    children
;;

let text ?(attrs = []) ?class_ children =
  Node.div
    ~attrs:
      (Cn.with_class
         ?class_
         [ "bg-muted gap-2 rounded-lg border px-2.5 text-sm font-medium \
            [&_svg:not([class*='size-'])]:size-4 flex items-center \
            [&_svg]:pointer-events-none"
         ]
       :: attrs)
    children
;;

(* shadcn's Separator (decorative) with the group's classes. *)
let separator ?(attrs = []) ?class_ ?(orientation = Orientation.Vertical) () =
  Node.div
    ~attrs:
      ([ data "slot" "button-group-separator"
       ; Attr.role "none"
       ; data "orientation" (Orientation.name orientation)
       ; Cn.with_class
           ?class_
           [ "shrink-0 bg-border data-horizontal:h-px data-horizontal:w-full \
              data-vertical:w-px data-vertical:self-stretch"
           ; "bg-input relative self-stretch data-horizontal:mx-px \
              data-horizontal:w-auto data-vertical:my-px data-vertical:h-auto"
           ]
       ]
       @ attrs)
    []
;;
