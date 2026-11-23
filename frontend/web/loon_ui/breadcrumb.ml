open! Import

let root ?(attrs = []) ?class_ children =
  Node.create
    "nav"
    ~attrs:
      ([ aria "label" "breadcrumb"
       ; data "slot" "breadcrumb"
       ; Cn.with_class ?class_ [ "" ]
       ]
       @ attrs)
    children
;;

let list ?(attrs = []) ?class_ children =
  Node.ol
    ~attrs:
      ([ data "slot" "breadcrumb-list"
       ; Cn.with_class
           ?class_
           [ "text-muted-foreground gap-1.5 text-sm flex flex-wrap items-center \
              wrap-break-word" ]
       ]
       @ attrs)
    children
;;

let item ?(attrs = []) ?class_ children =
  Node.li
    ~attrs:
      ([ data "slot" "breadcrumb-item"
       ; Cn.with_class ?class_ [ "gap-1 inline-flex items-center" ]
       ]
       @ attrs)
    children
;;

let link_base = "hover:text-foreground transition-colors"
let link_classes ?class_ () = Cn.cn (link_base :: Option.to_list class_)

let link ?(attrs = []) ?class_ ~href children =
  Node.a
    ~attrs:
      ([ data "slot" "breadcrumb-link"
       ; Attr.href href
       ; Cn.with_class ?class_ [ link_base ]
       ]
       @ attrs)
    children
;;

let page ?(attrs = []) ?class_ children =
  Node.span
    ~attrs:
      ([ data "slot" "breadcrumb-page"
       ; Attr.role "link"
       ; aria "disabled" "true"
       ; aria "current" "page"
       ; Cn.with_class ?class_ [ "text-foreground font-normal" ]
       ]
       @ attrs)
    children
;;

let separator ?(attrs = []) ?class_ ?children () =
  Node.li
    ~attrs:
      ([ data "slot" "breadcrumb-separator"
       ; Attr.role "presentation"
       ; aria "hidden" "true"
       ; Cn.with_class ?class_ [ "[&>svg]:size-3.5" ]
       ]
       @ attrs)
    (Option.value children ~default:[ Icon.view ~class_:"" Chevron_right ])
;;

let ellipsis ?(attrs = []) ?class_ () =
  Node.span
    ~attrs:
      ([ data "slot" "breadcrumb-ellipsis"
       ; Attr.role "presentation"
       ; aria "hidden" "true"
       ; Cn.with_class
           ?class_
           [ "size-5 [&>svg]:size-4 flex items-center justify-center" ]
       ]
       @ attrs)
    [ Icon.view Ellipsis
    ; Node.span ~attrs:[ Attr.class_ "sr-only" ] [ Node.text "More" ]
    ]
;;
