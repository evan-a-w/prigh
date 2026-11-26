open! Import

let part tag slot classes ?(attrs = []) ?class_ children =
  Node.create
    tag
    ~attrs:([ data "slot" slot; Cn.with_class ?class_ [ classes ] ] @ attrs)
    children
;;

let root ?(attrs = []) ?class_ children =
  Node.div
    ~attrs:
      [ data "slot" "table-container"
      ; Attr.class_ "relative w-full overflow-x-auto"
      ]
    [ Node.table
        ~attrs:
          ([ data "slot" "table"
           ; Cn.with_class ?class_ [ "w-full caption-bottom text-sm" ]
           ]
           @ attrs)
        children
    ]
;;

let header = part "thead" "table-header" "[&_tr]:border-b"
let body = part "tbody" "table-body" "[&_tr:last-child]:border-0"

let footer =
  part
    "tfoot"
    "table-footer"
    "bg-muted/50 border-t font-medium [&>tr]:last:border-b-0"
;;

let row ?(attrs = []) ?class_ ?(selected = false) children =
  part
    "tr"
    "table-row"
    "hover:bg-muted/50 data-[state=selected]:bg-muted border-b \
     transition-colors has-aria-expanded:bg-muted/50"
    ~attrs:((if selected then [ data "state" "selected" ] else []) @ attrs)
    ?class_
    children
;;

let head =
  part
    "th"
    "table-head"
    "text-foreground h-10 px-2 text-left align-middle font-medium \
     whitespace-nowrap [&:has([role=checkbox])]:pr-0"
;;

let cell ?(attrs = []) ?class_ ?col_span children =
  part
    "td"
    "table-cell"
    "p-2 align-middle whitespace-nowrap [&:has([role=checkbox])]:pr-0"
    ~attrs:
      (Option.value_map col_span ~default:[] ~f:(fun n ->
         [ Attr.create "colspan" (Int.to_string n) ])
       @ attrs)
    ?class_
    children
;;

let caption =
  part "caption" "table-caption" "text-muted-foreground mt-4 text-sm"
;;
