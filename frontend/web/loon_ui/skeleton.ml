open! Import

let view ?(attrs = []) ?class_ children =
  Node.div
    ~attrs:
      ([ data "slot" "skeleton"
       ; Cn.with_class ?class_ [ "bg-muted rounded-md animate-pulse" ]
       ]
       @ attrs)
    children
;;
