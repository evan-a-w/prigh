open! Import

let view ?(attrs = []) ?class_ children =
  Node.create
    "kbd"
    ~attrs:
      ([ data "slot" "kbd"
       ; Cn.with_class
           ?class_
           [ "bg-muted text-muted-foreground \
              in-data-[slot=tooltip-content]:bg-background/20 \
              in-data-[slot=tooltip-content]:text-background \
              dark:in-data-[slot=tooltip-content]:bg-background/10 h-5 w-fit min-w-5 \
              gap-1 rounded-sm px-1 font-sans text-xs font-medium \
              [&_svg:not([class*='size-'])]:size-3 pointer-events-none inline-flex \
              items-center justify-center select-none"
           ]
       ]
       @ attrs)
    children
;;

let group ?(attrs = []) ?class_ children =
  Node.create
    "kbd"
    ~attrs:
      ([ data "slot" "kbd-group"
       ; Cn.with_class ?class_ [ "gap-1 inline-flex items-center" ]
       ]
       @ attrs)
    children
;;
