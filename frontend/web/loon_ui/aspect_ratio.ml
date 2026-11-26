open! Import

let view ?(attrs = []) ?class_ ?(ratio = 1.) children =
  Node.div
    ~attrs:
      [ Attr.style
          (Css_gen.concat
             [ Css_gen.create ~field:"position" ~value:"relative"
             ; Css_gen.create ~field:"width" ~value:"100%"
             ; Css_gen.create
                 ~field:"padding-bottom"
                 ~value:(sprintf "%s%%" (Float.to_string_hum ~strip_zero:true ~decimals:4 (100. /. ratio)))
             ])
      ; data "radix-aspect-ratio-wrapper" ""
      ]
    [ Node.div
        ~attrs:
          ([ data "slot" "aspect-ratio"
           ; Attr.style
               (Css_gen.concat
                  [ Css_gen.create ~field:"position" ~value:"absolute"
                  ; Css_gen.create ~field:"top" ~value:"0"
                  ; Css_gen.create ~field:"right" ~value:"0"
                  ; Css_gen.create ~field:"bottom" ~value:"0"
                  ; Css_gen.create ~field:"left" ~value:"0"
                  ])
           ]
           @ Option.value_map class_ ~default:[] ~f:(fun c -> [ Cn.attr [ c ] ])
           @ attrs)
        children
    ]
;;
