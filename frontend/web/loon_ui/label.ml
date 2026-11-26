open! Import

let view ?(attrs = []) ?class_ ?for_ children =
  Node.label
    ~attrs:
      ([ data "slot" "label"
       ; Cn.with_class
           ?class_
           [ "gap-2 text-sm leading-none font-medium \
              group-data-[disabled=true]:opacity-50 peer-disabled:opacity-50 \
              flex items-center select-none \
              group-data-[disabled=true]:pointer-events-none \
              peer-disabled:cursor-not-allowed"
           ]
       ; Attr.on_mousedown (fun ev ->
           (* Radix: a double click does not select the label's text. *)
           if (Js_of_ocaml.Js.Unsafe.get ev "detail" : int) > 1
           then Dom_util.prevent_default ev
           else Effect.Ignore)
       ]
       @ Option.value_map for_ ~default:[] ~f:(fun id -> [ Attr.for_ id ])
       @ attrs)
    children
;;
