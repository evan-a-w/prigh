open! Core
open! Import

let view ?(variant = Loon.Badge.Variant.Secondary) ?(attrs = []) ~cls text =
  Node.span
    ~attrs:
      ([ Attr.class_ (Loon.Badge.classes ~variant ~class_:cls ())
       ; Attr.create "data-slot" "badge"
       ]
       @ attrs)
    [ Node.text text ]
;;
