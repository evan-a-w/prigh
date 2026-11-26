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
end

let view ?(attrs = []) ?class_ ?(orientation = Orientation.Horizontal) ?(decorative = true) ()
  =
  let role =
    if decorative
    then [ Attr.role "none" ]
    else
      Attr.role "separator"
      ::
      (match orientation with
       | Vertical -> [ aria "orientation" "vertical" ]
       | Horizontal -> [])
  in
  Node.div
    ~attrs:
      ([ data "slot" "separator" ]
       @ role
       @ [ data "orientation" (Orientation.name orientation)
         ; Cn.with_class
             ?class_
             [ "shrink-0 bg-border data-horizontal:h-px data-horizontal:w-full \
                data-vertical:w-px data-vertical:self-stretch"
             ]
         ]
       @ attrs)
    []
;;
