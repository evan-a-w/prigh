open! Import

let view ?(attrs = []) ?class_ () =
  Icon.view
    ~attrs:([ data "slot" "spinner"; Attr.role "status"; aria "label" "Loading" ] @ attrs)
    ~class_:(Cn.cn ([ "size-4 animate-spin" ] @ Option.to_list class_))
    Loader_circle
;;
