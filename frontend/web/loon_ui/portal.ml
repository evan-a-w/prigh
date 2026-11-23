open! Import
open Js_of_ocaml

let matches (el : Dom_html.element Js.t) selector =
  try Js.to_bool ((Js.Unsafe.coerce el)##matches (Js.string selector)) with
  | _ -> false
;;

let is_shown el = matches el ":popover-open"

let show_one (el : Dom_html.element Js.t) =
  if
    Js.Optdef.test (Js.Unsafe.coerce el)##.showPopover
    && Js.to_bool (Js.Unsafe.coerce el)##.isConnected
    && not (is_shown el)
  then (
    try (Js.Unsafe.coerce el)##showPopover with
    | _ -> ())
;;

let show (el : Dom_html.element Js.t) =
  if Js.to_bool (el##hasAttribute (Js.string "popover")) then show_one el;
  List.iter
    (Dom.list_of_nodeList (el##querySelectorAll (Js.string "[popover]")))
    ~f:show_one
;;

let style fields =
  Attr.style
    (Css_gen.concat
       (List.map fields ~f:(fun (field, value) -> Css_gen.create ~field ~value)))
;;

let reset =
  [ "inset", "auto"
  ; "margin", "0"
  ; "border", "0"
  ; "padding", "0"
  ; "background", "transparent"
  ; "color", "inherit"
  ; "overflow", "visible"
  ; "max-width", "none"
  ; "max-height", "none"
    (* What the content would inherit from where it is in the DOM (a table
       cell's [whitespace-nowrap], a header's weight), not from the body
       as React's portals do. *)
  ; "white-space", "normal"
  ; "text-align", "start"
  ; "font-weight", "normal"
  ; "font-style", "normal"
  ; "letter-spacing", "normal"
  ; "text-transform", "none"
  ]
;;

let positioned = [ Attr.create "popover" "manual"; style reset ]

module Show_hook = Vdom.Attr.Hooks.Make (struct
    module State = Unit

    module Input = struct
      type t = unit [@@deriving sexp_of]

      let combine () () = ()
    end

    let init () _ = ()

    let on_mount =
      `Schedule_immediately_after_this_dom_patch_completes
        (fun () () el -> show el)
    ;;

    let update ~old_input:() ~new_input:() () _ = ()
    let destroy () () _ = ()
  end)

let fullscreen =
  [ Attr.create "popover" "manual"
  ; style
      (reset
       @ [ "position", "fixed"
         ; "inset", "0"
         ; "width", "100%"
         ; "height", "100%"
         ; "pointer-events", "none"
         ; "z-index", "50"
         ])
  ; Vdom.Attr.create_hook "loon-portal" (Show_hook.create ())
  ]
;;
