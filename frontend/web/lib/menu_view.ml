open! Core
open! Import
open Html

let slot name = Attr.create "data-slot" name

let backdrop ?(cls = "") ~on_close children =
  let outside (ev : Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t) =
    Js_of_ocaml.Js.Unsafe.equals ev##.target ev##.currentTarget
  in
  Node.div
    ~attrs:
      [ Attr.class_ (String.strip ("menu-backdrop " ^ cls))
      ; Attr.on_click (fun ev -> if outside ev then on_close else Effect.Ignore)
      ; Attr.on_contextmenu (fun ev ->
          Js_of_ocaml.Dom.preventDefault ev;
          if outside ev then on_close else Effect.Ignore)
      ]
    children
;;

let surface ?(cls = "") ?(attrs = []) ~label children =
  Node.div
    ~attrs:
      ([ Attr.class_ (String.strip (cls ^ " menu-surface"))
       ; slot "dropdown-menu-content"
       ; Attr.role "menu"
       ; Attr.create "aria-label" label
       ; Attr.tabindex (-1)
       ]
       @ attrs)
    children
;;

let section ?(cls = "") children =
  Node.div
    ~attrs:
      [ Attr.class_ (String.strip ("menu-section " ^ cls))
      ; slot "dropdown-menu-group"
      ; Attr.role "group"
      ]
    children
;;

let heading text =
  Node.div
    ~attrs:[ Attr.class_ "menu-heading"; slot "dropdown-menu-label" ]
    [ Node.text text ]
;;

let item
      ?(cls = "")
      ?(selected = false)
      ?(danger = false)
      ?(disabled = false)
      ?icon:glyph
      ?hint
      ?shortcut
      ?(attrs = [])
      label
  =
  let label_node =
    match hint with
    | Some hint when not (String.is_empty hint) ->
      Node.span
        ~attrs:[ Attr.class_ "menu-text" ]
        [ span ~cls:"menu-label" label; span ~cls:"menu-hint" hint ]
    | _ -> span ~cls:"menu-label" label
  in
  Node.div
    ~attrs:
      ([ classes
           ("menu-item" :: (if String.is_empty cls then [] else [ cls ]))
           [ "selected", selected; "danger", danger; "disabled", disabled ]
       ; slot "dropdown-menu-item"
       ; Attr.role "menuitem"
       ; (if danger then Attr.create "data-variant" "destructive" else Attr.empty)
       ; (if disabled then Attr.create "aria-disabled" "true" else Attr.empty)
       ]
       @ attrs)
    [ Option.value_map glyph ~default:Node.none ~f:(fun g -> icon g)
    ; label_node
    ; Option.value_map shortcut ~default:Node.none ~f:(fun key ->
        Node.kbd
          ~attrs:[ Attr.class_ "menu-command"; slot "dropdown-menu-shortcut" ]
          [ Node.text key ])
    ]
;;
