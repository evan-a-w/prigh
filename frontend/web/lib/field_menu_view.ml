open! Core
open! Import
open Html
module Action = App.Action

let id = "field-menu"

let item_icon : Field_menu.Item.t -> Icon.t = function
  | Cut -> Scissors
  | Copy -> Copy
  | Paste -> Clipboard
  | Select_all -> Lines
;;

let view (m : App.Model.t) ~inject =
  match m.field_menu with
  | None -> Node.none
  | Some menu ->
    let close = inject Action.Close_field_menu in
    let row (item : Field_menu.Item.t) =
      let disabled = not (Field_menu.enabled menu item) in
      let selected = Field_menu.Item.equal item (Field_menu.selected menu) in
      Menu_view.item
        ~selected
        ~disabled
        ~icon:(item_icon item)
        ?shortcut:(Field_menu.Item.key (Field_menu.field menu).kind item)
        ~attrs:
          [ Attr.create
              "data-item"
              (String.lowercase (Sexp.to_string (Field_menu.Item.sexp_of_t item)))
          ; (* Keeps the field's focus and selection until an item is
               chosen. *)
            Attr.on_mousedown (fun ev ->
              Js_of_ocaml.Dom.preventDefault ev;
              Effect.Ignore)
          ; Attr.on_click (fun _ ->
              if disabled
              then Effect.Ignore
              else inject (Action.Field_menu_choose (Some item)))
          ; Attr.on_mouseenter (fun _ ->
              if selected || disabled
              then Effect.Ignore
              else inject (Action.Field_menu_select item))
          ]
        (Field_menu.Item.label item)
    in
    Menu_view.backdrop
      ~cls:"context-menu-backdrop field-menu-backdrop"
      ~on_close:close
      [ Menu_view.surface
          ~cls:"context-menu field-menu"
          ~label:"Edit"
          ~attrs:[ Attr.id id ]
          (List.map (Field_menu.sections menu) ~f:(fun items ->
             Menu_view.section (List.map items ~f:row)))
      ]
;;
