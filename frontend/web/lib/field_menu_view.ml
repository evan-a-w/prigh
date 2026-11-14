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
      Node.div
        ~attrs:
          [ classes
              [ "menu-item" ]
              [ "selected", selected; "disabled", disabled ]
          ; Attr.role "menuitem"
          ; Attr.create
              "data-item"
              (String.lowercase
                 (Sexp.to_string (Field_menu.Item.sexp_of_t item)))
          ; (if disabled then Attr.create "aria-disabled" "true" else Attr.empty)
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
        [ icon (item_icon item)
        ; span ~cls:"menu-label" (Field_menu.Item.label item)
        ; (match Field_menu.Item.key (Field_menu.field menu).kind item with
           | None -> Node.none
           | Some key ->
             Node.kbd ~attrs:[ Attr.class_ "menu-command" ] [ Node.text key ])
        ]
    in
    let section items = div ~cls:"menu-section" (List.map items ~f:row) in
    Node.div
      ~attrs:
        [ Attr.class_ "menu-backdrop context-menu-backdrop field-menu-backdrop"
        ; Attr.on_click (fun ev ->
            if Js_of_ocaml.Js.Unsafe.equals ev##.target ev##.currentTarget
            then close
            else Effect.Ignore)
        ; Attr.on_contextmenu (fun ev ->
            Js_of_ocaml.Dom.preventDefault ev;
            if Js_of_ocaml.Js.Unsafe.equals ev##.target ev##.currentTarget
            then close
            else Effect.Ignore)
        ]
      [ Node.div
          ~attrs:
            [ Attr.class_ "context-menu field-menu menu-surface"
            ; Attr.id id
            ; Attr.role "menu"
            ; Attr.create "aria-label" "Edit"
            ; Attr.tabindex (-1)
            ]
          (List.map (Field_menu.sections menu) ~f:section)
      ]
;;
