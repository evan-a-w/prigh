open! Core
open! Import
open Html

let slot name = Attr.create "data-slot" name

let view
      ?(noun = "items")
      ?empty
      ?(leading = fun _ -> [])
      ?(trailing = fun _ -> [])
      (p : Picker.t)
      ~on_query
      ~on_choose
  =
  let visible = Picker.visible p in
  let input =
    Node.div
      ~attrs:[ Attr.class_ "search"; slot "command-input-wrapper" ]
      [ icon Search
      ; Node.input
          ~attrs:
            [ Attr.id "picker-input"
            ; slot "command-input"
            ; Attr.type_ "text"
            ; Attr.create "aria-label" ("Filter " ^ noun)
            ; Attr.role "combobox"
            ; Attr.create "aria-controls" "picker-list"
            ; Attr.create "aria-expanded" "true"
            ; Attr.create "aria-autocomplete" "list"
            ; (if List.is_empty visible
               then Attr.empty
               else
                 Attr.create
                   "aria-activedescendant"
                   (sprintf "picker-option-%d" (Picker.selected p)))
            ; Attr.placeholder ("Filter " ^ noun ^ "…")
            ; Attr.value (Picker.query p)
            ; Attr.create "autocomplete" "off"
            ; Attr.create "spellcheck" "false"
            ; Attr.on_input (fun _ text -> on_query text)
            ]
          ()
      ]
  in
  let row i (item : Picker.Item.t) =
    let selected = i = Picker.selected p in
    Node.div
      ~attrs:
        [ classes
            [ "picker-item" ]
            [ "selected", selected; "marked", item.marked; "dimmed", item.dimmed ]
        ; slot "command-item"
        ; Attr.role "option"
        ; Attr.id (sprintf "picker-option-%d" i)
        ; Attr.create "aria-selected" (Bool.to_string selected)
        ; (if item.marked then Attr.create "aria-checked" "true" else Attr.empty)
        ; Attr.on_click (fun _ -> on_choose item.id)
        ]
      (leading item
       @ [ span ~cls:"picker-label" item.label
         ; (if String.is_empty item.detail
            then Node.none
            else span ~cls:"picker-detail" item.detail)
         ; (if item.marked then Html.mark ~cls:"picker-check" Check else Node.none)
         ]
       @ trailing item)
  in
  [ input
  ; (match visible with
     | [] ->
       Node.div
         ~attrs:[ Attr.class_ "picker-empty"; slot "command-empty" ]
         [ Option.value
             empty
             ~default:
               (Node.textf
                  "Nothing matches “%s”: Backspace widens the search."
                  (Picker.query p))
         ]
     | visible ->
       Node.div
         ~attrs:
           [ Attr.class_ "picker-items"
           ; slot "command-list"
           ; Attr.role "listbox"
           ; Attr.id "picker-list"
           ; Attr.create "aria-label" (Picker.title p)
           ]
         (List.mapi visible ~f:row))
  ]
;;
