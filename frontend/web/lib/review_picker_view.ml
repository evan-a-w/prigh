open! Core
open! Import
open Html
module Action = App.Action

let option_id i = sprintf "review-pick-option-%d" i

let title (p : Review_picker.t) =
  match Review_picker.which p with
  | Target -> "What to review"
  | Base -> "What to compare it with"
  | Since _ -> "What the since-view compares with"
;;

let row ~selected i (item : Review_picker.Item.t) ~inject =
  Node.div
    ~attrs:
      [ classes
          [ "menu-item"; "popover-item"; "review-pick-item" ]
          [ "selected", i = selected; "marked", item.marked ]
      ; Attr.role "option"
      ; Attr.id (option_id i)
      ; Attr.create "data-id" (Review_picker.Item.id item)
      ; Attr.create "aria-selected" (Bool.to_string (i = selected))
      ; Attr.create "aria-checked" (Bool.to_string item.marked)
      ; Attr.on_click (fun _ ->
          inject (Action.Picker_choose (Review_picker.Item.id item)))
      ]
    [ (if item.marked
       then Html.mark ~cls:"picked popover-check" Check
       else Node.span ~attrs:[ Attr.class_ "mark popover-check" ] [])
    ; Node.span
        ~attrs:[ Attr.class_ "menu-text" ]
        [ span ~cls:"menu-label" item.label
        ; (if String.is_empty item.detail
           then Node.none
           else span ~cls:"menu-detail" item.detail)
        ]
    ]
;;

(* Under their headings unless filtered (then best first, the typed one
   last). *)
let rows (p : Review_picker.t) ~inject =
  let selected = Review_picker.selected p in
  let numbered =
    List.mapi (Review_picker.visible p) ~f:(fun i item -> i, item)
  in
  if not (String.is_empty (Review_picker.query p))
  then List.map numbered ~f:(fun (i, item) -> row ~selected i item ~inject)
  else
    List.group numbered ~break:(fun (_, a) (_, b) ->
      not (String.equal a.Review_picker.Item.group b.group))
    |> List.map ~f:(fun group ->
      let name =
        Option.value_map (List.hd group) ~default:"" ~f:(fun (_, item) ->
          item.Review_picker.Item.group)
      in
      Node.div
        ~attrs:
          [ Attr.class_ "popover-group"
          ; Attr.role "group"
          ; Attr.create "aria-label" name
          ]
        (div ~cls:"menu-heading" [ Node.text name ]
         :: List.map group ~f:(fun (i, item) -> row ~selected i item ~inject)))
;;

let view (p : Review_picker.t) ~inject =
  let visible = Review_picker.visible p in
  let active =
    if List.is_empty visible
    then Attr.empty
    else
      Attr.create "aria-activedescendant" (option_id (Review_picker.selected p))
  in
  let which =
    match Review_picker.which p with
    | Target -> "target"
    | Base -> "base"
    | Since _ -> "since"
  in
  Node.div
    ~attrs:
      [ Attr.class_ "review-popover menu-surface"
      ; Attr.id "review-popover"
      ; Attr.create "data-which" which
      ; Attr.role "dialog"
      ; Attr.create "aria-label" (title p)
      ]
    [ div
        ~cls:"popover-search"
        [ icon Search
        ; Node.input
            ~attrs:
              [ Attr.id "review-pick-filter"
              ; Attr.type_ "text"
              ; Attr.create "aria-label" (title p)
              ; Attr.role "combobox"
              ; Attr.create "aria-controls" "review-pick-list"
              ; Attr.create "aria-expanded" "true"
              ; Attr.create "aria-autocomplete" "list"
              ; active
              ; Attr.placeholder
                  (match Review_picker.which p with
                   | Target -> "Filter worktrees and branches…"
                   | Base -> "A branch, tag, commit or any revision…"
                   | Since _ -> "Filter the actions on this file…")
              ; Attr.value (Review_picker.query p)
              ; Attr.create "autocomplete" "off"
              ; Attr.create "spellcheck" "false"
              ; Attr.on_input (fun _ text -> inject (Action.Dialog_input text))
              ]
            ()
        ; span
            ~cls:"popover-shortcut"
            (match Review_picker.which p with
             | Target -> "r"
             | Base -> "b"
             | Since _ -> "L")
        ]
    ; (match visible with
       | [] ->
         div
           ~cls:"popover-empty"
           [ Node.textf
               "Nothing matches “%s”: Backspace widens the search."
               (Review_picker.query p)
           ]
       | _ ->
         Node.div
           ~key:(Review_picker.query p)
           ~attrs:
             [ Attr.class_ "popover-items"
             ; Attr.id "review-pick-list"
             ; Attr.role "listbox"
             ; Attr.create "aria-label" (title p)
             ]
           (rows p ~inject))
    ; div ~cls:"menu-keys" [ Node.text "↑ ↓ and Enter choose, Esc closes" ]
    ]
;;
