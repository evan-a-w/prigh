open! Core
open! Import
open Html
module Action = App.Action

let popup (m : App.Model.t) ~inject =
  match App.Model.popup m with
  | None -> Node.none
  | Some c ->
    let title =
      match Completion.source c with
      | Command -> "Commands"
      | Argument Model -> "Models"
      | Argument Thinking -> "Thinking"
      | Argument (Login | Logout) -> "Providers"
      | Argument Directory -> "Directories"
      | Path -> "Files"
    in
    div
      ~cls:"popup"
      ~attrs:[ Attr.role "listbox" ]
      [ div ~cls:"popup-title" [ Node.text title; span ~cls:"popup-keys" "↑↓ Tab Enter Esc" ]
      ; div
          ~cls:"popup-items"
          (List.mapi (List.take (Completion.items c) 50) ~f:(fun i (item : Picker.Item.t) ->
             Node.div
               ~attrs:
                 [ classes
                     [ "popup-item" ]
                     [ "selected", i = Completion.selected c; "dimmed", item.dimmed; "marked", item.marked ]
                 ; Attr.role "option"
                 ; Attr.on_mousedown (fun ev ->
                     Js_of_ocaml.Dom.preventDefault ev;
                     inject (Action.Complete_choose i))
                 ]
               [ span ~cls:"popup-label" item.label
               ; (if String.is_empty item.detail then Node.none else span ~cls:"popup-detail" item.detail)
               ]))
      ]
;;

let images (m : App.Model.t) ~inject =
  match m.images with
  | [] -> Node.none
  | list ->
    div
      ~cls:"pending-images"
      (List.mapi list ~f:(fun i image ->
         div
           ~cls:"pending-image"
           [ Node.img
               ~attrs:
                 [ Attr.src (Chat_view.image_src image)
                 ; Attr.alt (Image.to_string_hum image)
                 ; Attr.title (Image.to_string_hum image)
                 ]
               ()
           ; button
               ~cls:"remove-image"
               ~title:"Remove image"
               ~on_click:(inject (Action.Remove_image i))
               [ icon Close ]
           ]))
;;

let placeholder (m : App.Model.t) =
  if App.Model.running m
  then "Steer the agent (Enter) or queue a follow-up (Alt+Enter)…"
  else if m.narrow
  then "Ask prigh…  / for commands"
  else "Ask prigh anything…  / for commands, @ for files, paste images"
;;

let view (m : App.Model.t) ~inject =
  let running = App.Model.running m in
  let has_input = not (String.is_empty (String.strip m.draft) && List.is_empty m.images) in
  Node.footer
    ~attrs:[ Attr.class_ "composer" ]
    [ popup m ~inject
    ; div
        ~cls:"composer-box"
        [ images m ~inject
        ; div
            ~cls:"composer-row"
            [ Node.textarea
                ~attrs:
                  [ Attr.class_ "editor"
                  ; Attr.id "editor"
                  ; Attr.placeholder (placeholder m)
                  ; Attr.value m.draft
                  ; Attr.rows 1
                  ; Attr.create "autocomplete" "off"
                  ; Attr.create "enterkeyhint" (if running then "send" else "send")
                  ; Attr.on_input (fun ev text ->
                      let cursor = Option.value (caret ev) ~default:(String.length text) in
                      inject (Action.Edit { text; cursor }))
                  ]
                []
            ; div
                ~cls:"composer-buttons"
                [ (if running
                   then
                     button
                       ~cls:"stop"
                       ~title:"Stop (Esc)"
                       ~on_click:(inject Action.Abort)
                       [ icon Stop ]
                   else Node.none)
                ; button
                    ~cls:"primary send"
                    ~title:(if running then "Steer (Enter)" else "Send (Enter)")
                    ~disabled:(not has_input)
                    ~on_click:(inject Action.Send)
                    [ icon Send ]
                ]
            ]
        ]
    ; Status_view.view m ~inject
    ]
;;
