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
      | Argument Models -> "Fallback models, in order"
      | Argument Thinking -> "Thinking"
      | Argument (Login | Logout) -> "Providers"
      | Argument Directory -> "Directories"
      | Argument Backend_directory -> "Directories on the backend"
      | Argument Path | Path -> "Files"
      | Argument Verbosity -> "Verbosity"
      | Argument Confirm -> "Tool confirmation"
      | Argument Mode -> "Modes"
      | Argument Plan -> "Plan mode"
      | Argument Session -> "Sessions"
      | Argument Host -> "Hosts"
      | Argument User -> "Users"
      | Argument Skill -> "Skills"
      | Argument Budget -> "Budget"
      | Argument Goal -> "Goal"
      | Argument Resume -> "Automatic resume"
      | Argument Notify -> "Notifications"
    in
    div
      ~cls:"popup"
      ~attrs:[ Attr.role "listbox" ]
      [ div
          ~cls:"popup-title"
          [ Node.text title
          ; span ~cls:"popup-keys" "↑↓ Tab Enter Esc · Ctrl+Enter as typed"
          ]
      ; div
          ~cls:"popup-items"
          (List.mapi
             (List.take (Completion.items c) 50)
             ~f:(fun i (item : Picker.Item.t) ->
               Node.div
                 ~attrs:
                   [ classes
                       [ "popup-item" ]
                       [ "selected", i = Completion.selected c
                       ; "dimmed", item.dimmed
                       ; "marked", item.marked
                       ]
                   ; Attr.role "option"
                   ; Attr.on_mousedown (fun ev ->
                       Js_of_ocaml.Dom.preventDefault ev;
                       inject (Action.Complete_choose i))
                   ]
                 [ span ~cls:"popup-label" item.label
                 ; (if String.is_empty item.detail
                    then Node.none
                    else span ~cls:"popup-detail" item.detail)
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

let connected (m : App.Model.t) =
  match m.connection, m.state with
  | Connected, Some _ -> Option.is_none m.session_gone
  | Reconnecting _, _ | _, None -> false
;;

(* One line at any width (CSS cuts it with an ellipsis, never wraps it). *)
let placeholder (m : App.Model.t) =
  (* A phone, or the chat beside a side panel. *)
  let cramped = m.narrow || m.agents.open_ || m.files.open_ in
  if Option.is_some m.session_gone
  then "Session gone: /new starts one"
  else if not (connected m)
  then
    if cramped
    then "Offline: send once back"
    else "Offline: write now, send once it is back"
  else if App.Model.running m && cramped
  then "Steer the agent…"
  else if App.Model.running m
  then "Steer (Enter) or queue a follow-up (Alt+Enter)…"
  else if cramped
  then "Ask prigh…  / commands"
  else "Ask prigh anything…  / commands, @ files"
;;

let view (m : App.Model.t) ~inject =
  let running = App.Model.running m in
  let has_input =
    not
      (String.is_empty (String.strip m.draft)
       && List.is_empty m.images
       && List.is_empty m.files.attachments)
  in
  let sending = m.pending.sending > 0 in
  let connected = connected m in
  Node.footer
    ~attrs:
      [ classes [ "composer" ] [ "offline", not connected; "sending", sending ]
      ]
    [ popup m ~inject
    ; Queue_view.view m ~inject
    ; div
        ~cls:"composer-box"
        [ images m ~inject
        ; Files_view.chips m ~inject
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
                  ; Attr.create
                      "enterkeyhint"
                      (if running then "send" else "send")
                  ; Attr.on_input (fun ev text ->
                      let cursor =
                        Option.value (caret ev) ~default:(String.length text)
                      in
                      inject (Action.Edit { text; cursor }))
                  ]
                []
            ; div
                ~cls:"composer-buttons"
                [ Files_view.attach_button m ~inject
                ; (if running
                   then
                     button
                       ~cls:"stop"
                       ~title:"Stop (Esc)"
                       ~on_click:(inject Action.Abort)
                       [ icon Stop ]
                   else Node.none)
                ; (if sending
                   then
                     button
                       ~cls:"primary send busy"
                       ~title:"Sending…"
                       ~disabled:true
                       ~attrs:[ Attr.create "aria-busy" "true" ]
                       ~on_click:Effect.Ignore
                       [ Html.lamp () ]
                   else
                     button
                       ~cls:"primary send"
                       ~title:
                         (if not connected
                          then "Not connected to the backend"
                          else if running
                          then "Steer (Enter)"
                          else "Send (Enter)")
                       ~disabled:((not has_input) || not connected)
                       ~on_click:(inject Action.Send)
                       [ icon Send ])
                ]
            ]
        ]
    ; Status_view.view m ~inject
    ]
;;
