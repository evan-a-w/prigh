open! Core
open! Import
open Html
module Action = App.Action

let session_title (m : App.Model.t) (state : State.t) =
  match state.session_name, state.session_description with
  | Some name, _ when not (String.is_empty (String.strip name)) -> name
  | _, Some description -> description
  | _ ->
    (match
       List.find m.sessions ~f:(fun s -> String.equal s.id state.session_id)
     with
     | Some s when Option.is_some s.description || Option.is_some s.first_prompt
       -> Session_list.title s
     | _ ->
       (* Before the session list knows it: the first prompt, at once. *)
       List.find_map (Chat.entries m.chat) ~f:(function
         | User u ->
           (match String.strip (Injected_note.typed u) with
            | "" -> None
            | text -> Some (List.hd_exn (String.split_lines text)))
         | _ -> None)
       |> Option.value ~default:"New session")
;;

let panel_toggles (m : App.Model.t) ~inject =
  (* A panel opened from a phone's menu: the menu gets out of its way (the
     filter's menu hangs from its button, so that one keeps it). *)
  let opening a =
    if m.narrow && m.status_tray
    then Effect.Many [ inject Action.Toggle_status_tray; inject a ]
    else inject a
  in
  div
    ~cls:"panel-toggles"
    [ Filter_view.button m ~inject
    ; Files_view.toggle m ~inject:opening
    ; Terminal_view.toggle m ~inject:opening
    ; Agents_view.toggle m ~inject:opening
    ]
;;

(* On a phone: one round button for the panels' toggles, what applies and
   the settings, which open in a small menu under it. *)
let more (m : App.Model.t) ~inject =
  Node.div
    ~attrs:[ classes [ "chrome-more" ] [ "open", m.status_tray ] ]
    [ Node.button
        ~attrs:
          [ Attr.class_ "btn icon ghost round chrome-button more-toggle"
          ; Attr.type_ "button"
          ; Attr.title "Panels and settings"
          ; Attr.create "aria-label" "Panels and settings"
          ; Attr.create "aria-controls" "chrome-menu"
          ; Attr.create "aria-expanded" (Bool.to_string m.status_tray)
          ; Attr.on_click (fun _ -> inject Action.Toggle_status_tray)
          ]
        [ icon More ]
    ; (if m.status_tray
       then
         Node.fragment
           [ Node.div
               ~attrs:
                 [ Attr.class_ "chrome-backdrop"
                 ; Attr.on_click (fun _ -> inject Action.Toggle_status_tray)
                 ]
               []
           ; Node.div
               ~attrs:
                 [ Attr.class_ "chrome-menu menu-surface"; Attr.id "chrome-menu" ]
               [ Status_view.notes m ~inject
               ; panel_toggles m ~inject
               ; Sidebar_view.settings ~inject
               ]
           ]
       else Node.none)
    ]
;;

(* Over the top of the chat, with no bar of its own: the sidebar's toggle
   (when it is not docked) at the left, the status's island in the middle
   (where the style puts it there), and what applies and the panels'
   toggles at the right (on a phone, in a menu). *)
let chrome (m : App.Model.t) ~inject =
  let docked = App.Model.docked m in
  let toggle =
    if docked
    then Node.none
    else
      button
        ~cls:"icon ghost round chrome-button toggle-sidebar"
        ~title:"Sessions (Ctrl+B)"
        ~label:"Sessions"
        ~on_click:(inject Action.Toggle_sidebar)
        [ icon Menu ]
  in
  let island =
    match m.status_style with
    | Island -> Status_style_view.island m ~inject
    | Ring | Whisper | Border | Hub | Sidebar -> Node.none
  in
  let tools =
    if m.narrow
    then more m ~inject
    else
      div
        ~cls:"chrome-tools"
        [ Status_view.notes m ~inject
        ; panel_toggles m ~inject
        ; (if docked then Node.none else Sidebar_view.settings ~inject)
        ]
  in
  div
    ~cls:"chrome-anchor"
    [ Node.div
        ~attrs:
          [ classes
              [ "chat-chrome" ]
              [ "docked", docked ]
          ]
        [ div ~cls:"chrome-left" [ toggle ]
        ; div ~cls:"chrome-center" [ island ]
        ; div ~cls:"chrome-right" [ tools ]
        ]
    ]
;;
