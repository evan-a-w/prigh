open! Core
open! Import
open Html
module Model = App.Model
module Action = App.Action

let banner (m : Model.t) =
  match m.connection with
  | Connected -> Node.none
  | Reconnecting { attempt; _ } ->
    div
      ~cls:"banner"
      ~attrs:[ Attr.role "status" ]
      [ Node.span ~attrs:[ Attr.class_ "spinner" ] []
      ; Node.text
          (if attempt = 0
           then "Connection lost: reconnecting…"
           else
             sprintf
               "Connection lost: reconnecting (attempt %d, next in %.1fs)…"
               (attempt + 1)
               (Float.of_int (App.Connection.delay_ms ~attempt) /. 1000.))
      ]
;;

let toasts (m : Model.t) ~inject =
  div
    ~cls:"toasts"
    ~attrs:[ Attr.create "aria-live" "polite" ]
    (List.map m.toasts ~f:(fun (t : App.Toast.t) ->
       Node.div
         ~attrs:
           [ classes [ "toast" ] [ "error", t.error ]
           ; Attr.role (if t.error then "alert" else "status")
           ; Attr.title "Dismiss"
           ; Attr.on_click (fun _ -> inject (Action.Dismiss_toast t.id))
           ]
         [ Node.text t.text ]))
;;

let empty (m : Model.t) =
  let hint key text = Node.li [ Node.kbd [ Node.text key ]; Node.text text ] in
  div
    ~cls:"empty"
    [ Node.h2 [ Node.text "What are we building?" ]
    ; (match m.state with
       | Some state ->
         div
           ~cls:"empty-where"
           [ Node.text (Sidebar_view.home_relative state.cwd) ]
       | None -> Node.none)
    ; Node.ul
        ~attrs:[ Attr.class_ "empty-hints" ]
        [ hint "/" " commands"
        ; hint "@" " mention a file"
        ; hint "!" " run a command"
        ; hint "Ctrl+L" " switch model"
        ; hint "Ctrl+K" " find a session"
        ]
    ]
;;

let view (m : Model.t) ~inject =
  let chat =
    match Chat.entries m.chat with
    | [] -> empty m
    | _ -> Chat_view.view m.chat
  in
  Node.div
    ~attrs:
      [ classes
          [ "app" ]
          [ "sidebar-open", m.sidebar_open
          ; "narrow", m.narrow
          ; "agents-open", m.agents.open_
          ]
      ]
    [ Sidebar_view.view m ~inject
    ; (if m.narrow && m.sidebar_open
       then
         Node.div
           ~attrs:
             [ Attr.class_ "scrim"
             ; Attr.on_click (fun _ -> inject Action.Toggle_sidebar)
             ]
           []
       else Node.none)
    ; Node.main
        ~attrs:[ Attr.class_ "main" ]
        [ Topbar_view.view m ~inject
        ; banner m
        ; Node.div ~attrs:[ Attr.class_ "chat"; Attr.id "chat" ] [ chat ]
        ; Composer_view.view m ~inject
        ]
    ; Agents_view.view m ~inject
    ; Dialog_view.view m ~inject
    ; toasts m ~inject
    ]
;;
