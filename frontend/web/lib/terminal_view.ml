open! Core
open! Import
open Html
module Action = App.Action

let close ~inject =
  button
    ~cls:"terminal-close ml-auto narrow:size-11"
    ~variant:Ghost
    ~size:Icon_sm
    ~title:"Close (Ctrl+`): the shell keeps running for 10 minutes"
    ~label:"Close terminal"
    ~on_click:(inject Action.Close_terminal)
    [ icon Close ]
;;

let restart ~inject label =
  button
    ~cls:"terminal-restart"
    ~variant:Outline
    ~size:Sm
    ~on_click:(inject Action.New_shell)
    [ icon Restart; Node.text label ]
;;

let state_label : Terminal.Status.t -> string option = function
  | Connecting -> Some "connecting…"
  | Reconnecting -> Some "reconnecting…"
  | Exited -> Some "exited"
  | Failed _ -> Some "unavailable"
  | Connected -> None
;;

let notice ~inject : Terminal.Status.t -> Node.t = function
  | Connecting | Connected | Reconnecting -> Node.none
  | Exited ->
    div
      ~cls:"terminal-notice"
      ~attrs:[ Attr.role "status" ]
      [ span "The shell exited: press a key in it, or"
      ; restart ~inject "New shell"
      ]
  | Failed message ->
    div
      ~cls:"terminal-notice failed"
      ~attrs:[ Attr.role "alert" ]
      [ div
          [ span
              ~cls:"terminal-error"
              ("No terminal: "
               ^ Option.value
                   (String.chop_prefix message ~prefix:"no terminal: ")
                   ~default:message)
          ; span ~cls:"terminal-advice" (Terminal.advice message)
          ]
      ; restart ~inject "Retry"
      ]
;;

let view (m : App.Model.t) ~inject ~widget =
  let t = m.terminal in
  if not t.open_
  then Node.none
  else (
    let head, body =
      match m.state with
      | None ->
        ( [ Loon.Badge.view
              ~class_:"terminal-state"
              ~variant:Secondary
              [ Node.text "connecting…" ]
          ]
        , [ div
              ~cls:"terminal-waiting"
              [ Node.text "Connecting to the backend…" ]
          ] )
      | Some state ->
        let target = Terminal.target t state ~hello:m.hello in
        let status = Terminal.status t target in
        let name, cwd = Terminal.where t state in
        ( [ span ~cls:"terminal-machine" name
          ; span
              ~cls:"terminal-cwd where"
              ~attrs:[ Attr.title cwd ]
              (Sidebar_view.home_relative
                 ?host_home:(Sidebar_view.state_host_home m)
                 cwd)
          ; (match state_label status with
             | Some label ->
               Loon.Badge.view
                 ~class_:
                   (match status with
                    | Failed _ -> "terminal-state failed"
                    | _ -> "terminal-state")
                 ~variant:
                   (match status with
                    | Failed _ -> Destructive
                    | _ -> Secondary)
                 [ Node.text label ]
             | None -> Node.none)
          ]
        , [ widget target; notice ~inject status ] )
    in
    Node.create
      "section"
      ~attrs:
        [ Attr.class_ "terminal-panel"
        ; Attr.id "terminal-panel"
        ; Attr.create "aria-label" "Terminal"
        ]
      [ Resize_view.handle m ~inject Terminal
      ; div
          ~cls:"terminal-head"
          ((icon ~cls:"terminal-icon" Terminal :: head) @ [ close ~inject ])
      ; div ~cls:"terminal-body" body
      ])
;;
