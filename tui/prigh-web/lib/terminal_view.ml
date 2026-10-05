open! Core
open! Import
open Html
module Action = App.Action

let close ~inject =
  button
    ~cls:"icon ghost terminal-close"
    ~title:"Close (Ctrl+`): the shell keeps running for 10 minutes"
    ~on_click:(inject Action.Close_terminal)
    [ icon Close ]
;;

let restart ~inject label =
  button
    ~cls:"small terminal-restart"
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
        ( [ span ~cls:"terminal-state" "connecting…" ]
        , [ div
              ~cls:"terminal-waiting"
              [ Node.text "Connecting to the backend…" ]
          ] )
      | Some state ->
        let target = Terminal.target t state ~hello:m.hello in
        let status = Terminal.status t target in
        let name, cwd = Terminal.where state in
        ( [ span ~cls:"terminal-machine" name
          ; span
              ~cls:"terminal-cwd where"
              ~attrs:[ Attr.title cwd ]
              (Sidebar_view.home_relative cwd)
          ; (match state_label status with
             | Some label ->
               span
                 ~cls:
                   (match status with
                    | Failed _ -> "terminal-state failed"
                    | _ -> "terminal-state")
                 label
             | None -> Node.none)
          ]
        , [ widget target; notice ~inject status ] )
    in
    Node.create
      "section"
      ~attrs:
        ([ Attr.class_ "terminal-panel"
         ; Attr.id "terminal-panel"
         ; Attr.create "aria-label" "Terminal"
         ]
         @
         match t.height with
         | Some px ->
           [ Attr.css_var ~name:"terminal-height" (sprintf "%dpx" px) ]
         | None -> [])
      [ div
          ~cls:"terminal-resize"
          ~attrs:[ Attr.title "Drag to resize"; Attr.role "separator" ]
          []
      ; div
          ~cls:"terminal-head"
          ((icon ~cls:"terminal-icon" Terminal :: head) @ [ close ~inject ])
      ; div ~cls:"terminal-body" body
      ])
;;

let toggle (m : App.Model.t) ~inject =
  button
    ~cls:
      (if m.terminal.open_
       then "icon ghost terminal-toggle open"
       else "icon ghost terminal-toggle")
    ~title:"Terminal (Ctrl+`)"
    ~on_click:(inject Action.Toggle_terminal)
    [ icon Terminal ]
;;
