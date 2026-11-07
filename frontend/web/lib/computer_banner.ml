open! Core
open! Import
open Html

let shown (m : App.Model.t) = App.Model.running m && Chat.uses_computer m.chat

let host_name (m : App.Model.t) =
  match Option.map m.state ~f:Active_host.of_state with
  | Some (Connected host) -> host.name
  | Some (Disconnected { name; _ }) -> name
  | Some Unset | None -> "a computer"
;;

let view m ~inject =
  if not (shown m)
  then Node.none
  else
    div
      ~cls:"computer-banner"
      ~attrs:[ Attr.role "status"; Attr.create "aria-live" "polite" ]
      [ lamp ~title:"Controlling the screen" ()
      ; Node.span
          ~attrs:[ Attr.class_ "computer-banner-text" ]
          [ Node.text "prigh is controlling "
          ; Node.strong [ Node.text (host_name m) ]
          ]
      ; button
          ~cls:"computer-banner-stop"
          ~title:"Stop the agent (Esc)"
          ~on_click:(inject App.Action.Abort)
          [ Node.text "Stop" ]
      ]
;;
