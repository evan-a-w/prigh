open! Core
open! Import
open Html

(* The host the user allowed prigh to control, while a run does. *)
let controlled (m : App.Model.t) =
  if App.Model.running m
  then Option.bind m.state ~f:(fun s -> s.computer_control)
  else None
;;

let view m ~inject =
  match controlled m with
  | None -> Node.none
  | Some host ->
    div
      ~cls:"computer-banner"
      ~attrs:[ Attr.role "status"; Attr.create "aria-live" "polite" ]
      [ lamp ~title:"Controlling the screen" ()
      ; Node.span
          ~attrs:[ Attr.class_ "computer-banner-text" ]
          [ Node.text "prigh is controlling "; Node.strong [ Node.text host ] ]
      ; button
          ~cls:"computer-banner-stop"
          ~title:"Stop the agent (Esc)"
          ~on_click:(inject App.Action.Abort)
          [ Node.text "Stop" ]
      ]
;;
