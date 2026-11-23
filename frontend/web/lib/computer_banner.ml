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
    Node.div
      ~attrs:
        [ Attr.create "data-slot" "alert"
        ; Attr.role "status"
        ; Attr.create "aria-live" "polite"
        ; Attr.class_
            (Loon.Alert.classes
               ~class_:
                 "computer-banner loon flex items-center gap-2.5 \
                  border-primary/40 bg-primary/10 py-1 pr-1 pl-3 text-[13.5px]"
               ())
        ]
      [ lamp ~title:"Controlling the screen" ()
      ; Node.span
          ~attrs:
            [ Attr.class_
                "computer-banner-text min-w-0 flex-1 truncate \
                 [&_strong]:font-semibold [&_strong]:text-primary"
            ]
          [ Node.text "prigh is controlling "; Node.strong [ Node.text host ] ]
      ; button
          ~cls:"computer-banner-stop narrow:h-11 narrow:min-w-11"
          ~variant:Destructive
          ~size:Sm
          ~title:"Stop the agent (Esc)"
          ~on_click:(inject App.Action.Abort)
          [ Node.text "Stop" ]
      ]
;;
