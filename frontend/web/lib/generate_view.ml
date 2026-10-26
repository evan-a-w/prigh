open! Core
open! Import
open Html

let button ?(label = "Generate") (g : Generate.t) target ~inject =
  let running = Generate.running_for g target in
  let undo = Option.is_some (Generate.undo_for g target) in
  div
    ~cls:"generate"
    ~attrs:[ Attr.create "data-target" (Sexp.to_string [%sexp (target : Generate.Target.t)]) ]
    [ (if running
       then
         Html.button
           ~cls:"small ghost generate-stop"
           ~title:"Stop the draft, keeping what came (Esc)"
           ~attrs:[ Attr.create "aria-busy" "true" ]
           ~on_click:(inject App.Action.Generate_stop)
           [ lamp (); Node.text "Stop" ]
       else
         Html.button
           ~cls:"small ghost generate-go"
           ~title:(sprintf "Draft it with the model %s (Alt+G)" (Generate.Target.source target))
           ~on_click:(inject (App.Action.Generate target))
           [ span ~cls:"generate-spark" ~attrs:[ Attr.create "aria-hidden" "true" ] "✦"
           ; Node.text label
           ])
    ; (if undo && not running
       then
         Html.button
           ~cls:"small ghost generate-undo"
           ~title:"Put back the text from before the draft"
           ~on_click:(inject (App.Action.Generate_undo target))
           [ icon Undo; Node.text "Undo" ]
       else Node.none)
    ]
;;
