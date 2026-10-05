open! Core
open! Import
open Html
module Action = App.Action

let view (m : App.Model.t) ~inject =
  match m.btw with
  | None -> Node.none
  | Some btw ->
    let status =
      match btw.status with
      | Streaming ->
        div
          ~cls:"btw-status"
          [ Node.span ~attrs:[ Attr.class_ "spinner" ] []
          ; Node.text "Answering…"
          ]
      | Done -> Node.none
      | Failed e -> div ~cls:"btw-error" [ Node.textf "Failed: %s" e ]
    in
    Node.section
      ~attrs:
        [ Attr.class_ "btw-panel"
        ; Attr.id "btw"
        ; Attr.create "aria-label" "Side question"
        ; Attr.create "aria-live" "polite"
        ]
      [ div
          ~cls:"btw-head"
          [ span ~cls:"btw-tag" "btw"
          ; span ~cls:"btw-question" btw.question
          ; button
              ~cls:"icon ghost"
              ~title:"Close (Esc)"
              ~on_click:(inject Action.Close_btw)
              [ icon Close ]
          ]
      ; div
          ~cls:"btw-answer"
          [ (if String.is_empty btw.answer
             then Node.none
             else
               Markdown_view.render
                 ~streaming:(Btw.Status.equal btw.status Streaming)
                 btw.answer)
          ]
      ; status
      ; div ~cls:"btw-note" [ Node.text "Not added to the conversation." ]
      ]
;;
