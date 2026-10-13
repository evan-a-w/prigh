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
      | Streaming -> [ Html.lamp (); Node.text "Answering…" ]
      | Done -> [ span ~cls:"sr-only" "Answered" ]
      | Failed e ->
        [ span
            ~cls:"btw-error"
            (sprintf "Couldn't answer: %s. /btw asks again." e)
        ]
    in
    (* Said once when it starts, ends or fails; not at every chunk. *)
    let status =
      div ~cls:"btw-status" ~attrs:[ Attr.role "status" ] status
    in
    Node.section
      ~attrs:
        [ Attr.class_ "btw-panel"
        ; Attr.id "btw"
        ; Attr.create "aria-label" "Side question"
        ]
      [ div
          ~cls:"btw-head"
          [ span ~cls:"btw-tag" "btw"
          ; span ~cls:"btw-question" ~attrs:[ Attr.create "dir" "auto" ] btw.question
          ; button
              ~cls:"icon ghost"
              ~title:"Close (Esc)"
              ~label:"Close the side question"
              ~on_click:(inject Action.Close_btw)
              [ icon Close ]
          ]
      ; div
          ~cls:"btw-answer"
          ~attrs:
            [ (if Btw.Status.equal btw.status Streaming
               then Attr.create "aria-busy" "true"
               else Attr.empty)
            ]
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
