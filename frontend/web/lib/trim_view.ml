open! Core
open! Import
open Html
module Action = App.Action

let divider_class = "trim-divider"

let busy text =
  Node.span
    ~attrs:[ Attr.class_ "trim-busy"; Attr.role "status" ]
    [ Html.lamp (); Node.text text ]
;;

let action ~title ?(attrs = []) ~on_click text =
  Node.button
    ~attrs:
      ([ Attr.class_ "trim-action"
       ; Attr.type_ "button"
       ; Attr.title title
       ; Attr.on_click (fun _ -> on_click)
       ]
       @ attrs)
    [ Node.text text ]
;;

let view (m : App.Model.t) ~inject =
  match Option.bind m.state ~f:(fun s -> s.trim) with
  | None -> Node.none
  | Some _ when m.paging.older > 0 -> Node.none
  | Some trim ->
    let label = Trim.label ~messages:trim.messages ~tokens:trim.tokens in
    let messages =
      match m.trimmed.shown with
      | Shown chat ->
        Node.section
          ~attrs:
            [ Attr.class_ "trimmed-messages"
            ; Attr.create
                "aria-label"
                "Trimmed messages, not in the model's context"
            ]
          [ div
              ~cls:"trimmed-head"
              [ span ~cls:"trimmed-title" "Not in the model's context"
              ; span
                  ~cls:"trimmed-note"
                  "Kept in the transcript only; the model no longer sees them."
              ]
          ; Chat_view.view
              ~fold:(App.Model.fold m)
              (App.Model.message_time m)
              chat
          ]
      | Folded | Loading -> Node.none
    in
    let toggle =
      match m.trimmed.shown with
      | Folded ->
        action
          ~title:"Show the trimmed messages above, read-only"
          ~attrs:[ Attr.create "aria-expanded" "false" ]
          ~on_click:(inject (Action.Show_trimmed true))
          "Show"
      | Loading -> busy "Loading…"
      | Shown _ ->
        action
          ~title:"Fold the trimmed messages again"
          ~attrs:[ Attr.create "aria-expanded" "true" ]
          ~on_click:(inject (Action.Show_trimmed false))
          "Hide"
    in
    let undo =
      if m.trimmed.restoring
      then busy "Restoring…"
      else
        action
          ~title:"Bring them back into the model's context (/trim undo)"
          ~on_click:(inject Action.Untrim)
          "Undo"
    in
    Node.fragment
      [ messages
      ; Node.div
          ~attrs:
            [ Attr.class_ divider_class
            ; Attr.role "group"
            ; Attr.create "aria-label" label
            ]
          [ Node.span
              ~attrs:
                [ Attr.class_ "scissors"; Attr.create "aria-hidden" "true" ]
              [ Node.text "✂" ]
          ; span ~cls:"trim-label" label
          ; Node.span
              ~attrs:[ Attr.class_ "trim-actions" ]
              [ toggle
              ; Node.span
                  ~attrs:
                    [ Attr.class_ "trim-sep"; Attr.create "aria-hidden" "true" ]
                  [ Node.text "·" ]
              ; undo
              ]
          ]
      ]
;;
