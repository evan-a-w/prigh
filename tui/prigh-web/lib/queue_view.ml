open! Core
open! Import
open Html
module Action = App.Action
module Message = Queued.Message

let tag (kind : Message.Kind.t) =
  let text, title =
    match kind with
    | Steer ->
      ( "Steers this turn"
      , "Reaches the agent after its current tool calls, in this turn" )
    | Follow_up ->
      "After this turn", "Sent as the next prompt once the run ends"
  in
  Node.span
    ~attrs:[ Attr.class_ "queued-tag"; Attr.title title ]
    [ Node.text text ]
;;

let item (q : Message.t) ~inject =
  Node.li
    ~attrs:[ classes [ "queued" ] [ "steer", Message.Kind.equal q.kind Steer ] ]
    [ tag q.kind
    ; Node.span ~attrs:[ Attr.class_ "queued-text" ] [ Node.text q.text ]
    ; div
        ~cls:"queued-actions"
        [ button
            ~cls:"ghost queued-edit"
            ~title:"Edit: take it back into the editor"
            ~on_click:(inject (Action.Edit_queued q))
            [ icon Pencil; span ~cls:"queued-action-label" "Edit" ]
        ; button
            ~cls:"icon ghost queued-remove"
            ~title:"Remove from the queue"
            ~on_click:(inject (Action.Remove_queued q))
            [ icon Close ]
        ]
    ]
;;

let view (m : App.Model.t) ~inject =
  match Queued.items m.queue with
  | [] -> Node.none
  | items ->
    Node.ul
      ~attrs:[ Attr.class_ "queue"; Attr.create "aria-label" "Queued messages" ]
      (List.map items ~f:(item ~inject))
;;
