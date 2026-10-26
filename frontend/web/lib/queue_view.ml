open! Core
open! Import
open Html
module Action = App.Action
module Message = Queued.Message

let tag (recipient : Recipient.t) (kind : Message.Kind.t) =
  let text, title =
    match recipient, kind with
    | Main, Steer ->
      ( "Steers this turn"
      , "Reaches the agent after its current tool calls, in this turn" )
    | Main, Follow_up ->
      "After this turn", "Sent as the next prompt once the run ends"
    | Agent id, Steer ->
      ( sprintf "To %s, next step" id
      , sprintf "Reaches subagent %s after its current tool calls" id )
    | Agent id, Follow_up ->
      ( sprintf "To %s, after its turn" id
      , sprintf
          "Reaches subagent %s when it ends its turn, before it reports"
          id )
  in
  Node.span
    ~attrs:[ Attr.class_ "queued-tag"; Attr.title title ]
    [ Node.text text ]
;;

let item ~inject (recipient : Recipient.t) (q : Message.t) =
  Node.li
    ~attrs:
      [ classes
          [ "queued" ]
          [ "steer", Message.Kind.equal q.kind Steer
          ; "to-agent", not (Recipient.equal recipient Main)
          ]
      ]
    [ tag recipient q.kind
    ; Node.span
        ~attrs:[ Attr.class_ "queued-text"; Attr.create "dir" "auto" ]
        [ Node.text q.text ]
    ; div
        ~cls:"queued-actions"
        [ button
            ~cls:"ghost queued-edit"
            ~title:"Edit: take it back into the editor"
            ~on_click:(inject (Action.Edit_queued (recipient, q)))
            [ icon Pencil; span ~cls:"queued-action-label" "Edit" ]
        ; button
            ~cls:"icon ghost queued-remove"
            ~title:"Remove from the queue"
            ~on_click:(inject (Action.Remove_queued (recipient, q)))
            [ icon Close ]
        ]
    ]
;;

let view (m : App.Model.t) ~inject =
  let items =
    List.map (Queued.items m.queue) ~f:(fun q -> Recipient.Main, q)
    @ List.concat_map m.agent_queues ~f:(fun (a : Agent_queue.t) ->
      List.map
        (Message.of_queues ~steer:a.steer ~follow_up:a.follow_up)
        ~f:(fun q -> Recipient.Agent a.agent_id, q))
  in
  match items with
  | [] -> Node.none
  | items ->
    Node.ul
      ~attrs:[ Attr.class_ "queue"; Attr.create "aria-label" "Queued messages" ]
      (List.map items ~f:(fun (recipient, q) -> item ~inject recipient q))
;;
