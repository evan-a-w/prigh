open! Core
open! Import
open Html
module Action = App.Action
module Message = Queued.Message

(* What it waits for: a word in the pill (lit for a steer), the rest in the
   tooltip. *)
let tag (recipient : Recipient.t) (kind : Message.Kind.t) =
  let text, title =
    match recipient, kind with
    | Main, Steer ->
      "Steer", "Reaches the agent after its current tool calls, in this turn"
    | Main, Follow_up -> "Queued", "Sent as the next prompt once the run ends"
    | Agent _, Steer ->
      "Steer", "Reaches the subagent after its current tool calls"
    | Agent _, Follow_up ->
      "Queued", "Reaches the subagent when it ends its turn, before it reports"
  in
  Node.span
    ~attrs:
      [ Attr.class_
          "queued-tag shrink-0 font-medium text-foreground \
           in-[.steer]:text-primary"
      ; Attr.title title
      ]
    [ Node.text text ]
;;

let action ~cls ~title ~label ~on_click glyph =
  Node.button
    ~attrs:
      [ Attr.type_ "button"
      ; Attr.class_
          (cls
           ^ " flex size-7 shrink-0 items-center justify-center rounded-full \
              text-muted-foreground hover:bg-accent narrow:size-11")
      ; Attr.title title
      ; Attr.on_click (fun _ -> on_click)
      ]
    [ Loon.Icon.view ~class_:"size-3.5" glyph; span ~cls:"sr-only" label ]
;;

let item ~inject (recipient : Recipient.t) (q : Message.t) =
  Node.li
    ~attrs:
      [ classes
          [ "queued"
          ; "flex h-9 max-w-[min(100%,36rem)] min-w-0 items-center gap-2 \
             rounded-full border border-border bg-card pr-1 pl-3 text-[13px] \
             shadow-xs narrow:h-11"
          ]
          [ "steer", Message.Kind.equal q.kind Steer
          ; "to-agent", not (Recipient.equal recipient Main)
          ]
      ]
    [ Loon.Icon.view ~class_:"size-3.5 shrink-0 text-muted-foreground" Clock
    ; tag recipient q.kind
    ; Node.span
        ~attrs:
          [ Attr.class_ "queued-text min-w-0 truncate text-muted-foreground"
          ; Attr.create "dir" "auto"
          ; Attr.title q.text
          ]
        [ Node.text q.text ]
    ; div
        ~cls:"queued-actions flex shrink-0 items-center"
        [ action
            ~cls:"queued-edit hover:text-foreground"
            ~title:"Edit: take it back into the editor"
            ~label:"Edit"
            ~on_click:(inject (Action.Edit_queued (recipient, q)))
            Pencil
        ; action
            ~cls:"queued-remove hover:text-destructive"
            ~title:"Remove from the queue"
            ~label:"Remove from the queue"
            ~on_click:(inject (Action.Remove_queued (recipient, q)))
            X
        ]
    ]
;;

let view (m : App.Model.t) ~inject =
  (* The queue of the chat on show: the main agent's, or the subagent's. *)
  let items =
    match App.Model.recipient m with
    | Main -> List.map (Queued.items m.queue) ~f:(fun q -> Recipient.Main, q)
    | Agent id ->
      List.concat_map m.agent_queues ~f:(fun (a : Agent_queue.t) ->
        if String.equal a.agent_id id
        then
          List.map
            (Message.of_queues ~steer:a.steer ~follow_up:a.follow_up)
            ~f:(fun q -> Recipient.Agent a.agent_id, q)
        else [])
  in
  match items with
  | [] -> Node.none
  | items ->
    Node.ul
      ~attrs:
        [ Attr.class_
            "queue loon flex max-h-[30vh] flex-col items-end gap-1.5 \
             overflow-y-auto"
        ; Attr.create "aria-label" "Queued messages"
        ]
      (List.map items ~f:(fun (recipient, q) -> item ~inject recipient q))
;;
