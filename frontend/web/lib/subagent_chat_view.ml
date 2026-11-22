open! Core
open! Import
open Html
module Model = App.Model
module Action = App.Action
module Icon = Loon.Icon

let back_label (m : Model.t) =
  match m.state with
  | Some state -> Session_view.session_title m state
  | None -> "the session"
;;

let note ?(failed = false) children =
  Node.div
    ~attrs:
      [ Attr.classes
          ([ "subagent-chat-note"
           ; "my-6 flex items-center gap-2 text-sm text-muted-foreground"
           ]
           @ if failed then [ "failed" ] else [])
      ]
    children
;;

let transcript (m : Model.t) ~inject (a : Agents.Agent.t) =
  match Chat.find_subagent m.chat a.id with
  | Some s when not (List.is_empty (Chat.entries s.chat)) ->
    Chat_view.view
      ~fold:(Fold.scoped (Model.fold m) s.agent_id)
      (Model.message_time m)
      s.chat
  | Some _ -> note [ lamp (); Node.text "Starting…" ]
  | None ->
    let retry why =
      note
        ~failed:true
        [ Node.text why
        ; button
            ~cls:"ghost small"
            ~title:"Ask the backend for its transcript again"
            ~on_click:(inject (Action.Open_subagent_chat a.id))
            [ Node.text "Retry" ]
        ]
    in
    (match Map.find m.agents.fetches a.id with
     | Some (Asked _) -> note [ lamp (); Node.text "Loading the transcript…" ]
     | Some (Failed e) -> retry (sprintf "Couldn't load the transcript: %s." e)
     | None -> retry "The transcript is not loaded.")
;;

(* The transcript ends with the report once loaded; before that, or when
   it failed, the report or the error is said below it. *)
let ending (m : Model.t) (a : Agents.Agent.t) =
  match a.result, Chat.find_subagent m.chat a.id with
  | None, _ -> Node.none
  | Some { is_error = false; _ }, Some _ -> Node.none
  | Some { is_error = false; text }, None ->
    div
      ~cls:"subagent-chat-report mt-4 border-t pt-3"
      [ div ~cls:"mb-1 text-xs text-muted-foreground" [ Node.text "Report" ]
      ; Markdown_view.render text
      ]
  | Some { is_error = true; text }, _ ->
    div
      ~cls:
        "subagent-chat-failed mt-4 rounded-md border border-destructive/40 p-3 \
         text-sm"
      [ div ~cls:"mb-1 font-medium text-destructive" [ Node.text "Failed" ]
      ; Node.pre
          ~attrs:[ Attr.class_ "whitespace-pre-wrap text-xs" ]
          [ Node.text text ]
      ]
;;

let view (m : Model.t) ~inject id =
  match Agents.find_agent m.agents id with
  | None ->
    note
      [ Node.text "That subagent is gone."
      ; button
          ~cls:"ghost small"
          ~on_click:(inject Action.Close_subagent_chat)
          [ Node.text "Back" ]
      ]
  | Some a ->
    let now = Option.value m.now ~default:Time_ns.epoch in
    let activity = Session_card.Activity.of_agent a ~now in
    Node.div
      ~attrs:[ Attr.class_ "loon subagent-chat"; Attr.create "data-agent" a.id ]
      [ Node.div
          ~attrs:
            [ Attr.class_
                "subagent-chat-head mb-4 flex max-w-(--column) items-center \
                 gap-2 border-b pb-3"
            ]
          [ Node.button
              ~attrs:
                [ Attr.type_ "button"
                ; Attr.class_
                    "subagent-chat-back -ml-2 inline-flex h-8 min-w-0 shrink \
                     items-center gap-1.5 rounded-md px-2 text-sm \
                     text-muted-foreground hover:bg-accent \
                     hover:text-foreground narrow:h-11"
                ; Attr.title "Back to the session (Esc)"
                ; Attr.on_click (fun _ -> inject Action.Close_subagent_chat)
                ]
              [ Icon.view ~class_:"size-4 shrink-0" Arrow_left
              ; Node.span
                  ~attrs:[ Attr.class_ "truncate" ]
                  [ Node.text (back_label m) ]
              ]
          ; Node.span
              ~attrs:[ Attr.class_ "text-muted-foreground" ]
              [ Node.text "/" ]
          ; Node.h2
              ~attrs:
                [ Attr.class_
                    "subagent-chat-title min-w-0 truncate text-sm font-medium"
                ; Attr.title (String.strip a.task)
                ]
              [ Node.text (Agents.Agent.title a) ]
          ; Node.span
              ~attrs:
                [ Attr.class_
                    "subagent-chat-status ml-auto shrink-0 text-xs \
                     text-muted-foreground"
                ]
              [ Node.text (Session_card.Activity.to_string activity) ]
          ]
      ; transcript m ~inject a
      ; ending m a
      ]
;;

let footer ~inject =
  Node.div
    ~attrs:
      [ Attr.class_
          "loon composer subagent-chat-footer flex items-center \
           justify-between gap-2 text-xs text-muted-foreground"
      ]
    [ Node.span [ Node.text "A subagent's transcript: read-only." ]
    ; Node.button
        ~attrs:
          [ Attr.type_ "button"
          ; Attr.class_
              "inline-flex h-8 items-center gap-1.5 rounded-md px-2 \
               hover:bg-accent hover:text-foreground narrow:h-11"
          ; Attr.on_click (fun _ -> inject Action.Close_subagent_chat)
          ]
        [ Icon.view ~class_:"size-3.5" Arrow_left; Node.text "Back (Esc)" ]
    ]
;;
