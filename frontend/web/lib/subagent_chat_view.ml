open! Core
open! Import
open Html
module Model = App.Model
module Action = App.Action

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
      ~names:(Model.model_names m)
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
          [ Node.text "Back to the session" ]
      ]
  | Some a ->
    Node.div
      ~attrs:[ Attr.class_ "subagent-chat"; Attr.create "data-subagent" a.id ]
      [ transcript m ~inject a; ending m a ]
;;
