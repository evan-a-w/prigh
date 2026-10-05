open! Core
open! Import
open Html
module Action = App.Action

let session_title (m : App.Model.t) (state : State.t) =
  match state.session_name, state.session_description with
  | Some name, _ when not (String.is_empty (String.strip name)) -> name
  | _, Some description -> description
  | _ ->
    (match
       List.find m.sessions ~f:(fun s -> String.equal s.id state.session_id)
     with
     | Some s when Option.is_some s.description || Option.is_some s.first_prompt
       -> Session_list.title s
     | _ ->
       (* Before the session list knows it: the first prompt, at once. *)
       List.find_map (Chat.entries m.chat) ~f:(function
         | User { text; _ } when not (String.is_empty (String.strip text)) ->
           Some (List.hd_exn (String.split_lines (String.strip text)))
         | _ -> None)
       |> Option.value ~default:"New session")
;;

let view (m : App.Model.t) ~inject =
  let toggle =
    button
      ~cls:"icon ghost toggle-sidebar"
      ~title:"Sessions (Ctrl+B)"
      ~on_click:(inject Action.Toggle_sidebar)
      [ icon Menu ]
  in
  match m.state with
  | None ->
    Node.header
      ~attrs:[ Attr.class_ "topbar" ]
      [ toggle
      ; div ~cls:"title" [ span ~cls:"session-name muted" "Connecting…" ]
      ]
  | Some state ->
    Node.header
      ~attrs:[ Attr.class_ "topbar" ]
      [ toggle
      ; div
          ~cls:"title"
          [ Node.button
              ~attrs:
                [ Attr.class_ "session-name"
                ; Attr.type_ "button"
                ; Attr.title "Rename (/name)"
                ; Attr.on_click (fun _ -> inject Action.Open_rename)
                ]
              [ Node.span [ Node.text (session_title m state) ]
              ; icon ~cls:"edit-hint" Pencil
              ]
          ; div
              ~cls:"session-where"
              [ span
                  ~cls:"where"
                  ~attrs:[ Attr.title state.cwd ]
                  (Sidebar_view.home_relative state.cwd)
              ; (match state.git_branch with
                 | Some branch ->
                   Node.span
                     ~attrs:[ Attr.class_ "branch" ]
                     [ icon Branch; Node.text branch ]
                 | None -> Node.none)
              ]
          ]
      ; div
          ~cls:"controls"
          [ button
              ~cls:"chip-button model-chip"
              ~title:"Switch model (Ctrl+L)"
              ~on_click:(inject Action.Open_model_picker)
              [ icon Cpu
              ; span ~cls:"chip-label" state.model.name
              ; icon ~cls:"chevron" Chevron
              ]
          ; (if state.model.supports_thinking
             then
               button
                 ~cls:"chip-button thinking-chip"
                 ~title:"Thinking level (/thinking)"
                 ~on_click:(inject Action.Open_thinking_picker)
                 [ icon Brain
                 ; span ~cls:"chip-label" state.thinking
                 ; icon ~cls:"chevron" Chevron
                 ]
             else Node.none)
          ; Terminal_view.toggle m ~inject
          ; Agents_view.toggle m ~inject
          ]
      ]
;;
