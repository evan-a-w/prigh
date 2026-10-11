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
         | User u ->
           (match String.strip (Injected_note.typed u) with
            | "" -> None
            | text -> Some (List.hd_exn (String.split_lines text)))
         | _ -> None)
       |> Option.value ~default:"New session")
;;

(* A choice the backend has still to confirm. *)
let pending_chip ?(attrs = []) ?title ~cls label =
  button
    ~cls:("chip-button awaiting " ^ cls)
    ~title:(Option.value title ~default:(sprintf "Switching to %s…" label))
    ~disabled:true
    ~attrs:(Attr.create "aria-busy" "true" :: attrs)
    ~on_click:Effect.Ignore
    [ Html.lamp (); span ~cls:"chip-label" label ]
;;

let thinking_chip (m : App.Model.t) (state : State.t) ~inject =
  match m.pending.thinking with
  | Some level ->
    pending_chip
      ~cls:"thinking-chip"
      ~title:(sprintf "Switching to thinking %s…" level)
      ("Thinking " ^ level)
  | None when state.model.supports_thinking ->
    button
      ~cls:"chip-button thinking-chip"
      ~title:
        (sprintf
           "Thinking %s: how much the model reasons before it answers (Alt+T \
            cycles, /thinking)"
           state.thinking)
      ~on_click:(inject Action.Open_thinking_picker)
      [ icon Brain
      ; span ~cls:"chip-key" "Thinking"
      ; span ~cls:"chip-label" state.thinking
      ; icon ~cls:"chevron" Chevron
      ]
  | None ->
    button
      ~cls:"chip-button thinking-chip unsupported"
      ~title:(sprintf "%s has no thinking levels" state.model.name)
      ~on_click:(inject Action.Open_thinking_picker)
      [ icon Brain; span ~cls:"chip-label" "No thinking" ]
;;

(* What asks first: phones show [label] (ask, auto-edit, auto, plan). *)
let mode_chip (m : App.Model.t) (state : State.t) ~inject =
  let short mode = [ Attr.create "data-short" (Session_mode.label mode) ] in
  match m.pending.mode with
  | Some mode ->
    pending_chip
      ~attrs:(short mode)
      ~cls:"mode-chip"
      ~title:(sprintf "Switching to %s…" (Session_mode.label mode))
      (Session_mode.title mode)
  | None ->
    let mode = Session_mode.of_state state in
    button
      ~cls:("chip-button mode-chip" ^ if mode.plan then " plan-mode" else "")
      ~title:
        (sprintf
           "%s (Alt+M cycles, /mode picks)"
           (String.capitalize (Session_mode.describe mode)))
      ~attrs:(short mode)
      ~on_click:(inject (Action.Run "/mode"))
      [ icon Shield
      ; span ~cls:"chip-label" (Session_mode.title mode)
      ; icon ~cls:"chevron" Chevron
      ]
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
          [ (match m.pending.model with
             | Some key ->
               let name =
                 List.find m.models ~f:(fun model -> String.equal model.key key)
                 |> Option.value_map ~default:key ~f:(fun model -> model.name)
               in
               pending_chip ~cls:"model-chip" name
             | None ->
               button
                 ~cls:"chip-button model-chip"
                 ~title:"Switch model (Ctrl+L)"
                 ~on_click:(inject Action.Open_model_picker)
                 [ icon Cpu
                 ; span ~cls:"chip-label" state.model.name
                 ; icon ~cls:"chevron" Chevron
                 ])
          ; thinking_chip m state ~inject
          ; mode_chip m state ~inject
          ]
      ; div
          ~cls:"panel-toggles"
          [ Files_view.toggle m ~inject
          ; Terminal_view.toggle m ~inject
          ; Agents_view.toggle m ~inject
          ]
      ]
;;
