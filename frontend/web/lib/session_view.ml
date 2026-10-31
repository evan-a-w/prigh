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

let rename (m : App.Model.t) (state : State.t) ~inject =
  Node.button
    ~attrs:
      [ Attr.class_ "session-name"
      ; Attr.type_ "button"
      ; Attr.title "Rename (/name)"
      ; Attr.on_click (fun _ -> inject Action.Open_rename)
      ]
    [ Node.span
        ~attrs:[ Attr.create "dir" "auto" ]
        [ Node.text (session_title m state) ]
    ; icon ~cls:"edit-hint" Pencil
    ]
;;

let panel_toggles (m : App.Model.t) ~inject =
  div
    ~cls:"panel-toggles"
    [ Filter_view.button m ~inject
    ; Files_view.toggle m ~inject
    ; Terminal_view.toggle m ~inject
    ; Agents_view.toggle m ~inject
    ]
;;

(* It opens the pull requests ([/pr]). *)
let branch (state : State.t) ~inject =
  match state.git_branch with
  | Some branch ->
    Node.button
      ~attrs:
        [ Attr.class_ "branch"
        ; Attr.type_ "button"
        ; Attr.title
            (sprintf
               "On branch %s, in %s: pull requests (/pr)"
               branch
               state.cwd)
        ; Attr.create "aria-haspopup" "dialog"
        ; Attr.on_click (fun _ -> inject (Action.Run "/pr"))
        ]
      [ icon Branch; span ~cls:"branch-name" branch ]
  | None -> Node.none
;;

(* A phone's header has room for the title: the thinking level, the mode,
   the status and the panels' toggles fold into a tray under it, opened by a
   glance at the status (a dot and the context's percent). Only on a phone,
   and CSS shows it only where the header is narrow (under 480px); wider,
   the tray is laid out in the header as ever. *)
let tray_button (m : App.Model.t) ~inject =
  if not m.narrow
  then Node.none
  else
    Node.button
      ~attrs:
        [ Attr.class_ "btn ghost bar-tray-toggle"
        ; Attr.type_ "button"
        ; Attr.title "Status, thinking, mode and panels"
        ; Attr.create "aria-label" "Status and controls"
        ; Attr.create "aria-controls" "bar-tray"
        ; Attr.create
            "aria-expanded"
            (Bool.to_string m.status_tray)
        ; Attr.on_click (fun _ -> inject Action.Toggle_status_tray)
        ]
      [ Status_view.glance m; icon ~cls:"tray-chevron" Chevron ]
;;

(* The chat's header, docked or not: the title (with the branch beside it
   where there is room), then what applies and the status at the right, then
   the panels' toggles. The sidebar's toggle and the settings only when the
   sidebar, which has its own, is not docked. *)
let bar (m : App.Model.t) ~inject =
  let docked = App.Model.docked m in
  let toggle =
    if docked
    then Node.none
    else
      button
        ~cls:"icon ghost toggle-sidebar"
        ~title:"Sessions (Ctrl+B)"
        ~label:"Sessions"
        ~on_click:(inject Action.Toggle_sidebar)
        [ icon Menu ]
  in
  let settings = if docked then Node.none else Sidebar_view.settings ~inject in
  Node.header
    ~attrs:[ classes [ "session-bar" ] [ "docked", docked ] ]
    (match m.state with
     | None ->
       [ toggle
       ; Status_view.extras m ~plan:true ~inject
       ; Status_view.view m ~inject
       ; settings
       ]
     | Some state ->
       [ toggle
       ; div
           ~cls:"session-title"
           [ rename m state ~inject; branch state ~inject ]
       ; tray_button m ~inject
       ; Node.div
           ~attrs:
             [ classes [ "bar-tray" ] [ "open", m.narrow && m.status_tray ]
             ; Attr.id "bar-tray"
             ]
           [ Status_view.extras m ~plan:true ~inject
           ; Session_chips_view.view m ~inject
           ; Status_view.view m ~inject
           ; panel_toggles m ~inject
           ]
       ; (if m.narrow && m.status_tray
          then
            Node.div
              ~attrs:
                [ Attr.class_ "bar-tray-backdrop"
                ; Attr.on_click (fun _ -> inject Action.Toggle_status_tray)
                ]
              []
          else Node.none)
       ; settings
       ])
;;
