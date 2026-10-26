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
    [ Files_view.toggle m ~inject
    ; Terminal_view.toggle m ~inject
    ; Agents_view.toggle m ~inject
    ]
;;

(* Over the chat's top right corner, quietly: no bar under them. *)
let tools (m : App.Model.t) ~inject =
  match m.state with
  | None -> Node.none
  | Some _ -> div ~cls:"chat-tools" [ panel_toggles m ~inject ]
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

let card (m : App.Model.t) ~inject =
  Node.section
    ~attrs:
      [ Attr.class_ "session-card"; Attr.create "aria-label" "This session" ]
    [ Status_view.extras m ~host:false ~inject
    ; div
        ~cls:"card-status"
        [ Status_view.view m ~inject
        ; (match m.state with
           | None -> Node.none
           | Some state ->
             div
               ~cls:"card-where"
               [ Status_view.host m ~always:true ~inject
               ; branch state ~inject
               ])
        ]
    ]
;;

let bar (m : App.Model.t) ~inject =
  let toggle =
    button
      ~cls:"icon ghost toggle-sidebar"
      ~title:"Sessions (Ctrl+B)"
      ~label:"Sessions"
      ~on_click:(inject Action.Toggle_sidebar)
      [ icon Menu ]
  in
  Node.header
    ~attrs:[ Attr.class_ "session-bar" ]
    (match m.state with
     | None ->
       [ toggle
       ; Status_view.extras m ~host:true ~inject
       ; Status_view.view m ~inject
       ; Sidebar_view.settings ~inject
       ]
     | Some state ->
       [ toggle
       ; rename m state ~inject
       ; Status_view.extras m ~host:true ~inject
       ; Status_view.view m ~inject
       ; panel_toggles m ~inject
       ; Sidebar_view.settings ~inject
       ])
;;
