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

(* The cwd, elided in the middle when narrow: the directory it ends in stays. *)
let where cwd =
  let path = Sidebar_view.home_relative cwd in
  let head, tail =
    match String.rsplit2 path ~on:'/' with
    | Some (dir, base) when not (String.is_empty base) -> dir, "/" ^ base
    | _ -> "", path
  in
  Node.span
    ~attrs:[ Attr.class_ "where"; Attr.title cwd ]
    [ (if String.is_empty head then Node.none else span ~cls:"where-head" head)
    ; span ~cls:"where-tail" tail
    ]
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

let card (m : App.Model.t) ~inject =
  let section children =
    Node.section
      ~attrs:
        [ Attr.class_ "session-card"; Attr.create "aria-label" "This session" ]
      children
  in
  match m.state with
  | None -> section [ Status_view.view m ~inject ]
  | Some state ->
    section
      [ div ~cls:"card-head" [ rename m state ~inject; panel_toggles m ~inject ]
      ; div
          ~cls:"session-where"
          [ where state.cwd
          ; (match state.git_branch with
             | Some branch ->
               Node.span
                 ~attrs:[ Attr.class_ "branch"; Attr.title branch ]
                 [ icon Branch; span ~cls:"branch-name" branch ]
             | None -> Node.none)
          ]
      ; div ~cls:"card-host" [ Status_view.host m ~always:true ~inject ]
        (* The status last, at the foot of the sidebar: it stays put as
           what applies comes and goes above it. *)
      ; Status_view.extras m ~host:false ~inject
      ; Status_view.view m ~inject
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
