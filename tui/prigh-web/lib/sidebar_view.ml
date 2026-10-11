open! Core
open! Import
open Html
module Action = App.Action

let home_relative cwd =
  match String.chop_prefix cwd ~prefix:"/home/" with
  | Some rest ->
    (match String.lsplit2 rest ~on:'/' with
     | Some (_, path) -> "~/" ^ path
     | None -> "~")
  | None -> cwd
;;

let age (m : App.Model.t) (s : Session_summary.t) =
  match
    m.now, Rel_time.parse (Option.value s.updated_at ~default:s.created_at)
  with
  | Some now, Some time -> Rel_time.ago ~now time
  | _ -> ""
;;

let shift_click ev = Js_of_ocaml.Js.to_bool ev##.shiftKey

let session (m : App.Model.t) ~inject ~current (s : Session_summary.t) =
  let selected = Option.equal String.equal current (Some s.id) in
  let running = s.running || (selected && App.Model.active m) in
  let selecting = m.marks.selecting in
  let marked = Set.mem m.marks.marked s.path in
  let loading =
    match m.pending.session with
    | Some (Switching path) -> String.equal path s.path
    | Some Starting | None -> false
  in
  let action ~cls ~title ~icon:i effect =
    Node.button
      ~attrs:
        [ Attr.class_ ("btn icon ghost " ^ cls)
        ; Attr.type_ "button"
        ; Attr.title title
        ; Attr.on_click (fun ev ->
            Js_of_ocaml.Dom_html.stopPropagation ev;
            effect)
        ]
      [ icon i ]
  in
  let status =
    if loading
    then Node.span ~attrs:[ Attr.class_ "spinner"; Attr.title "opening…" ] []
    else if running
    then
      span
        ~cls:"dot running"
        ~attrs:
          [ Attr.title
              (if s.background > 0 && not (selected && App.Model.running m)
               then
                 sprintf
                   "running: %d subagent%s or job%s in the background"
                   s.background
                   (if s.background = 1 then "" else "s")
                   (if s.background = 1 then "" else "s")
               else "running")
          ]
        ""
    else if s.live
    then span ~cls:"dot live" ~attrs:[ Attr.title "open in the backend" ] ""
    else Node.none
  in
  Node.div
    ~attrs:
      [ classes
          [ "session" ]
          [ "selected", selected
          ; "running", running
          ; "opening", loading
          ; "archived", s.archived
          ; "marked", marked
          ]
      ; (if loading then Attr.create "aria-busy" "true" else Attr.empty)
      ; (if selecting
         then Attr.create "aria-checked" (Bool.to_string marked)
         else Attr.empty)
      ; Attr.id (Session_list.dom_id s.id)
      ; Attr.tabindex 0
      ; Attr.role (if selecting then "checkbox" else "button")
      ; Attr.create "data-session" s.id
      ; Attr.on_click (fun ev ->
          if selecting
          then inject (Action.Mark_session { path = s.path; range = shift_click ev })
          else inject (Action.Switch_session s.path))
      ; Attr.title s.path
      ]
    [ div
        ~cls:"session-top"
        [ (if selecting
           then
             Node.span
               ~attrs:[ classes [ "check" ] [ "on", marked ] ]
               [ (if marked then icon Check else Node.none) ]
           else status)
        ; span ~cls:"session-title" (Session_list.title s)
        ; (if s.background > 0
           then
             span
               ~cls:"session-bg"
               ~attrs:[ Attr.title "subagents and jobs running in the background" ]
               (sprintf "%d bg" s.background)
           else Node.none)
        ; span ~cls:"session-age" (age m s)
        ]
    ; div
        ~cls:"session-meta"
        [ span ~cls:"session-cwd" (home_relative s.cwd)
        ; span
            ~cls:"session-count"
            (sprintf
               "%d msg%s"
               s.message_count
               (if s.message_count = 1 then "" else "s"))
        ; (if selecting
           then span ~cls:"session-size" (Session_summary.size_string s.bytes)
           else Node.none)
        ; (if selecting
           then Node.none
           else
             Node.span
               ~attrs:[ Attr.class_ "session-actions" ]
               [ (if s.archived
                  then
                    action
                      ~cls:"unarchive"
                      ~title:"Unarchive (A)"
                      ~icon:Unarchive
                      (inject (Action.Set_archived { path = s.path; archived = false }))
                  else
                    action
                      ~cls:"archive"
                      ~title:"Archive (A)"
                      ~icon:Archive
                      (inject (Action.Set_archived { path = s.path; archived = true })))
               ; action
                   ~cls:"delete"
                   ~title:"Delete session (Delete)"
                   ~icon:Trash
                   (inject (Action.Ask_delete s.path))
               ])
        ]
    ]
;;

(* Above the list: how to select several sessions, or what is selected. *)
let toolbar (m : App.Model.t) ~inject =
  if m.marks.selecting
  then (
    let marked = Session_marks.marked m.marks m.sessions in
    let count = List.length marked in
    div
      ~cls:"sessions-toolbar selecting"
      [ span
          ~cls:"selection-count"
          (if count = 0
           then "Click sessions to select"
           else
             sprintf
               "%d selected · %s"
               count
               (Session_summary.size_string (Session_summary.total_bytes marked)))
      ; button
          ~cls:"small danger"
          ~title:"Delete the selected sessions (Delete)"
          ~disabled:(count = 0)
          ~on_click:(inject Action.Delete_marked)
          [ Node.text "Delete" ]
      ; button
          ~cls:"small ghost"
          ~title:"Stop selecting (Esc)"
          ~on_click:(inject (Action.Select_sessions false))
          [ Node.text "Done" ]
      ])
  else
    div
      ~cls:"sessions-toolbar"
      [ span ~cls:"sessions-label" "Sessions"
      ; button
          ~cls:"small ghost"
          ~title:"Select sessions to delete (/delete)"
          ~on_click:(inject (Action.Select_sessions true))
          [ Node.text "Select" ]
      ]
;;

let archived_divider (m : App.Model.t) ~inject archived =
  div
    ~cls:"sessions-divider"
    [ span
        ~cls:"divider-label"
        (sprintf
           "Archived · %d · %s"
           (List.length archived)
           (Session_summary.size_string (Session_summary.total_bytes archived)))
    ; (if m.marks.selecting
       then Node.none
       else
         button
           ~cls:"small ghost danger-text"
           ~title:"Delete every archived session (/delete-archived)"
           ~on_click:(inject Action.Ask_delete_archived)
           [ Node.text "Delete all" ])
    ]
;;

let user_footer (m : App.Model.t) ~inject =
  let help =
    button
      ~cls:"icon ghost"
      ~title:"Commands and keys (/help)"
      ~on_click:(inject Action.Open_help)
      [ icon Help ]
  in
  let sign_out =
    button
      ~cls:"icon ghost"
      ~title:"Sign out"
      ~on_click:(inject Action.Sign_out)
      [ icon Logout ]
  in
  match Account_view.trigger m ~inject with
  | Some account -> div ~cls:"sidebar-footer" [ account; help ]
  | None ->
    div
      ~cls:"sidebar-footer"
      [ button
          ~cls:"ghost help-button"
          ~title:"Commands and keys (/help)"
          ~on_click:(inject Action.Open_help)
          [ icon Help; Node.text "Commands & keys" ]
      ; (if m.saved_login then sign_out else Node.none)
      ]
;;

let view (m : App.Model.t) ~inject =
  let current = Option.map m.state ~f:(fun s -> s.session_id) in
  let sessions = Session_list.filter m.sessions ~query:m.session_query in
  Node.aside
    ~attrs:[ Attr.class_ "sidebar"; Attr.id "sidebar" ]
    [ div
        ~cls:"sidebar-head"
        [ div
            ~cls:"brand"
            [ span ~cls:"logo" "prigh"; span ~cls:"brand-sub" "web" ]
        ; button
            ~cls:"icon ghost"
            ~title:"Hide sidebar (Ctrl+B)"
            ~on_click:(inject Action.Toggle_sidebar)
            [ icon Sidebar ]
        ]
    ; (match m.pending.session with
       | Some Starting ->
         button
           ~cls:"new-session opening"
           ~title:"Starting a session…"
           ~disabled:true
           ~on_click:Effect.Ignore
           [ Node.span ~attrs:[ Attr.class_ "spinner" ] []
           ; Node.text "Starting…"
           ]
       | Some (Switching _) | None ->
         button
           ~cls:"new-session"
           ~title:"Start a new session (/new)"
           ~on_click:(inject Action.New_session)
           [ icon Plus; Node.text "New session" ])
    ; div
        ~cls:"search"
        [ icon Search
        ; Node.input
            ~attrs:
              [ Attr.id "session-search"
              ; Attr.type_ "search"
              ; Attr.placeholder "Search sessions  (Ctrl+K)"
              ; Attr.value m.session_query
              ; Attr.create "autocomplete" "off"
              ; Attr.on_input (fun _ text ->
                  inject (Action.Set_session_query text))
              ]
            ()
        ]
    ; (match m.sessions with
       | [] -> Node.none
       | _ -> toolbar m ~inject)
    ; div
        ~cls:"sessions"
        (match sessions, m.sessions with
         | [], [] ->
           [ div ~cls:"sessions-empty" [ Node.text "No saved sessions yet." ] ]
         | [], _ ->
           [ div
               ~cls:"sessions-empty"
               [ Node.textf "Nothing matches “%s”." m.session_query ]
           ]
         | _, _ ->
           let active, archived =
             Session_list.sections m.sessions ~query:m.session_query
           in
           List.map active ~f:(session m ~inject ~current)
           @ (match archived with
              | [] -> []
              | archived ->
                archived_divider m ~inject archived
                :: List.map archived ~f:(session m ~inject ~current)))
    ; user_footer m ~inject
    ]
;;
