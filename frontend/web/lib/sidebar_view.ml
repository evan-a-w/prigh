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

(* What a row shows besides its session: rebuilt only when one changes
   (the list is drawn again at every event otherwise). *)
module Row = struct
  type t =
    { selected : bool
    ; running : bool
    ; background : bool (** only its subagents or jobs run *)
    ; selecting : bool
    ; marked : bool
    ; loading : bool
    ; age : string
    }
  [@@deriving equal]
end

let rows : (Session_summary.t, Row.t) View_cache.t = View_cache.create ()

let row ~inject (s : Session_summary.t) (row : Row.t) =
  let { Row.selected; running; background; selecting; marked; loading; age } =
    row
  in
  let action ~cls ~title ~label ~icon:i effect =
    Node.button
      ~attrs:
        [ Attr.class_ ("btn icon ghost " ^ cls)
        ; Attr.type_ "button"
        ; Attr.title title
        ; Attr.create "aria-label" label
        ; Attr.on_click (fun ev ->
            Js_of_ocaml.Dom_html.stopPropagation ev;
            effect)
        ]
      [ icon i ]
  in
  (* The lamp: work in progress here (its turn, or only its subagents and
     jobs), or the row being opened. Archived sessions never run. *)
  let mark =
    if loading
    then lamp ~title:"Opening…" ()
    else if running
    then
      lamp
        ~title:
          (if background
           then
             sprintf
               "Running: %d subagent%s or job%s in the background"
               s.background
               (if s.background = 1 then "" else "s")
               (if s.background = 1 then "" else "s")
           else "Running")
        ()
    else if s.live && not s.archived
    then span ~cls:"live" ~attrs:[ Attr.title "Open in the backend" ] ""
    else Node.none
  in
  let title = Session_list.title s in
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
        (* Its transcript is fetched ahead while the pointer rests on it (or
           a finger or the keyboard is on it), so that the click shows it at
           once. *)
      ; Attr.on_pointerenter (fun _ -> inject (Action.Prefetch_session s.path))
      ]
    [ Node.button
        ~attrs:
          [ Attr.class_ "session-open"
          ; Attr.type_ "button"
          ; Attr.id (Session_list.dom_id s.id)
          ; Attr.create "data-session" s.id
          ; Attr.title s.path
          ; (if selecting
             then Attr.many [ Attr.role "checkbox"; Attr.create "aria-checked" (Bool.to_string marked) ]
             else if selected
             then Attr.create "aria-current" "true"
             else Attr.empty)
          ; (if loading then Attr.create "aria-busy" "true" else Attr.empty)
          ; Attr.on_click (fun ev ->
              if selecting
              then
                inject (Action.Mark_session { path = s.path; range = shift_click ev })
              else inject (Action.Switch_session s.path))
          ; Attr.on_pointerdown (fun _ -> inject (Action.Prefetch_session s.path))
          ; Attr.on_focus (fun _ -> inject (Action.Prefetch_session s.path))
          ]
        [ (if selecting
           then
             check marked
           else Node.none)
        ; div ~cls:"session-mark" [ mark ]
        ; div
            ~cls:"session-main"
            [ span ~cls:"session-title" ~attrs:[ Attr.create "dir" "auto" ] title
            ; div
                ~cls:"session-meta"
                [ span ~cls:"session-age" age
                ; span ~cls:"session-cwd" (home_relative s.cwd)
                ; (if selecting
                   then
                     span ~cls:"session-size" (Session_summary.size_string s.bytes)
                   else Node.none)
                ]
            ]
        ]
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
                  ~label:("Unarchive " ^ title)
                  ~icon:Unarchive
                  (inject (Action.Set_archived { path = s.path; archived = false }))
              else
                action
                  ~cls:"archive"
                  ~title:"Archive (A)"
                  ~label:("Archive " ^ title)
                  ~icon:Archive
                  (inject (Action.Set_archived { path = s.path; archived = true })))
           ; action
               ~cls:"delete"
               ~title:"Delete session (Delete)"
               ~label:("Delete " ^ title)
               ~icon:Trash
               (inject (Action.Ask_delete s.path))
           ])
    ]
;;

let session (m : App.Model.t) ~inject ~current (s : Session_summary.t) =
  let selected = Option.equal String.equal current (Some s.id) in
  let deps =
    { Row.selected
    ; running = s.running || (selected && App.Model.active m)
    ; background = s.background > 0 && not (selected && App.Model.running m)
    ; selecting = m.marks.selecting
    ; marked = Set.mem m.marks.marked s.path
    ; loading =
        (match m.pending.session with
         | Some (Switching path) -> String.equal path s.path
         | Some Starting | None -> false)
    ; age = age m s
    }
  in
  View_cache.find ~inject rows s ~deps ~equal:Row.equal ~f:(fun () -> row ~inject s deps)
;;

(* Above the list: its name and how to select several sessions, or how
   selecting works. *)
let toolbar (m : App.Model.t) ~inject =
  if m.marks.selecting
  then
    div
      ~cls:"sessions-toolbar selecting"
      [ span ~cls:"sessions-label" "Select sessions"
      ; Node.span
          ~attrs:[ Attr.class_ "sessions-hint" ]
          [ Node.kbd [ Node.text "Shift" ]; Node.text " for a range" ]
      ]
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

(* Selecting: what deleting would remove, at the foot of the list. *)
let selection_bar (m : App.Model.t) ~inject =
  let marked = Session_marks.marked m.marks m.sessions in
  let count = List.length marked in
  div
    ~cls:"selection-bar"
    [ button
        ~cls:"small ghost"
        ~title:"Stop selecting (Esc)"
        ~on_click:(inject (Action.Select_sessions false))
        [ Node.text "Cancel" ]
    ; button
        ~cls:"small danger"
        ~title:"Delete the selected sessions (Delete)"
        ~disabled:(count = 0)
        ~on_click:(inject Action.Delete_marked)
        [ Node.text
            (if count = 0
             then "Delete sessions"
             else
               sprintf
                 "Delete %s (%s)"
                 (if count = 1 then "1 session" else sprintf "%d sessions" count)
                 (Session_summary.size_string (Session_summary.total_bytes marked)))
        ]
    ]
;;

(* A faint rule with the count, folding the archived sessions away. *)
let archived_divider ~inject ~shown archived =
  Node.button
    ~attrs:
      [ Attr.class_ "sessions-divider"
      ; Attr.type_ "button"
      ; Attr.create "aria-expanded" (Bool.to_string shown)
      ; Attr.title
          (if shown then "Hide the archived sessions" else "Show the archived sessions")
      ; Attr.on_click (fun _ -> inject Action.Toggle_archived)
      ]
    [ span ~cls:"divider-label" "Archived"
    ; span ~cls:"divider-count" (Int.to_string (List.length archived))
    ; Node.span ~attrs:[ Attr.class_ "divider-rule" ] []
    ; Node.span ~attrs:[ Attr.class_ "chev" ] []
    ]
;;

let delete_archived ~inject archived =
  Node.button
    ~attrs:
      [ Attr.class_ "delete-archived"
      ; Attr.type_ "button"
      ; Attr.title
          (sprintf
             "Delete every archived session, %s (/delete-archived)"
             (Session_summary.size_string (Session_summary.total_bytes archived)))
      ; Attr.on_click (fun _ -> inject Action.Ask_delete_archived)
      ]
    [ Node.text "Delete archived" ]
;;

let settings ~inject =
  button
    ~cls:"icon ghost settings-toggle"
    ~title:"Settings (Ctrl+, or /settings)"
    ~label:"Settings"
    ~attrs:[ Attr.create "aria-haspopup" "dialog" ]
    ~on_click:(inject Action.Open_settings)
    [ icon Gear ]
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
  | Some account ->
    div ~cls:"sidebar-footer" [ account; help; settings ~inject ]
  | None ->
    div
      ~cls:"sidebar-footer"
      [ button
          ~cls:"ghost help-button"
          ~title:"Commands and keys (/help)"
          ~on_click:(inject Action.Open_help)
          [ icon Help; Node.text "Commands & keys" ]
      ; settings ~inject
      ; (if m.saved_login then sign_out else Node.none)
      ]
;;

let view (m : App.Model.t) ~inject ~session:card =
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
            ~label:"Hide sidebar"
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
           [ lamp (); Node.text "Starting…" ]
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
              ; Attr.placeholder "Search sessions (Ctrl+K)"
              ; Attr.create "aria-label" "Search sessions"
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
           [ div
               ~cls:"sessions-empty"
               [ Node.text "No sessions yet: send a message to start one." ]
           ]
         | [], _ ->
           [ div
               ~cls:"sessions-empty"
               [ Node.textf "Nothing matches “%s”." m.session_query ]
           ]
         | _, _ ->
           let active, archived =
             Session_list.sections m.sessions ~query:m.session_query
           in
           let shown =
             Session_list.archived_shown
               ~query:m.session_query
               ~archived_open:m.archived_open
           in
           List.map active ~f:(session m ~inject ~current)
           @
           (match archived with
            | [] -> []
            | archived ->
              archived_divider ~inject ~shown archived
              ::
              (if shown
               then
                 List.map archived ~f:(session m ~inject ~current)
                 @
                 if m.marks.selecting
                 then []
                 else [ delete_archived ~inject archived ]
               else [])))
    ; (if m.marks.selecting then selection_bar m ~inject else Node.none)
    ; card
    ; user_footer m ~inject
    ]
;;
