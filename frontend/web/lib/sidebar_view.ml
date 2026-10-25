open! Core
open! Import
open Html
module Action = App.Action
module Worktree_command = Prigh_ui.Worktree_command

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

(* ---- the repository's worktrees *)

let worktree_session (m : App.Model.t) ~inject (ref_ : Worktree.Session_ref.t) =
  let summary = List.find m.sessions ~f:(fun s -> String.equal s.id ref_.id) in
  let here =
    Option.exists m.state ~f:(fun s -> String.equal s.session_id ref_.id)
  in
  let name =
    match summary, ref_.name with
    | Some s, _ -> Session_list.title s
    | None, Some name -> name
    | None, None -> ref_.id
  in
  let running = ref_.running || Option.exists summary ~f:(fun s -> s.running) in
  Node.button
    ~attrs:
      [ classes [ "worktree-session" ] [ "here", here; "running", running ]
      ; Attr.type_ "button"
      ; Attr.title (if here then "This session" else "Switch to this session")
      ; (if here then Attr.create "aria-current" "true" else Attr.empty)
      ; Attr.on_click (fun _ -> inject (Action.Switch_session ref_.path))
      ]
    [ (if running then lamp ~title:"Running" () else Node.none)
    ; Node.span
        ~attrs:[ Attr.class_ "worktree-session-name"; Attr.create "dir" "auto" ]
        [ Node.text name ]
    ]
;;

let worktree (m : App.Model.t) ~inject (w : Worktree.t) =
  let label = Worktree.label w in
  let entering =
    Option.equal String.equal (Worktrees.entering m.worktrees) (Some w.path)
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
  let changes =
    match w.changes with
    | Some n when n > 0 ->
      Node.span
        ~attrs:
          [ Attr.class_ "worktree-changes"
          ; Attr.title
              (sprintf
                 "%d file%s with uncommitted changes"
                 n
                 (if n = 1 then "" else "s"))
          ]
        [ Node.textf "%d changed" n ]
    | _ -> Node.none
  in
  let sync =
    match
      List.filter_opt
        [ Option.some_if (w.ahead > 0) (sprintf "↑%d" w.ahead)
        ; Option.some_if (w.behind > 0) (sprintf "↓%d" w.behind)
        ]
    with
    | [] -> Node.none
    | parts ->
      Node.span
        ~attrs:
          [ Attr.class_ "worktree-sync"
          ; Attr.title
              (sprintf
                 "%d ahead of and %d behind %s"
                 w.ahead
                 w.behind
                 (Option.value w.upstream ~default:"its upstream"))
          ]
        [ Node.text (String.concat ~sep:" " parts) ]
  in
  let tag text = span ~cls:"worktree-tag" text in
  Node.div
    ~attrs:
      [ classes
          [ "worktree" ]
          [ "current", w.current
          ; "dirty", Worktree.dirty w
          ; "gone", w.prunable
          ; "opening", entering
          ]
      ]
    [ Node.button
        ~attrs:
          [ Attr.class_ "worktree-open"
          ; Attr.type_ "button"
          ; Attr.create "data-worktree" w.path
          ; Attr.title
              (String.concat
                 ~sep:" · "
                 (List.filter_opt
                    [ Some w.path
                    ; Option.some_if w.main "the repository's main worktree"
                    ; Some
                        (if w.current
                         then "this session works here"
                         else "work here (/worktree open)")
                    ]))
          ; (if w.current
             then Attr.create "aria-current" "true"
             else Attr.empty)
          ; (if entering then Attr.create "aria-busy" "true" else Attr.empty)
          ; Attr.on_click (fun _ ->
              inject
                (Action.Open_worktree { path = w.path; new_session = false }))
          ]
        [ div
            ~cls:"worktree-mark"
            [ (if entering
               then lamp ~title:"Opening…" ()
               else icon ~cls:"worktree-icon" Branch)
            ]
        ; div
            ~cls:"worktree-main"
            [ div
                ~cls:"worktree-line"
                [ span ~cls:"worktree-branch" label
                ; changes
                ; sync
                ; (if w.locked then tag "locked" else Node.none)
                ; (if w.prunable then tag "gone" else Node.none)
                ]
            ; span ~cls:"worktree-path" (home_relative w.path)
            ]
        ]
    ; Node.span
        ~attrs:[ Attr.class_ "worktree-actions" ]
        [ (if w.prunable
           then Node.none
           else
             action
               ~cls:"new-here"
               ~title:"New session in this worktree (/worktree open -n)"
               ~label:("New session in " ^ label)
               ~icon:Plus
               (inject
                  (Action.Open_worktree { path = w.path; new_session = true })))
        ; (if Option.is_some (Worktree_command.cannot_remove w)
           then Node.none
           else
             action
               ~cls:"delete"
               ~title:"Remove this worktree (/worktree rm)"
               ~label:("Remove " ^ label)
               ~icon:Trash
               (inject (Action.Ask_remove_worktree w.path)))
        ]
    ; (match w.sessions with
       | [] -> Node.none
       | sessions ->
         div
           ~cls:"worktree-sessions"
           (List.map sessions ~f:(worktree_session m ~inject)))
    ]
;;

(* Shown only in a repository: nothing elsewhere. *)
let worktrees (m : App.Model.t) ~inject =
  match Worktrees.listing m.worktrees with
  | None -> Node.none
  | Some listing ->
    let shown = m.worktrees_open in
    Node.section
      ~attrs:[ Attr.class_ "worktrees"; Attr.create "aria-label" "Worktrees" ]
      [ div
          ~cls:"worktrees-head"
          [ Node.button
              ~attrs:
                [ Attr.class_ "worktrees-toggle"
                ; Attr.type_ "button"
                ; Attr.create "aria-expanded" (Bool.to_string shown)
                ; Attr.title
                    (sprintf
                       "Git worktrees of %s (%s)"
                       (home_relative listing.root)
                       (if shown then "hide them" else "show them"))
                ; Attr.on_click (fun _ -> inject Action.Toggle_worktrees)
                ]
              [ span ~cls:"sessions-label" "Worktrees"
              ; span
                  ~cls:"divider-count"
                  (Int.to_string (List.length listing.worktrees))
              ; Node.span ~attrs:[ Attr.class_ "chev" ] []
              ]
          ; (if Worktrees.loading m.worktrees
             then lamp ~title:"Listing the worktrees…" ()
             else Node.none)
          ; button
              ~cls:"icon ghost small"
              ~title:"List the worktrees again"
              ~label:"Refresh worktrees"
              ~on_click:(inject Action.Refresh_worktrees)
              [ icon Restart ]
          ; button
              ~cls:"icon ghost small"
              ~title:"New worktree (/worktree new BRANCH [BASE])"
              ~label:"New worktree"
              ~on_click:(inject Action.New_worktree)
              [ icon Plus ]
          ]
      ; (if shown
         then
           div
             ~cls:"worktrees-list"
             (List.map listing.worktrees ~f:(worktree m ~inject))
         else Node.none)
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
    ; worktrees m ~inject
    ; user_footer m ~inject
    ]
;;
