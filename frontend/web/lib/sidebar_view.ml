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
    ; where : string (** its directory, as the list puts it *)
    }
  [@@deriving equal]
end

let rows : (Session_summary.t, Row.t) View_cache.t = View_cache.create ()

let row ~inject (s : Session_summary.t) (row : Row.t) =
  let { Row.selected
      ; running
      ; background
      ; selecting
      ; marked
      ; loading
      ; age
      ; where
      }
    =
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
                ; (if String.is_empty where
                   then Node.none
                   else span ~cls:"session-cwd" where)
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

(* [within]: the directory of the project or worktree the row is under,
   whose subdirectories it names relative to it; outside the tree, the whole
   cwd. *)
let session ?within (m : App.Model.t) ~inject ~current (s : Session_summary.t) =
  let selected = Option.equal String.equal current (Some s.id) in
  let where =
    match within with
    | None -> home_relative s.cwd
    | Some dir when String.equal dir s.cwd -> ""
    | Some dir ->
      (match String.chop_prefix s.cwd ~prefix:(dir ^ "/") with
       | Some sub -> sub ^ "/"
       | None -> home_relative s.cwd)
  in
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
    ; where
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

(* ---- the tree: projects, their worktrees, their sessions *)

module Project_tree = Prigh_ui.Project_tree

let hover_action ~cls ~title ~label ~icon:i effect =
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
;;

(* A folded row still says what is inside: the count, work in progress, the
   session we are in. *)
let summary ~folded ~current sessions =
  let running =
    List.count sessions ~f:(fun (s : Session_summary.t) -> s.running)
  in
  let here =
    List.exists sessions ~f:(fun (s : Session_summary.t) ->
      Option.equal String.equal current (Some s.id))
  in
  ( here
  , if not folded
    then Node.none
    else
      Node.span
        ~attrs:[ Attr.class_ "tree-count" ]
        [ (if running > 0
           then lamp ~title:(sprintf "%d running" running) ()
           else Node.none)
        ; Node.text (Int.to_string (List.length sessions))
        ] )
;;

let toggle_attrs ~folded ~dir ~inject =
  [ Attr.type_ "button"
  ; Attr.create "aria-expanded" (Bool.to_string (not folded))
  ; Attr.create "data-dir" dir
  ; Attr.title (home_relative dir)
  ; Attr.on_click (fun _ -> inject (Action.Toggle_folded dir))
  ]
;;

let changes (w : Worktree.t) =
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
;;

let sync (w : Worktree.t) =
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
;;

let worktree_node
      (m : App.Model.t)
      ~inject
      ~current
      ~searching
      (w : Project_tree.Worktree_node.t)
  =
  let label = Project_tree.Worktree_node.label w in
  let folded = (not searching) && Set.mem m.folded w.path in
  let here, count = summary ~folded ~current w.sessions in
  let here = here || Option.exists w.listed ~f:(fun l -> l.current) in
  let gone = Option.exists w.listed ~f:(fun l -> l.prunable) in
  let entering =
    Option.equal String.equal (Worktrees.entering m.worktrees) (Some w.path)
  in
  let removable =
    match w.listed with
    | Some l -> Option.is_none (Worktree_command.cannot_remove l)
    | None -> not here
  in
  let tag text ~title =
    Node.span
      ~attrs:[ Attr.class_ "worktree-tag"; Attr.title title ]
      [ Node.text text ]
  in
  Node.div
    ~attrs:
      [ classes
          [ "worktree" ]
          [ "here", here
          ; "folded", folded
          ; "agent", Option.is_some w.agent
          ; "dirty", Option.exists w.listed ~f:Worktree.dirty
          ; "gone", gone
          ; "opening", entering
          ]
      ; Attr.create "data-worktree" w.path
      ]
    [ div
        ~cls:"worktree-row"
        [ Node.button
            ~attrs:
              (Attr.class_ "worktree-toggle"
               :: toggle_attrs ~folded ~dir:w.path ~inject)
            [ (if entering
               then lamp ~title:"Opening…" ()
               else icon ~cls:"worktree-icon" Branch)
            ; span ~cls:"worktree-branch" label
            ; (match w.agent with
               | Some (a, _) ->
                 tag
                   "subagent"
                   ~title:(sprintf "Made for subagent %s: %s" a.agent a.task)
               | None -> Node.none)
            ; Option.value_map w.listed ~default:Node.none ~f:changes
            ; Option.value_map w.listed ~default:Node.none ~f:sync
            ; (if Option.exists w.listed ~f:(fun l -> l.locked)
               then tag "locked" ~title:"git worktree unlock to remove it"
               else Node.none)
            ; (if gone
               then tag "gone" ~title:"Its directory was deleted"
               else Node.none)
            ; count
            ]
        ; Node.span
            ~attrs:[ Attr.class_ "tree-actions" ]
            [ (if gone
               then Node.none
               else
                 hover_action
                   ~cls:"new-here"
                   ~title:"New session in this worktree"
                   ~label:("New session in " ^ label)
                   ~icon:Plus
                   (inject (Action.New_session_in w.path)))
            ; (if removable
               then
                 hover_action
                   ~cls:"delete"
                   ~title:"Remove this worktree (/worktree rm)"
                   ~label:("Remove " ^ label)
                   ~icon:Trash
                   (inject (Action.Ask_remove_worktree w.path))
               else Node.none)
            ]
        ]
    ; (match w.agent with
       | Some (_, owner) when not folded ->
         div
           ~cls:"worktree-origin"
           [ Node.text "from "
           ; Node.button
               ~attrs:
                 [ Attr.class_ "worktree-owner"
                 ; Attr.type_ "button"
                 ; Attr.title "The session whose subagent works here"
                 ; Attr.on_click (fun _ ->
                     inject (Action.Switch_session owner.path))
                 ]
               [ Node.span
                   ~attrs:[ Attr.create "dir" "auto" ]
                   [ Node.text (Session_list.title owner) ]
               ]
           ]
       | Some _ | None -> Node.none)
    ; (if folded || List.is_empty w.sessions
       then Node.none
       else
         div
           ~cls:"worktree-sessions"
           (List.map w.sessions ~f:(session ~within:w.path m ~inject ~current)))
    ]
;;

let project
      (m : App.Model.t)
      ~inject
      ~current
      ~searching
      (p : Project_tree.Project.t)
  =
  let name = Project_tree.Project.name p in
  let folded = (not searching) && Set.mem m.folded p.root in
  let here, count =
    summary ~folded ~current (Project_tree.Project.all_sessions p)
  in
  let here =
    here
    || Option.exists p.listed ~f:(fun l -> l.current)
    || List.exists p.worktrees ~f:(fun w ->
      Option.exists w.listed ~f:(fun l -> l.current))
  in
  Node.section
    ~attrs:
      [ classes [ "project" ] [ "here", here; "folded", folded; "git", p.git ]
      ; Attr.create "aria-label" name
      ; Attr.create "data-project" p.root
      ]
    [ div
        ~cls:"project-row"
        [ Node.button
            ~attrs:
              (Attr.class_ "project-toggle"
               :: toggle_attrs ~folded ~dir:p.root ~inject)
            [ Node.span ~attrs:[ Attr.class_ "chev" ] []
            ; icon ~cls:"project-icon" (if p.git then Repo else Folder)
            ; span ~cls:"project-name" name
            ; (match p.branch with
               | Some branch when p.git -> span ~cls:"project-branch" branch
               | _ -> Node.none)
            ; count
            ]
        ; Node.span
            ~attrs:[ Attr.class_ "tree-actions" ]
            [ hover_action
                ~cls:"new-here"
                ~title:(sprintf "New session in %s" (home_relative p.root))
                ~label:("New session in " ^ name)
                ~icon:Plus
                (inject (Action.New_session_in p.root))
            ; (if p.git
               then
                 hover_action
                   ~cls:"new-worktree"
                   ~title:
                     "New worktree: work on a branch of its own, in a new \
                      session"
                   ~label:("New worktree of " ^ name)
                   ~icon:Branch_plus
                   (inject (Action.New_worktree { repo = Some p.root }))
               else Node.none)
            ]
        ]
    ; (if folded
       then Node.none
       else
         div
           ~cls:"project-body"
           (List.map p.sessions ~f:(session ~within:p.root m ~inject ~current)
            @ List.map
                p.worktrees
                ~f:(worktree_node m ~inject ~current ~searching)))
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
           let _, archived =
             Session_list.sections m.sessions ~query:m.session_query
           in
           let searching = Session_list.searching ~query:m.session_query in
           let shown =
             Session_list.archived_shown
               ~query:m.session_query
               ~archived_open:m.archived_open
           in
           List.map
             (Session_list.tree
                m.sessions
                ~query:m.session_query
                ~listing:(Worktrees.listing m.worktrees))
             ~f:(project m ~inject ~current ~searching)
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
    ; user_footer m ~inject
    ]
;;
