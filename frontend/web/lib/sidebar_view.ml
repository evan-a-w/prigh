open! Core
open! Import
open Html
module Action = App.Action

(* [cwd] with [home], the home of the tool host it is on, as [~]; with that
   unknown (an older backend or host), any /home/USER is taken for it. *)
let home_relative ?home cwd =
  match home with
  | Some home when String.equal cwd home -> "~"
  | Some home ->
    (match String.chop_prefix cwd ~prefix:(home ^ "/") with
     | Some rest when not (String.equal home "/") -> "~/" ^ rest
     | Some _ | None -> cwd)
  | None ->
    (match String.chop_prefix cwd ~prefix:"/home/" with
     | Some rest ->
       (match String.lsplit2 rest ~on:'/' with
        | Some (_, path) -> "~/" ^ path
        | None -> "~")
     | None -> cwd)
;;

let host_home (state : State.t) ~host =
  List.find_map state.hosts ~f:(fun (h : Host.t) ->
    if String.equal h.id host then h.home else None)
;;

(* The home of the session's active tool host. *)
let active_home (state : State.t) = host_home state ~host:state.active_host

let state_home (m : App.Model.t) = Option.bind m.state ~f:active_home

(* The home of the host [s]'s cwd is on, when that host is connected. *)
let session_home (m : App.Model.t) (s : Session_summary.t) =
  Option.bind m.state ~f:(fun state ->
    if String.equal s.id state.session_id
    then active_home state
    else Option.bind s.host ~f:(fun host -> host_home state ~host))
;;

let age (m : App.Model.t) (s : Session_summary.t) =
  match
    m.now, Rel_time.parse (Option.value s.updated_at ~default:s.created_at)
  with
  | Some now, Some time -> Rel_time.ago ~now time
  | _ -> ""
;;

let shift_click ev = Js_of_ocaml.Js.to_bool ev##.shiftKey

module Session_worktrees = Prigh_ui.Session_worktrees

(* A context menu's event: at the pointer, or (from the keyboard, which
   gives no position) under the row. *)
let open_menu
      ~inject
      target
      ~row
      (ev : Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t)
  =
  let x = Float.to_int (Js_of_ocaml.Js.to_float ev##.clientX)
  and y = Float.to_int (Js_of_ocaml.Js.to_float ev##.clientY) in
  let anchor : Context_menu.Anchor.t =
    if x = 0 && y = 0 then Element row else Point { x; y }
  in
  Js_of_ocaml.Dom.preventDefault ev;
  Effect.Many
    [ Effect.Stop_propagation; inject (Action.Open_menu { target; anchor }) ]
;;

(* The row's ⋯: its menu, under the button. *)
let more_button ~inject ~id ~label target =
  Node.button
    ~attrs:
      [ Attr.class_ "btn icon ghost more"
      ; Attr.type_ "button"
      ; Attr.id id
      ; Attr.title "More (right-click, Shift+F10)"
      ; Attr.create "aria-label" ("More for " ^ label)
      ; Attr.create "aria-haspopup" "menu"
      ; Attr.on_click (fun _ ->
          Effect.Many
            [ Effect.Stop_propagation
            ; inject (Action.Open_menu { target; anchor = Element id })
            ])
      ]
    [ icon More ]
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
      [ Node.textf "● %d" n ]
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

let tag ?(cls = "") text ~title =
  Node.span
    ~attrs:[ Attr.class_ ("worktree-tag " ^ cls); Attr.title title ]
    [ Node.text text ]
;;

(* A worktree under its session: its branch, what is in it, why it is
   there. Clicking (or right-clicking) opens its menu. *)
let worktree_row
      ?home
      ~inject
      ~entering
      (s : Session_summary.t)
      (n : Session_worktrees.Node.t)
  =
  let w = n.worktree in
  let label = Session_worktrees.Node.label n in
  let id = Context_menu.worktree_dom_id ~session:s.id w.path in
  let target =
    Context_menu.Target.Worktree { session = s.path; path = w.path }
  in
  let gone = Option.exists n.listed ~f:(fun l -> l.prunable) in
  let opening = Option.equal String.equal entering (Some w.path) in
  Node.button
    ~attrs:
      [ classes
          [ "worktree" ]
          [ "here", n.here
          ; "agent", Session_worktrees.Node.agent n
          ; "dirty", Option.exists n.listed ~f:Worktree.dirty
          ; "gone", gone
          ; "opening", opening
          ]
      ; Attr.type_ "button"
      ; Attr.id id
      ; Attr.create "data-worktree" w.path
      ; Attr.create "data-owner" s.id
      ; Attr.create "aria-haspopup" "menu"
      ; Attr.title (home_relative ?home w.path)
      ; Attr.on_click (fun _ ->
          inject (Action.Open_menu { target; anchor = Element id }))
      ; Attr.on_contextmenu (open_menu ~inject target ~row:id)
      ]
    [ (if opening
       then lamp ~title:"Opening…" ()
       else icon ~cls:"worktree-icon" Branch)
    ; span ~cls:"worktree-branch" label
    ; (match w.source with
       | Agent { agent; task } ->
         tag
           ~cls:"agent"
           "subagent"
           ~title:(sprintf "Made for subagent %s: %s" agent task)
       | Pull_request number ->
         tag
           (sprintf "#%d" number)
           ~title:(sprintf "Pull request #%d, checked out" number)
       | Created | Entered -> Node.none)
    ; (if n.here
       then tag ~cls:"here" "here" ~title:"The session works in it"
       else Node.none)
    ; Option.value_map n.listed ~default:Node.none ~f:changes
    ; Option.value_map n.listed ~default:Node.none ~f:sync
    ; (if Option.exists n.listed ~f:(fun l -> l.locked)
       then tag "locked" ~title:"git worktree unlock to remove it"
       else Node.none)
    ; (if gone then tag "gone" ~title:"Its directory was deleted" else Node.none)
    ]
;;

let place ?cls ?home (s : Session_summary.t) ~cwd =
  let class_ base = Attr.classes (base :: Option.to_list cls) in
  match s.repo with
  | Some r ->
    Node.span
      ~attrs:
        [ class_ "session-place"
        ; Attr.title (home_relative ?home cwd)
        ]
      [ span ~cls:"session-repo" (Session_worktrees.repo_name r)
      ; (match r.branch with
         | Some branch ->
           Node.span
             ~attrs:[ Attr.class_ "session-branch" ]
             [ icon ~cls:"place-icon" Branch; Node.text branch ]
         | None -> Node.none)
      ]
  | None ->
    Node.span
      ~attrs:[ class_ "session-cwd"; Attr.title cwd ]
      [ Node.text (home_relative ?home cwd) ]
;;

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
    ; cwd : string
    ; home : string option
    ; worktrees : Session_worktrees.Node.t list
    ; folded : bool (** its worktrees *)
    ; entering : string option
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
      ; cwd
      ; home
      ; worktrees
      ; folded
      ; entering
      }
    =
    row
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
  let dom_id = Session_list.dom_id s.id in
  let target = Context_menu.Target.Session s.path in
  let fold =
    match worktrees with
    | [] -> Node.none
    | nodes ->
      Node.button
        ~attrs:
          [ classes [ "session-fold" ] [ "folded", folded ]
          ; Attr.type_ "button"
          ; Attr.create "aria-expanded" (Bool.to_string (not folded))
          ; Attr.title
              (sprintf
                 "%s its worktree%s"
                 (if folded then "Show" else "Hide")
                 (if List.length nodes = 1 then "" else "s"))
          ; Attr.on_click (fun _ -> inject (Action.Toggle_folded s.id))
          ]
        [ icon ~cls:"fold-icon" Branch
        ; Node.text (Int.to_string (List.length nodes))
        ; Node.span ~attrs:[ Attr.class_ "chev" ] []
        ]
  in
  Node.div
    ~attrs:
      [ classes
          [ "session-item" ]
          [ "selected", selected
          ; "has-worktrees", not (List.is_empty worktrees)
          ]
      ]
    [ Node.div
        ~attrs:
          [ classes
              [ "session" ]
              [ "selected", selected
              ; "running", running
              ; "opening", loading
              ; "archived", s.archived
              ; "marked", marked
              ]
            (* Its transcript is fetched ahead while the pointer rests on it
               (or a finger or the keyboard is on it), so that the click shows
               it at once. *)
          ; Attr.on_pointerenter (fun _ ->
              inject (Action.Prefetch_session s.path))
          ; (if selecting
             then Attr.empty
             else Attr.on_contextmenu (open_menu ~inject target ~row:dom_id))
          ]
        [ Node.button
            ~attrs:
              [ Attr.class_ "session-open"
              ; Attr.type_ "button"
              ; Attr.id dom_id
              ; Attr.create "data-session" s.id
              ; Attr.title s.path
              ; (if selecting
                 then
                   Attr.many
                     [ Attr.role "checkbox"
                     ; Attr.create "aria-checked" (Bool.to_string marked)
                     ]
                 else if selected
                 then Attr.create "aria-current" "true"
                 else Attr.empty)
              ; (if loading then Attr.create "aria-busy" "true" else Attr.empty)
              ; Attr.on_click (fun ev ->
                  if selecting
                  then
                    inject
                      (Action.Mark_session
                         { path = s.path; range = shift_click ev })
                  else inject (Action.Switch_session s.path))
              ; Attr.on_pointerdown (fun _ ->
                  inject (Action.Prefetch_session s.path))
              ; Attr.on_focus (fun _ -> inject (Action.Prefetch_session s.path))
              ]
            [ (if selecting then check marked else Node.none)
            ; div ~cls:"session-mark" [ mark ]
            ; div
                ~cls:"session-main"
                [ span
                    ~cls:"session-title"
                    ~attrs:[ Attr.create "dir" "auto" ]
                    title
                ; div
                    ~cls:"session-meta"
                    [ span ~cls:"session-age" age
                    ; place ?home s ~cwd
                    ; (if selecting
                       then
                         span
                           ~cls:"session-size"
                           (Session_summary.size_string s.bytes)
                       else Node.none)
                    ]
                ]
            ]
        ; (if selecting then Node.none else fold)
        ; (if selecting
           then Node.none
           else
             Node.span
               ~attrs:[ Attr.class_ "session-actions" ]
               [ more_button ~inject ~id:("more-" ^ dom_id) ~label:title target
               ])
        ]
    ; (if folded || selecting || List.is_empty worktrees
       then Node.none
       else
         Node.div
           ~attrs:
             [ Attr.class_ "session-worktrees"
             ; Attr.role "group"
             ; Attr.create "aria-label" ("Worktrees of " ^ title)
             ]
           (List.map worktrees ~f:(worktree_row ?home ~inject ~entering s)))
    ]
;;

let session (m : App.Model.t) ~inject ~current (s : Session_summary.t) =
  let selected = Option.equal String.equal current (Some s.id) in
  let cwd =
    (* The current session's as it is now: [/cd] moves it. *)
    match m.state with
    | Some state when selected -> state.cwd
    | _ -> s.cwd
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
    ; cwd
    ; home = session_home m s
    ; worktrees =
        Session_worktrees.of_session
          ~listings:
            (Option.to_list (Worktrees.listing m.worktrees)
             @ Repo_listings.listings m.repo_worktrees)
          ~cwd
          s
    ; folded = Set.mem m.folded s.id
    ; entering = Worktrees.entering m.worktrees
    }
  in
  View_cache.find ~inject rows s ~deps ~equal:Row.equal ~f:(fun () ->
    row ~inject s deps)
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
                 (Session_summary.size_string
                    (Session_summary.total_bytes marked)))
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
          (if shown
           then "Hide the archived sessions"
           else "Show the archived sessions")
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
             (Session_summary.size_string
                (Session_summary.total_bytes archived)))
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
  let sign_out =
    button
      ~cls:"icon ghost"
      ~title:"Sign out"
      ~on_click:(inject Action.Sign_out)
      [ icon Logout ]
  in
  match Account_view.trigger m ~inject with
  | Some account -> div ~cls:"sidebar-footer" [ account; settings ~inject ]
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
