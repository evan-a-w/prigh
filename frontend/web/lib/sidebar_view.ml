open! Core
open! Import
open Html
module Action = App.Action

let home_relative = Tilde.of_path

let state_host_home (m : App.Model.t) =
  Option.bind m.state ~f:Tilde.active_host_home
;;

let session_host_home (m : App.Model.t) s = Tilde.session_host_home m.state s

let age (m : App.Model.t) (s : Session_summary.t) =
  match
    m.now, Rel_time.parse (Option.value s.updated_at ~default:s.created_at)
  with
  | Some now, Some time -> Rel_time.short ~now time
  | _ -> ""
;;

let current_activity (m : App.Model.t) =
  match m.state with
  | Some state when state.running ->
    (match Chat.activity m.chat with
     | Thinking | Waiting _ -> Some "thinking…"
     | Retrying _ -> Some "retrying…"
     | Idle | Working | Writing | Running_tools -> Some "working…")
  | Some _ when m.pending.sending > 0 || Agents.running_agents m.agents > 0 ->
    Some "working…"
  | Some _ | None -> None
;;

let shift_click ev = Js_of_ocaml.Js.to_bool ev##.shiftKey

let toggle_click ev =
  Js_of_ocaml.Js.to_bool ev##.ctrlKey || Js_of_ocaml.Js.to_bool ev##.metaKey
;;

module Session_worktrees = Prigh_ui.Session_worktrees
module Button = Loon.Button
module Kbd = Loon.Kbd

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

let place ?cls ?host_home (s : Session_summary.t) ~cwd =
  let class_ base = Attr.classes (base :: Option.to_list cls) in
  match s.repo with
  | Some r ->
    Node.span
      ~attrs:
        [ class_ "session-place"; Attr.title (home_relative ?host_home cwd) ]
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
      [ Node.text (home_relative ?host_home cwd) ]
;;

(* What a row shows besides its session: rebuilt only when one changes
   (the list is drawn again at every event otherwise). *)
module Row = struct
  type t =
    { selected : bool
    ; status : string option (** working…, thinking…: else its age *)
    ; selecting : bool
    ; marked : bool
    ; loading : bool
    ; age : string
    ; project : string
    ; matched : (int * int) list (** in the title, by the search *)
    ; snippet : Session_hits.Snippet.t option (** of its messages, likewise *)
    }
  [@@deriving equal]
end

let rows : (Session_summary.t, Row.t) View_cache.t = View_cache.create ()

(* On a phone, behind the row: what a swipe to the left shows (the same as
   its menu's Archive and Delete). Out of the tab order: the menu has them. *)
let swipe_actions ~inject (s : Session_summary.t) =
  let action ~cls ~label ~icon:glyph on_click =
    Node.button
      ~attrs:
        [ Attr.class_ ("session-swipe-action " ^ cls)
        ; Attr.type_ "button"
        ; Attr.tabindex (-1)
        ; Attr.on_click (fun _ ->
            Effect.Many [ Effect.Stop_propagation; inject on_click ])
        ]
      [ icon glyph; Node.span [ Node.text label ] ]
  in
  Node.span
    ~attrs:[ Attr.class_ "session-swipe"; Attr.create "aria-hidden" "true" ]
    [ (if s.archived
       then
         action
           ~cls:"archive"
           ~label:"Unarchive"
           ~icon:Unarchive
           (Action.Set_archived { path = s.path; archived = false })
       else
         action
           ~cls:"archive"
           ~label:"Archive"
           ~icon:Archive
           (Action.Set_archived { path = s.path; archived = true }))
    ; action
        ~cls:"delete"
        ~label:"Delete"
        ~icon:Trash
        (Action.Ask_delete s.path)
    ]
;;

(* What the search matched, marked. *)
let marked_text ?(before = []) ~cls text ranges =
  Node.span
    ~attrs:[ Attr.class_ cls; Attr.create "dir" "auto" ]
    (before
     @ List.map (Prigh_ui.Session_search.pieces text ranges) ~f:(function
       | `Plain, s -> Node.text s
       | `Match, s ->
         Node.create
           "mark"
           ~attrs:[ Attr.class_ "rounded-sm bg-primary/25 text-foreground" ]
           [ Node.text s ]))
;;

(* The row's ⋯: its menu, under the button. *)
let more_button ~inject ~id ~label target =
  Button.view
    ~variant:Ghost
    ~size:Icon_xs
    ~class_:
      "more text-muted-foreground hover:bg-transparent hover:text-foreground \
       narrow:size-11"
    ~attrs:
      [ Attr.id id
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

let row ~inject (s : Session_summary.t) (row : Row.t) =
  let { Row.selected
      ; status
      ; selecting
      ; marked
      ; loading
      ; age
      ; project
      ; matched
      ; snippet
      }
    =
    row
  in
  let title = Session_list.title s in
  let dom_id = Session_list.dom_id s.id in
  let target = Context_menu.Target.Session s.path in
  let right =
    if selecting
    then
      span
        ~cls:"session-size shrink-0 tabular-nums"
        (Session_summary.size_string s.bytes)
    else if loading
    then span ~cls:"session-status shrink-0" "opening…"
    else (
      match status with
      | Some word ->
        span ~cls:"session-status working shrink-0 text-sidebar-foreground" word
      | None -> span ~cls:"session-age shrink-0 tabular-nums" age)
  in
  Node.div
    ~attrs:[ classes [ "session-item" ] [ "selected", selected ] ]
    [ Node.div
        ~attrs:
          [ classes
              [ "session"
              ; "group/session relative rounded-md text-sidebar-foreground \
                 hover:bg-sidebar-accent"
              ]
              [ "selected", selected
              ; "bg-sidebar-accent", selected || loading
              ; "running", Option.is_some status
              ; "opening", loading
              ; "archived", s.archived
              ; "marked", marked
              ; "bg-primary/15", marked
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
              [ Attr.class_
                  "session-open flex w-full min-w-0 items-start gap-2 \
                   rounded-md px-2.5 py-1.5 text-left outline-none \
                   focus-visible:ring-2 focus-visible:ring-sidebar-ring \
                   narrow:py-2 [@media(hover:none)]:pr-12"
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
                  (* Ctrl/Cmd toggles the session into the selection, Shift
                     selects a range (from the session on show when none
                     is selected yet); a middle click opens a new window. *)
                  if selecting || toggle_click ev || shift_click ev
                  then (
                    Js_of_ocaml.Dom.preventDefault ev;
                    inject
                      (Action.Mark_session
                         { path = s.path; range = shift_click ev }))
                  else inject (Action.Switch_session s.path))
              ; (if selecting
                 then Attr.empty
                 else
                   Attr.on_mousedown (fun ev ->
                     if ev##.button = 1
                     then (
                       Js_of_ocaml.Dom.preventDefault ev;
                       inject (Action.Open_session_window s.id))
                     else Effect.Ignore))
              ; Attr.on_pointerdown (fun _ ->
                  inject (Action.Prefetch_session s.path))
              ; Attr.on_focus (fun _ -> inject (Action.Prefetch_session s.path))
              ]
            [ (if selecting then check marked else Node.none)
            ; div
                ~cls:"session-main flex min-w-0 flex-1 flex-col gap-0.5"
                ([ div
                     ~cls:
                       "session-meta flex min-w-0 items-center gap-2 text-xs \
                        text-muted-foreground"
                     [ span ~cls:"session-project min-w-0 truncate" project
                     ; div ~cls:"ml-auto" []
                     ; right
                     ]
                 ; marked_text
                     ~cls:
                       (if s.archived
                        then
                          "session-title truncate text-[13px] \
                           text-muted-foreground"
                        else "session-title truncate text-[13px]")
                     ~before:
                       (if Session_list.is_pinned s
                        then
                          [ Node.span
                              ~attrs:
                                [ Attr.class_
                                    "session-pin mr-1 inline-flex align-[-1px] \
                                     text-primary"
                                ; Attr.role "img"
                                ; Attr.create "aria-label" "Pinned"
                                ; Attr.title "Pinned to the top (P unpins)"
                                ]
                              [ icon ~cls:"size-3" Pin ]
                          ]
                        else [])
                     title
                     matched
                 ]
                 @ Option.to_list
                     (Option.map snippet ~f:(fun s ->
                        marked_text
                          ~cls:
                            "session-snippet line-clamp-2 text-xs \
                             text-muted-foreground"
                          s.text
                          s.ranges)))
            ]
        ; (if selecting
           then Node.none
           else
             Node.span
               ~attrs:
                 [ Attr.class_
                     "session-actions absolute right-1 bottom-1 rounded-md \
                      bg-sidebar-accent opacity-0 \
                      group-hover/session:opacity-100 \
                      group-focus-within/session:opacity-100 \
                      [@media(hover:none)]:top-1/2 \
                      [@media(hover:none)]:bottom-auto \
                      [@media(hover:none)]:-translate-y-1/2 \
                      [@media(hover:none)]:bg-transparent \
                      [@media(hover:none)]:opacity-70"
                 ]
               [ more_button ~inject ~id:("more-" ^ dom_id) ~label:title target
               ])
        ; (if selecting then Node.none else swipe_actions ~inject s)
        ]
    ]
;;

let session
      (m : App.Model.t)
      ~inject
      ~current
      (found : Prigh_ui.Session_search.Result.t)
  =
  let s = found.session in
  let selected = Option.equal String.equal current (Some s.id) in
  let cwd =
    (* The current session's as it is now: [/cd] moves it. *)
    match m.state with
    | Some state when selected -> state.cwd
    | _ -> s.cwd
  in
  let status =
    (* Only the current session says what its agent is doing; another
       works (its turn, or only its subagents and jobs) or not. *)
    match selected, current_activity m with
    | true, Some word -> Some word
    | _ ->
      if (s.running || s.background > 0) && not s.archived
      then Some "working…"
      else None
  in
  let deps =
    { Row.selected
    ; status
    ; selecting = m.marks.selecting
    ; marked = Set.mem m.marks.marked s.path
    ; loading =
        (match m.pending.session with
         | Some (Switching path) -> String.equal path s.path
         | Some Starting | None -> false)
    ; age = age m s
    ; project = Session_project.name ?host_home:(session_host_home m s) s ~cwd
    ; matched = found.title
    ; snippet = found.snippet
    }
  in
  View_cache.find ~inject rows s ~deps ~equal:Row.equal ~f:(fun () ->
    row ~inject s deps)
;;

let heading_cls =
  "flex min-h-7 items-center gap-2 px-2.5 pt-2 text-xs font-medium \
   text-muted-foreground"
;;

(* Above the list: its name and how to select several sessions, or how
   selecting works. *)
let toolbar (m : App.Model.t) ~inject =
  if m.marks.selecting
  then
    div
      ~cls:("sessions-toolbar selecting " ^ heading_cls)
      [ span
          ~cls:"sessions-label flex-1 text-sidebar-foreground"
          "Select sessions"
      ; Node.span
          ~attrs:[ Attr.class_ "sessions-hint keyboard font-normal" ]
          [ Kbd.view [ Node.text "Shift" ]; Node.text " for a range" ]
      ; Node.span
          ~attrs:[ Attr.class_ "sessions-count tabular-nums" ]
          [ Node.text
              (sprintf
                 "%d selected"
                 (List.length (Session_marks.marked m.marks m.sessions)))
          ]
      ]
  else
    div
      ~cls:("sessions-toolbar " ^ heading_cls)
      [ span ~cls:"sessions-label flex-1" "Sessions"
      ; Button.view
          ~variant:Ghost
          ~size:Xs
          ~class_:
            "h-6 px-1.5 font-normal text-muted-foreground \
             hover:text-foreground narrow:h-11"
          ~attrs:
            [ Attr.title
                "Select sessions to archive or delete (or Ctrl/⌘- or \
                 Shift-click them)"
            ]
          ~on_click:(inject (Action.Select_sessions true))
          [ Node.text "Select" ]
      ]
;;

(* Selecting: what deleting would remove, at the foot of the list. *)
let selection_bar (m : App.Model.t) ~inject =
  let marked = Session_marks.marked m.marks m.sessions in
  let count = List.length marked in
  div
    ~cls:
      "selection-bar flex items-center gap-2 border-t border-sidebar-border p-2"
    [ Button.view
        ~variant:Ghost
        ~size:Sm
        ~class_:"narrow:h-11"
        ~attrs:[ Attr.title "Stop selecting (Esc)" ]
        ~on_click:(inject (Action.Select_sessions false))
        [ Node.text "Cancel" ]
    ; (let unarchive =
         count > 0 && List.for_all marked ~f:(fun s -> s.archived)
       in
       Button.view
         ~variant:Outline
         ~size:Sm
         ~class_:"archive-marked narrow:h-11"
         ~disabled:(count = 0)
         ~attrs:
           [ Attr.title
               (if unarchive
                then "Unarchive the selected sessions"
                else "Archive the selected sessions")
           ]
         ~on_click:(inject (Action.Archive_marked (not unarchive)))
         [ icon (if unarchive then Unarchive else Archive)
         ; Node.text (if unarchive then "Unarchive" else "Archive")
         ])
    ; Button.view
        ~variant:Destructive
        ~size:Sm
        ~class_:"danger min-w-0 flex-1 truncate narrow:h-11"
        ~disabled:(count = 0)
        ~attrs:
          [ Attr.title
              (sprintf
                 "Delete the selected sessions, %s (Delete)"
                 (Session_summary.size_string
                    (Session_summary.total_bytes marked)))
          ]
        ~on_click:(inject Action.Delete_marked)
        [ Node.text
            (if count = 0 then "Delete" else sprintf "Delete %d" count)
        ]
    ]
;;

(* A quiet heading naming a group of sessions, with its count. *)
let group_heading label sessions =
  Node.div
    ~attrs:
      [ Attr.class_ ("sessions-divider sessions-group " ^ heading_cls)
      ; Attr.role "heading"
      ; Attr.create "aria-level" "3"
      ]
    [ span ~cls:"divider-label" label
    ; span
        ~cls:"divider-count ml-auto tabular-nums"
        (Int.to_string (List.length sessions))
    ]
;;

let delete_archived ~inject archived =
  Button.view
    ~variant:Ghost
    ~size:Xs
    ~class_:
      "delete-archived mx-1 justify-start font-normal text-muted-foreground \
       hover:text-destructive narrow:h-11"
    ~attrs:
      [ Attr.title
          (sprintf
             "Delete every archived session, %s (/delete-archived)"
             (Session_summary.size_string
                (Session_summary.total_bytes archived)))
      ]
    ~on_click:(inject Action.Ask_delete_archived)
    [ Node.text "Delete archived" ]
;;

let foot_button ?(attrs = []) ~cls ~title ~on_click glyph =
  Button.view
    ~variant:Ghost
    ~size:Icon_sm
    ~class_:(cls ^ " text-muted-foreground hover:text-foreground narrow:size-11")
    ~attrs:
      ([ Attr.title title
       ; Attr.create
           "aria-label"
           (String.strip (List.hd_exn (String.split title ~on:'(')))
       ]
       @ attrs)
    ~on_click
    [ icon glyph ]
;;

let settings ~inject =
  foot_button
    ~cls:"settings-toggle"
    ~title:"Settings (Ctrl+, or /settings)"
    ~attrs:[ Attr.create "aria-haspopup" "dialog" ]
    ~on_click:(inject Action.Open_settings)
    Gear
;;

(* Folds the archived sessions in or out of the list, with their count. *)
let archive_toggle ~inject ~shown archived =
  Button.view
    ~variant:Ghost
    ~size:Sm
    ~class_:
      "sessions-divider archive-toggle w-full justify-start gap-2 px-2.5 \
       font-normal text-muted-foreground hover:text-foreground narrow:h-11"
    ~attrs:
      [ Attr.id "archive-toggle"
      ; Attr.create "aria-expanded" (Bool.to_string shown)
      ; Attr.title
          (if shown
           then "Hide the archived sessions (right-click: more)"
           else "Show the archived sessions (right-click: more)")
      ; Attr.on_contextmenu
          (open_menu ~inject Context_menu.Target.Archive ~row:"archive-toggle")
      ]
    ~on_click:(inject Action.Toggle_archived)
    [ icon Archive
    ; span ~cls:"divider-label" "Archive"
    ; span
        ~cls:"divider-count ml-auto text-xs tabular-nums"
        (Int.to_string (List.length archived))
    ; icon
        ~cls:
          (if shown
           then "chev size-3.5 rotate-90 transition-transform"
           else "chev size-3.5 transition-transform")
        Next
    ]
;;

let account (m : App.Model.t) ~inject =
  Option.map (Account_view.who m) ~f:(fun { name; acting_as; _ } ->
    Node.button
      ~attrs:
        [ Attr.class_
            "account-button ml-auto flex min-w-0 items-center gap-2 \
             rounded-full p-0.5 text-xs text-muted-foreground outline-none \
             hover:text-foreground focus-visible:ring-2 \
             focus-visible:ring-sidebar-ring"
        ; Attr.type_ "button"
        ; Attr.title (name ^ ": switch, add, act as, sign out")
        ; Attr.create "aria-label" ("Accounts: " ^ name)
        ; Attr.create "aria-haspopup" "dialog"
        ; Attr.on_click (fun _ -> inject Action.Open_accounts)
        ]
      [ (match acting_as with
         | Some ns ->
           span ~cls:"user-acting truncate text-primary" ("acting as " ^ ns)
         | None -> Node.none)
      ; span
          ~cls:
            "avatar flex size-7 shrink-0 items-center justify-center \
             rounded-full bg-sidebar-accent text-[11px] font-medium \
             text-sidebar-foreground narrow:size-11"
          (Account_view.initial name)
      ])
;;

let footer (m : App.Model.t) ~inject ~archived =
  let sign_out =
    foot_button
      ~cls:"sign-out"
      ~title:"Sign out"
      ~on_click:(inject Action.Sign_out)
      Logout
  in
  div
    ~cls:"sidebar-footer flex flex-col gap-1 px-2 pb-2"
    [ (match archived with
       | [] -> Node.none
       | archived -> archive_toggle ~inject ~shown:m.archived_open archived)
    ; div
        ~cls:"flex items-center gap-0.5"
        [ settings ~inject
        ; foot_button
            ~cls:"usage-button"
            ~title:"Usage: your plans' limits and what they reset to (/usage)"
            ~attrs:[ Attr.create "aria-haspopup" "dialog" ]
            ~on_click:(inject (Action.Run "/usage"))
            Usage
        ; foot_button
            ~cls:"help-button"
            ~title:"Commands and keys (/help)"
            ~on_click:(inject Action.Open_help)
            Help
        ; (match account m ~inject with
           | Some account -> account
           | None ->
             if m.saved_login
             then div ~cls:"ml-auto" [ sign_out ]
             else Node.none)
        ]
    ]
;;

let view (m : App.Model.t) ~inject =
  let current = Option.map m.state ~f:(fun s -> s.session_id) in
  let { Session_list.Sections.pinned; active; archived } =
    Session_list.sections m.sessions
  in
  let session = session m ~inject ~current in
  let new_session =
    let starting =
      match m.pending.session with
      | Some Starting -> true
      | Some (Switching _) | None -> false
    in
    Button.view
      ~variant:Ghost
      ~size:Icon_sm
      ~class_:
        (if starting
         then "new-session opening text-muted-foreground narrow:size-11"
         else
           "new-session text-muted-foreground hover:text-foreground \
            narrow:size-11")
      ~disabled:starting
      ~attrs:
        ([ Attr.title
             (if starting
              then "Starting a session…"
              else "Start a new session (/new)")
         ; Attr.create "aria-label" "New session"
         ]
         @ if starting then [ Attr.create "aria-busy" "true" ] else [])
      ~on_click:(inject Action.New_session)
      [ icon Square_pen ]
  in
  Node.aside
    ~attrs:
      [ Attr.class_ "sidebar loon bg-sidebar text-sm text-sidebar-foreground"
      ; Attr.id "sidebar"
      ]
    [ div
        ~cls:
          "sidebar-head flex h-12 shrink-0 items-center gap-1 px-2 narrow:h-14"
        [ Button.view
            ~variant:Ghost
            ~size:Icon_sm
            ~class_:"text-muted-foreground hover:text-foreground narrow:size-11"
            ~attrs:
              [ Attr.title "Hide sidebar (Ctrl+B)"
              ; Attr.create "aria-label" "Hide sidebar"
              ]
            ~on_click:(inject Action.Toggle_sidebar)
            [ icon Sidebar ]
        ; span ~cls:"brand logo px-1 font-semibold tracking-tight" "prigh"
        ]
    ; div
        ~cls:"sidebar-actions flex shrink-0 items-center gap-0.5 px-2"
        [ Button.view
            ~variant:Ghost
            ~size:Sm
            ~class_:
              "control-open min-w-0 flex-1 justify-start gap-2 px-2 \
               font-normal text-muted-foreground hover:text-foreground \
               narrow:h-11"
            ~attrs:
              [ Attr.id "control-open"
              ; Attr.title "Search sessions, settings and commands (Ctrl+K)"
              ; Attr.create "aria-label" "Search"
              ]
            ~on_click:(inject Action.Open_control)
            [ icon Search
            ; Node.text "Search"
            ; Kbd.view ~class_:"ml-auto narrow:hidden" [ Node.text "Ctrl K" ]
            ]
        ; new_session
        ]
    ; (match m.sessions with
       | [] -> Node.none
       | _ -> toolbar m ~inject)
    ; div
        ~cls:
          "sessions flex min-h-0 flex-1 flex-col gap-0.5 overflow-y-auto px-2 \
           pb-2"
        (match m.sessions with
         | [] ->
           [ div
               ~cls:"sessions-empty px-2.5 py-3 text-xs text-muted-foreground"
               [ Node.text "No sessions yet: send a message to start one." ]
           ]
         | _ ->
           (match pinned with
            | [] -> List.map active ~f:session
            | pinned ->
              (group_heading "Pinned" pinned :: List.map pinned ~f:session)
              @
                (match active with
                | [] -> []
                | active ->
                  group_heading "Recent" active :: List.map active ~f:session))
           @
             (match archived with
             | [] -> []
             | archived ->
               if m.archived_open
               then
                 (group_heading "Archived" archived
                  :: List.map archived ~f:session)
                 @
                 if m.marks.selecting
                 then []
                 else
                   [ delete_archived
                       ~inject
                       (List.map archived ~f:(fun r -> r.session))
                   ]
               else []))
    ; (if m.marks.selecting then selection_bar m ~inject else Node.none)
    ; footer m ~inject ~archived
    ]
;;
