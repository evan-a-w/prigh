open! Core
open Prigh_web
module H = Harness

(* The sidebar's rows: each session's project in a word over its title, and
   at the right what its agent is doing, else how long ago it worked; its
   foot; the top bar over the chat. *)

let list sessions = sprintf "[%s]" (String.concat ~sep:"," sessions)
let rows h = H.text h ~selector:".sessions .session"

let%expect_test "a session's project: its repository, else its directory" =
  let prigh = "/home/u/src/prigh" in
  let h =
    H.create
      ~verbose:false
      ~sessions:
        (list
           [ H.session_json ~repo:(prigh, prigh, Some "main") ~cwd:prigh "s1"
           ; H.session_json
               ~repo:
                 (prigh, "/home/u/.prigh/worktrees/prigh/feat-x", Some "feat/x")
               ~cwd:"/home/u/.prigh/worktrees/prigh/feat-x/lib"
               "s2"
           ; H.session_json
               ~repo:("/srv/loon.git", "/srv/loon.git", None)
               ~cwd:"/srv/loon.git"
               "s3"
           ; H.session_json ~cwd:"/home/u/notes/" "s4"
           ; H.session_json ~cwd:"/home/u" "s5"
           ; H.session_json ~cwd:"/" "s6"
           ])
      ()
  in
  List.iter (H.model h).sessions ~f:(fun s ->
    printf
      "%s -> %s\n"
      s.cwd
      (Session_project.name ~host_home:"/home/u" s ~cwd:s.cwd));
  [%expect
    {|
    /home/u/src/prigh -> prigh
    /home/u/.prigh/worktrees/prigh/feat-x/lib -> prigh
    /srv/loon.git -> loon
    /home/u/notes/ -> notes
    /home/u -> ~
    / -> /
    |}]
;;

let%expect_test "at the right: working, else how long ago" =
  let h =
    H.create
      ~verbose:false
      ~sessions:
        (list
           [ H.session_json ~name:"Here" ~cwd:"/work" "s1"
           ; H.session_json ~name:"Its turn" ~cwd:"/work" ~running:true "s2"
           ; H.session_json
               ~name:"Its subagents"
               ~cwd:"/work"
               ~background:2
               "s3"
           ; H.session_json
               ~name:"Old"
               ~cwd:"/site"
               ~updated_at:"2026-10-01 10:00:00Z"
               "s4"
           ; H.session_json
               ~name:"Archived, running"
               ~cwd:"/site"
               ~archived:true
               ~running:true
               "s5"
           ])
      ()
  in
  rows h;
  [%expect
    {|
    (work 2m Here) (More (right-click, Shift+F10)) (Archive) (Delete)
    (work working… Its turn) (More (right-click, Shift+F10)) (Archive) (Delete)
    (work working… Its subagents) (More (right-click, Shift+F10)) (Archive) (Delete)
    (site 4d Old) (More (right-click, Shift+F10)) (Archive) (Delete)
    |}];
  H.quiet h (fun () -> H.act h Toggle_archived);
  rows h;
  [%expect
    {|
    (work 2m Here) (More (right-click, Shift+F10)) (Archive) (Delete)
    (work working… Its turn) (More (right-click, Shift+F10)) (Archive) (Delete)
    (work working… Its subagents) (More (right-click, Shift+F10)) (Archive) (Delete)
    (site 4d Old) (More (right-click, Shift+F10)) (Archive) (Delete)
    (site 2m Archived, running) (More (right-click, Shift+F10)) (Unarchive) (Delete)
    |}]
;;

let assistant content =
  sprintf
    {|{"role":"assistant","content":[%s],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"claude-opus-5-5"}|}
    (String.concat ~sep:"," content)
;;

let event fields = sprintf {|{"type":"event",%s}|} fields

let update delta content =
  event
    (sprintf
       {|"event":"message_update","partial":%s,"delta":%s|}
       (assistant content)
       delta)
;;

let%expect_test "the current session: thinking, working" =
  let h =
    H.create
      ~verbose:false
      ~sessions:(list [ H.session_json ~name:"Here" ~cwd:"/work" "s1" ])
      ()
  in
  let show () = H.text h ~selector:".session.selected .session-status" in
  show ();
  H.event
    h
    (event
       (sprintf
          {|"event":"state","state":%s|}
          (H.state_json ~fields:[ "running", `True ] ())));
  H.event h (event {|"event":"agent_start"|});
  H.event h (event {|"event":"turn_start"|});
  H.event
    h
    (event (sprintf {|"event":"message_start","message":%s|} (assistant [])));
  show ();
  H.event
    h
    (update
       {|{"type":"thinking_delta","text":"Hmm"}|}
       [ {|{"type":"thinking","text":"Hmm"}|} ]);
  show ();
  H.event
    h
    (update
       {|{"type":"text_delta","text":"So"}|}
       [ {|{"type":"thinking","text":"Hmm"}|}; {|{"type":"text","text":"So"}|} ]);
  show ();
  [%expect
    {|
    thinking…
    thinking…
    working…
    |}]
;;

let%expect_test "the foot: the archived sessions' count folds them in" =
  let h =
    H.create
      ~sessions:
        (list
           [ H.session_json ~name:"Here" ~cwd:"/work" "s1"
           ; H.session_json ~name:"Gone" ~cwd:"/work" ~archived:true "s2"
           ; H.session_json ~name:"Older" ~cwd:"/work" ~archived:true "s3"
           ])
      ()
  in
  H.names h ~selector:".sidebar-footer button";
  H.click h ~selector:".archive-toggle";
  H.names h ~selector:".archive-toggle";
  rows h;
  [%expect
    {|
    button "Archive 2" -- Show the archived sessions (right-click: more)
    button "Settings" -- Settings (Ctrl+, or /settings)
    button "Usage and limits" -- Usage and limits (/usage)
    button "Commands and keys" -- Commands and keys (/help)
    Toggle_archived
    button "Archive 2" -- Hide the archived sessions (right-click: more)
    (work 2m Here) (More (right-click, Shift+F10)) (Archive) (Delete)
    (work 2m Gone) (More (right-click, Shift+F10)) (Unarchive) (Delete)
    (work 2m Older) (More (right-click, Shift+F10)) (Unarchive) (Delete)
    |}]
;;

let%expect_test "the top bar: project / name, the toggles, the card's" =
  let h =
    H.create
      ~sessions:
        (list
           [ H.session_json
               ~name:"Rebuild prigh-web"
               ~repo:("/home/u/src/prigh", "/home/u/src/prigh", Some "main")
               ~cwd:"/home/u/src/prigh"
               ~worktrees:
                 [ ( "/home/u/.prigh/worktrees/prigh/feat-ui"
                   , Some "feat/ui"
                   , "/home/u/src/prigh"
                   , "created" )
                 ]
               "s1"
           ])
      ~state:
        (H.state_json
           ~fields:
             [ "session_id", `String "s1"
             ; "cwd", `String "/home/u/src/prigh"
             ; "session_name", `String "Rebuild prigh-web"
             ]
           ())
      ()
  in
  H.text h ~selector:".session-crumbs";
  H.names h ~selector:".chat-chrome .panel-toggles button";
  H.click h ~selector:".session-card-toggle";
  H.names h ~selector:".session-card-toggle[aria-pressed=false]";
  print_s [%sexp ((H.model h).session_card_open : bool)];
  H.click h ~selector:".session-card-toggle";
  print_s [%sexp ((H.model h).session_card_open : bool)];
  [%expect
    {|
    prigh
    /
    Rebuild prigh-web
    button "Filter the transcript" -- Filter the transcript (/filter)
    button "Files" -- Files (Ctrl+Shift+F)
    button "Terminal" -- Terminal (Ctrl+`)
    button "Review changes" -- Review changes (Alt+R)
    button "Subagents and worktrees"
    Toggle_session_card
    button "Subagents and worktrees"
    false
    Toggle_session_card
    true
    |}]
;;

let%expect_test "the Archive row's menu: show or hide, delete them all" =
  let h =
    H.create
      ~sessions:
        (list
           [ H.session_json ~name:"Here" ~cwd:"/work" "s1"
           ; H.session_json ~name:"Gone" ~cwd:"/work" ~archived:true "s2"
           ; H.session_json ~name:"Older" ~cwd:"/work" ~archived:true "s3"
           ])
      ()
  in
  H.right_click h ~at:(40, 600) ~selector:".archive-toggle";
  H.text h ~selector:".context-menu-head";
  H.names h ~selector:"[role=menuitem]";
  H.click h ~selector:"[data-item=show_archived]";
  rows h;
  H.right_click h ~selector:".archive-toggle";
  H.names h ~selector:"[role=menuitem]";
  print_endline "-- Delete all archived:";
  H.click h ~selector:"[data-item=delete_archived]";
  H.text h ~selector:".modal";
  print_endline "-- Esc:";
  H.key h "Escape";
  H.elements h ~selector:".modal";
  [%expect
    {|
    (Open_menu ((target Archive) (anchor (Point (x 40) (y 600)))))
    (Focus dialog)
    (Place_menu (Point (x 40) (y 600)))
    Archived sessions 2 sessions
    [menuitem] div "Show archived"
    [menuitem] div "Delete all 2 archived sessions\226\128\166 /delete-archived"
    (Picker_choose show_archived)
    (Focus editor)
    (work 2m Here) (More (right-click, Shift+F10)) (Archive) (Delete)
    (work 2m Gone) (More (right-click, Shift+F10)) (Unarchive) (Delete)
    (work 2m Older) (More (right-click, Shift+F10)) (Unarchive) (Delete)
    (Open_menu ((target Archive) (anchor (Element archive-toggle))))
    (Focus dialog)
    (Place_menu (Element archive-toggle))
    [menuitem] div "Hide archived"
    [menuitem] div "Delete all 2 archived sessions\226\128\166 /delete-archived"
    -- Delete all archived:
    (Picker_choose delete_archived)
    (Focus dialog)
    Delete all 2 archived sessions?
    (Close (Esc))
    They will be deleted, with their subagent logs: 0 B on disk. This can't be undone.
    Gone 0 B
    Older 0 B
    (Cancel) (Delete 2)
    -- Esc:
    Close_dialog
    (Focus editor)
    |}]
;;

let%expect_test "Ctrl or Cmd, and Shift, click sessions into a selection" =
  let h =
    H.create
      ~sessions:
        (list
           [ H.session_json ~name:"One" ~cwd:"/work" "s1"
           ; H.session_json ~name:"Two" ~cwd:"/work" "s2"
           ; H.session_json ~name:"Three" ~cwd:"/work" "s3"
           ; H.session_json ~name:"Four" ~cwd:"/work" ~archived:true "s4"
           ])
      ()
  in
  let marked () =
    H.text h ~selector:".sessions-toolbar";
    H.text h ~selector:".session[aria-checked=true] .session-title";
    H.names h ~selector:".selection-bar button"
  in
  print_endline "-- Ctrl+click the third:";
  H.modified_click h ~ctrl:true ~selector:".session-open" ~nth:2;
  marked ();
  print_endline "-- Cmd+click the second:";
  H.modified_click h ~meta:true ~selector:".session-open" ~nth:1;
  marked ();
  print_endline "-- Ctrl+click the second again:";
  H.modified_click h ~ctrl:true ~selector:".session-open" ~nth:1;
  marked ();
  print_endline "-- Archive:";
  H.click h ~selector:".archive-marked";
  H.elements h ~selector:".selection-bar";
  print_endline "-- Shift+click from the session on show:";
  H.modified_click h ~shift:true ~selector:".session-open" ~nth:1;
  marked ();
  print_endline "-- Esc:";
  H.key h "Escape";
  H.elements h ~selector:".selection-bar";
  [%expect
    {|
    -- Ctrl+click the third:
    (Mark_session (path /sessions/s3.jsonl) (range false))
    Select sessions Shift for a range 1 selected
    button "Cancel" -- Stop selecting (Esc)
    button "Archive" -- Archive the selected sessions
    button "Delete 1" -- Delete the selected sessions, 0 B (Delete)
    -- Cmd+click the second:
    (Mark_session (path /sessions/s2.jsonl) (range false))
    Select sessions Shift for a range 2 selected
    button "Cancel" -- Stop selecting (Esc)
    button "Archive" -- Archive the selected sessions
    button "Delete 2" -- Delete the selected sessions, 0 B (Delete)
    -- Ctrl+click the second again:
    (Mark_session (path /sessions/s2.jsonl) (range false))
    Select sessions Shift for a range 1 selected
    button "Cancel" -- Stop selecting (Esc)
    button "Archive" -- Archive the selected sessions
    button "Delete 1" -- Delete the selected sessions, 0 B (Delete)
    -- Archive:
    (Archive_marked true)
    (Rpc (method_ set_archived)
     (params ((path /sessions/s3.jsonl) (archived true)))
     (tag (Archived_set (title Three) (archived true))))
    -- Shift+click from the session on show:
    (Mark_session (path /sessions/s2.jsonl) (range true))
    Select sessions Shift for a range 2 selected
    button "Cancel" -- Stop selecting (Esc)
    button "Archive" -- Archive the selected sessions
    button "Delete 2" -- Delete the selected sessions, 0 B (Delete)
    -- Esc:
    (Select_sessions false)
    |}]
;;
