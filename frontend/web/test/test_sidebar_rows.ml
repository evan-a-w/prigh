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
    button "Archive 2" -- Show the archived sessions
    button "Settings" -- Settings (Ctrl+, or /settings)
    button "Pull requests" -- Pull requests (/pr)
    button "Commands and keys" -- Commands and keys (/help)
    Toggle_archived
    button "Archive 2" -- Hide the archived sessions
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
    button "Session card" -- Subagents and worktrees
    Toggle_session_card
    button "Session card" -- Subagents and worktrees
    false
    Toggle_session_card
    true
    |}]
;;
