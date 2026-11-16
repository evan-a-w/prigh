open! Core
module H = Harness

(* A session's worktrees stay under it in the sidebar when the user goes
   into another session, in another repository, and back. *)

let dir = "/home/u/.prigh/worktrees/work"
let login_path = dir ^ "/feat-login"
let agent_path = dir ^ "/agent-tidy"

let worktree ?(main = false) ?(changes = 0) path branch =
  sprintf
    {|{"path":"%s","branch":"%s","head":"0123456789abcdef","main":%b,"managed":%b,"locked":false,"prunable":false,"changes":%d,"upstream":null,"ahead":0,"behind":0,"current":false,"sessions":[]}|}
    path
    branch
    main
    (not main)
    changes
;;

let listing ~root worktrees =
  sprintf
    {|{"root":"%s","project_dir":"%s","worktrees":[%s]}|}
    root
    dir
    (String.concat ~sep:"," (worktree root "main" ~main:true :: worktrees))
;;

let work_listing ?(agent = false) () =
  listing
    ~root:"/work"
    (worktree login_path "feat/login" ~changes:2
     :: (if agent then [ worktree agent_path "agent/tidy" ] else []))
;;

let site_listing = listing ~root:"/site" []

let sessions ?(agent = false) () =
  sprintf
    "[%s,%s]"
    (H.session_json
       ~name:"Main work"
       ~cwd:"/work"
       ~repo:("/work", "/work", Some "main")
       ~pinned:true
       ~worktrees:
         ((login_path, Some "feat/login", "/work", "created")
          :: (if agent
              then [ agent_path, Some "agent/tidy", "/work", "agent:a1:Tidy" ]
              else []))
       "s1")
    (H.session_json
       ~name:"Site"
       ~cwd:"/site"
       ~repo:("/site", "/site", Some "main")
       "s2")
;;

let state ~session ~cwd =
  H.state_json ~fields:[ "session_id", `String session; "cwd", `String cwd ] ()
;;

let switch h ~session ~cwd =
  H.quiet h (fun () ->
    H.event
      h
      (sprintf {|{"event":"state","state":%s}|} (state ~session ~cwd)))
;;

let sidebar h = H.text h ~selector:".sessions"

let%expect_test "a session's worktrees stay when the user leaves it" =
  let h = H.create ~sessions:(sessions ()) ~state:(state ~session:"s2" ~cwd:"/site") () in
  (* In the site: its own listing, and the work repository's for s1's row. *)
  H.reply h "list_worktrees" site_listing;
  H.reply h "list_worktrees" (work_listing ());
  sidebar h;
  (* Into the work session, where a subagent makes a worktree. *)
  switch h ~session:"s1" ~cwd:"/work";
  H.reply h "list_worktrees" (work_listing ());
  H.act h (Event (Sessions_changed None));
  H.reply h "list_sessions" (sessions ~agent:true ());
  sidebar h;
  (* Back to the site: the work session's worktrees are still under it. *)
  H.event h (sprintf {|{"event":"state","state":%s}|} (state ~session:"s2" ~cwd:"/site"));
  sidebar h;
  [%expect {| |}]
;;
