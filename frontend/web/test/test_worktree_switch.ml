open! Core
module H = Harness

(* A session's worktrees stay under it in the sidebar when the user goes
   into another session, in another repository, and back: the repositories'
   listings belong to the user, not to the session they are in. *)

let dir = "/home/u/.prigh/worktrees/work"
let login_path = dir ^ "/feat-login"
let agent_path = dir ^ "/agent-tidy"
let docs_path = dir ^ "/docs"

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

let work_listing ?(agent = false) ?(changes = 2) () =
  listing
    ~root:"/work"
    ([ worktree login_path "feat/login" ~changes; worktree docs_path "docs" ]
     @ if agent then [ worktree agent_path "agent/tidy" ~changes:1 ] else [])
;;

let site_listing = listing ~root:"/site" []

let site_session =
  H.session_json
    ~name:"Site"
    ~cwd:"/site"
    ~repo:("/site", "/site", Some "main")
    "s2"
;;

(* The work session, pinned, and the site's; then [more]. *)
let sessions ?(agent = false) ?(more = []) () =
  sprintf
    "[%s]"
    (String.concat
       ~sep:","
       ([ H.session_json
            ~name:"Main work"
            ~cwd:"/work"
            ~repo:("/work", "/work", Some "main")
            ~pinned:true
            ~worktrees:
              ((login_path, Some "feat/login", "/work", "created")
               ::
               (if agent
                then [ agent_path, Some "agent/tidy", "/work", "agent:a1:Tidy" ]
                else []))
            "s1"
        ; site_session
        ]
        @ more))
;;

let state ~session ~cwd =
  H.state_json ~fields:[ "session_id", `String session; "cwd", `String cwd ] ()
;;

let switch ?(verbose = false) h ~session ~cwd =
  let go () =
    H.event h (sprintf {|{"event":"state","state":%s}|} (state ~session ~cwd))
  in
  if verbose then go () else H.quiet h go
;;

(* The sessions, then the worktrees each shows. *)
let sidebar h =
  H.text h ~selector:".sessions";
  Worktree_lines.print h
;;

(* The repositories whose listings are kept, and those being checked. *)
let cache h =
  let t = (H.model h).repo_worktrees in
  print_s
    [%message
      ""
        ~listed:
          (List.map (Prigh_web.Repo_listings.listings t) ~f:(fun l -> l.root)
           : string list)
        ~unsettled:(Prigh_web.Repo_listings.unsettled t : string list)]
;;

(* In the site, its listing and the work repository's (for the work
   session's row) answered; then into the work session, its listing
   answered. *)
let in_work ?(more = []) () =
  let h =
    H.create
      ~sessions:(sessions ~more ())
      ~state:(state ~session:"s2" ~cwd:"/site")
      ()
  in
  H.reply h "list_worktrees" site_listing;
  H.reply h "list_worktrees" (work_listing ());
  switch h ~session:"s1" ~cwd:"/work";
  H.reply h "list_worktrees" (work_listing ());
  h
;;

(* A subagent of the work session makes a worktree. *)
let subagent_worktree ?more h =
  H.act h (Event (Sessions_changed None));
  H.reply h "list_sessions" (sessions ~agent:true ?more ())
;;

let%expect_test "a session's worktrees stay when the user leaves it" =
  let h = in_work () in
  sidebar h;
  [%expect
    {|
    Pinned 1
    (work 2m Main work) (More (right-click, Shift+F10)) (Archive) (Delete)
    Recent 1
    (site 2m Site) (More (right-click, Shift+F10)) (Archive) (Delete)
    (feat/login ● 2)
    |}];
  (* The work session's listing is checked again; meanwhile the new one
     shows, its repository's listing being older than it. *)
  subagent_worktree h;
  sidebar h;
  H.reply h "list_worktrees" (work_listing ~agent:true ());
  sidebar h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nbackend\
                            \n/work") (purpose (Sidebar 3)))))
    Pinned 1
    (work 2m Main work) (More (right-click, Shift+F10)) (Archive) (Delete)
    Recent 1
    (site 2m Site) (More (right-click, Shift+F10)) (Archive) (Delete)
    (feat/login ● 2) (agent/tidy subagent)
    Pinned 1
    (work 2m Main work) (More (right-click, Shift+F10)) (Archive) (Delete)
    Recent 1
    (site 2m Site) (More (right-click, Shift+F10)) (Archive) (Delete)
    (feat/login ● 2) (agent/tidy subagent ● 1)
    |}];
  (* Back to the site: the work session's worktrees stay, with their
     changes, and its repository is not asked again. *)
  switch h ~verbose:true ~session:"s2" ~cwd:"/site";
  sidebar h;
  [%expect
    {|
    (Set_url_session s2)
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before ()))))
    Scroll_to_bottom
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Rpc (method_ get_review) (params ()) (tag (Review Review)))
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s2\
                            \nbackend\
                            \n/site") (purpose (Sidebar 4)))))
    Pinned 1
    (work 2m Main work) (More (right-click, Shift+F10)) (Archive) (Delete)
    Recent 1
    (site 2m Site) (More (right-click, Shift+F10)) (Archive) (Delete)
    (feat/login ● 2) (agent/tidy subagent ● 1)
    |}];
  H.reply h "list_worktrees" site_listing;
  sidebar h;
  [%expect
    {|
    Pinned 1
    (work 2m Main work) (More (right-click, Shift+F10)) (Archive) (Delete)
    Recent 1
    (site 2m Site) (More (right-click, Shift+F10)) (Archive) (Delete)
    (feat/login ● 2) (agent/tidy subagent ● 1)
    |}];
  (* And into the work session again: the same, its listing checked. *)
  switch h ~session:"s1" ~cwd:"/work";
  sidebar h;
  H.reply h "list_worktrees" (work_listing ~agent:true ~changes:5 ());
  sidebar h;
  [%expect
    {|
    Pinned 1
    (work 2m Main work) (More (right-click, Shift+F10)) (Archive) (Delete)
    Recent 1
    (site 2m Site) (More (right-click, Shift+F10)) (Archive) (Delete)
    (feat/login ● 2) (agent/tidy subagent ● 1)
    Pinned 1
    (work 2m Main work) (More (right-click, Shift+F10)) (Archive) (Delete)
    Recent 1
    (site 2m Site) (More (right-click, Shift+F10)) (Archive) (Delete)
    (feat/login ● 5) (agent/tidy subagent ● 1)
    |}]
;;

let%expect_test
    "left before the check came: its repository is asked, the rows stay"
  =
  let h = in_work () in
  subagent_worktree h;
  switch h ~verbose:true ~session:"s2" ~cwd:"/site";
  sidebar h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nbackend\
                            \n/work") (purpose (Sidebar 3)))))
    (Set_url_session s2)
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before ()))))
    Scroll_to_bottom
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Rpc (method_ get_review) (params ()) (tag (Review Review)))
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s2\
                            \nbackend\
                            \n/site") (purpose (Sidebar 4)))))
    (Rpc (method_ list_worktrees) (params ((cwd /work)))
     (tag (Repo_worktrees /work)))
    Pinned 1
    (work 2m Main work) (More (right-click, Shift+F10)) (Archive) (Delete)
    Recent 1
    (site 2m Site) (More (right-click, Shift+F10)) (Archive) (Delete)
    (feat/login ● 2) (agent/tidy subagent)
    |}];
  (* The work session's own check comes too late: ignored. The repository's
     listing settles it. *)
  H.reply h "list_worktrees" (work_listing ());
  H.reply h "list_worktrees" site_listing;
  H.reply h "list_worktrees" (work_listing ~agent:true ());
  sidebar h;
  [%expect
    {|
    Pinned 1
    (work 2m Main work) (More (right-click, Shift+F10)) (Archive) (Delete)
    Recent 1
    (site 2m Site) (More (right-click, Shift+F10)) (Archive) (Delete)
    (feat/login ● 2) (agent/tidy subagent ● 1)
    |}]
;;

let%expect_test "an old session's worktrees are listed too" =
  let old =
    H.session_json
      ~name:"Old work"
      ~cwd:"/work"
      ~repo:("/work", "/work", Some "main")
      ~updated_at:"2026-10-02 10:00:00Z"
      ~worktrees:[ docs_path, Some "docs", "/work", "created" ]
      "s3"
  in
  let h = in_work ~more:[ old ] () in
  subagent_worktree ~more:[ old ] h;
  H.reply h "list_worktrees" (work_listing ~agent:true ());
  switch h ~session:"s2" ~cwd:"/site";
  H.reply h "list_worktrees" site_listing;
  sidebar h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nbackend\
                            \n/work") (purpose (Sidebar 3)))))
    Pinned 1
    (work 2m Main work) (More (right-click, Shift+F10)) (Archive) (Delete)
    Recent 2
    (site 2m Site) (More (right-click, Shift+F10)) (Archive) (Delete)
    (work 3d Old work) (More (right-click, Shift+F10)) (Archive) (Delete)
    (feat/login ● 2) (agent/tidy subagent ● 1)
    (docs)
    |}]
;;

let%expect_test "the cache: only the repositories of sessions listed, per user" =
  let h = in_work () in
  switch h ~session:"s2" ~cwd:"/site";
  H.reply h "list_worktrees" site_listing;
  cache h;
  [%expect {| ((listed (/site /work)) (unsettled ())) |}];
  (* The work session is deleted: its repository goes. *)
  H.act h (Event (Sessions_changed None));
  H.reply h "list_sessions" (sprintf "[%s]" site_session);
  cache h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s2\
                            \nbackend\
                            \n/site") (purpose (Sidebar 4)))))
    ((listed (/site)) (unsettled (/site)))
    |}];
  let h = in_work () in
  H.quiet h (fun () ->
    H.act
      h
      (Reply
         ( User_switched
         , Ok
             (Jsonaf.of_string
                {|{"client_id":"c1","namespace":"bob","user":"alice"}|}) )));
  cache h;
  [%expect {| ((listed ()) (unsettled ())) |}]
;;
