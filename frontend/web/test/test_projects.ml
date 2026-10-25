open! Core
module H = Harness

(* The sidebar's tree: projects (repositories and plain folders), their
   worktrees (a subagent's too) and their sessions. *)

let prigh = "/home/u/src/prigh"
let site = "/home/u/src/site"
let trees = "/home/u/.prigh/worktrees"
let login = trees ^ "/prigh/feat-login"
let agent_wt = trees ^ "/prigh/agent-fix-the-parser"
let redesign = trees ^ "/site/redesign"

let sessions ?(extra = []) () =
  sprintf
    "[%s]"
    (String.concat
       ~sep:","
       ([ H.session_json
            ~name:"Main work"
            ~cwd:prigh
            ~repo:(prigh, prigh, Some "main")
            ~live:true
            ~agent_worktrees:
              [ "2", "Fix the parser", agent_wt, "agent/fix-the-parser", prigh ]
            "s1"
        ; H.session_json
            ~name:"Login form"
            ~cwd:login
            ~repo:(prigh, login, Some "feat/login")
            ~live:true
            ~running:true
            ~updated_at:"2026-10-05 09:40:00Z"
            "s2"
        ; H.session_json
            ~name:"Landing page"
            ~cwd:site
            ~repo:(site, site, Some "trunk")
            ~updated_at:"2026-10-05 08:00:00Z"
            "s3"
        ; H.session_json
            ~name:"Backend tests"
            ~cwd:(prigh ^ "/backend")
            ~repo:(prigh, prigh, Some "main")
            ~updated_at:"2026-10-04 08:00:00Z"
            "s4"
        ; H.session_json
            ~name:"Shopping list"
            ~cwd:"/home/u/notes"
            ~updated_at:"2026-10-03 08:00:00Z"
            "s5"
        ; H.session_json
            ~name:"New hero"
            ~cwd:(redesign ^ "/css")
            ~repo:(site, redesign, Some "redesign")
            ~updated_at:"2026-10-02 08:00:00Z"
            "s6"
        ; H.session_json
            ~name:"Old experiment"
            ~cwd:prigh
            ~repo:(prigh, prigh, Some "main")
            ~archived:true
            ~updated_at:"2026-09-01 08:00:00Z"
            "s7"
        ]
        @ extra))
;;

let state =
  H.state_json
    ~fields:
      [ "session_id", `String "s1"
      ; "cwd", `String prigh
      ; "git_branch", `String "main"
      ]
    ()
;;

let create () = H.create ~state ~sessions:(sessions ()) ()
let tree h = H.text h ~selector:".sessions"
let dialog h = H.text h ~selector:".modal"

let%expect_test "projects, their worktrees and their sessions" =
  let h = create () in
  tree h;
  [%expect
    {|
    (prigh main) (New session in ~/src/prigh) (New worktree: work on a branch of its own, in a new session)
    (Main work 2m ago) (Archive (A)) (Delete session (Delete))
    (Backend tests 1d ago backend/) (Archive (A)) (Delete session (Delete))
    (agent/fix-the-parser subagent) (New session in this worktree) (Remove this worktree (/worktree rm))
    from (Main work)
    (feat/login) (New session in this worktree) (Remove this worktree (/worktree rm))
    (Login form 20m ago) (Archive (A)) (Delete session (Delete))
    (site trunk) (New session in ~/src/site) (New worktree: work on a branch of its own, in a new session)
    (Landing page 2h ago) (Archive (A)) (Delete session (Delete))
    (redesign) (New session in this worktree) (Remove this worktree (/worktree rm))
    (New hero 3d ago css/) (Archive (A)) (Delete session (Delete))
    (notes) (New session in ~/notes)
    (Shopping list 2d ago) (Archive (A)) (Delete session (Delete))
    (Archived 1)
    |}];
  H.names h ~selector:".project-row button";
  H.names h ~selector:".worktree-row button";
  [%expect
    {|
    button "prigh main" -- ~/src/prigh
    button "New session in prigh" -- New session in ~/src/prigh
    button "New worktree of prigh" -- New worktree: work on a branch of its own, in a new session
    button "site trunk" -- ~/src/site
    button "New session in site" -- New session in ~/src/site
    button "New worktree of site" -- New worktree: work on a branch of its own, in a new session
    button "notes" -- ~/notes
    button "New session in notes" -- New session in ~/notes
    button "agent/fix-the-parser subagent" -- ~/.prigh/worktrees/prigh/agent-fix-the-parser
    button "New session in agent/fix-the-parser" -- New session in this worktree
    button "Remove agent/fix-the-parser" -- Remove this worktree (/worktree rm)
    button "feat/login" -- ~/.prigh/worktrees/prigh/feat-login
    button "New session in feat/login" -- New session in this worktree
    button "Remove feat/login" -- Remove this worktree (/worktree rm)
    button "redesign" -- ~/.prigh/worktrees/site/redesign
    button "New session in redesign" -- New session in this worktree
    button "Remove redesign" -- Remove this worktree (/worktree rm)
    |}];
  H.elements h ~selector:".project";
  H.elements h ~selector:".worktree";
  [%expect
    {|
    <section class="git here project">
    <section class="git project">
    <section class="project">
    <div class="agent worktree">
    <div class="worktree">
    <div class="worktree">
    |}]
;;

let%expect_test
    "the session's repository adds its other worktrees and their changes"
  =
  let h = create () in
  H.reply
    h
    "list_worktrees"
    (sprintf
       {|{"root":"%s","project_dir":"%s/prigh","worktrees":[%s]}|}
       prigh
       trees
       (String.concat
          ~sep:","
          [ sprintf
              {|{"path":"%s","branch":"main","head":"abc","main":true,"managed":false,"locked":false,"prunable":false,"changes":0,"upstream":null,"ahead":0,"behind":0,"current":true,"sessions":[]}|}
              prigh
          ; sprintf
              {|{"path":"%s","branch":"feat/login","head":"abc","main":false,"managed":true,"locked":false,"prunable":false,"changes":2,"upstream":"origin/feat/login","ahead":1,"behind":0,"current":false,"sessions":[]}|}
              login
          ; sprintf
              {|{"path":"%s/prigh/docs","branch":"docs","head":"abc","main":false,"managed":true,"locked":false,"prunable":false,"changes":0,"upstream":null,"ahead":0,"behind":0,"current":false,"sessions":[]}|}
              trees
          ]));
  H.text h ~selector:".project[data-project='/home/u/src/prigh']";
  [%expect
    {|
    (prigh main) (New session in ~/src/prigh) (New worktree: work on a branch of its own, in a new session)
    (Main work 2m ago) (Archive (A)) (Delete session (Delete))
    (Backend tests 1d ago backend/) (Archive (A)) (Delete session (Delete))
    (agent/fix-the-parser subagent) (New session in this worktree) (Remove this worktree (/worktree rm))
    from (Main work)
    (feat/login 2 changed ↑1) (New session in this worktree) (Remove this worktree (/worktree rm))
    (Login form 20m ago) (Archive (A)) (Delete session (Delete))
    (docs) (New session in this worktree) (Remove this worktree (/worktree rm))
    |}]
;;

let%expect_test
    "folding: saved as a preference, summarised, skipped by the keyboard"
  =
  let h = create () in
  H.click h ~selector:".project-toggle" ~nth:0;
  H.click h ~selector:".worktree-toggle" ~nth:0;
  tree h;
  [%expect
    {|
    (Toggle_folded /home/u/src/prigh)
    (Cache_prefs (((key web.sidebar-folded) (value ((/home/u/src/prigh))))))
    (Rpc (method_ set_prefs)
     (params ((prefs ((web.sidebar-folded (/home/u/src/prigh))))))
     (tag Show_error))
    (Toggle_folded /home/u/.prigh/worktrees/site/redesign)
    (Cache_prefs
     (((key web.sidebar-folded)
       (value ((/home/u/.prigh/worktrees/site/redesign /home/u/src/prigh))))))
    (Rpc (method_ set_prefs)
     (params
      ((prefs
        ((web.sidebar-folded
          (/home/u/.prigh/worktrees/site/redesign /home/u/src/prigh))))))
     (tag Show_error))
    (prigh main 3) (New session in ~/src/prigh) (New worktree: work on a branch of its own, in a new session)
    (site trunk) (New session in ~/src/site) (New worktree: work on a branch of its own, in a new session)
    (Landing page 2h ago) (Archive (A)) (Delete session (Delete))
    (redesign 1) (New session in this worktree) (Remove this worktree (/worktree rm))
    (notes) (New session in ~/notes)
    (Shopping list 2d ago) (Archive (A)) (Delete session (Delete))
    (Archived 1)
    |}];
  (* The keyboard goes past what is folded. *)
  H.key h "ArrowDown" ~target:Session_search;
  H.key h "ArrowDown" ~target:(Session "s3");
  [%expect
    {|
    (Session_nav (from ()) (by 1))
    (Focus session-s3)
    (Session_nav (from (s3)) (by 1))
    (Focus session-s5)
    |}];
  (* A search shows everything that matches, folded or not. *)
  H.act h (Set_session_query "login");
  tree h;
  [%expect
    {|
    (prigh) (New session in ~/src/prigh) (New worktree: work on a branch of its own, in a new session)
    (feat/login) (New session in this worktree) (Remove this worktree (/worktree rm))
    (Login form 20m ago) (Archive (A)) (Delete session (Delete))
    (Archived 1)
    (Old experiment 1mo ago ~/src/prigh) (Unarchive (A)) (Delete session (Delete))
    (Delete archived)
    |}];
  H.act h (Set_session_query "");
  (* Unfolded again: nothing left to save. *)
  H.click h ~selector:".project-toggle" ~nth:0;
  [%expect
    {|
    (Toggle_folded /home/u/src/prigh)
    (Cache_prefs
     (((key web.sidebar-folded)
       (value ((/home/u/.prigh/worktrees/site/redesign))))))
    (Rpc (method_ set_prefs)
     (params
      ((prefs ((web.sidebar-folded (/home/u/.prigh/worktrees/site/redesign))))))
     (tag Show_error))
    |}]
;;

let%expect_test "the folds come from the user's preferences" =
  let h = H.create ~state ~sessions:(sessions ()) ~verbose:false () in
  H.event
    h
    (sprintf
       {|{"event":"prefs_changed","prefs":{"web.sidebar-folded":["%s","%s"]}}|}
       site
       login);
  H.elements h ~selector:".project.folded";
  H.elements h ~selector:".worktree.folded";
  [%expect
    {|
    (Cache_prefs
     (((key web.sidebar-folded)
       (value ((/home/u/src/site /home/u/.prigh/worktrees/prigh/feat-login))))))
    <section class="folded git project">
    <div class="folded worktree">
    |}]
;;

let%expect_test "search finds sessions in every project" =
  let h = create () in
  H.act h (Set_session_query "hero");
  tree h;
  H.act h (Set_session_query "prigh");
  H.text h ~selector:".session-title";
  [%expect
    {|
    (site) (New session in ~/src/site) (New worktree: work on a branch of its own, in a new session)
    (redesign) (New session in this worktree) (Remove this worktree (/worktree rm))
    (New hero 3d ago css/) (Archive (A)) (Delete session (Delete))
    (prigh) (New session in ~/src/prigh) (New worktree: work on a branch of its own, in a new session)
    (feat/login) (New session in this worktree) (Remove this worktree (/worktree rm))
    (Login form 20m ago) (Archive (A)) (Delete session (Delete))
    Main work
    Backend tests
    Login form
    New hero
    Old experiment
    |}]
;;

let%expect_test "a new session in a project or a worktree" =
  let h = create () in
  H.click h ~selector:".project-row .new-here" ~nth:1;
  [%expect
    {|
    (New_session_in /home/u/src/site)
    (Rpc (method_ new_session) (params ())
     (tag
      (Worktree_session (path /home/u/src/site)
       (notice "New session in /home/u/src/site"))))
    |}];
  H.reply
    h
    "new_session"
    (sprintf
       {|{"state":%s}|}
       (H.state_json ~fields:[ "session_id", `String "s9" ] ()));
  [%expect
    {|
    (Set_url_session s9)
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s9.jsonl) (limit 60)))
     (tag (Page (path /sessions/s9.jsonl) (before ()))))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Rpc (method_ set_cwd) (params ((path /home/u/src/site)))
     (tag (Worktree_entered "New session in /home/u/src/site")))
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s9\
                            \nbackend\
                            \n/work") (purpose (Sidebar 2)))))
    |}];
  H.click h ~selector:".worktree-row .new-here" ~nth:1;
  [%expect
    {|
    (New_session_in /home/u/.prigh/worktrees/prigh/feat-login)
    (Rpc (method_ new_session) (params ())
     (tag
      (Worktree_session (path /home/u/.prigh/worktrees/prigh/feat-login)
       (notice "New session in /home/u/.prigh/worktrees/prigh/feat-login"))))
    |}]
;;

let%expect_test "a new worktree of a project starts a session in it" =
  let h = create () in
  H.click h ~selector:".project-row .new-worktree" ~nth:1;
  dialog h;
  [%expect
    {|
    (New_worktree (repo (/home/u/src/site)))
    (Focus worktree-branch)
    New worktree of site
    (Close (Esc))
    Branch [] A new branch, or an existing one to check out
    Base [] Where a new branch starts (empty: trunk)
    A new session starts there once it is created.
    Enter creates, Esc cancels (Cancel) (Create and start a session)
    |}];
  H.input h ~selector:"#worktree-branch" "feat/pricing";
  H.key h "Enter" ~target:Field;
  [%expect
    {|
    (New_worktree_input (field Branch) (text feat/pricing))
    Dialog_accept
    (Rpc (method_ create_worktree)
     (params ((branch feat/pricing) (cwd /home/u/src/site)))
     (tag
      (Worktree_created (command "/worktree new feat/pricing")
       (new_session true))))
    |}];
  H.reply
    h
    "create_worktree"
    (sprintf
       {|{"worktree":{"path":"%s/site/feat-pricing","branch":"feat/pricing","head":"abc","main":false,"managed":true,"locked":false,"prunable":false,"changes":0,"upstream":null,"ahead":0,"behind":0,"current":false,"sessions":[]},"new_branch":true,"root":"%s","cwd":"%s/site/feat-pricing"}|}
       trees
       site
       trees);
  H.text h ~selector:".toast";
  [%expect
    {|
    (Focus editor)
    (Expire_toast (id 0) (after_ms 4700))
    (Rpc (method_ new_session) (params ())
     (tag
      (Worktree_session (path /home/u/.prigh/worktrees/site/feat-pricing)
       (notice "New session in /home/u/.prigh/worktrees/site/feat-pricing"))))
    (Rpc (method_ list_worktrees) (params ())
     (tag
      (Worktrees (place  "s1\
                        \nbackend\
                        \n/home/u/src/prigh")
       (purpose (Sidebar 2)))))
    Worktree feat/pricing at /home/u/.prigh/worktrees/site/feat-pricing (new branch)
    |}]
;;

let%expect_test "removing another repository's worktree names the repository" =
  let h = create () in
  H.click
    h
    ~selector:
      ".worktree[data-worktree='/home/u/.prigh/worktrees/site/redesign'] \
       .delete";
  dialog h;
  H.key h "Enter" ~target:Page;
  [%expect
    {|
    (Ask_remove_worktree /home/u/.prigh/worktrees/site/redesign)
    (Focus dialog)
    Remove worktree?
    (Close (Esc))
    Remove the worktree redesign at /home/u/.prigh/worktrees/site/redesign? (the branch stays)
    Enter removes, Esc cancels (Cancel) (Remove)
    Dialog_accept
    (Rpc (method_ remove_worktree)
     (params
      ((path /home/u/.prigh/worktrees/site/redesign) (cwd /home/u/src/site)))
     (tag (Worktree_removed (target redesign) (force false))))
    |}];
  (* The session's own worktree has no removal; one of its own worktrees
     cannot be removed while it works there. *)
  H.elements h ~selector:".worktree.here .delete";
  [%expect {| |}]
;;

let%expect_test "a subagent's worktree: tagged, linked to its session" =
  let h = create () in
  H.show h ~selector:".worktree.agent .worktree-row";
  H.click h ~selector:".worktree-owner";
  [%expect
    {|
    <div class="worktree-row">
      <button type="button"
              aria-expanded="true"
              data-dir="/home/u/.prigh/worktrees/prigh/agent-fix-the-parser"
              title="~/.prigh/worktrees/prigh/agent-fix-the-parser"
              class="worktree-toggle"
              @on_click>
        <icon class="branch"> </icon>
        <span class="worktree-branch"> agent/fix-the-parser </span>
        <span title="Made for subagent 2: Fix the parser" class="worktree-tag"> subagent </span>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      </button>
      <span class="tree-actions">
        <button type="button"
                title="New session in this worktree"
                aria-label="New session in agent/fix-the-parser"
                class="btn ghost icon new-here"
                @on_click>
          <icon class="plus"> </icon>
        </button>
        <button type="button"
                title="Remove this worktree (/worktree rm)"
                aria-label="Remove agent/fix-the-parser"
                class="btn delete ghost icon"
                @on_click>
          <icon class="trash"> </icon>
        </button>
      </span>
    </div>
    (Switch_session /sessions/s1.jsonl)
    (Focus editor)
    |}]
;;

let%expect_test "selecting: ranges follow the tree" =
  let h = create () in
  H.act h (Select_sessions true);
  H.click h ~selector:".session-open" ~nth:0;
  H.click h ~selector:".session-open" ~nth:3;
  H.act h (Mark_session { path = "/sessions/s6.jsonl"; range = true });
  H.text h ~selector:".selection-bar";
  H.elements h ~selector:".session.marked";
  [%expect
    {|
    (Mark_session (path /sessions/s1.jsonl) (range false))
    (Mark_session (path /sessions/s3.jsonl) (range false))
    (Cancel) (Delete 3 sessions (0 B))
    <div class="marked selected session">
    <div class="marked session">
    <div class="marked session">
    |}]
;;
