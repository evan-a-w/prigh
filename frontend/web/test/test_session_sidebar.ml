open! Core
module H = Harness

(* The sidebar: a flat list of sessions, each saying where it works (its
   repository and branch) with the worktrees that matter to it under it
   (made by it, moved into, a pull request's, a subagent's), and their
   context menus. *)

let prigh = "/home/u/src/prigh"
let site = "/home/u/src/site"
let trees = "/home/u/.prigh/worktrees"
let login = trees ^ "/prigh/feat-login"
let agent_wt = trees ^ "/prigh/agent-fix-the-parser"
let pr_wt = trees ^ "/prigh/pr-7"
let redesign = trees ^ "/site/redesign"

let sessions () =
  sprintf
    "[%s]"
    (String.concat
       ~sep:","
       [ H.session_json
           ~name:"Main work"
           ~cwd:prigh
           ~repo:(prigh, prigh, Some "main")
           ~live:true
           ~worktrees:
             [ login, Some "feat/login", prigh, "created"
             ; ( agent_wt
               , Some "agent/fix-the-parser"
               , prigh
               , "agent:2:Fix the parser" )
             ; pr_wt, Some "pr-7", prigh, "pr:7"
             ]
           "s1"
       ; H.session_json
           ~name:"Login form"
           ~cwd:login
           ~repo:(prigh, login, Some "feat/login")
           ~live:true
           ~running:true
           ~updated_at:"2026-10-05 09:40:00Z"
           ~worktrees:[ login, Some "feat/login", prigh, "entered" ]
           "s2"
       ; H.session_json
           ~name:"Landing page"
           ~cwd:site
           ~repo:(site, site, Some "trunk")
           ~updated_at:"2026-10-05 08:00:00Z"
           "s3"
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
           ~worktrees:[ redesign, Some "redesign", site, "entered" ]
           "s6"
       ; H.session_json
           ~name:"Old experiment"
           ~cwd:prigh
           ~repo:(prigh, prigh, Some "main")
           ~archived:true
           ~updated_at:"2026-09-01 08:00:00Z"
           "s7"
       ])
;;

let state =
  H.state_json
    ~fields:
      [ "session_id", `String "s1"
      ; "session_path", `String "/sessions/s1.jsonl"
      ; "cwd", `String prigh
      ; "git_branch", `String "main"
      ]
    ()
;;

let create () = H.create ~state ~sessions:(sessions ()) ()
let quiet () = H.create ~state ~sessions:(sessions ()) ~verbose:false ()
let list h = H.text h ~selector:".sessions"
let menu h = H.text h ~selector:".context-menu"
let dialog h = H.text h ~selector:".modal"

let worktree ~owner path =
  sprintf "[data-owner=%s][data-worktree='%s']" owner path
;;

let choose h id =
  H.click h ~selector:(sprintf ".context-menu [data-item=%s]" id)
;;

let worktree_json ?(changes = 0) ?(ahead = 0) ?(main = false) path branch =
  sprintf
    {|{"path":"%s","branch":"%s","head":"abc","main":%b,"managed":%b,"locked":false,"prunable":false,"changes":%d,"upstream":"origin/%s","ahead":%d,"behind":0,"current":false,"sessions":[]}|}
    path
    branch
    main
    (not main)
    changes
    branch
    ahead
;;

let listing root worktrees =
  sprintf
    {|{"root":"%s","project_dir":"%s/%s","worktrees":[%s]}|}
    root
    trees
    (Filename.basename root)
    (String.concat ~sep:"," worktrees)
;;

let%expect_test "a flat list of sessions, each with its worktrees under it" =
  let h = create () in
  list h;
  [%expect
    {|
    (Main work 2m ago prigh main) (3) (More (right-click, Shift+F10))
    (feat/login) (agent/fix-the-parser subagent) (pr-7 #7)
    (Login form 20m ago prigh feat/login) (1) (More (right-click, Shift+F10))
    (feat/login here)
    (Landing page 2h ago site trunk) (More (right-click, Shift+F10))
    (Shopping list 2d ago ~/notes) (More (right-click, Shift+F10))
    (New hero 3d ago site redesign) (1) (More (right-click, Shift+F10))
    (redesign here)
    (Archived 1)
    |}];
  H.elements h ~selector:".session-item";
  H.elements h ~selector:".worktree";
  [%expect
    {|
    <div class="has-worktrees selected session-item">
    <div class="has-worktrees session-item">
    <div class="session-item">
    <div class="session-item">
    <div class="has-worktrees session-item">
    <button class="worktree">
    <button class="agent worktree">
    <button class="worktree">
    <button class="here worktree">
    <button class="here worktree">
    |}];
  H.names h ~selector:".worktree";
  [%expect
    {|
    button "feat/login" -- ~/.prigh/worktrees/prigh/feat-login
    button "agent/fix-the-parser subagent" -- ~/.prigh/worktrees/prigh/agent-fix-the-parser
    button "pr-7 #7" -- ~/.prigh/worktrees/prigh/pr-7
    button "feat/login here" -- ~/.prigh/worktrees/prigh/feat-login
    button "redesign here" -- ~/.prigh/worktrees/site/redesign
    |}]
;;

let%expect_test
    "the worktrees' repositories are listed: changes, ahead, removed ones gone"
  =
  let h = create () in
  H.reply
    h
    "list_worktrees"
    (listing
       prigh
       [ worktree_json ~main:true prigh "main"
       ; worktree_json ~changes:2 ~ahead:1 login "feat/login"
       ; worktree_json agent_wt "agent/fix-the-parser"
       ]);
  H.text h ~selector:".session-worktrees";
  [%expect
    {|
    (feat/login ● 2 ↑1) (agent/fix-the-parser subagent)
    (feat/login here ● 2 ↑1)
    (redesign here)
    |}];
  H.reply
    h
    "list_worktrees"
    (listing site [ worktree_json ~main:true site "trunk" ]);
  (* redesign is not listed any more: removed outside prigh. *)
  H.elements h ~selector:".worktree";
  [%expect
    {|
    <button class="dirty worktree">
    <button class="agent worktree">
    <button class="dirty here worktree">
    |}]
;;

let%expect_test "folding a session's worktrees: a preference, by session" =
  let h = create () in
  H.click h ~selector:".session-fold" ~nth:0;
  list h;
  [%expect
    {|
    (Toggle_folded s1)
    (Cache_prefs (((key web.sidebar-folded) (value ((s1))))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.sidebar-folded (s1))))))
     (tag Show_error))
    (Main work 2m ago prigh main) (3) (More (right-click, Shift+F10))
    (Login form 20m ago prigh feat/login) (1) (More (right-click, Shift+F10))
    (feat/login here)
    (Landing page 2h ago site trunk) (More (right-click, Shift+F10))
    (Shopping list 2d ago ~/notes) (More (right-click, Shift+F10))
    (New hero 3d ago site redesign) (1) (More (right-click, Shift+F10))
    (redesign here)
    (Archived 1)
    |}];
  H.click h ~selector:".session-fold" ~nth:0;
  [%expect
    {|
    (Toggle_folded s1)
    (Cache_prefs (((key web.sidebar-folded) (value ()))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.sidebar-folded null)))))
     (tag Show_error))
    |}];
  let h = quiet () in
  H.event h {|{"event":"prefs_changed","prefs":{"web.sidebar-folded":["s6"]}}|};
  H.elements h ~selector:".session-fold.folded";
  [%expect
    {|
    (Cache_prefs (((key web.sidebar-folded) (value ((s6))))))
    <button class="folded session-fold">
    |}]
;;

let%expect_test "search: by repository, branch and worktree" =
  let h = create () in
  H.act h (Set_session_query "pr-7");
  H.text h ~selector:".session-title";
  H.act h (Set_session_query "redesign");
  H.text h ~selector:".session-title";
  [%expect
    {|
    Main work
    New hero
    |}]
;;

let%expect_test "a session's menu: right-click, Shift+F10, ContextMenu, ⋯; Esc" =
  let h = create () in
  H.right_click h ~selector:".session" ~nth:2 ~at:(40, 200);
  menu h;
  H.show h ~selector:".context-menu";
  [%expect
    {|
    (Open_menu
     ((target (Session /sessions/s3.jsonl)) (anchor (Point (x 40) (y 200)))))
    (Focus dialog)
    (Place_menu (Point (x 40) (y 200)))
    Landing page site trunk
    Open Enter
    Open in new tab
    Rename… /name
    Fork… /fork
    New worktree… /worktree new
    Review changes /review
    Open in file browser /files
    Open terminal there /terminal
    Copy path
    Copy session id
    Archive A
    Delete… Delete
    <div id="dialog"
         role="menu"
         aria-label="Session"
         tabindex="-1"
         data-x="40"
         data-y="200"
         class="context-menu menu-surface">
      <div class="context-menu-head">
        <span class="context-menu-title"> Landing page </span>
        <span title="~/src/site" class="context-menu-sub session-place">
          <span class="session-repo"> site </span>
          <span class="session-branch">
            <icon class="branch"> </icon>
            trunk
          </span>
        </span>
      </div>
      <div class="menu-section">
        <div role="menuitem" data-item="open" class="menu-item selected" @on_click>
          <icon class="arrow_down"> </icon>
          <span class="menu-label"> Open </span>
          <kbd class="menu-command"> Enter </kbd>
        </div>
        <div role="menuitem" data-item="open_tab" class="menu-item" @on_click>
          <icon class="external"> </icon>
          <span class="menu-label"> Open in new tab </span>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        </div>
      </div>
      <div class="menu-section">
        <div role="menuitem" data-item="rename" class="menu-item" @on_click>
          <icon class="pencil"> </icon>
          <span class="menu-label"> Rename… </span>
          <kbd class="menu-command"> /name </kbd>
        </div>
        <div role="menuitem" data-item="fork" class="menu-item" @on_click>
          <icon class="fork"> </icon>
          <span class="menu-label"> Fork… </span>
          <kbd class="menu-command"> /fork </kbd>
        </div>
        <div role="menuitem" data-item="new_worktree" class="menu-item" @on_click>
          <icon class="branch_plus"> </icon>
          <span class="menu-label"> New worktree… </span>
          <kbd class="menu-command"> /worktree new </kbd>
        </div>
      </div>
      <div class="menu-section">
        <div role="menuitem" data-item="review" class="menu-item" @on_click>
          <icon class="review"> </icon>
          <span class="menu-label"> Review changes </span>
          <kbd class="menu-command"> /review </kbd>
        </div>
        <div role="menuitem" data-item="files" class="menu-item" @on_click>
          <icon class="folder"> </icon>
          <span class="menu-label"> Open in file browser </span>
          <kbd class="menu-command"> /files </kbd>
        </div>
        <div role="menuitem" data-item="terminal" class="menu-item" @on_click>
          <icon class="terminal"> </icon>
          <span class="menu-label"> Open terminal there </span>
          <kbd class="menu-command"> /terminal </kbd>
        </div>
      </div>
      <div class="menu-section">
        <div role="menuitem" data-item="copy_path" class="menu-item" @on_click>
          <icon class="copy"> </icon>
          <span class="menu-label"> Copy path </span>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        </div>
        <div role="menuitem" data-item="copy_id" class="menu-item" @on_click>
          <icon class="copy"> </icon>
          <span class="menu-label"> Copy session id </span>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        </div>
      </div>
      <div class="menu-section">
        <div role="menuitem" data-item="archive" class="menu-item" @on_click>
          <icon class="archive"> </icon>
          <span class="menu-label"> Archive </span>
          <kbd class="menu-command"> A </kbd>
        </div>
        <div role="menuitem" data-item="delete" class="danger menu-item" @on_click>
          <icon class="trash"> </icon>
          <span class="menu-label"> Delete… </span>
          <kbd class="menu-command"> Delete </kbd>
        </div>
      </div>
    </div>
    |}];
  (* Esc closes it, nothing else. *)
  H.key h "Escape" ~target:Page;
  H.elements h ~selector:".context-menu";
  [%expect
    {|
    Close_dialog
    (Focus editor)
    |}];
  H.key h "F10" ~shift:true ~target:(Session "s1");
  menu h;
  H.key h "Escape" ~target:Page;
  H.key h "ContextMenu" ~target:(Session "s5");
  menu h;
  H.key h "Escape" ~target:Page;
  H.click h ~selector:"#more-session-s6";
  H.names h ~selector:".context-menu [role=menuitem]";
  [%expect
    {|
    (Open_menu
     ((target (Session /sessions/s1.jsonl)) (anchor (Element session-s1))))
    (Focus dialog)
    (Place_menu (Element session-s1))
    Main work prigh main
    Open in new tab
    Rename… /name
    Fork… /fork
    New worktree… /worktree new
    Review changes /review
    Open in file browser /files
    Open terminal there /terminal
    Copy path
    Copy session id
    Archive A
    Delete… Delete
    Close_dialog
    (Focus editor)
    (Open_menu
     ((target (Session /sessions/s5.jsonl)) (anchor (Element session-s5))))
    (Focus dialog)
    (Place_menu (Element session-s5))
    Shopping list ~/notes
    Open Enter
    Open in new tab
    Rename… /name
    Fork… /fork
    Review changes /review
    Open in file browser /files
    Open terminal there /terminal
    Copy path
    Copy session id
    Archive A
    Delete… Delete
    Close_dialog
    (Focus editor)
    (Open_menu
     ((target (Session /sessions/s6.jsonl)) (anchor (Element more-session-s6))))
    (Focus dialog)
    (Place_menu (Element more-session-s6))
    [menuitem] div "Open Enter"
    [menuitem] div "Open in new tab"
    [menuitem] div "Rename\226\128\166 /name"
    [menuitem] div "Fork\226\128\166 /fork"
    [menuitem] div "New worktree\226\128\166 /worktree new"
    [menuitem] div "Review changes /review"
    [menuitem] div "Open in file browser /files"
    [menuitem] div "Open terminal there /terminal"
    [menuitem] div "Copy path"
    [menuitem] div "Copy session id"
    [menuitem] div "Archive A"
    [menuitem] div "Delete\226\128\166 Delete"
    |}]
;;

let%expect_test "the menu from the keyboard: ↑↓ move, Enter chooses" =
  let h = create () in
  H.key h "ContextMenu" ~target:(Session "s3");
  H.key h "ArrowDown" ~target:Page;
  H.key h "ArrowDown" ~target:Page;
  H.elements h ~selector:".context-menu .menu-item.selected";
  H.key h "Enter" ~target:Page;
  [%expect
    {|
    (Open_menu
     ((target (Session /sessions/s3.jsonl)) (anchor (Element session-s3))))
    (Focus dialog)
    (Place_menu (Element session-s3))
    (Dialog_move 1)
    (Dialog_move 1)
    <div class="menu-item selected">
    Dialog_accept
    (Focus editor)
    (Rpc (method_ switch_session) (params ((path /sessions/s3.jsonl)))
     (tag
      (Plan_switched
       ((switch (/sessions/s3.jsonl)) (enter ()) (run (/name)) (notice ())))))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s3.jsonl) (limit 60)))
     (tag (Page (path /sessions/s3.jsonl) (before ()))))
    |}]
;;

let%expect_test "a session's items on another session: switch there first" =
  let h = create () in
  H.right_click h ~selector:".session" ~nth:2;
  choose h "fork";
  [%expect
    {|
    (Open_menu
     ((target (Session /sessions/s3.jsonl)) (anchor (Element session-s3))))
    (Focus dialog)
    (Place_menu (Element session-s3))
    (Picker_choose fork)
    (Focus editor)
    (Rpc (method_ switch_session) (params ((path /sessions/s3.jsonl)))
     (tag
      (Plan_switched
       ((switch (/sessions/s3.jsonl)) (enter ()) (run (/fork)) (notice ())))))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s3.jsonl) (limit 60)))
     (tag (Page (path /sessions/s3.jsonl) (before ()))))
    |}];
  H.reply
    h
    "switch_session"
    (sprintf
       {|{"state":%s}|}
       (H.state_json
          ~fields:
            [ "session_id", `String "s3"
            ; "session_path", `String "/sessions/s3.jsonl"
            ; "cwd", `String site
            ]
          ()));
  [%expect
    {|
    (Set_url_session s3)
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Rpc (method_ get_review) (params ()) (tag (Review Review)))
    (Focus dialog)
    (Rpc (method_ get_entries) (params ()) (tag (Entries Fork)))
    (Rpc (method_ list_worktrees) (params ())
     (tag
      (Worktrees (place  "s3\
                        \nbackend\
                        \n/home/u/src/site")
       (purpose (Sidebar 2)))))
    (Rpc (method_ list_worktrees) (params ((cwd /home/u/src/prigh)))
     (tag (Repo_worktrees /home/u/src/prigh)))
    |}]
;;

let%expect_test "a session's items on the current session" =
  let h = create () in
  let on_s1 id =
    H.right_click h ~selector:".session" ~nth:0;
    choose h id
  in
  on_s1 "rename";
  dialog h;
  [%expect
    {|
    (Open_menu
     ((target (Session /sessions/s1.jsonl)) (anchor (Element session-s1))))
    (Focus dialog)
    (Place_menu (Element session-s1))
    (Picker_choose rename)
    (Focus dialog-input)
    Rename session
    (Close (Esc))
    []
    (✦ Generate)
    (Cancel) (Rename)
    |}];
  H.key h "Escape" ~target:Page;
  on_s1 "review";
  on_s1 "files";
  on_s1 "terminal";
  on_s1 "copy_path";
  on_s1 "copy_id";
  on_s1 "new_worktree";
  [%expect
    {|
    Close_dialog
    (Focus editor)
    (Open_menu
     ((target (Session /sessions/s1.jsonl)) (anchor (Element session-s1))))
    (Focus dialog)
    (Place_menu (Element session-s1))
    (Picker_choose review)
    (Focus editor)
    (Rpc (method_ git_diff) (params ()) (tag (Review Diff)))
    (Rpc (method_ git_status) (params ()) (tag (Git Status)))
    (Focus review-page)
    (Open_menu
     ((target (Session /sessions/s1.jsonl)) (anchor (Element session-s1))))
    (Focus dialog)
    (Place_menu (Element session-s1))
    (Picker_choose files)
    (Focus editor)
    (Rpc (method_ fs_list) (params ((path /home/u/src/prigh) (hidden false)))
     (tag (Files (Listed /home/u/src/prigh))))
    (Files (Focus files-list))
    (Open_menu
     ((target (Session /sessions/s1.jsonl)) (anchor (Element session-s1))))
    (Focus dialog)
    (Place_menu (Element session-s1))
    (Picker_choose terminal)
    (Focus editor)
    (Cache_prefs (((key web.terminal-open) (value (true)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.terminal-open true)))))
     (tag Show_error))
    Focus_terminal
    (Open_menu
     ((target (Session /sessions/s1.jsonl)) (anchor (Element session-s1))))
    (Focus dialog)
    (Place_menu (Element session-s1))
    (Picker_choose copy_path)
    (Focus editor)
    (Copy /home/u/src/prigh)
    (Expire_toast (id 0) (after_ms 2860))
    (Open_menu
     ((target (Session /sessions/s1.jsonl)) (anchor (Element session-s1))))
    (Focus dialog)
    (Place_menu (Element session-s1))
    (Picker_choose copy_id)
    (Focus editor)
    (Copy s1)
    (Expire_toast (id 1) (after_ms 2500))
    (Open_menu
     ((target (Session /sessions/s1.jsonl)) (anchor (Element session-s1))))
    (Focus dialog)
    (Place_menu (Element session-s1))
    (Picker_choose new_worktree)
    (Focus worktree-branch)
    |}];
  H.key h "Escape" ~target:Page;
  on_s1 "delete";
  dialog h;
  [%expect
    {|
    Close_dialog
    (Focus editor)
    (Open_menu
     ((target (Session /sessions/s1.jsonl)) (anchor (Element session-s1))))
    (Focus dialog)
    (Place_menu (Element session-s1))
    (Picker_choose delete)
    (Focus dialog)
    Can't delete this session
    (Close (Esc))
    Main work 0 B
    “Main work” is the session you are in: it will be kept (switch to another first)
    (Close)
    |}]
;;

let%expect_test "a worktree's menu and its items" =
  let h = create () in
  let on_login id =
    H.right_click h ~selector:(worktree ~owner:"s1" login);
    choose h id
  in
  H.click h ~selector:(worktree ~owner:"s1" login);
  menu h;
  [%expect
    {|
    (Open_menu
     ((target
       (Worktree (session /sessions/s1.jsonl)
        (path /home/u/.prigh/worktrees/prigh/feat-login)))
      (anchor (Element worktree-s1--home-u--prigh-worktrees-prigh-feat-login))))
    (Focus dialog)
    (Place_menu (Element worktree-s1--home-u--prigh-worktrees-prigh-feat-login))
    feat/login in Main work
    Switch session here /worktree open feat/login
    New session here /worktree open -n feat/login
    Review changes /review feat/login
    Open in file browser /files /home/u/.prigh/worktrees/prigh/feat-login
    Open terminal there /terminal /home/u/.prigh/worktrees/prigh/feat-login
    Create pull request… /pr new
    Copy path
    Remove worktree… /worktree rm feat/login
    |}];
  H.key h "Escape" ~target:Page;
  on_login "review";
  on_login "files";
  on_login "terminal";
  on_login "copy_path";
  [%expect
    {|
    Close_dialog
    (Focus editor)
    (Open_menu
     ((target
       (Worktree (session /sessions/s1.jsonl)
        (path /home/u/.prigh/worktrees/prigh/feat-login)))
      (anchor (Element worktree-s1--home-u--prigh-worktrees-prigh-feat-login))))
    (Focus dialog)
    (Place_menu (Element worktree-s1--home-u--prigh-worktrees-prigh-feat-login))
    (Picker_choose review)
    (Focus editor)
    (Rpc (method_ git_diff)
     (params ((dir /home/u/.prigh/worktrees/prigh/feat-login)))
     (tag (Review Diff)))
    (Rpc (method_ git_status)
     (params ((dir /home/u/.prigh/worktrees/prigh/feat-login)))
     (tag (Git Status)))
    (Focus review-page)
    (Open_menu
     ((target
       (Worktree (session /sessions/s1.jsonl)
        (path /home/u/.prigh/worktrees/prigh/feat-login)))
      (anchor (Element worktree-s1--home-u--prigh-worktrees-prigh-feat-login))))
    (Focus dialog)
    (Place_menu (Element worktree-s1--home-u--prigh-worktrees-prigh-feat-login))
    (Picker_choose files)
    (Focus editor)
    (Rpc (method_ fs_list)
     (params ((path /home/u/.prigh/worktrees/prigh/feat-login) (hidden false)))
     (tag (Files (Listed /home/u/.prigh/worktrees/prigh/feat-login))))
    (Files (Focus files-list))
    (Open_menu
     ((target
       (Worktree (session /sessions/s1.jsonl)
        (path /home/u/.prigh/worktrees/prigh/feat-login)))
      (anchor (Element worktree-s1--home-u--prigh-worktrees-prigh-feat-login))))
    (Focus dialog)
    (Place_menu (Element worktree-s1--home-u--prigh-worktrees-prigh-feat-login))
    (Picker_choose terminal)
    (Focus editor)
    (Cache_prefs (((key web.terminal-open) (value (true)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.terminal-open true)))))
     (tag Show_error))
    Focus_terminal
    (Open_menu
     ((target
       (Worktree (session /sessions/s1.jsonl)
        (path /home/u/.prigh/worktrees/prigh/feat-login)))
      (anchor (Element worktree-s1--home-u--prigh-worktrees-prigh-feat-login))))
    (Focus dialog)
    (Place_menu (Element worktree-s1--home-u--prigh-worktrees-prigh-feat-login))
    (Picker_choose copy_path)
    (Focus editor)
    (Copy /home/u/.prigh/worktrees/prigh/feat-login)
    (Expire_toast (id 0) (after_ms 3820))
    |}];
  on_login "switch_here";
  [%expect
    {|
    (Open_menu
     ((target
       (Worktree (session /sessions/s1.jsonl)
        (path /home/u/.prigh/worktrees/prigh/feat-login)))
      (anchor (Element worktree-s1--home-u--prigh-worktrees-prigh-feat-login))))
    (Focus dialog)
    (Place_menu (Element worktree-s1--home-u--prigh-worktrees-prigh-feat-login))
    (Picker_choose switch_here)
    (Focus editor)
    (Rpc (method_ set_cwd)
     (params ((path /home/u/.prigh/worktrees/prigh/feat-login)))
     (tag
      (Plan_entered
       ((switch (/sessions/s1.jsonl)) (enter ()) (run ())
        (notice
         ("Working in the worktree feat/login (/home/u/.prigh/worktrees/prigh/feat-login)"))))))
    |}];
  on_login "new_session_here";
  [%expect
    {|
    (Open_menu
     ((target
       (Worktree (session /sessions/s1.jsonl)
        (path /home/u/.prigh/worktrees/prigh/feat-login)))
      (anchor (Element worktree-s1--home-u--prigh-worktrees-prigh-feat-login))))
    (Focus dialog)
    (Place_menu (Element worktree-s1--home-u--prigh-worktrees-prigh-feat-login))
    (Picker_choose new_session_here)
    (Focus editor)
    (Rpc (method_ new_session) (params ())
     (tag
      (Worktree_session (path /home/u/.prigh/worktrees/prigh/feat-login)
       (notice "New session in /home/u/.prigh/worktrees/prigh/feat-login"))))
    |}]
;;

let%expect_test "a worktree's items on another session's worktree" =
  let h = create () in
  H.right_click h ~selector:(worktree ~owner:"s6" redesign);
  choose h "create_pr";
  [%expect
    {|
    (Open_menu
     ((target
       (Worktree (session /sessions/s6.jsonl)
        (path /home/u/.prigh/worktrees/site/redesign)))
      (anchor (Element worktree-s6--home-u--prigh-worktrees-site-redesign))))
    (Focus dialog)
    (Place_menu (Element worktree-s6--home-u--prigh-worktrees-site-redesign))
    (Picker_choose create_pr)
    (Focus editor)
    (Rpc (method_ switch_session) (params ((path /sessions/s6.jsonl)))
     (tag
      (Plan_switched
       ((switch (/sessions/s6.jsonl))
        (enter (/home/u/.prigh/worktrees/site/redesign)) (run ("/pr new"))
        (notice ())))))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s6.jsonl) (limit 60)))
     (tag (Page (path /sessions/s6.jsonl) (before ()))))
    |}]
;;

let%expect_test "removing a worktree from its menu, or Delete on its row" =
  let h = create () in
  H.right_click h ~selector:(worktree ~owner:"s6" redesign);
  choose h "remove";
  dialog h;
  [%expect
    {|
    (Open_menu
     ((target
       (Worktree (session /sessions/s6.jsonl)
        (path /home/u/.prigh/worktrees/site/redesign)))
      (anchor (Element worktree-s6--home-u--prigh-worktrees-site-redesign))))
    (Focus dialog)
    (Place_menu (Element worktree-s6--home-u--prigh-worktrees-site-redesign))
    (Picker_choose remove)
    (Focus dialog)
    Remove worktree?
    (Close (Esc))
    Remove the worktree redesign at /home/u/.prigh/worktrees/site/redesign? (the branch stays)
    Enter removes, Esc cancels (Cancel) (Remove)
    |}];
  H.key h "Escape" ~target:Page;
  H.key h "F10" ~shift:true ~target:(Worktree { session = "s1"; path = pr_wt });
  menu h;
  H.key h "Escape" ~target:Page;
  [%expect
    {|
    Close_dialog
    (Focus editor)
    (Open_menu
     ((target
       (Worktree (session /sessions/s1.jsonl)
        (path /home/u/.prigh/worktrees/prigh/pr-7)))
      (anchor (Element worktree-s1--home-u--prigh-worktrees-prigh-pr-7))))
    (Focus dialog)
    (Place_menu (Element worktree-s1--home-u--prigh-worktrees-prigh-pr-7))
    pr-7 in Main work
    Switch session here /worktree open pr-7
    New session here /worktree open -n pr-7
    Review changes /review pr-7
    Open in file browser /files /home/u/.prigh/worktrees/prigh/pr-7
    Open terminal there /terminal /home/u/.prigh/worktrees/prigh/pr-7
    Create pull request… /pr new
    Copy path
    Remove worktree… /worktree rm pr-7
    Close_dialog
    (Focus editor)
    |}];
  H.key h "Delete" ~target:(Worktree { session = "s1"; path = pr_wt });
  H.key h "Enter" ~target:Page;
  [%expect
    {|
    (Ask_remove_worktree /home/u/.prigh/worktrees/prigh/pr-7)
    (Focus dialog)
    Dialog_accept
    (Rpc (method_ remove_worktree)
     (params
      ((path /home/u/.prigh/worktrees/prigh/pr-7) (cwd /home/u/src/prigh)))
     (tag (Worktree_removed (target pr-7) (force false))))
    |}]
;;

let%expect_test "selecting: no menus, ranges in list order" =
  let h = create () in
  H.act h (Select_sessions true);
  H.click h ~selector:".session-open" ~nth:0;
  H.click h ~selector:".session-open" ~nth:2;
  H.text h ~selector:".selection-bar";
  H.elements h ~selector:".session.marked";
  H.elements h ~selector:".worktree";
  H.elements h ~selector:".more";
  [%expect
    {|
    (Mark_session (path /sessions/s1.jsonl) (range false))
    (Mark_session (path /sessions/s3.jsonl) (range false))
    (Cancel) (Delete 2 sessions (0 B))
    <div class="marked selected session">
    <div class="marked session">
    |}]
;;
