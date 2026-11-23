open! Core
module H = Harness

(* The sidebar: a flat list of sessions, each saying where it works (its
   repository and branch); the worktrees that matter to a session (made by
   it, moved into, a pull request's, a subagent's) are in its card, with
   their context menus. *)

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

(* A worktree's menu, as its row in the session card opens it. *)
let worktree_menu h ~owner path =
  H.act
    h
    (Open_menu
       { target =
           Worktree { session = sprintf "/sessions/%s.jsonl" owner; path }
       ; anchor =
           Element
             (String.map
                ("worktree-" ^ owner ^ "-" ^ path)
                ~f:(fun c ->
                  if Char.is_alphanum c || Char.equal c '-' then c else '-'))
       })
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

let%expect_test "a flat list of sessions; their cards list their worktrees" =
  let h = create () in
  list h;
  [%expect
    {|
    (prigh 2m Main work) (More (right-click, Shift+F10)) (Archive) (Delete)
    (prigh working… Login form) (More (right-click, Shift+F10)) (Archive) (Delete)
    (site 2h Landing page) (More (right-click, Shift+F10)) (Archive) (Delete)
    (notes 2d Shopping list) (More (right-click, Shift+F10)) (Archive) (Delete)
    (site 3d New hero) (More (right-click, Shift+F10)) (Archive) (Delete)
    |}];
  H.elements h ~selector:".session";
  Worktree_lines.print h;
  [%expect
    {|
    <div class="bg-sidebar-accent group/session hover:bg-sidebar-accent relative rounded-md selected session text-sidebar-foreground">
    <div class="group/session hover:bg-sidebar-accent relative rounded-md running session text-sidebar-foreground">
    <div class="group/session hover:bg-sidebar-accent relative rounded-md session text-sidebar-foreground">
    <div class="group/session hover:bg-sidebar-accent relative rounded-md session text-sidebar-foreground">
    <div class="group/session hover:bg-sidebar-accent relative rounded-md session text-sidebar-foreground">
    (feat/login) (agent/fix-the-parser subagent) (pr-7 #7)
    (feat/login here)
    (redesign here)
    |}];
  H.names h ~selector:".session";
  [%expect
    {|
    div "prigh 2m Main work Archive Delete"
    div "prigh working\226\128\166 Login form Archive Delete"
    div "site 2h Landing page Archive Delete"
    div "notes 2d Shopping list Archive Delete"
    div "site 3d New hero Archive Delete"
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
  Worktree_lines.print h;
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
  Worktree_lines.print h;
  [%expect
    {|
    (feat/login ● 2 ↑1) (agent/fix-the-parser subagent)
    (feat/login here ● 2 ↑1)
    |}]
;;

let%expect_test "search: by repository, branch and worktree" =
  let h = create () in
  H.act h Open_control;
  H.act h (Control_query "pr-7");
  H.text h ~selector:".control-row:not(.x) .control-label";
  H.act h (Control_query "redesign");
  H.text h ~selector:".control-row .control-label";
  [%expect
    {|
    (Focus control-input)
    (Search_sessions_later (search 1) (after_ms 150))
    Main work
    (Search_sessions_later (search 2) (after_ms 150))
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
    Open in new window
    Rename… /name
    Fork… /fork
    New worktree… /worktree new
    Review changes /review
    Open in file browser /files
    Open terminal there /terminal
    Copy path
    Copy session id
    Pin to top P
    Archive A
    Delete… Delete
    <div data-slot="dropdown-menu-content"
         role="menu"
         aria-label="Session"
         tabindex="-1"
         id="dialog"
         data-x="40"
         data-y="200"
         class="[&:not(.placed)]:opacity-0 [&:not(.placed)]:pointer-events-none [&_.menu-label]:shrink-0 animate-in bg-popover cn-menu-target context-menu duration-100 fade-in-0 fixed loon max-[760px]:[&_.menu-item]:min-h-10 max-[760px]:[&_.menu-item]:text-[15px] max-[760px]:bottom-2 max-[760px]:left-2! max-[760px]:max-w-none max-[760px]:right-2 max-[760px]:top-auto! max-h-[calc(100vh-16px)] max-w-[min(360px,calc(100vw-16px))] menu-surface min-w-32 min-w-[248px] motion-reduce:animate-none narrow:[&_.menu-item]:min-h-11! narrow:animate-[sheet-up_0.18s_ease-out] narrow:border-border narrow:border-t narrow:overscroll-contain narrow:pb-[max(8px,var(--safe-b))] narrow:pl-[max(8px,var(--safe-l))] narrow:pr-[max(8px,var(--safe-r))] narrow:pt-0 narrow:ring-0 narrow:rounded-b-none narrow:rounded-t-xl narrow:shadow-[0_-12px_40px_-12px_var(--edge-shadow)] narrow:z-(--z-menu-surface) overflow-x-hidden overflow-y-auto p-1 ring-1 ring-foreground/10 rounded-lg shadow-md text-popover-foreground zoom-in-95">
      <div class="-mx-1 [&_.session-place]:flex-none border-b border-border context-menu-head flex flex-col gap-px mb-1 pb-1.5 pt-1 px-2.5">
        <span class="context-menu-title flex font-medium gap-1.5 items-center text-[13px] text-foreground truncate"> Landing page </span>
        <span title="~/src/site"
              class="context-menu-sub font-mono session-place text-muted-foreground text-xs truncate">
          <span class="session-repo"> site </span>
          <span class="session-branch">
            <icon class="branch"> </icon>
            trunk
          </span>
        </span>
      </div>
      <div data-slot="dropdown-menu-group"
           role="group"
           class=" [&+&]:-mx-1 [&+&]:border-border [&+&]:border-t [&+&]:mt-1 [&+&]:pt-1 [&+&]:px-1 flex flex-col menu-section">
        <div data-slot="dropdown-menu-item"
             data-highlighted=""
             role="menuitem"
             data-item="open"
             class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none selected text-sm"
             @on_click
             @on_mouseenter>
          <icon class="arrow_down"> </icon>
          <span class="menu-label truncate"> Open </span>
          <kbd data-slot="dropdown-menu-shortcut"
               class="font-mono group-data-highlighted/dropdown-menu-item:text-accent-foreground group-hover/dropdown-menu-item:text-accent-foreground menu-command min-w-0 ml-auto pl-3 text-muted-foreground text-xs truncate"> Enter </kbd>
        </div>
        <div data-slot="dropdown-menu-item"
             role="menuitem"
             data-item="open_window"
             class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm"
             @on_click
             @on_mouseenter>
          <icon class="external"> </icon>
          <span class="menu-label truncate"> Open in new window </span>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        </div>
      </div>
      <div data-slot="dropdown-menu-group"
           role="group"
           class=" [&+&]:-mx-1 [&+&]:border-border [&+&]:border-t [&+&]:mt-1 [&+&]:pt-1 [&+&]:px-1 flex flex-col menu-section">
        <div data-slot="dropdown-menu-item"
             role="menuitem"
             data-item="rename"
             class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm"
             @on_click
             @on_mouseenter>
          <icon class="pencil"> </icon>
          <span class="menu-label truncate"> Rename… </span>
          <kbd data-slot="dropdown-menu-shortcut"
               class="font-mono group-data-highlighted/dropdown-menu-item:text-accent-foreground group-hover/dropdown-menu-item:text-accent-foreground menu-command min-w-0 ml-auto pl-3 text-muted-foreground text-xs truncate"> /name </kbd>
        </div>
        <div data-slot="dropdown-menu-item"
             role="menuitem"
             data-item="fork"
             class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm"
             @on_click
             @on_mouseenter>
          <icon class="fork"> </icon>
          <span class="menu-label truncate"> Fork… </span>
          <kbd data-slot="dropdown-menu-shortcut"
               class="font-mono group-data-highlighted/dropdown-menu-item:text-accent-foreground group-hover/dropdown-menu-item:text-accent-foreground menu-command min-w-0 ml-auto pl-3 text-muted-foreground text-xs truncate"> /fork </kbd>
        </div>
        <div data-slot="dropdown-menu-item"
             role="menuitem"
             data-item="new_worktree"
             class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm"
             @on_click
             @on_mouseenter>
          <icon class="branch_plus"> </icon>
          <span class="menu-label truncate"> New worktree… </span>
          <kbd data-slot="dropdown-menu-shortcut"
               class="font-mono group-data-highlighted/dropdown-menu-item:text-accent-foreground group-hover/dropdown-menu-item:text-accent-foreground menu-command min-w-0 ml-auto pl-3 text-muted-foreground text-xs truncate"> /worktree new </kbd>
        </div>
      </div>
      <div data-slot="dropdown-menu-group"
           role="group"
           class=" [&+&]:-mx-1 [&+&]:border-border [&+&]:border-t [&+&]:mt-1 [&+&]:pt-1 [&+&]:px-1 flex flex-col menu-section">
        <div data-slot="dropdown-menu-item"
             role="menuitem"
             data-item="review"
             class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm"
             @on_click
             @on_mouseenter>
          <icon class="review"> </icon>
          <span class="menu-label truncate"> Review changes </span>
          <kbd data-slot="dropdown-menu-shortcut"
               class="font-mono group-data-highlighted/dropdown-menu-item:text-accent-foreground group-hover/dropdown-menu-item:text-accent-foreground menu-command min-w-0 ml-auto pl-3 text-muted-foreground text-xs truncate"> /review </kbd>
        </div>
        <div data-slot="dropdown-menu-item"
             role="menuitem"
             data-item="files"
             class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm"
             @on_click
             @on_mouseenter>
          <icon class="folder"> </icon>
          <span class="menu-label truncate"> Open in file browser </span>
          <kbd data-slot="dropdown-menu-shortcut"
               class="font-mono group-data-highlighted/dropdown-menu-item:text-accent-foreground group-hover/dropdown-menu-item:text-accent-foreground menu-command min-w-0 ml-auto pl-3 text-muted-foreground text-xs truncate"> /files </kbd>
        </div>
        <div data-slot="dropdown-menu-item"
             role="menuitem"
             data-item="terminal"
             class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm"
             @on_click
             @on_mouseenter>
          <icon class="terminal"> </icon>
          <span class="menu-label truncate"> Open terminal there </span>
          <kbd data-slot="dropdown-menu-shortcut"
               class="font-mono group-data-highlighted/dropdown-menu-item:text-accent-foreground group-hover/dropdown-menu-item:text-accent-foreground menu-command min-w-0 ml-auto pl-3 text-muted-foreground text-xs truncate"> /terminal </kbd>
        </div>
      </div>
      <div data-slot="dropdown-menu-group"
           role="group"
           class=" [&+&]:-mx-1 [&+&]:border-border [&+&]:border-t [&+&]:mt-1 [&+&]:pt-1 [&+&]:px-1 flex flex-col menu-section">
        <div data-slot="dropdown-menu-item"
             role="menuitem"
             data-item="copy_path"
             class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm"
             @on_click
             @on_mouseenter>
          <icon class="copy"> </icon>
          <span class="menu-label truncate"> Copy path </span>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        </div>
        <div data-slot="dropdown-menu-item"
             role="menuitem"
             data-item="copy_id"
             class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm"
             @on_click
             @on_mouseenter>
          <icon class="copy"> </icon>
          <span class="menu-label truncate"> Copy session id </span>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        </div>
      </div>
      <div data-slot="dropdown-menu-group"
           role="group"
           class=" [&+&]:-mx-1 [&+&]:border-border [&+&]:border-t [&+&]:mt-1 [&+&]:pt-1 [&+&]:px-1 flex flex-col menu-section">
        <div data-slot="dropdown-menu-item"
             role="menuitem"
             data-item="pin"
             class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm"
             @on_click
             @on_mouseenter>
          <icon class="pin"> </icon>
          <span class="menu-label truncate"> Pin to top </span>
          <kbd data-slot="dropdown-menu-shortcut"
               class="font-mono group-data-highlighted/dropdown-menu-item:text-accent-foreground group-hover/dropdown-menu-item:text-accent-foreground menu-command min-w-0 ml-auto pl-3 text-muted-foreground text-xs truncate"> P </kbd>
        </div>
        <div data-slot="dropdown-menu-item"
             role="menuitem"
             data-item="archive"
             class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm"
             @on_click
             @on_mouseenter>
          <icon class="archive"> </icon>
          <span class="menu-label truncate"> Archive </span>
          <kbd data-slot="dropdown-menu-shortcut"
               class="font-mono group-data-highlighted/dropdown-menu-item:text-accent-foreground group-hover/dropdown-menu-item:text-accent-foreground menu-command min-w-0 ml-auto pl-3 text-muted-foreground text-xs truncate"> A </kbd>
        </div>
        <div data-slot="dropdown-menu-item"
             role="menuitem"
             data-variant="destructive"
             data-item="delete"
             class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default danger data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm"
             @on_click
             @on_mouseenter>
          <icon class="trash"> </icon>
          <span class="menu-label truncate"> Delete… </span>
          <kbd data-slot="dropdown-menu-shortcut"
               class="font-mono group-data-highlighted/dropdown-menu-item:text-accent-foreground group-hover/dropdown-menu-item:text-accent-foreground menu-command min-w-0 ml-auto pl-3 text-muted-foreground text-xs truncate"> Delete </kbd>
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
    Open in new window
    Rename… /name
    Fork… /fork
    New worktree… /worktree new
    Review changes /review
    Open in file browser /files
    Open terminal there /terminal
    Copy path
    Copy session id
    Pin to top P
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
    Open in new window
    Rename… /name
    Fork… /fork
    Review changes /review
    Open in file browser /files
    Open terminal there /terminal
    Copy path
    Copy session id
    Pin to top P
    Archive A
    Delete… Delete
    Close_dialog
    (Focus editor)
    (Open_menu
     ((target (Session /sessions/s6.jsonl)) (anchor (Element more-session-s6))))
    (Focus dialog)
    (Place_menu (Element more-session-s6))
    [menuitem] div "Open Enter"
    [menuitem] div "Open in new window"
    [menuitem] div "Rename\226\128\166 /name"
    [menuitem] div "Fork\226\128\166 /fork"
    [menuitem] div "New worktree\226\128\166 /worktree new"
    [menuitem] div "Review changes /review"
    [menuitem] div "Open in file browser /files"
    [menuitem] div "Open terminal there /terminal"
    [menuitem] div "Copy path"
    [menuitem] div "Copy session id"
    [menuitem] div "Pin to top P"
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
    <div class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none selected text-sm">
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
    (Copy (text /home/u/src/prigh) (copied "Copied the path: /home/u/src/prigh"))
    (Expire_toast (id 0) (after_ms 2860))
    (Open_menu
     ((target (Session /sessions/s1.jsonl)) (anchor (Element session-s1))))
    (Focus dialog)
    (Place_menu (Element session-s1))
    (Picker_choose copy_id)
    (Focus editor)
    (Copy (text s1) (copied "Copied the session id: s1"))
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
    Delete “Main work”?
    (Close (Esc))
    You'll move to “Login form”.
    It will be deleted, with its subagent logs: 0 B on disk. This can't be undone.
    Main work 0 B
    (Cancel) (Delete)
    |}]
;;

let%expect_test "a worktree's menu and its items" =
  let h = create () in
  let on_login id =
    worktree_menu h ~owner:"s1" login;
    choose h id
  in
  worktree_menu h ~owner:"s1" login;
  menu h;
  [%expect
    {|
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
    (Focus dialog)
    (Place_menu (Element worktree-s1--home-u--prigh-worktrees-prigh-feat-login))
    (Picker_choose files)
    (Focus editor)
    (Rpc (method_ fs_list)
     (params ((path /home/u/.prigh/worktrees/prigh/feat-login) (hidden false)))
     (tag (Files (Listed /home/u/.prigh/worktrees/prigh/feat-login))))
    (Files (Focus files-list))
    (Focus dialog)
    (Place_menu (Element worktree-s1--home-u--prigh-worktrees-prigh-feat-login))
    (Picker_choose terminal)
    (Focus editor)
    (Cache_prefs (((key web.terminal-open) (value (true)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.terminal-open true)))))
     (tag Show_error))
    Focus_terminal
    (Focus dialog)
    (Place_menu (Element worktree-s1--home-u--prigh-worktrees-prigh-feat-login))
    (Picker_choose copy_path)
    (Focus editor)
    (Copy (text /home/u/.prigh/worktrees/prigh/feat-login)
     (copied "Copied the path: /home/u/.prigh/worktrees/prigh/feat-login"))
    (Expire_toast (id 0) (after_ms 3820))
    |}];
  on_login "switch_here";
  [%expect
    {|
    (Focus dialog)
    (Place_menu (Element worktree-s1--home-u--prigh-worktrees-prigh-feat-login))
    (Picker_choose switch_here)
    (Focus editor)
    (Rpc (method_ set_cwd)
     (params ((path /home/u/.prigh/worktrees/prigh/feat-login) (worktree true)))
     (tag
      (Plan_entered
       ((switch (/sessions/s1.jsonl)) (enter ()) (run ())
        (notice
         ("Working in the worktree feat/login (/home/u/.prigh/worktrees/prigh/feat-login)"))))))
    |}];
  on_login "new_session_here";
  [%expect
    {|
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
  worktree_menu h ~owner:"s6" redesign;
  choose h "create_pr";
  [%expect
    {|
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
  worktree_menu h ~owner:"s6" redesign;
  choose h "remove";
  dialog h;
  [%expect
    {|
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
  H.elements h ~selector:".more";
  [%expect
    {|
    (Mark_session (path /sessions/s1.jsonl) (range false))
    (Mark_session (path /sessions/s3.jsonl) (range false))
    (Archive) (Delete 2)
    <div class="bg-primary/15 bg-sidebar-accent group/session hover:bg-sidebar-accent marked relative rounded-md selected session text-sidebar-foreground">
    <div class="bg-primary/15 group/session hover:bg-sidebar-accent marked relative rounded-md session text-sidebar-foreground">
    |}]
;;

(* A session that switched into a worktree remembers its home: the row says
   where from, its menu offers to switch back. *)

let home_sessions () =
  sprintf
    "[%s]"
    (String.concat
       ~sep:","
       [ H.session_json
           ~name:"Login form"
           ~cwd:login
           ~repo:(prigh, login, Some "feat/login")
           ~live:true
           ~worktrees:[ login, Some "feat/login", prigh, "entered" ]
           ~home:(prigh, Some "main")
           "s1"
       ; H.session_json
           ~name:"Notes"
           ~cwd:redesign
           ~repo:(site, redesign, Some "redesign")
           ~worktrees:[ redesign, Some "redesign", site, "entered" ]
           ~home:("/home/u/notes", None)
           "s2"
       ; H.session_json
           ~name:"Landing page"
           ~cwd:site
           ~repo:(site, site, Some "trunk")
           "s3"
       ])
;;

let home_state ?home () =
  H.state_json
    ~fields:
      ([ "session_id", `String "s1"
       ; "session_path", `String "/sessions/s1.jsonl"
       ; "cwd", `String login
       ; "git_branch", `String "feat/login"
       ]
       @ Option.value_map home ~default:[] ~f:(fun home ->
         [ "home", Jsonaf.of_string (H.home_json home) ]))
    ()
;;

let%expect_test
    "a switched session: from main on its row, Switch back first in its menu"
  =
  let h =
    H.create
      ~state:(home_state ~home:(prigh, Some "main") ())
      ~sessions:(home_sessions ())
      ()
  in
  list h;
  H.elements h ~selector:".session-home";
  [%expect
    {|
    (prigh 2m Login form) (More (right-click, Shift+F10)) (Archive) (Delete)
    (site 2m Notes) (More (right-click, Shift+F10)) (Archive) (Delete)
    (site 2m Landing page) (More (right-click, Shift+F10)) (Archive) (Delete)
    |}];
  H.right_click h ~selector:".session" ~nth:0;
  menu h;
  H.key h "Escape" ~target:Page;
  H.right_click h ~selector:".session" ~nth:1;
  menu h;
  H.key h "Escape" ~target:Page;
  H.right_click h ~selector:".session" ~nth:2;
  menu h;
  [%expect
    {|
    (Open_menu
     ((target (Session /sessions/s1.jsonl)) (anchor (Element session-s1))))
    (Focus dialog)
    (Place_menu (Element session-s1))
    Login form prigh feat/login
    Switch back to main /worktree back
    Open in new window
    Rename… /name
    Fork… /fork
    New worktree… /worktree new
    Review changes /review
    Open in file browser /files
    Open terminal there /terminal
    Copy path
    Copy session id
    Pin to top P
    Archive A
    Delete… Delete
    Close_dialog
    (Focus editor)
    (Open_menu
     ((target (Session /sessions/s2.jsonl)) (anchor (Element session-s2))))
    (Focus dialog)
    (Place_menu (Element session-s2))
    Notes site redesign
    Switch back to ~/notes /worktree back
    Open Enter
    Open in new window
    Rename… /name
    Fork… /fork
    New worktree… /worktree new
    Review changes /review
    Open in file browser /files
    Open terminal there /terminal
    Copy path
    Copy session id
    Pin to top P
    Archive A
    Delete… Delete
    Close_dialog
    (Focus editor)
    (Open_menu
     ((target (Session /sessions/s3.jsonl)) (anchor (Element session-s3))))
    (Focus dialog)
    (Place_menu (Element session-s3))
    Landing page site trunk
    Open Enter
    Open in new window
    Rename… /name
    Fork… /fork
    New worktree… /worktree new
    Review changes /review
    Open in file browser /files
    Open terminal there /terminal
    Copy path
    Copy session id
    Pin to top P
    Archive A
    Delete… Delete
    |}]
;;

let%expect_test
    "a switched session's home is shown with its tool host's home directory"
  =
  let sessions =
    sprintf
      "[%s]"
      (String.concat
         ~sep:","
         [ H.session_json
             ~name:"Laptop"
             ~cwd:"/Users/ann/wt"
             ~host:"laptop"
             ~home:("/Users/ann/notes", None)
             "s1"
         ; H.session_json
             ~name:"Server"
             ~cwd:"/home/prigh/wt"
             ~host:"backend"
             ~home:("/home/bob/notes", None)
             "s2"
         ])
  in
  let state =
    H.state_json
      ~fields:
        [ "session_id", `String "s9"
        ; "active_host", `String "laptop"
        ; ( "hosts"
          , Jsonaf.of_string
              {|[{"id":"backend","name":"srv","cwd":"/home/prigh","home":"/home/prigh"},{"id":"laptop","name":"laptop","cwd":"/Users/ann","home":"/Users/ann"}]|}
          )
        ]
      ()
  in
  let h = H.create ~sessions ~state () in
  H.text h ~selector:".session-home";
  H.right_click h ~selector:".session" ~nth:0;
  H.text h ~selector:".context-menu [data-item=switch_back]";
  H.key h "Escape" ~target:Page;
  H.right_click h ~selector:".session" ~nth:1;
  H.text h ~selector:".context-menu [data-item=switch_back]";
  [%expect
    {|
    (Open_menu
     ((target (Session /sessions/s1.jsonl)) (anchor (Element session-s1))))
    (Focus dialog)
    (Place_menu (Element session-s1))
    Switch back to ~/notes /worktree back
    Close_dialog
    (Focus editor)
    (Open_menu
     ((target (Session /sessions/s2.jsonl)) (anchor (Element session-s2))))
    (Focus dialog)
    (Place_menu (Element session-s2))
    Switch back to /home/bob/notes /worktree back
    |}]
;;

let%expect_test "Switch back runs /worktree back; the session's home goes" =
  let h =
    H.create
      ~state:(home_state ~home:(prigh, Some "main") ())
      ~sessions:(home_sessions ())
      ()
  in
  H.right_click h ~selector:".session" ~nth:0;
  choose h "switch_back";
  H.reply h "worktree_back" (sprintf {|{"cwd":%S,"created":false}|} prigh);
  H.text h ~selector:".toast";
  H.event h (sprintf {|{"event":"state","state":%s}|} (home_state ()));
  list h;
  [%expect
    {|
    (Open_menu
     ((target (Session /sessions/s1.jsonl)) (anchor (Element session-s1))))
    (Focus dialog)
    (Place_menu (Element session-s1))
    (Picker_choose switch_back)
    (Focus editor)
    (Rpc (method_ worktree_back) (params ())
     (tag
      (Worktree_went_back (command "/worktree back")
       (home
        (((cwd /home/u/src/prigh) (host (backend)) (host_name (srv))
          (repo
           (((root /home/u/src/prigh) (worktree /home/u/src/prigh)
             (branch (main)) (main true))))))))))
    (Expire_toast (id 0) (after_ms 2540))
    Back in main (~/src/prigh)
    (prigh 2m Login form) (More (right-click, Shift+F10)) (Archive) (Delete)
    (site 2m Notes) (More (right-click, Shift+F10)) (Archive) (Delete)
    (site 2m Landing page) (More (right-click, Shift+F10)) (Archive) (Delete)
    |}]
;;

let%expect_test "/worktree back: without a home, or when it fails" =
  let h = H.create ~state:(home_state ()) ~sessions:(home_sessions ()) () in
  H.type_ h "/worktree back";
  H.act h Send;
  H.text h ~selector:".toast";
  let h =
    H.create
      ~state:(home_state ~home:(prigh, Some "main") ())
      ~sessions:(home_sessions ())
      ()
  in
  H.type_ h "/worktree back main";
  H.act h Send;
  H.fail
    h
    "worktree_back"
    "the tool host srv that /home/u/src/prigh is on is not connected";
  H.text h ~selector:".toast";
  H.text h ~selector:".composer textarea";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/worktree back") (max 500))) (tag Ignore))
    This session hasn't switched into a worktree; /worktree open TARGET switches. (Dismiss)
    (Rpc (method_ push_pref)
     (params ((key history) (value "/worktree back main") (max 500)))
     (tag Ignore))
    (Rpc (method_ worktree_back) (params ((main true)))
     (tag
      (Worktree_went_back (command "/worktree back main")
       (home
        (((cwd /home/u/src/prigh) (host (backend)) (host_name (srv))
          (repo
           (((root /home/u/src/prigh) (worktree /home/u/src/prigh)
             (branch (main)) (main true))))))))))
    Couldn't switch back: the tool host srv that /home/u/src/prigh is on is not connected. (Dismiss)
    [/worktree back main]
    |}]
;;

(* A session in a new window: its menu's Open in new window, a middle click
   (a Ctrl or Cmd click selects it: test_sidebar_rows); a plain click switches
   to it. *)
let%expect_test "a session in a new window" =
  let h = create () in
  H.right_click h ~selector:".session" ~nth:1;
  choose h "open_window";
  print_endline "-- middle click:";
  H.middle_click h ~selector:".session-open" ~nth:1;
  print_endline "-- a plain click:";
  H.modified_click h ~selector:".session-open" ~nth:1;
  [%expect
    {|
    (Open_menu
     ((target (Session /sessions/s2.jsonl)) (anchor (Element session-s2))))
    (Focus dialog)
    (Place_menu (Element session-s2))
    (Picker_choose open_window)
    (Focus editor)
    (Open_window s2)
    -- middle click:
    (Open_session_window s2)
    (Open_window s2)
    -- a plain click:
    (Switch_session /sessions/s2.jsonl)
    (Rpc (method_ switch_session) (params ((path /sessions/s2.jsonl)))
     (tag Reload_state))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before ()))))
    (Focus editor)
    |}]
;;
