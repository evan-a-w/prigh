open! Core
module H = Harness

(* Archived sessions, selecting several, deleting them, and sessions that stay
   active while their background work runs. *)

let sessions =
  sprintf
    "[%s]"
    (String.concat
       ~sep:","
       [ H.session_json
           ~first_prompt:"fix the parser bug"
           ~live:true
           ~clients:1
           ~bytes:4_000
           "s1"
       ; H.session_json
           ~name:"Release notes"
           ~live:true
           ~running:true
           ~background:2
           ~bytes:12_000
           "s2"
       ; H.session_json
           ~name:"Open elsewhere"
           ~live:true
           ~clients:1
           ~bytes:3_000
           "s3"
       ; H.session_json ~name:"Old lexer" ~archived:true ~bytes:200_000 "s4"
       ; H.session_json ~name:"Older docs" ~archived:true ~bytes:1_300_000 "s5"
       ])
;;

let sidebar h =
  H.text h ~selector:".sidebar .sessions-toolbar";
  H.text h ~selector:".sidebar .sessions";
  H.text h ~selector:".sidebar .selection-bar"
;;

let%expect_test "active sessions first, then the archived ones under a divider" =
  let h = H.create ~sessions () in
  sidebar h;
  [%expect
    {|
    Sessions (Select)
    (work 2m fix the parser bug) (More (right-click, Shift+F10)) (Archive) (Delete)
    (work working… Release notes) (More (right-click, Shift+F10)) (Archive) (Delete)
    (work 2m Open elsewhere) (More (right-click, Shift+F10)) (Archive) (Delete)
    |}];
  H.elements h ~selector:".session";
  [%expect
    {|
    <div class="bg-sidebar-accent group/session hover:bg-sidebar-accent relative rounded-md selected session text-sidebar-foreground">
    <div class="group/session hover:bg-sidebar-accent relative rounded-md running session text-sidebar-foreground">
    <div class="group/session hover:bg-sidebar-accent relative rounded-md session text-sidebar-foreground">
    |}];
  (* A session whose turn has ended but whose background work runs is
     running. *)
  H.elements h ~selector:".session-mark > *";
  [%expect {| |}];
  (* The archived sessions are folded under their divider: a click unfolds
     them (ending with a way to delete them all) and the keyboard reaches
     them; folded, it skips them. *)
  H.show h ~selector:".sessions-divider";
  H.act h Toggle_archived;
  sidebar h;
  H.elements h ~selector:".sessions-divider[aria-expanded=true]";
  H.elements h ~selector:".session.archived .lamp";
  H.elements h ~selector:".session.archived .live";
  [%expect
    {|
    <button type="button"
            data-slot="button"
            data-variant="ghost"
            data-size="sm"
            id="archive-toggle"
            aria-expanded="false"
            title="Show the archived sessions (right-click: more)"
            class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 active:not-aria-[haspopup]:translate-y-px archive-toggle aria-expanded:bg-muted aria-expanded:text-foreground aria-invalid:border-destructive aria-invalid:ring-3 aria-invalid:ring-destructive/20 bg-clip-padding border border-transparent dark:aria-invalid:border-destructive/50 dark:aria-invalid:ring-destructive/40 dark:hover:bg-muted/50 disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-3 focus-visible:ring-ring/50 font-normal gap-2 group/button h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground in-data-[slot=button-group]:rounded-lg inline-flex items-center justify-start narrow:h-11 outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg select-none sessions-divider shrink-0 text-[0.8rem] text-muted-foreground transition-all w-full whitespace-nowrap"
            @on_click
            @on_contextmenu>
      <icon class="archive"> </icon>
      <span class="divider-label"> Archive </span>
      <span class="divider-count ml-auto tabular-nums text-xs"> 2 </span>
      <icon class="next"> </icon>
    </button>
    Sessions (Select)
    (work 2m fix the parser bug) (More (right-click, Shift+F10)) (Archive) (Delete)
    (work working… Release notes) (More (right-click, Shift+F10)) (Archive) (Delete)
    (work 2m Open elsewhere) (More (right-click, Shift+F10)) (Archive) (Delete)
    Archived 2
    (work 2m Old lexer) (More (right-click, Shift+F10)) (Unarchive) (Delete)
    (work 2m Older docs) (More (right-click, Shift+F10)) (Unarchive) (Delete)
    (Delete archived)
    <button class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 active:not-aria-[haspopup]:translate-y-px archive-toggle aria-expanded:bg-muted aria-expanded:text-foreground aria-invalid:border-destructive aria-invalid:ring-3 aria-invalid:ring-destructive/20 bg-clip-padding border border-transparent dark:aria-invalid:border-destructive/50 dark:aria-invalid:ring-destructive/40 dark:hover:bg-muted/50 disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-3 focus-visible:ring-ring/50 font-normal gap-2 group/button h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground in-data-[slot=button-group]:rounded-lg inline-flex items-center justify-start narrow:h-11 outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg select-none sessions-divider shrink-0 text-[0.8rem] text-muted-foreground transition-all w-full whitespace-nowrap">
    |}];
  H.key h "ArrowDown" ~target:(Session "s3");
  H.act h Toggle_archived;
  H.key h "ArrowDown" ~target:(Session "s3");
  H.text h ~selector:".sessions";
  [%expect
    {|
    (Session_nav (from s3) (by 1))
    (Focus session-s4)
    (Session_nav (from s3) (by 1))
    (Focus session-s3)
    (work 2m fix the parser bug) (More (right-click, Shift+F10)) (Archive) (Delete)
    (work working… Release notes) (More (right-click, Shift+F10)) (Archive) (Delete)
    (work 2m Open elsewhere) (More (right-click, Shift+F10)) (Archive) (Delete)
    |}];
  (* The control centre finds the archived sessions too, folded or not. *)
  H.act h Open_control;
  H.act h (Control_query "old");
  H.text h ~selector:".control-list";
  [%expect
    {|
    (Focus control-input)
    (Search_sessions_later (search 1) (after_ms 150))
    Settings
    Mask old tool outputs
    on
    Sessions
    Old lexer
    2m ago · /work
    Old er docs
    2m ago · /work
    Commands
    /compact
    summarise older messages to free context
    /compaction
    show when conversations are compacted and what summarises them; at TOKENS|off: also once the context exceeds TOKENS, not only at 80% of the window; model MODEL|off: the model that writes the summaries (off: the session's); mask on|off: replace old tool outputs with placeholders first
    |}]
;;

let%expect_test "the current session is active while its subagents or jobs run" =
  let h = H.create ~sessions () in
  let state fields =
    H.event
      h
      (sprintf {|{"event":"state","state":%s}|} (H.state_json ~fields ()))
  in
  state
    [ ( "subagents"
      , Jsonaf.of_string {|[{"id":"a1","task":"survey","running":true}]|} )
    ];
  H.elements h ~selector:"#session-s1 .session-mark > *";
  [%expect {| (Rpc (method_ list_subagents) (params ()) (tag Subagents)) |}];
  state
    [ ( "subagents"
      , Jsonaf.of_string {|[{"id":"a1","task":"survey","running":false}]|} )
    ];
  H.elements h ~selector:"#session-s1 .session-mark > *";
  [%expect {| (Rpc (method_ list_subagents) (params ()) (tag Subagents)) |}];
  state
    [ ( "jobs"
      , Jsonaf.of_string
          {|[{"id":"j1","command":"make","running":true,"exit":null}]|} )
    ];
  H.elements h ~selector:"#session-s1 .session-mark > *";
  [%expect
    {|
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    |}]
;;

let%expect_test "the list follows sessions_changed, with no polling" =
  let h = H.create ~sessions () in
  H.act h (Tick H.now);
  [%expect {| |}];
  H.event h {|{"event":"agent_end","messages":[]}|};
  [%expect {| |}];
  H.event h {|{"event":"sessions_changed"}|};
  [%expect {| (Rpc (method_ list_sessions) (params ()) (tag Sessions)) |}]
;;

let%expect_test
    "sessions_changed with entries updates the list without a refetch"
  =
  let h = H.create ~sessions () in
  H.event
    h
    (sprintf
       {|{"event":"sessions_changed","changed":[%s,%s],"removed":["s3"]}|}
       (H.session_json ~name:"Release notes, renamed" ~bytes:12_000 "s2")
       (H.session_json
          ~name:"Brand new"
          ~updated_at:"2026-10-05 09:59:00Z"
          "s6"));
  sidebar h;
  [%expect
    {|
    Sessions (Select)
    (work 1m Brand new) (More (right-click, Shift+F10)) (Archive) (Delete)
    (work 2m fix the parser bug) (More (right-click, Shift+F10)) (Archive) (Delete)
    (work 2m Release notes, renamed) (More (right-click, Shift+F10)) (Archive) (Delete)
    |}]
;;

let%expect_test
    "a list asked for before a change is not shown over it: it is asked for \
     again"
  =
  let h = H.create ~sessions () in
  H.event h {|{"event":"sessions_changed"}|};
  [%expect {| (Rpc (method_ list_sessions) (params ()) (tag Sessions)) |}];
  (* The client hands the answer on after a change that came after it. *)
  H.event
    h
    (sprintf
       {|{"event":"sessions_changed","changed":[%s],"removed":[]}|}
       (H.session_json
          ~name:"Brand new"
          ~updated_at:"2026-10-05 09:59:00Z"
          "s6"));
  H.reply h "list_sessions" sessions;
  [%expect {| (Rpc (method_ list_sessions) (params ()) (tag Sessions)) |}];
  sidebar h;
  [%expect
    {|
    Sessions (Select)
    (work 1m Brand new) (More (right-click, Shift+F10)) (Archive) (Delete)
    (work 2m fix the parser bug) (More (right-click, Shift+F10)) (Archive) (Delete)
    (work working… Release notes) (More (right-click, Shift+F10)) (Archive) (Delete)
    (work 2m Open elsewhere) (More (right-click, Shift+F10)) (Archive) (Delete)
    |}];
  (* Asked for after the change: shown. *)
  H.reply h "list_sessions" sessions;
  [%expect {| |}];
  sidebar h;
  [%expect
    {|
    Sessions (Select)
    (work 2m fix the parser bug) (More (right-click, Shift+F10)) (Archive) (Delete)
    (work working… Release notes) (More (right-click, Shift+F10)) (Archive) (Delete)
    (work 2m Open elsewhere) (More (right-click, Shift+F10)) (Archive) (Delete)
    |}]
;;

let%expect_test "archiving and unarchiving: at once, and back if refused" =
  let h = H.create ~sessions () in
  H.act h (Set_archived { path = "/sessions/s1.jsonl"; archived = true });
  [%expect
    {|
    (Rpc (method_ set_archived)
     (params ((path /sessions/s1.jsonl) (archived true)))
     (tag (Archived_set (title "fix the parser bug") (archived true))))
    |}];
  sidebar h;
  [%expect
    {|
    Sessions (Select)
    (work working… Release notes) (More (right-click, Shift+F10)) (Archive) (Delete)
    (work 2m Open elsewhere) (More (right-click, Shift+F10)) (Archive) (Delete)
    |}];
  H.reply h "set_archived" "{}";
  H.text h ~selector:".toast";
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 4020))
    Archived “fix the parser bug” (a new message unarchives it)
    |}];
  (* Running sessions are refused here, before asking the backend. *)
  H.act h (Set_archived { path = "/sessions/s2.jsonl"; archived = true });
  H.text h ~selector:".toast.error";
  [%expect
    {| “Release notes” is running: wait for it or stop it (Esc in it) before archiving. (Dismiss) |}];
  (* The backend refusing puts it back: the list is fetched again. *)
  H.act h (Set_archived { path = "/sessions/s4.jsonl"; archived = false });
  H.fail h "set_archived" "no such session";
  [%expect
    {|
    (Rpc (method_ set_archived)
     (params ((path /sessions/s4.jsonl) (archived false)))
     (tag (Archived_set (title "Old lexer") (archived false))))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    |}];
  H.text h ~selector:".toast.error";
  [%expect
    {|
    “Release notes” is running: wait for it or stop it (Esc in it) before archiving. (Dismiss)
    Couldn't unarchive “Old lexer”: no such session (Dismiss)
    |}]
;;

let%expect_test "A on a session archives or unarchives it" =
  let h = H.create ~sessions () in
  H.key h "a" ~target:(Session "s3");
  H.key h "a" ~target:(Session "s4");
  [%expect
    {|
    (Set_archived (path /sessions/s3.jsonl) (archived true))
    (Rpc (method_ set_archived)
     (params ((path /sessions/s3.jsonl) (archived true)))
     (tag (Archived_set (title "Open elsewhere") (archived true))))
    (Set_archived (path /sessions/s4.jsonl) (archived false))
    (Rpc (method_ set_archived)
     (params ((path /sessions/s4.jsonl) (archived false)))
     (tag (Archived_set (title "Old lexer") (archived false))))
    |}]
;;

let%expect_test "/archive and /unarchive: this session, or one by name or id" =
  let h = H.create ~sessions () in
  let run command =
    H.type_ h command;
    H.act h Send
  in
  run "/archive";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /archive) (max 500)))
     (tag Ignore))
    (Rpc (method_ set_archived)
     (params ((path /sessions/s1.jsonl) (archived true)))
     (tag (Archived_set (title "fix the parser bug") (archived true))))
    |}];
  run "/unarchive Old lexer";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/unarchive Old lexer") (max 500)))
     (tag Ignore))
    (Rpc (method_ set_archived)
     (params ((path /sessions/s4.jsonl) (archived false)))
     (tag (Archived_set (title "Old lexer") (archived false))))
    |}];
  (* A mistake stays in the editor, the error says what to do. *)
  run "/archive s";
  H.text h ~selector:".toast.error";
  H.text h ~selector:".composer textarea";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/archive s") (max 500))) (tag Ignore))
    “s” matches 5 sessions (s1, s2, s3): give more of the id. (Dismiss)
    [/archive s]
    |}]
;;

let%expect_test "selecting sessions: click, shift-click a range, delete them" =
  let h = H.create ~sessions () in
  H.act h Toggle_archived;
  H.act h (Select_sessions true);
  sidebar h;
  [%expect
    {|
    Select sessions Shift for a range 0 selected
    (☐ work 3.9 KB fix the parser bug)
    (☐ work 11.7 KB Release notes)
    (☐ work 2.9 KB Open elsewhere)
    Archived 2
    (☐ work 195.3 KB Old lexer)
    (☐ work 1.2 MB Older docs)
    (Cancel) (Archive) (Delete)
    |}];
  H.elements h ~selector:".selection-bar button[disabled]";
  [%expect
    {|
    <button class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 active:not-aria-[haspopup]:translate-y-px archive-marked aria-expanded:bg-muted aria-expanded:text-foreground aria-invalid:border-destructive aria-invalid:ring-3 aria-invalid:ring-destructive/20 bg-background bg-clip-padding border border-border dark:aria-invalid:border-destructive/50 dark:aria-invalid:ring-destructive/40 dark:bg-input/30 dark:border-input dark:hover:bg-input/50 disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-3 focus-visible:ring-ring/50 font-medium gap-1 group/button h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground in-data-[slot=button-group]:rounded-lg inline-flex items-center justify-center narrow:h-11 outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg select-none shrink-0 text-[0.8rem] transition-all whitespace-nowrap">
    <button class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 active:not-aria-[haspopup]:translate-y-px aria-invalid:border-destructive aria-invalid:ring-3 aria-invalid:ring-destructive/20 bg-clip-padding bg-destructive/10 border border-transparent danger dark:aria-invalid:border-destructive/50 dark:aria-invalid:ring-destructive/40 dark:bg-destructive/20 dark:focus-visible:ring-destructive/40 dark:hover:bg-destructive/30 disabled:opacity-50 disabled:pointer-events-none flex-1 focus-visible:border-destructive/40 focus-visible:ring-3 focus-visible:ring-destructive/20 font-medium gap-1 group/button h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-destructive/20 in-data-[slot=button-group]:rounded-lg inline-flex items-center justify-center min-w-0 narrow:h-11 outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg select-none shrink-0 text-[0.8rem] text-destructive transition-all truncate whitespace-nowrap">
    |}];
  (* A click marks; a shift-click marks everything from the last click,
     across the divider. *)
  H.act h (Mark_session { path = "/sessions/s2.jsonl"; range = false });
  H.act h (Mark_session { path = "/sessions/s4.jsonl"; range = true });
  H.elements h ~selector:".session.marked";
  H.text h ~selector:".selection-bar";
  [%expect
    {|
    <div class="bg-primary/15 group/session hover:bg-sidebar-accent marked relative rounded-md running session text-sidebar-foreground">
    <div class="bg-primary/15 group/session hover:bg-sidebar-accent marked relative rounded-md session text-sidebar-foreground">
    <div class="archived bg-primary/15 group/session hover:bg-sidebar-accent marked relative rounded-md session text-sidebar-foreground">
    (Cancel) (Archive) (Delete 3)
    |}];
  (* Enter does what a click does (unmarking s3), Shift+Enter what a
     shift-click does (marking from s3, the last one, to s5); Space clicks
     the row's button itself. *)
  H.key h "Enter" ~target:(Session "s3");
  H.key h "Enter" ~shift:true ~target:(Session "s5");
  H.key h " " ~target:(Session "s5");
  H.elements h ~selector:".session.marked";
  [%expect
    {|
    (Mark_session (path /sessions/s3.jsonl) (range false))
    (Mark_session (path /sessions/s5.jsonl) (range true))
    (browser default)
    <div class="bg-primary/15 group/session hover:bg-sidebar-accent marked relative rounded-md running session text-sidebar-foreground">
    <div class="bg-primary/15 group/session hover:bg-sidebar-accent marked relative rounded-md session text-sidebar-foreground">
    <div class="archived bg-primary/15 group/session hover:bg-sidebar-accent marked relative rounded-md session text-sidebar-foreground">
    <div class="archived bg-primary/15 group/session hover:bg-sidebar-accent marked relative rounded-md session text-sidebar-foreground">
    |}];
  H.elements h ~selector:".session-open[aria-checked=true]";
  [%expect
    {|
    <button class="[@media(hover:none)]:pr-12 flex focus-visible:ring-2 focus-visible:ring-sidebar-ring gap-2 items-start min-w-0 narrow:py-2 outline-none px-2.5 py-1.5 rounded-md session-open text-left w-full">
    <button class="[@media(hover:none)]:pr-12 flex focus-visible:ring-2 focus-visible:ring-sidebar-ring gap-2 items-start min-w-0 narrow:py-2 outline-none px-2.5 py-1.5 rounded-md session-open text-left w-full">
    <button class="[@media(hover:none)]:pr-12 flex focus-visible:ring-2 focus-visible:ring-sidebar-ring gap-2 items-start min-w-0 narrow:py-2 outline-none px-2.5 py-1.5 rounded-md session-open text-left w-full">
    <button class="[@media(hover:none)]:pr-12 flex focus-visible:ring-2 focus-visible:ring-sidebar-ring gap-2 items-start min-w-0 narrow:py-2 outline-none px-2.5 py-1.5 rounded-md session-open text-left w-full">
    |}];
  (* The confirmation says how many, how much disk, and which will be kept
     (those are not asked for). *)
  H.act h Delete_marked;
  H.text h ~selector:".modal";
  [%expect
    {|
    (Focus dialog)
    Delete 2 sessions?
    (Close (Esc))
    They will be deleted, with their subagent logs: 1.4 MB on disk. This can't be undone.
    Release notes 11.7 KB
    Open elsewhere 2.9 KB
    Old lexer 195.3 KB
    Older docs 1.2 MB
    “Release notes” is running: it will be kept (wait for it or stop it first)
    “Open elsewhere” is open in another tab or terminal: it will be kept (close it there first)
    (Cancel) (Delete 2)
    |}];
  H.act h Dialog_accept;
  [%expect
    {|
    (Focus editor)
    (Rpc (method_ delete_sessions)
     (params ((paths (/sessions/s4.jsonl /sessions/s5.jsonl))))
     (tag Sessions_deleted))
    |}];
  H.reply
    h
    "delete_sessions"
    {|{"deleted":[{"path":"/sessions/s4.jsonl","id":"s4","bytes":200000},{"path":"/sessions/s5.jsonl","id":"s5","bytes":1300000}],
       "refused":[]}|};
  H.text h ~selector:".toast";
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 2580))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    Deleted 2 sessions (1.4 MB)
    |}];
  (* What was kept stays selected. *)
  sidebar h;
  [%expect
    {|
    Select sessions Shift for a range 2 selected
    (☐ work 3.9 KB fix the parser bug)
    (☑ work 11.7 KB Release notes)
    (☑ work 2.9 KB Open elsewhere)
    (Cancel) (Archive) (Delete 2)
    |}];
  H.key h "Escape" ~target:(Session "s2");
  sidebar h;
  [%expect
    {|
    (Select_sessions false)
    Sessions (Select)
    (work 2m fix the parser bug) (More (right-click, Shift+F10)) (Archive) (Delete)
    (work working… Release notes) (More (right-click, Shift+F10)) (Archive) (Delete)
    (work 2m Open elsewhere) (More (right-click, Shift+F10)) (Archive) (Delete)
    |}]
;;

let%expect_test "deleting one: where you move, and the open ones kept" =
  let h = H.create ~sessions () in
  H.act h (Ask_delete "/sessions/s1.jsonl");
  H.text h ~selector:".modal p";
  H.text h ~selector:".modal .delete-warnings";
  [%expect
    {|
    (Focus dialog)
    You'll move to “Release notes”.
    It will be deleted, with its subagent logs: 3.9 KB on disk. This can't be undone.
    |}];
  H.act h Close_dialog;
  H.key h "Delete" ~target:(Session "s3");
  H.text h ~selector:".modal .delete-warnings";
  [%expect
    {|
    (Focus editor)
    (Ask_delete /sessions/s3.jsonl)
    (Focus dialog)
    “Open elsewhere” is open in another tab or terminal: it will be kept (close it there first)
    |}];
  (* Esc keeps it. *)
  H.key h "Escape" ~target:Page;
  [%expect
    {|
    Close_dialog
    (Focus editor)
    |}];
  (* Nothing it could delete: it only closes. *)
  H.act h (Ask_delete "/sessions/s3.jsonl");
  H.text h ~selector:".modal";
  H.key h "Enter" ~target:Page;
  [%expect
    {|
    (Focus dialog)
    Can't delete this session
    (Close (Esc))
    Open elsewhere 2.9 KB
    “Open elsewhere” is open in another tab or terminal: it will be kept (close it there first)
    (Close)
    Dialog_accept
    (Focus editor)
    |}]
;;

let%expect_test "deleting every archived session" =
  let h = H.create ~sessions () in
  H.act h Ask_delete_archived;
  H.text h ~selector:".modal";
  [%expect
    {|
    (Focus dialog)
    Delete all 2 archived sessions?
    (Close (Esc))
    They will be deleted, with their subagent logs: 1.4 MB on disk. This can't be undone.
    Old lexer 195.3 KB
    Older docs 1.2 MB
    (Cancel) (Delete 2)
    |}];
  H.act h Dialog_accept;
  H.reply
    h
    "delete_archived"
    {|{"deleted":[{"path":"/sessions/s4.jsonl","id":"s4","bytes":200000},{"path":"/sessions/s5.jsonl","id":"s5","bytes":1300000}],"refused":[]}|};
  H.text h ~selector:".toast";
  [%expect
    {|
    (Focus editor)
    (Rpc (method_ delete_archived) (params ()) (tag Sessions_deleted))
    (Expire_toast (id 0) (after_ms 2580))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    Deleted 2 sessions (1.4 MB)
    |}];
  (* With none archived, there is nothing to ask. *)
  H.type_ h "/delete-archived";
  H.act h Send;
  H.text h ~selector:".toast";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value /delete-archived) (max 500))) (tag Ignore))
    (Expire_toast (id 1) (after_ms 2740))
    Deleted 2 sessions (1.4 MB)
    There are no archived sessions.
    |}]
;;

let%expect_test
    "/delete: sessions by name or id with a confirmation; alone, a message"
  =
  let h = H.create ~sessions () in
  H.type_ h "/delete s4 Older docs";
  H.act h Send;
  H.text h ~selector:".toast.error";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/delete s4 Older docs") (max 500)))
     (tag Ignore))
    No session “Older”: give its name, id or path (Ctrl+K searches them). (Dismiss)
    |}];
  H.type_ h "/delete s4 s5";
  H.act h Send;
  H.text h ~selector:".modal .delete-list";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/delete s4 s5") (max 500))) (tag Ignore))
    (Focus dialog)
    Old lexer 195.3 KB
    Older docs 1.2 MB
    |}];
  (* Alone it picks a message to delete (sessions are selected in the
     sidebar). *)
  H.act h Close_dialog;
  H.type_ h "/delete";
  H.act h Send;
  H.text h ~selector:".modal h2";
  [%expect
    {|
    (Focus editor)
    (Rpc (method_ push_pref) (params ((key history) (value /delete) (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ get_message_points) (params ())
     (tag (Message_points Delete_picker)))
    Delete a message
    |}]
;;
