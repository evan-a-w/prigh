open! Core
module H = Harness

let sessions =
  sprintf
    "[%s]"
    (String.concat
       ~sep:","
       [ H.session_json
           ~first_prompt:"fix the parser bug"
           ~live:true
           ~updated_at:"2026-10-05 09:59:40Z"
           ~messages:1
           "s1"
       ; H.session_json
           ~name:"Release notes"
           ~first_prompt:"write the release notes"
           ~cwd:"/home/ann/docs"
           ~live:true
           ~running:true
           ~updated_at:"2026-10-05 07:00:00Z"
           "s2"
       ; H.session_json
           ~description:"Refactor the lexer"
           ~first_prompt:"lexer please"
           ~cwd:"/home/ann/src/compiler"
           ~updated_at:"2026-09-20 10:00:00Z"
           ~messages:12
           "s3"
       ])
;;

let%expect_test
    "the sidebar: titles, ages, cwd, sizes, live and running, current"
  =
  let h = H.create ~sessions () in
  H.text h ~selector:".sessions";
  [%expect
    {|
    (fix the parser bug just now /work) (Rename (/name)) (Archive (A)) (Delete session (Delete))
    (Release notes 3h ago ~/docs) (Archive (A)) (Delete session (Delete))
    (Refactor the lexer 15d ago ~/src/compiler) (Archive (A)) (Delete session (Delete))
    |}];
  H.show h ~selector:".session.selected";
  [%expect
    {|
    <div class="selected session" @on_pointerenter>
      <button type="button"
              id="session-s1"
              data-session="s1"
              title="/sessions/s1.jsonl"
              aria-current="true"
              class="session-open"
              @on_click
              @on_focus
              @on_pointerdown>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        <div class="session-mark">
          <span title="Open in the backend" class="live">  </span>
        </div>
        <div class="session-main">
          <span dir="auto" class="session-title"> fix the parser bug </span>
          <div class="session-meta">
            <span class="session-age"> just now </span>
            <span title="/work" class="session-cwd"> /work </span>
            <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          </div>
        </div>
      </button>
      <span class="session-actions">
        <button type="button"
                title="Rename (/name)"
                aria-label="Rename fix the parser bug"
                class="btn ghost icon rename"
                @on_click>
          <icon class="pencil"> </icon>
        </button>
        <button type="button"
                title="Archive (A)"
                aria-label="Archive fix the parser bug"
                class="archive btn ghost icon"
                @on_click>
          <icon class="archive"> </icon>
        </button>
        <button type="button"
                title="Delete session (Delete)"
                aria-label="Delete fix the parser bug"
                class="btn delete ghost icon"
                @on_click>
          <icon class="trash"> </icon>
        </button>
      </span>
    </div>
    |}];
  H.show h ~selector:".session.running .lamp";
  [%expect
    {| <span title="Running" role="img" aria-label="Running" class="lamp"> </span> |}];
  (* The current session's row has its cwd as it is now: [/cd] moves it. *)
  H.event
    h
    (sprintf
       {|{"event":"state","state":%s}|}
       (H.state_json ~fields:[ "cwd", `String "/home/ann/src/parser" ] ()));
  H.names h ~selector:".session-cwd";
  [%expect
    {|
    span "~/src/parser" -- /home/ann/src/parser
    span "~/docs" -- /home/ann/docs
    span "~/src/compiler" -- /home/ann/src/compiler
    |}];
  H.text h ~selector:".session.selected .session-meta";
  [%expect {| just now ~/src/parser |}];
  (* Only the current one has a rename button: it opens what /name opens. *)
  H.elements h ~selector:".session .rename";
  H.click h ~selector:".session.selected .rename";
  let clicked = (H.model h).dialog in
  H.quiet h (fun () -> H.act h Close_dialog);
  H.quiet h (fun () -> H.act h (Run "/name"));
  print_s
    [%sexp
      (Option.equal Prigh_web.Dialog.equal clicked (H.model h).dialog : bool)];
  H.quiet h (fun () -> H.act h Close_dialog);
  [%expect
    {|
    <button class="btn ghost icon rename">
    Open_rename
    (Focus dialog-input)
    true
    |}];
  (* Without the sidebar, the bar names it after its first prompt until it
     has a name. *)
  H.quiet h (fun () -> H.act h Toggle_sidebar);
  H.text h ~selector:".session-bar .session-name";
  H.quiet h (fun () -> H.act h Toggle_sidebar);
  [%expect {| (fix the parser bug) |}];
  (* The clock moves the ages on, and refreshes the list. *)
  H.act h (Tick (Time_ns.add H.now (Time_ns.Span.of_min 5.)));
  H.text h ~selector:".session-age";
  [%expect
    {|
    5m ago
    3h ago
    15d ago
    |}]
;;

let%expect_test "fuzzy search over titles, prompts and directories" =
  let h = H.create ~sessions () in
  H.key h "k" ~ctrl:true;
  [%expect
    {|
    Open_sessions
    (Focus session-search)
    |}];
  H.act h (Set_session_query "lexr");
  H.text h ~selector:".session-title";
  [%expect {| Refactor the lexer |}];
  H.act h (Set_session_query "docs");
  H.text h ~selector:".session-title";
  [%expect {| Release notes |}];
  H.act h (Set_session_query "zzz");
  H.text h ~selector:".sessions";
  [%expect {| Nothing matches “zzz”. |}];
  H.act h (Set_session_query "");
  H.text h ~selector:".session-title";
  [%expect
    {|
    fix the parser bug
    Release notes
    Refactor the lexer
    |}]
;;

let%expect_test "no sessions yet" =
  let h = H.create () in
  H.text h ~selector:".sessions";
  [%expect {| No sessions yet: send a message to start one. |}]
;;

let%expect_test "switching resets what belonged to the old session" =
  let h = H.create ~sessions () in
  H.act h (Switch_session "/sessions/s1.jsonl");
  [%expect {| (Focus editor) |}];
  H.event
    h
    {|{"event":"message_start","message":{"role":"user","text":"hello"}}|};
  H.event
    h
    {|{"event":"queue_update","steer":1,"follow_up":1,"steer_texts":["also this"],"follow_up_texts":["then that"]}|};
  H.event
    h
    {|{"event":"tool_confirm","call_id":"c1","name":"bash","summary":"rm -rf build"}|};
  [%expect {| (Focus confirm) |}];
  H.act h (Run "/btw why?");
  H.act h (Show_toast { text = "Agent a9 has already finished."; error = true });
  H.type_ h "/mo";
  [%expect
    {|
    (Rpc (method_ btw) (params ((question why?) (btw_id btw-1)))
     (tag (Btw btw-1)))
    |}];
  H.act h (Switch_session "/sessions/s2.jsonl");
  [%expect
    {|
    (Rpc (method_ switch_session) (params ((path /sessions/s2.jsonl)))
     (tag Reload_state))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before ()))))
    (Focus editor)
    |}];
  H.event
    h
    (sprintf
       {|{"event":"state","state":%s}|}
       (H.state_json
          ~fields:
            [ "session_id", `String "s2"
            ; "session_path", `String "/sessions/s2.jsonl"
            ; "session_name", `String "Release notes"
            ]
          ()));
  [%expect
    {|
    (Rpc (method_ btw_cancel) (params ((btw_id btw-1))) (tag Ignore))
    (Set_url_session s2)
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    |}];
  let m = H.model h in
  print_s
    [%message
      (Prigh_web.Chat.entries m.chat |> List.length : int)
        (Prigh_web.Queued.count m.queue : int)
        (List.length m.confirms : int)
        (Option.is_some m.btw : bool)
        (Option.is_some m.completion : bool)
        (List.length m.toasts : int)];
  [%expect
    {|
    (("(Prigh_web.Chat.entries m.chat) |> List.length" 0)
     ("Prigh_web.Queued.count m.queue" 0) ("List.length m.confirms" 0)
     ("Option.is_some m.btw" false) ("Option.is_some m.completion" false)
     ("List.length m.toasts" 0))
    |}];
  H.reply h "list_sessions" sessions;
  H.text h ~selector:".session.selected .session-title";
  [%expect {| Release notes |}]
;;

let%expect_test
    "a session in progress: its confirmations, queue and running tools come \
     back; replies for the session we left are dropped"
  =
  let h = H.create ~sessions () in
  H.event
    h
    (sprintf
       {|{"event":"state","state":%s}|}
       (H.state_json
          ~fields:[ "session_id", `String "s2"; "running", `True ]
          ()));
  [%expect
    {|
    (Set_url_session s2)
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before ()))))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    |}];
  H.act
    h
    (Reply
       ( H.newest "s1"
       , Ok (Jsonaf.of_string @@ H.page {|[{"role":"user","text":"from s1"}]|})
       ));
  print_s [%sexp (List.length (Prigh_web.Chat.entries (H.model h).chat) : int)];
  [%expect {| 0 |}];
  H.reply
    h
    "get_pending"
    {|{"steer_texts":["a"],"follow_up_texts":["b","c"],"confirms":[{"call_id":"c1","name":"bash","summary":"rm -rf build"}]}|};
  H.text h ~selector:".modal";
  H.text h ~selector:".status-extras .queued";
  [%expect
    {|
    (Focus confirm)
    Confirm
    (Close (Esc))
    Run bash: rm -rf build?
    (y Allow once) (n Deny) (r Deny, saying why)
    (3 queued)
    |}];
  H.reply
    h
    "get_messages_page"
    (H.page
       {|[{"role":"user","text":"clean up"},
       {"role":"assistant","content":[{"type":"tool_call","id":"c1","name":"bash","arguments":"{\"command\":\"rm -rf build\"}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}]|});
  H.show h ~selector:".tool";
  [%expect
    {|
    Scroll_to_bottom
    <div class="running tool tool-bash">
      <div class="tool-head">
        <span aria-hidden="true" class="lamp"> </span>
        <span class="name"> bash </span>
        <span class="arg command"> rm -rf build </span>
      </div>
      <div class="tool-body">
        <div class="live output">
          <div class="missed"> Output from while this page was away shows when it ends. </div>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "new session" =
  let h = H.create ~sessions () in
  H.act h New_session;
  [%expect
    {|
    (Rpc (method_ new_session) (params ()) (tag Reload_state))
    (Focus editor)
    |}];
  H.act h (Edit { text = "/new"; cursor = 4 });
  H.key h "Enter";
  [%expect
    {|
    (Complete_accept (run true))
    (Rpc (method_ push_pref) (params ((key history) (value /new) (max 500)))
     (tag Ignore))
    (Rpc (method_ new_session) (params ()) (tag Reload_state))
    |}]
;;

let%expect_test
    "deleting asks first; Esc keeps the session; backend errors say what to do"
  =
  let h = H.create ~sessions () in
  H.act h (Ask_delete "/sessions/s3.jsonl");
  H.text h ~selector:".modal";
  [%expect
    {|
    (Focus dialog)
    Delete session?
    (Close (Esc))
    It will be deleted, with its subagent logs: 0 B on disk. This can't be undone.
    Refactor the lexer 0 B
    (Cancel) (Delete)
    |}];
  H.key h "Escape" ~target:Page;
  [%expect
    {|
    Close_dialog
    (Focus editor)
    |}];
  H.text h ~selector:".modal";
  [%expect {| |}];
  (* Running: nothing to delete, so nothing is asked of the backend. Enter
     on a focused button (Tab to Close) is the button's. *)
  H.act h (Ask_delete "/sessions/s2.jsonl");
  H.text h ~selector:".modal";
  H.key h "Enter" ~target:Control;
  H.key h "Enter" ~target:Page;
  [%expect
    {|
    (Focus dialog)
    Can't delete this session
    (Close (Esc))
    Release notes 0 B
    “Release notes” is running: it will be kept (wait for it or stop it first)
    (Close)
    (browser default)
    Dialog_accept
    (Focus editor)
    |}];
  (* Refused all the same (opened elsewhere since): the backend says why. *)
  H.act h (Ask_delete "/sessions/s3.jsonl");
  H.act h Dialog_accept;
  H.reply
    h
    "delete_sessions"
    {|{"deleted":[],"refused":[{"path":"/sessions/s3.jsonl","error":"session s3 is open in 1 other frontend(s): close it there first"}]}|};
  H.text h ~selector:".toasts";
  [%expect
    {|
    (Focus dialog)
    (Focus editor)
    (Rpc (method_ delete_sessions) (params ((paths (/sessions/s3.jsonl))))
     (tag Sessions_deleted))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    Not deleted: session s3 is open in 1 other frontend(s): close it there first (Dismiss)
    |}];
  H.act h (Ask_delete "/sessions/s3.jsonl");
  H.act h Dialog_accept;
  H.reply
    h
    "delete_sessions"
    {|{"deleted":[{"path":"/sessions/s3.jsonl","id":"s3","bytes":2048}],"refused":[]}|};
  [%expect
    {|
    (Focus dialog)
    (Focus editor)
    (Rpc (method_ delete_sessions) (params ((paths (/sessions/s3.jsonl))))
     (tag Sessions_deleted))
    (Expire_toast (id 1) (after_ms 2540))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    |}];
  H.text h ~selector:".toast:not(.error)";
  [%expect {| Deleted 1 session (2.0 KB) |}];
  (* Gone from the sidebar at once. *)
  H.text h ~selector:".sessions";
  [%expect
    {|
    (fix the parser bug just now /work) (Rename (/name)) (Archive (A)) (Delete session (Delete))
    (Release notes 3h ago ~/docs) (Archive (A)) (Delete session (Delete))
    |}]
;;

let%expect_test "renaming the current session" =
  let h = H.create ~sessions () in
  H.act h Open_rename;
  H.show h ~selector:"#dialog-input";
  [%expect
    {|
    (Focus dialog-input)
    <input id="dialog-input"
           aria-label="Session name"
           type="text"
           placeholder="Session name"
           autocomplete="off"
           spellcheck="false"
           class="text-input"
           #value=""
           @on_input/>
    |}];
  H.key h "Enter" ~target:Field;
  H.text h ~selector:".toast";
  [%expect
    {|
    Dialog_accept
    Type a name for the session (Esc keeps the current one). (Dismiss)
    |}];
  H.act h (Dialog_input "Parser work");
  H.key h "Enter" ~target:Field;
  [%expect
    {|
    Dialog_accept
    (Focus editor)
    (Rpc (method_ set_session_name) (params ((name "Parser work")))
     (tag Refresh_sessions))
    |}];
  H.reply h "set_session_name" "{}";
  [%expect {| (Rpc (method_ list_sessions) (params ()) (tag Sessions)) |}];
  (* /name with an argument renames at once. *)
  H.type_ h "/name Lexer";
  H.key h "Enter";
  [%expect
    {|
    Send
    (Rpc (method_ push_pref)
     (params ((key history) (value "/name Lexer") (max 500))) (tag Ignore))
    (Rpc (method_ set_session_name) (params ((name Lexer)))
     (tag Refresh_sessions))
    |}]
;;

let%expect_test
    "the sidebar collapses; on phones it is a drawer, closed at first"
  =
  let h = H.create ~sessions () in
  let classes () = H.show h ~selector:".app > .scrim" in
  print_s [%sexp ((H.model h).sidebar_open : bool)];
  H.key h "b" ~ctrl:true;
  print_s [%sexp ((H.model h).sidebar_open : bool)];
  [%expect
    {|
    true
    Toggle_sidebar
    false
    |}];
  H.act h (Set_narrow true);
  print_s [%sexp ((H.model h).sidebar_open : bool)];
  [%expect {| false |}];
  H.act h Toggle_sidebar;
  classes ();
  [%expect {| <div class="scrim" @on_click> </div> |}];
  (* Esc closes the drawer; choosing a session closes it too. *)
  H.key h "Escape" ~target:Page;
  [%expect {| Toggle_sidebar |}];
  H.act h Open_sessions;
  H.act h (Switch_session "/sessions/s3.jsonl");
  print_s [%sexp ((H.model h).sidebar_open : bool)];
  [%expect
    {|
    (Focus session-search)
    (Rpc (method_ switch_session) (params ((path /sessions/s3.jsonl)))
     (tag Reload_state))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s3.jsonl) (limit 60)))
     (tag (Page (path /sessions/s3.jsonl) (before ()))))
    (Focus editor)
    false
    |}];
  H.act h (Set_narrow false);
  print_s [%sexp ((H.model h).sidebar_open : bool)];
  [%expect {| true |}]
;;

(* The backend moves the client to the other session without telling it:
   the page asks for the new state, which resets the transcript. Until the
   new session's messages arrive the chat says what it waits for. *)
let%expect_test "new session and switching session reload the state" =
  let sessions =
    sprintf
      "[%s,%s]"
      (Harness.session_json ~first_prompt:"hello" "s1")
      (Harness.session_json ~first_prompt:"older work" "s3")
  in
  let h = Harness.create ~sessions () in
  [%expect {| |}];
  Harness.event
    h
    {|{"event":"message_start","message":{"role":"user","text":"hello"}}|};
  Harness.act h New_session;
  Harness.text h ~selector:".new-session";
  Harness.text h ~selector:".chat";
  [%expect
    {|
    (Rpc (method_ new_session) (params ()) (tag Reload_state))
    (Focus editor)
    (Starting…)
    Starting a session…
    |}];
  Harness.reply h "new_session" "{}";
  Harness.reply
    h
    "get_state"
    (Harness.state_json
       ~fields:
         [ "session_id", `String "s2"
         ; "session_path", `String "/sessions/s2.jsonl"
         ]
       ());
  Harness.text h ~selector:".new-session";
  Harness.text h ~selector:".chat";
  [%expect
    {|
    (Rpc (method_ get_state) (params ()) (tag State))
    (Set_url_session s2)
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before ()))))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (New session)
    Loading the session…
    |}];
  Harness.reply h "get_messages_page" (Harness.page "[]");
  Harness.text h ~selector:".chat";
  [%expect
    {|
    Scroll_to_bottom
    What are we building?
    /work
    / commands
    @ mention a file
    ! run a command
    Ctrl+L switch model
    Ctrl+K find a session
    |}];
  (* The session clicked is marked until it has loaded. *)
  Harness.act h (Switch_session "/sessions/s3.jsonl");
  Harness.show h ~selector:".session.opening .session-mark";
  Harness.text h ~selector:".chat";
  [%expect
    {|
    (Rpc (method_ switch_session) (params ((path /sessions/s3.jsonl)))
     (tag Reload_state))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s3.jsonl) (limit 60)))
     (tag (Page (path /sessions/s3.jsonl) (before ()))))
    (Focus editor)
    <div class="session-mark">
      <span title="Opening…" role="img" aria-label="Opening…" class="lamp"> </span>
    </div>
    Loading the session…
    |}];
  Harness.reply h "switch_session" "{}";
  Harness.reply
    h
    "get_state"
    (Harness.state_json
       ~fields:
         [ "session_id", `String "s3"
         ; "session_path", `String "/sessions/s3.jsonl"
         ]
       ());
  Harness.elements h ~selector:".session.opening";
  Harness.text h ~selector:".chat";
  [%expect
    {|
    (Rpc (method_ get_state) (params ()) (tag State))
    (Set_url_session s3)
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    Loading the session…
    |}];
  Harness.reply
    h
    "get_messages_page"
    (Harness.page {|[{"role":"user","text":"older work"}]|});
  Harness.text h ~selector:".chat";
  [%expect
    {|
    Scroll_to_bottom
    older work
    |}];
  (* A switch the backend refuses leaves the session as it was. *)
  Harness.act h (Switch_session "/sessions/s1.jsonl");
  Harness.fail h "switch_session" "no such session";
  Harness.elements h ~selector:".session.opening";
  Harness.text h ~selector:".chat";
  Harness.text h ~selector:".toast";
  [%expect
    {|
    (Rpc (method_ switch_session) (params ((path /sessions/s1.jsonl)))
     (tag Reload_state))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s1.jsonl) (limit 60)))
     (tag (Page (path /sessions/s1.jsonl) (before ()))))
    (Focus editor)
    (Rpc (method_ get_state) (params ()) (tag State))
    older work
    Couldn't open the session: no such session (Dismiss)
    |}];
  (* So does a transcript that fails to load, saying so. *)
  Harness.act h (Switch_session "/sessions/s1.jsonl");
  Harness.quiet h (fun () ->
    Harness.reply h "switch_session" "{}";
    Harness.reply h "get_state" (Harness.state_json ()));
  Harness.fail h "get_messages_page" "disk on fire";
  Harness.text h ~selector:".chat";
  Harness.text h ~selector:".toast";
  [%expect
    {|
    (Rpc (method_ switch_session) (params ((path /sessions/s1.jsonl)))
     (tag Reload_state))
    (Focus editor)
    hello
    Couldn't load the session's messages: disk on fire. Reload the page to try again. (Dismiss)
    |}]
;;

let%expect_test
    "the sidebar from the keyboard: ↓ ↑ through the sessions, Enter opens, Esc \
     goes back to the editor"
  =
  let h = H.create ~sessions () in
  H.key h "k" ~ctrl:true;
  H.type_ h "";
  H.act h (Set_session_query "re");
  H.text h ~selector:".session-title";
  H.key h "ArrowDown" ~target:Session_search;
  H.key h "ArrowDown" ~target:(Session "s2");
  H.key h "ArrowDown" ~target:(Session "s3");
  H.key h "ArrowUp" ~target:(Session "s2");
  H.key h "ArrowUp" ~target:(Session "s2");
  [%expect
    {|
    Open_sessions
    (Focus session-search)
    Release notes
    Refactor the lexer
    fix the parser bug
    (Session_nav (from ()) (by 1))
    (Focus session-s2)
    (Session_nav (from (s2)) (by 1))
    (Focus session-s3)
    (Session_nav (from (s3)) (by 1))
    (Focus session-s1)
    (Session_nav (from (s2)) (by -1))
    (Focus session-search)
    (Session_nav (from (s2)) (by -1))
    (Focus session-search)
    |}];
  H.key h "Enter" ~target:(Session "s3");
  H.key h "Delete" ~target:(Session "s3");
  H.key h "Escape" ~target:Page;
  H.key h "Escape" ~target:(Session "s3");
  [%expect
    {|
    (Switch_session /sessions/s3.jsonl)
    (Rpc (method_ switch_session) (params ((path /sessions/s3.jsonl)))
     (tag Reload_state))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s3.jsonl) (limit 60)))
     (tag (Page (path /sessions/s3.jsonl) (before ()))))
    (Focus editor)
    (Ask_delete /sessions/s3.jsonl)
    (Focus dialog)
    Close_dialog
    (Focus editor)
    Leave_sidebar
    (Focus editor)
    |}];
  H.key h "Enter" ~target:Session_search;
  [%expect
    {|
    Open_first_session
    (Rpc (method_ switch_session) (params ((path /sessions/s2.jsonl)))
     (tag Reload_state))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before ()))))
    (Focus editor)
    |}]
;;

let%expect_test
    "switching: the reply carries the new state, saving a round trip"
  =
  let h = H.create ~sessions () in
  H.act h (Switch_session "/sessions/s3.jsonl");
  H.reply
    h
    "switch_session"
    (sprintf
       {|{"state":%s}|}
       (H.state_json
          ~fields:
            [ "session_id", `String "s3"
            ; "session_path", `String "/sessions/s3.jsonl"
            ]
          ()));
  [%expect
    {|
    (Rpc (method_ switch_session) (params ((path /sessions/s3.jsonl)))
     (tag Reload_state))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s3.jsonl) (limit 60)))
     (tag (Page (path /sessions/s3.jsonl) (before ()))))
    (Focus editor)
    (Set_url_session s3)
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    |}]
;;

(* Clicking B, then C before B opened: B's state (its reply, its event) is
   not shown on the way to C; a switch that fails asks the backend where it
   is. *)
let%expect_test "sessions clicked in quick succession" =
  let h = H.create () in
  let state s = H.state_json ~fields:[ "session_id", `String s ] () in
  let where () =
    let m = H.model h in
    print_s
      [%message
        ""
          ~session:
            (Option.map m.state ~f:(fun s -> s.session_id) : string option)
          ~opening:(m.pending.session : Prigh_web.In_flight.Session.t option)]
  in
  H.quiet h (fun () ->
    H.act h (Switch_session "/sessions/s2.jsonl");
    H.act h (Switch_session "/sessions/s3.jsonl"));
  H.event h (sprintf {|{"event":"state","state":%s}|} (state "s2"));
  H.reply h "switch_session" (sprintf {|{"state":%s}|} (state "s2"));
  where ();
  H.quiet h (fun () ->
    H.reply h "switch_session" (sprintf {|{"state":%s}|} (state "s3")));
  where ();
  H.quiet h (fun () -> H.act h (Switch_session "/sessions/s4.jsonl"));
  H.fail h "switch_session" "no session /sessions/s4.jsonl";
  where ();
  [%expect
    {|
    ((session (s1)) (opening ((Switching /sessions/s3.jsonl))))
    ((session (s3)) (opening ()))
    (Rpc (method_ get_state) (params ()) (tag State))
    ((session (s3)) (opening ()))
    |}]
;;
