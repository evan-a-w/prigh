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
    fix the parser bug just now
    /work 1 msg (Delete session)
    Release notes 3h ago
    ~/docs 4 msgs (Delete session)
    Refactor the lexer 15d ago
    ~/src/compiler 12 msgs (Delete session)
    |}];
  H.show h ~selector:".session.selected";
  [%expect
    {|
    <div id="session-s1"
         tabindex="0"
         role="button"
         data-session="s1"
         title="/sessions/s1.jsonl"
         class="selected session"
         @on_click>
      <div class="session-top">
        <span title="open in the backend" class="dot live">  </span>
        <span class="session-title"> fix the parser bug </span>
        <span class="session-age"> just now </span>
      </div>
      <div class="session-meta">
        <span class="session-cwd"> /work </span>
        <span class="session-count"> 1 msg </span>
        <button type="button" title="Delete session" class="btn delete ghost icon" @on_click>
          <icon class="trash"> </icon>
        </button>
      </div>
    </div>
    |}];
  H.show h ~selector:".session.running .dot";
  [%expect {| <span title="running" class="dot running">  </span> |}];
  (* The topbar names the session after its first prompt until it has a name. *)
  H.text h ~selector:".topbar .title";
  [%expect
    {|
    (fix the parser bug)
    /work main
    |}];
  (* The clock moves the ages on, and refreshes the list. *)
  H.act h (Tick (Time_ns.add H.now (Time_ns.Span.of_min 5.)));
  H.text h ~selector:".session-age";
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
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
  [%expect {| No saved sessions yet. |}]
;;

let%expect_test "switching resets what belonged to the old session" =
  let h = H.create ~sessions () in
  H.act h (Switch_session "/sessions/s1.jsonl");
  [%expect {| (Focus editor) |}];
  H.event
    h
    {|{"event":"message_start","message":{"role":"user","text":"hello"}}|};
  H.event h {|{"event":"queue_update","steer":1,"follow_up":1}|};
  H.event
    h
    {|{"event":"tool_confirm","call_id":"c1","name":"bash","summary":"rm -rf build"}|};
  [%expect {| (Focus confirm) |}];
  H.act h (Run "/btw why?");
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
    (Rpc (method_ get_messages) (params ()) (tag (Messages s2)))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    |}];
  let m = H.model h in
  print_s
    [%message
      (Prigh_web.Chat.entries m.chat |> List.length : int)
        (m.queue : int * int)
        (List.length m.confirms : int)
        (Option.is_some m.btw : bool)
        (Option.is_some m.completion : bool)];
  [%expect
    {|
    (("(Prigh_web.Chat.entries m.chat) |> List.length" 0) (m.queue (0 0))
     ("List.length m.confirms" 0) ("Option.is_some m.btw" false)
     ("Option.is_some m.completion" false))
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
    (Rpc (method_ get_messages) (params ()) (tag (Messages s2)))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    |}];
  H.act
    h
    (Reply
       ( Messages "s1"
       , Ok (Jsonaf.of_string {|[{"role":"user","text":"from s1"}]|}) ));
  print_s [%sexp (List.length (Prigh_web.Chat.entries (H.model h).chat) : int)];
  [%expect {| 0 |}];
  H.reply
    h
    "get_pending"
    {|{"steer_texts":["a"],"follow_up_texts":["b","c"],"confirms":[{"call_id":"c1","name":"bash","summary":"rm -rf build"}]}|};
  H.text h ~selector:".modal";
  H.text h ~selector:".status .queued";
  [%expect
    {|
    (Focus confirm)
    Allow bash?
    (Close (Esc))
    rm -rf build
    (Deny) (Allow)
    (3 queued)
    |}];
  H.reply
    h
    "get_messages"
    {|[{"role":"user","text":"clean up"},
       {"role":"assistant","content":[{"type":"tool_call","id":"c1","name":"bash","arguments":"{\"command\":\"rm -rf build\"}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}]|};
  H.show h ~selector:".tool";
  [%expect
    {|
    Follow_chat
    <div class="running tool tool-bash">
      <div class="tool-head">
        <span class="spinner"> </span>
        <span class="name"> bash </span>
        <span class="arg command"> rm -rf build </span>
      </div>
    </div>
    |}]
;;

let%expect_test "new session" =
  let h = H.create ~sessions () in
  H.act h New_session;
  [%expect {| (Rpc (method_ new_session) (params ()) (tag Reload_state)) |}];
  H.act h (Edit { text = "/new"; cursor = 4 });
  H.key h "Enter";
  [%expect
    {|
    (Complete_accept (run true))
    (Save_history (/new))
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
    “Refactor the lexer” will be deleted. This can't be undone.
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
  H.act h (Ask_delete "/sessions/s2.jsonl");
  (* Enter on a focused button (Tab to Cancel) is the button's. *)
  H.key h "Enter" ~target:Control;
  H.key h "Enter" ~target:Page;
  [%expect
    {|
    (Focus dialog)
    (browser default)
    Dialog_accept
    (Focus editor)
    (Rpc (method_ delete_session) (params ((path /sessions/s2.jsonl)))
     (tag (Deleted "Release notes")))
    |}];
  H.fail h "delete_session" "cannot delete a live session";
  H.text h ~selector:".toasts";
  [%expect
    {| Couldn't delete "Release notes": cannot delete a live session. Switch to another session and close other tabs using it first. |}];
  H.act h (Ask_delete "/sessions/s3.jsonl");
  H.act h Dialog_accept;
  H.reply h "delete_session" "{}";
  [%expect
    {|
    (Focus dialog)
    (Focus editor)
    (Rpc (method_ delete_session) (params ((path /sessions/s3.jsonl)))
     (tag (Deleted "Refactor the lexer")))
    (Expire_toast (id 1) (after_ms 4000))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    |}];
  H.text h ~selector:".toast:not(.error)";
  [%expect {| Deleted "Refactor the lexer" |}]
;;

let%expect_test "renaming the current session" =
  let h = H.create ~sessions () in
  H.act h Open_rename;
  H.show h ~selector:"#dialog-input";
  [%expect
    {|
    (Focus dialog-input)
    <input id="dialog-input"
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
    Type a name for the session (Esc keeps the current one).
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
    (Save_history ("/name Lexer"))
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
    (Focus editor)
    false
    |}];
  H.act h (Set_narrow false);
  print_s [%sexp ((H.model h).sidebar_open : bool)];
  [%expect {| true |}]
;;

(* The backend moves the client to the other session without telling it:
   the page asks for the new state, which resets the transcript. *)
let%expect_test "new session and switching session reload the state" =
  let h = Harness.create () in
  [%expect {| |}];
  Harness.event
    h
    {|{"event":"message_start","message":{"role":"user","text":"hello"}}|};
  Harness.act h New_session;
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
  Harness.text h ~selector:".chat";
  [%expect
    {|
    (Rpc (method_ new_session) (params ()) (tag Reload_state))
    (Rpc (method_ get_state) (params ()) (tag State))
    (Set_url_session s2)
    (Rpc (method_ get_messages) (params ()) (tag (Messages s2)))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    What are we building?
    /work
    / commands
    @ mention a file
    ! run a command
    Ctrl+L switch model
    Ctrl+K find a session
    |}];
  Harness.act h (Switch_session "/sessions/s1.jsonl");
  Harness.reply h "switch_session" "{}";
  Harness.reply h "get_state" (Harness.state_json ());
  Harness.reply h "get_messages" {|[{"role":"user","text":"hello"}]|};
  Harness.text h ~selector:".chat";
  [%expect
    {|
    (Rpc (method_ switch_session) (params ((path /sessions/s1.jsonl)))
     (tag Reload_state))
    (Focus editor)
    (Rpc (method_ get_state) (params ()) (tag State))
    (Set_url_session s1)
    (Rpc (method_ get_messages) (params ()) (tag (Messages s1)))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    What are we building?
    /work
    / commands
    @ mention a file
    ! run a command
    Ctrl+L switch model
    Ctrl+K find a session
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
    (Focus editor)
    |}]
;;
