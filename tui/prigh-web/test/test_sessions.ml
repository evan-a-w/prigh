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
    <div title="/sessions/s1.jsonl" class="selected session" @on_click>
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
  [%expect {| |}];
  H.event
    h
    {|{"event":"message_start","message":{"role":"user","text":"hello"}}|};
  H.event h {|{"event":"queue_update","steer":1,"follow_up":1}|};
  H.event
    h
    {|{"event":"tool_confirm","call_id":"c1","name":"bash","summary":"rm -rf build"}|};
  [%expect {| (Focus confirm) |}];
  H.act h (Switch_session "/sessions/s2.jsonl");
  [%expect
    {|
    (Rpc (method_ switch_session) (params ((path /sessions/s2.jsonl)))
     (tag Show_error))
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
    (Set_url_session s2)
    (Rpc (method_ get_messages) (params ()) (tag Messages))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    |}];
  let m = H.model h in
  print_s
    [%message
      (Prigh_web.Chat.entries m.chat |> List.length : int)
        (m.queue : int * int)
        (List.length m.confirms : int)];
  [%expect
    {|
    (("(Prigh_web.Chat.entries m.chat) |> List.length" 0) (m.queue (0 0))
     ("List.length m.confirms" 0))
    |}];
  H.reply h "list_sessions" sessions;
  H.text h ~selector:".session.selected .session-title";
  [%expect {| Release notes |}]
;;

let%expect_test "new session" =
  let h = H.create ~sessions () in
  H.act h New_session;
  [%expect {| (Rpc (method_ new_session) (params ()) (tag Show_error)) |}];
  H.act h (Edit { text = "/new"; cursor = 4 });
  H.key h "Enter";
  [%expect
    {|
    (Complete_accept (run true))
    (Save_history (/new))
    (Rpc (method_ new_session) (params ()) (tag Show_error))
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
  H.key h "Enter" ~target:Page;
  [%expect
    {|
    (Focus dialog)
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
     (tag Show_error))
    false
    |}];
  H.act h (Set_narrow false);
  print_s [%sexp ((H.model h).sidebar_open : bool)];
  [%expect {| true |}]
;;
