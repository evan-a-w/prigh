open! Core
open Prigh_web
module H = Harness

(* The terminal panel: a shell on the session's active host. The page's
   xterm.js widget shows here as the target it connects to. *)

let hosts =
  {|[{"id":"backend","name":"backend","cwd":"/work"},
     {"id":"c7","name":"laptop","cwd":"/home/me/proj","session_id":"s1","session_name":null}]|}
;;

let state ?(session = "s1") ?(active_host = "backend") () =
  H.state_json
    ~fields:
      [ "session_id", `String session
      ; "session_path", `String (sprintf "/sessions/%s.jsonl" session)
      ; "active_host", `String active_host
      ; "hosts", Jsonaf.of_string hosts
      ]
    ()
;;

let state_event h ?session ?active_host () =
  H.event
    h
    (sprintf {|{"event":"state","state":%s}|} (state ?session ?active_host ()))
;;

let key (m : App.Model.t) =
  match m.state with
  | Some state ->
    Terminal.Target.key (Terminal.target m.terminal state ~hello:m.hello)
  | None -> ""
;;

let status h status =
  H.act h (Terminal_status { key = key (H.model h); status })
;;

let%expect_test
    "Ctrl+` opens the panel on the active host; Ctrl+` in it closes it"
  =
  let h = H.create ~state:(state ()) () in
  H.text h ~selector:".terminal-panel";
  [%expect {| |}];
  H.key h "`" ~ctrl:true ~code:"Backquote";
  [%expect
    {|
    Toggle_terminal
    (Cache_prefs (((key web.terminal-open) (value (true)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.terminal-open true)))))
     (tag Show_error))
    Focus_terminal
    |}];
  H.show h ~selector:".terminal-panel";
  [%expect
    {|
    <section id="terminal-panel" aria-label="Terminal" class="terminal-panel">
      <div role="separator"
           tabindex="0"
           aria-label="Resize the terminal"
           aria-orientation="horizontal"
           aria-valuenow="304"
           aria-valuemin="120"
           aria-valuemax="540"
           title="Drag to resize, double-click for the default size"
           data-resize="terminal"
           data-resize-var="--terminal-height"
           data-resize-unit="px"
           data-resize-grows="-1"
           data-resize-target="#terminal-panel"
           class="resizer resizer-terminal"
           @on_dblclick> </div>
      <div class="terminal-head">
        <icon class="terminal"> </icon>
        <span class="terminal-machine"> backend </span>
        <span title="/work" class="terminal-cwd where"> /work </span>
        <span class="terminal-state"> connecting… </span>
        <button type="button"
                title="Close (Ctrl+`): the shell keeps running for 10 minutes"
                aria-label="Close terminal"
                class="btn ghost icon terminal-close"
                @on_click>
          <icon class="close"> </icon>
        </button>
      </div>
      <div class="terminal-body">
        <div class="xterm-widget"> ((session s1)(host backend)(online true)(as_user())(generation 0)) </div>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      </div>
    </section>
    |}];
  H.text h ~selector:".terminal-toggle";
  [%expect {| (Terminal (Ctrl+`)) |}];
  status h Connected;
  H.text h ~selector:".terminal-head";
  [%expect
    {|
    backend /work (Close (Ctrl+`): the shell keeps running for 10 minutes)
    |}];
  (* Every other key is the shell's, Esc and Enter included. *)
  List.iter
    [ "Escape", false; "Enter", false; "k", true; "l", true; "b", true ]
    ~f:(fun (k, ctrl) -> H.key h k ~ctrl ~target:Terminal);
  [%expect
    {|
    (browser default)
    (browser default)
    (browser default)
    (browser default)
    (browser default)
    |}];
  H.key h "`" ~ctrl:true ~code:"Backquote" ~target:Terminal;
  [%expect
    {|
    Toggle_terminal
    (Cache_prefs (((key web.terminal-open) (value ()))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.terminal-open null)))))
     (tag Show_error))
    (Focus editor)
    |}];
  H.text h ~selector:".terminal-panel";
  [%expect {| |}]
;;

let%expect_test "the panel follows the session, the host and the user" =
  let h = H.create ~state:(state ()) () in
  H.act h Toggle_terminal;
  status h Connected;
  [%expect
    {|
    (Cache_prefs (((key web.terminal-open) (value (true)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.terminal-open true)))))
     (tag Show_error))
    Focus_terminal
    |}];
  let show () = H.text h ~selector:".terminal-panel" in
  show ();
  [%expect
    {|
    backend /work (Close (Ctrl+`): the shell keeps running for 10 minutes)
    ((session s1)(host backend)(online true)(as_user())(generation 0))
    |}];
  (* Another session: its own terminal, connecting afresh. *)
  state_event h ~session:"s2" ();
  show ();
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
    (Rpc (method_ get_review) (params ()) (tag (Review Review)))
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s2\
                            \nbackend\
                            \n/work") (purpose (Sidebar 2)))))
    backend /work connecting… (Close (Ctrl+`): the shell keeps running for 10 minutes)
    ((session s2)(host backend)(online true)(as_user())(generation 0))
    |}];
  status h Connected;
  (* /host moved the tools: the shell is there now. *)
  state_event h ~session:"s2" ~active_host:"c7" ();
  show ();
  [%expect
    {|
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s2\
                            \nc7\
                            \n/work") (purpose (Sidebar 3)))))
    laptop ~/proj connecting… (Close (Ctrl+`): the shell keeps running for 10 minutes)
    ((session s2)(host c7)(online true)(as_user())(generation 0))
    |}];
  (* A superuser acting as bob gets bob's terminal. *)
  H.reply h "get_messages_page" (H.page "[]");
  H.act
    h
    (Hello { client_id = "c1"; namespace = Some "bob"; user = Some "alice" });
  show ();
  [%expect
    {|
    Scroll_to_bottom
    laptop ~/proj connecting… (Close (Ctrl+`): the shell keeps running for 10 minutes)
    ((session s2)(host c7)(online true)(as_user(bob))(generation 0))
    |}];
  (* While the backend is away there is nothing to connect to. *)
  H.act h Backend_closed;
  H.act h (Reply (Reconnect 1, Ok (Jsonaf.of_string {|{"client_id":"c1"}|})));
  show ();
  [%expect
    {|
    (Reconnect (generation 1) (delay_ms 0) (session (s2)))
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ get_prefs)
     (params
      ((keys
        (web.theme web.accent web.notify web.verbosity web.terminal-open history
         web.migrated web.sidebar-width web.files-width web.files-split
         web.agents-width web.terminal-height))))
     (tag Prefs))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 1000000)))
     (tag (Resync (session s2) (from ()))))
    (Rpc (method_ get_pending) (params ()) (tag Resync_pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    laptop ~/proj connecting… (Close (Ctrl+`): the shell keeps running for 10 minutes)
    ((session s2)(host c7)(online true)(as_user())(generation 0))
    |}]
;;

let%expect_test "an exited shell, a failure and what to do next" =
  let h = H.create ~state:(state ~active_host:"c9" ()) () in
  H.act h Toggle_terminal;
  status h (Failed {|no terminal: the tool host "c9" is not connected|});
  H.text h ~selector:".terminal-panel";
  [%expect
    {|
    (Cache_prefs (((key web.terminal-open) (value (true)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.terminal-open true)))))
     (tag Show_error))
    Focus_terminal
    c9 (offline) /work unavailable (Close (Ctrl+`): the shell keeps running for 10 minutes)
    ((session s1)(host c9)(online false)(as_user())(generation 0))
    No terminal: the tool host "c9" is not connected Pick a connected tool host with /host, or Retry once it is back.
    (Retry)
    |}];
  (* The host is back: a new attempt. *)
  state_event h ~active_host:"c7" ();
  H.text h ~selector:".terminal-body";
  [%expect {|
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nc7\
                            \n/work") (purpose (Sidebar 2)))))
    ((session s1)(host c7)(online true)(as_user())(generation 0))
    |}];
  status h (Failed "tmux not found: install tmux or set PRIGH_TMUX");
  H.text h ~selector:".terminal-notice";
  [%expect
    {|
    No terminal: tmux not found: install tmux or set PRIGH_TMUX The shell needs tmux on the tool host: install it there (or point $PRIGH_TMUX at it) and Retry, or pick another host with /host.
    (Retry)
    |}];
  H.act h New_shell;
  H.text h ~selector:".terminal-body";
  [%expect
    {|
    Focus_terminal
    ((session s1)(host c7)(online true)(as_user())(generation 1))
    |}];
  status h Exited;
  H.text h ~selector:".terminal-panel";
  [%expect
    {|
    laptop ~/proj exited (Close (Ctrl+`): the shell keeps running for 10 minutes)
    ((session s1)(host c7)(online true)(as_user())(generation 1))
    The shell exited: press a key in it, or (New shell)
    |}];
  status h Reconnecting;
  H.text h ~selector:".terminal-head";
  [%expect
    {| laptop ~/proj reconnecting… (Close (Ctrl+`): the shell keeps running for 10 minutes) |}]
;;

let%expect_test "/terminal, the top bar's button and the height" =
  let h = H.create ~state:(state ()) () in
  H.act h (Set_narrow true);
  H.act h (Set_narrow false);
  H.type_ h "/terminal";
  H.act h Send;
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /terminal) (max 500)))
     (tag Ignore))
    (Cache_prefs (((key web.terminal-open) (value (true)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.terminal-open true)))))
     (tag Show_error))
    Focus_terminal
    |}];
  (* Already open: back to it. *)
  H.type_ h "/terminal";
  H.act h Send;
  H.type_ h "/terminal now";
  H.act h Send;
  H.text h ~selector:".toast";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /terminal) (max 500)))
     (tag Ignore))
    Focus_terminal
    (Rpc (method_ push_pref)
     (params ((key history) (value "/terminal now") (max 500))) (tag Ignore))
    Usage: /terminal (Ctrl+` opens and closes it; × in its header too) (Dismiss)
    |}];
  H.show h ~selector:".terminal-toggle";
  [%expect
    {|
    <button type="button"
            title="Terminal (Ctrl+`)"
            aria-label="Terminal"
            aria-expanded="true"
            class="btn ghost icon open terminal-toggle"
            @on_click>
      <icon class="terminal"> </icon>
    </button>
    |}];
  (* Its top edge resizes it: no lower than 120px. *)
  H.act h (Resize (Set (Terminal, 40)));
  H.show h ~selector:".terminal-panel > .resizer";
  H.act h (Resize (Set (Terminal, 300)));
  H.show h ~selector:"#terminal-panel";
  [%expect
    {|
    (Cache_prefs (((key web.terminal-height) (value (120)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.terminal-height 120)))))
     (tag Show_error))
    <div role="separator"
         tabindex="0"
         aria-label="Resize the terminal"
         aria-orientation="horizontal"
         aria-valuenow="120"
         aria-valuemin="120"
         aria-valuemax="540"
         title="Drag to resize, double-click for the default size"
         data-resize="terminal"
         data-resize-var="--terminal-height"
         data-resize-unit="px"
         data-resize-grows="-1"
         data-resize-target="#terminal-panel"
         class="resizer resizer-terminal"
         @on_dblclick> </div>
    (Cache_prefs (((key web.terminal-height) (value (300)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.terminal-height 300)))))
     (tag Show_error))
    <section id="terminal-panel" aria-label="Terminal" class="terminal-panel">
      <div role="separator"
           tabindex="0"
           aria-label="Resize the terminal"
           aria-orientation="horizontal"
           aria-valuenow="300"
           aria-valuemin="120"
           aria-valuemax="540"
           title="Drag to resize, double-click for the default size"
           data-resize="terminal"
           data-resize-var="--terminal-height"
           data-resize-unit="px"
           data-resize-grows="-1"
           data-resize-target="#terminal-panel"
           class="resizer resizer-terminal"
           @on_dblclick> </div>
      <div class="terminal-head">
        <icon class="terminal"> </icon>
        <span class="terminal-machine"> backend </span>
        <span title="/work" class="terminal-cwd where"> /work </span>
        <span class="terminal-state"> connecting… </span>
        <button type="button"
                title="Close (Ctrl+`): the shell keeps running for 10 minutes"
                aria-label="Close terminal"
                class="btn ghost icon terminal-close"
                @on_click>
          <icon class="close"> </icon>
        </button>
      </div>
      <div class="terminal-body">
        <div class="xterm-widget"> ((session s1)(host backend)(online true)(as_user())(generation 0)) </div>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      </div>
    </section>
    |}];
  H.act h Close_terminal;
  [%expect
    {|
    (Cache_prefs (((key web.terminal-open) (value ()))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.terminal-open null)))))
     (tag Show_error))
    (Focus editor)
    |}];
  (* On a phone it is a sheet over the page, the sidebar's drawer closed. *)
  H.act h (Set_narrow true);
  H.act h Toggle_sidebar;
  H.act h Toggle_terminal;
  print_s [%sexp ((H.model h).sidebar_open : bool)];
  [%expect
    {|
    (Cache_prefs (((key web.terminal-open) (value (true)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.terminal-open true)))))
     (tag Show_error))
    Focus_terminal
    false
    |}]
;;

let%expect_test "keys inside the terminal stay out of dialogs and confirmations"
  =
  let h = H.create ~state:(state ()) () in
  H.act h Toggle_terminal;
  H.event
    h
    {|{"event":"tool_confirm","call_id":"t1","name":"bash","summary":"rm -rf build"}|};
  H.key h "Enter" ~target:Terminal;
  H.key h "Escape" ~target:Terminal;
  H.key h "Enter";
  [%expect
    {|
    (Cache_prefs (((key web.terminal-open) (value (true)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.terminal-open true)))))
     (tag Show_error))
    Focus_terminal
    (Focus confirm)
    (browser default)
    (browser default)
    (Answer_confirm (call_id t1) (agent_id ()) (answer Allow))
    (Rpc (method_ tool_confirm_respond) (params ((call_id t1) (allow true)))
     (tag Answer))
    (Focus editor)
    (Expire_toast (id 0) (after_ms 2500))
    |}];
  H.act h Open_help;
  H.key h "`" ~ctrl:true ~code:"Backquote" ~target:Page;
  H.key h "Escape" ~target:Page;
  H.key h "`" ~ctrl:true ~code:"Backquote" ~target:Page;
  [%expect
    {|
    (Focus dialog)
    (browser default)
    Close_dialog
    (Focus editor)
    Toggle_terminal
    (Cache_prefs (((key web.terminal-open) (value ()))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.terminal-open null)))))
     (tag Show_error))
    (Focus editor)
    |}]
;;

let%expect_test "after a reload the panel reopens without taking the keyboard" =
  let h = H.create ~state:(state ()) () in
  H.act
    h
    (Restore_prefs
       { prefs = [ { key = Pref_sync.Key.terminal_open; value = Some `True } ]
       ; legacy = false
       });
  H.text h ~selector:".terminal-panel";
  [%expect
    {|
    (Apply_theme ((name Original) (accent (Preset Orange))))
    backend /work connecting… (Close (Ctrl+`): the shell keeps running for 10 minutes)
    ((session s1)(host backend)(online true)(as_user())(generation 0))
    |}]
;;
