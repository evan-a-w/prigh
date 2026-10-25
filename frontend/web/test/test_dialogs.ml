open! Core
module H = Harness

let confirm id summary =
  sprintf
    {|{"event":"tool_confirm","call_id":"%s","name":"bash","summary":"%s"}|}
    id
    summary
;;

let%expect_test "tool confirmations own the keyboard: Enter allows, Esc denies" =
  let h = H.create () in
  H.event h (confirm "c1" "rm -rf build");
  H.event h (confirm "c2" "git push");
  H.text h ~selector:".modal";
  [%expect
    {|
    (Focus confirm)
    Confirm
    (Close (Esc))
    Run bash: rm -rf build?
    (y Allow once) (n Deny) (r Deny, saying why)
    1 more waiting
    |}];
  (* Even with the editor focused and a popup open. *)
  H.type_ h "/";
  H.key h "Enter";
  [%expect
    {|
    (Answer_confirm (call_id c1) (agent_id ()) (answer Allow))
    (Rpc (method_ tool_confirm_respond) (params ((call_id c1) (allow true)))
     (tag Answer))
    (Focus confirm)
    (Expire_toast (id 0) (after_ms 2500))
    |}];
  H.key h "ArrowDown";
  H.key h "Escape";
  [%expect
    {|
    (browser default)
    (Answer_confirm (call_id c2) (agent_id ()) (answer Deny))
    (Rpc (method_ tool_confirm_respond) (params ((call_id c2) (allow false)))
     (tag Answer))
    (Focus editor)
    (Expire_toast (id 1) (after_ms 2500))
    |}];
  H.text h ~selector:".modal";
  [%expect {| |}];
  (* A confirmation the agent no longer waits for goes away. *)
  H.event h (confirm "c3" "ls");
  H.event
    h
    {|{"event":"tool_end","call":{"id":"c3","name":"bash","arguments":"{\"command\":\"ls\"}"},"result":{"tool_call_id":"c3","tool_name":"bash","text":"cancelled","is_error":true}}|};
  H.text h ~selector:".modal";
  [%expect
    {|
    (Focus confirm)
    (Focus editor)
    |}]
;;

let%expect_test "a dialog makes the page behind it inert: Tab stays in it" =
  let h = H.create () in
  let inert () = H.elements h ~selector:"[inert]" in
  inert ();
  [%expect {| |}];
  H.act h Open_help;
  inert ();
  [%expect
    {|
    (Focus dialog)
    <aside class="sidebar">
    <header class="topbar">
    <div class="chat-frame">
    <footer class="composer">
    |}];
  H.key h "Escape" ~target:Page;
  inert ();
  [%expect
    {|
    Close_dialog
    (Focus editor)
    |}];
  (* The agents panel too, and under a tool's confirmation. *)
  H.act h Toggle_subagents;
  H.event h (confirm "c1" "rm -rf build");
  inert ();
  [%expect
    {|
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Focus confirm)
    <aside class="sidebar">
    <header class="topbar">
    <div class="chat-frame">
    <footer class="composer">
    <aside class="agents-panel">
    |}];
  H.key h "Escape" ~target:Page;
  inert ();
  [%expect
    {|
    (Answer_confirm (call_id c1) (agent_id ()) (answer Deny))
    (Rpc (method_ tool_confirm_respond) (params ((call_id c1) (allow false)))
     (tag Answer))
    (Focus editor)
    (Expire_toast (id 0) (after_ms 2500))
    |}]
;;

let%expect_test "Enter on a focused button of a confirmation is that button's" =
  let h = H.create () in
  H.event h (confirm "c1" "rm -rf build");
  (* Tab to Deny, then Enter: the browser clicks Deny. *)
  H.key h "Enter" ~target:Control;
  H.key h "Enter" ~target:Page ~shift:true;
  [%expect
    {|
    (Focus confirm)
    (browser default)
    (browser default)
    |}];
  H.key h "Enter" ~target:Page;
  [%expect
    {|
    (Answer_confirm (call_id c1) (agent_id ()) (answer Allow))
    (Rpc (method_ tool_confirm_respond) (params ((call_id c1) (allow true)))
     (tag Answer))
    (Focus editor)
    (Expire_toast (id 0) (after_ms 2500))
    |}]
;;

let%expect_test "help lists every key and command" =
  let h = H.create () in
  H.act h Open_help;
  H.text h ~selector:".modal-body";
  [%expect
    {|
    (Focus dialog)
    Keys
    Enter send; while running, steer the agent
    Ctrl+Enter / ⌘+Enter send exactly what is typed, ignoring the completion popup
    Alt+Enter queue a follow-up for after the run
    Shift+Enter new line
    Esc stop the run; close a dialog, popup or side answer
    ↑ ↓ earlier prompts (in an empty editor or its first line)
    Alt+↑ take the last queued message back into the editor
    Tab complete a /command, its argument, an @path or a #snippet
    !cmd run a shell command (!!cmd: not added to the context; !&cmd: as a background job)
    Ctrl+L switch model
    Ctrl+P / Alt+P next / previous scoped model (/scoped-models)
    Alt+T cycle the thinking level
    Alt+M cycle the mode: ask → auto-edit → auto → plan (/mode, /plan)
    Alt+N insert a snippet at the caret: a list to filter (/snippet)
    Ctrl+O cycle the transcript verbosity
    Ctrl+X copy the last reply (when nothing is selected)
    Ctrl+↑ / Ctrl+↓ previous / next of your messages in the transcript (loading earlier ones)
    Ctrl+J jump to one of your messages: a list to filter (or the marks at the transcript's right edge)
    PageUp / PageDown scroll the transcript
    Ctrl+K search sessions (↓ ↑ through them, Enter opens, Delete deletes)
    Alt+Enter / Delete (/worktree) start a new session in the highlighted worktree / remove it
    A (on a session) archive or unarchive it (archived sessions are listed last, muted)
    Space (selecting) mark a session (Shift: the range from the last one); Delete deletes the marked ones, Esc stops selecting
    Ctrl+B show or hide the sidebar
    Ctrl+, settings: notifications, transcript detail, your account (/settings)
    ← → / ↑ ↓ (on a panel's edge) resize the panel (Shift: faster; Home / End: smallest / largest; Enter: the default size); or drag the edge, double-click to reset
    Alt+1…9 follow subagent or job N in the agents panel
    Alt+] Alt+[ the next or previous subagent or job
    Alt+0 close the agents panel
    Ctrl+Shift+F open or close the file browser (/files): ↑↓ select, Enter opens, Backspace goes up, F2 renames, Delete deletes, Esc closes
    Alt+R open or close the review page (/review): the changes file by file, beside the chat
    ] / [ (reviewing; also n p, j k) the next / previous changed file
    a / c / e / v / s (reviewing) approve the file; comment on the selected lines (click a line number, Shift+click for a range); edit or comment; the whole file or the changes; send the new comments to prigh
    Esc (reviewing) drop the comment or selection, then close the review
    Ctrl+Enter / Ctrl+S (reviewing) add the comment or reply; save the file being edited
    Ctrl+` open or close the terminal (/terminal); in it, every other key goes to the shell
    Tab (in /scoped-models) check or uncheck the highlighted model
    Delete (in /remind's list) remove the highlighted reminder
    y / a / n / r (confirmation) allow once, always allow (its rules, this session), deny, deny saying why; Enter allows, Esc denies
    ↑↓ Space ←→ (question) choose, pick, previous / next question; type for your own answer; Enter accepts, Esc declines
    Commands
    /help [command] show commands and keys, or a command's usage
    /hotkeys show the keyboard shortcuts
    /settings theme and accent, notifications, transcript detail, tool confirmation, your account and providers (Ctrl+,)
    /new start a new session
    /model [name] pick or switch the model
    /scoped-models pick the models Ctrl+P and Alt+P cycle through
    /fallback [model...|off] show or set the models that take over, in order, when one's usage runs out
    /thinking [off|low|on|high|max] pick or set the thinking level
    /change_default save the model and thinking level as the default for new sessions
    /verbosity [quiet|normal|verbose] how much of tool calls and thinking the transcript shows
    /theme [original|dark|light] pick or set the theme, for you in every browser
    /accent [name|#rrggbb] pick or set the accent colour: orange, blue, green, purple, pink, teal, yellow or any #rrggbb
    /confirm [on|off] ask before bash, job_input, edits and MCP tools run (mode ask or auto, also for new sessions)
    /mode [ask|auto-edit|auto|plan] pick or set the mode: what asks first, or read-only planning (Alt+M cycles)
    /plan [on|off] toggle plan mode: the model investigates and plans, changing no files, until you approve its plan
    /todos show the model's plan (its update_plan steps)
    /permissions show the mode and the allow/ask/deny rules (config and this session)
    /notify [on|off] tell you when a turn ends or something waits for you while the tab is hidden
    /compact [instructions] summarise older messages to free context
    /diff [path] show the uncommitted changes where the tools run, untracked files too
    /review [base] [instructions] alone: the review page, the changes file by file to approve, comment on and edit (Alt+R); with a base (HEAD: the uncommitted changes) or instructions: have the model review them for prioritised findings
    /init write or improve the project's AGENTS.md
    /name [name] rename the session
    /session show the session's details and statistics
    /sessions search the saved sessions
    /switch [path] switch to a saved session
    /archive [session] archive a session (this one without an argument): it moves under Archived in the sidebar
    /unarchive [session] unarchive a session (this one without an argument)
    /delete [session...] delete sessions, after a confirmation (none: select them in the sidebar)
    /delete-archived delete every archived session, after a confirmation
    /clone copy this session into a new one
    /fork start a new session from an earlier message
    /rewind go back to an earlier message in this session
    /tree show the session tree and move to any message in it
    /cd [path] change the working directory (asks to create a missing one)
    /mkdir [path] create a directory (and its parents) and change to it
    /worktree [list|new|open|rm ...] pick a git worktree of the repository to work in (Alt+Enter: in a new session, Delete removes it); new BRANCH [BASE] creates one and works there; open [-n] TARGET (-n: in a new session); rm [-f] TARGET (-f: with uncommitted changes)
    /default-dir [path|off] show or set the directory new sessions start in
    /host [name|backend] pick where tools run, and the directory there
    /export [path] export the transcript on the backend (markdown, or .jsonl)
    /import [path] import a session from a JSONL file on the backend
    /snippet [NAME|add|rm] insert a saved text at the caret (none: pick one, Alt+N); add NAME [TEXT] saves one (no TEXT: your last prompt), rm NAME deletes it
    /skills pick a skill to invoke (/skill:name)
    /skill:<name> [args] send a skill's instructions to the agent, with your arguments
    /mcp [reconnect] MCP servers: their tools, approve a project's, or restart failed ones
    /budget [dollars|off] show or set the session's cost budget: no turn starts once it is spent
    /goal [text|off|resume] set a goal the agent keeps working toward until it is met; show, clear or resume it
    /remind [stop|every|rm|off ...] stop [N] TEXT: remind the agent when it ends its turn (at most N times in a row, default 3); every N TEXT: every N turns; rm NAME, off: remove one or all; none: list the session's (Delete removes one)
    /resume [now|cancel] show, run now or cancel the automatic resume after a usage limit
    /copy copy the last reply to the clipboard
    /btw <question> ask a side question without interrupting the run (not added to the conversation)
    /abort stop the current run
    /terminal open a shell where the tools run, in the session's directory (Ctrl+`)
    /files [path] browse, preview, upload and download files where the tools run (Ctrl+Shift+F)
    /agents [n|id|cancel <n|id>] follow subagents and background jobs in the agents panel, or cancel one
    /jobs [id|kill <id>] background jobs in the agents panel: list them, show or kill one
    /login [provider] log in to a model provider (or /login custom)
    /logout [provider] remove a provider's login
    /auth show which providers are logged in
    /setusr [user] act as another user (superusers); without a user, pick one
    /signout sign out of this account
    /retry-backend-connection reconnect to the backend now
    /state show the session state as the backend reports it
    /clear clear the transcript view (the conversation is kept)
    /quit how to leave (close the tab; /signout signs out)
    Left to the browser
    Ctrl+C / Ctrl+V / Ctrl+Z copy, paste, undo: the browser's (Esc stops a run)
    Ctrl+F find in the page, which has the whole transcript
    Ctrl+T / Ctrl+N / Ctrl+W the browser's tabs and windows: Alt+T cycles thinking
    Ctrl+R reload: the session comes back (?session=); Tab completes @paths
    Ctrl+G no $EDITOR in a browser: edit here (Shift+Enter for new lines)
    |}];
  H.key h "Enter" ~target:Page;
  [%expect
    {|
    Dialog_accept
    (Focus editor)
    |}]
;;

let%expect_test "logging in: pick a provider, open the URL, answer the prompts" =
  let h = H.create () in
  H.type_ h "/login";
  H.act h Send;
  H.reply h "auth_status" H.auth_json;
  H.text h ~selector:".picker-items";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /login) (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Login_picker)))
    (Focus picker-input)
    Anthropic Claude subscription, logged in via auth.json ✓
    Anthropic API key
    OpenAI API key
    DeepSeek API key
    Custom provider add an OpenAI-compatible endpoint (aiproxy, LiteLLM, OpenRouter, vLLM, Ollama…)
    |}];
  H.act h (Picker_query "open");
  H.key h "Enter" ~target:Field;
  [%expect
    {|
    Dialog_accept
    (Focus dialog)
    (Rpc (method_ login) (params ((provider openai) (method api_key)))
     (tag Login_started))
    |}];
  H.text h ~selector:".modal";
  [%expect
    {|
    Log in to OpenAI
    (Close (Esc))
    Waiting for the provider…
    (Cancel)
    |}];
  H.reply h "login" "{}";
  H.event
    h
    {|{"event":"auth","kind":"auth_url","url":"https://example.com/auth?x=1","instructions":"Sign in, then paste the code."}|};
  H.event
    h
    {|{"event":"auth","kind":"progress","message":"Waiting for the browser"}|};
  H.event
    h
    {|{"event":"auth","kind":"prompt","id":"p1","prompt":"manual_code","message":"Paste the code","placeholder":"code#state"}|};
  H.text h ~selector:".modal";
  [%expect
    {|
    (Focus dialog-input)
    Log in to OpenAI
    (Close (Esc))
    Sign in, then paste the code.
    (Open the login page)
    https://example.com/auth?x=1
    Waiting for the browser
    Paste the code
    []
    (Cancel) (Continue)
    |}];
  H.show h ~selector:".login-url a";
  [%expect
    {|
    <a href="https://example.com/auth?x=1"
       target="_blank"
       rel="noopener noreferrer"
       class="btn link-button primary">
      Open the login page
      <icon class="external"> </icon>
    </a>
    |}];
  (* An empty answer is not sent. *)
  H.key h "Enter" ~target:Field;
  H.act h (Dialog_input " abc#def ");
  H.key h "Enter" ~target:Field;
  [%expect
    {|
    Dialog_accept
    Dialog_accept
    (Rpc (method_ auth_respond) (params ((id p1) (value abc#def)))
     (tag Show_error))
    |}];
  H.event
    h
    {|{"event":"auth","kind":"prompt","id":"p2","prompt":"select","message":"Which organisation?","options":[{"id":"o1","label":"Personal"},{"id":"o2","label":"Work"}]}|};
  H.key h "ArrowDown" ~target:Page;
  H.text h ~selector:".login-prompt";
  [%expect
    {|
    (Dialog_move 1)
    Which organisation?
    Personal
    Work
    |}];
  H.key h "Enter" ~target:Page;
  [%expect
    {|
    Dialog_accept
    (Rpc (method_ auth_respond) (params ((id p2) (value o2))) (tag Show_error))
    |}];
  H.event
    h
    {|{"event":"auth","kind":"done","provider":"openai","method":"api_key"}|};
  H.text h ~selector:".modal";
  H.text h ~selector:".toast";
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 2660))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ list_models) (params ()) (tag Models))
    Logged in to openai (api_key)
    |}]
;;

let%expect_test "a login that fails says so; Esc cancels one in progress" =
  let h = H.create () in
  H.act h (Start_login "anthropic");
  H.fail h "login" "already logging in to openai";
  H.text h ~selector:".modal-body";
  [%expect
    {|
    (Focus dialog)
    (Rpc (method_ login) (params ((provider anthropic))) (tag Login_started))
    Login failed: already logging in to openai. /login tries again.
    |}];
  H.key h "Escape" ~target:Page;
  [%expect
    {|
    Close_dialog
    (Focus editor)
    |}];
  H.act h (Start_login "anthropic");
  H.reply h "login" "{}";
  H.key h "Escape" ~target:Page;
  [%expect
    {|
    (Focus dialog)
    (Rpc (method_ login) (params ((provider anthropic))) (tag Login_started))
    Close_dialog
    (Focus editor)
    (Rpc (method_ auth_cancel) (params ()) (tag Show_error))
    |}];
  (* A failure after the dialog was closed is a toast. *)
  H.event
    h
    {|{"event":"auth","kind":"failed","provider":"anthropic","error":"timed out"}|};
  H.text h ~selector:".toast";
  [%expect {| |}]
;;

let%expect_test "/auth shows the providers; /logout picks a logged-in one" =
  let h = H.create () in
  H.type_ h "/auth";
  H.act h Send;
  H.reply h "auth_status" H.auth_json;
  H.text h ~selector:".modal-body";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /auth) (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Show)))
    (Focus dialog)
    Anthropic logged in with oauth (auth.json)
    (Log out)
    OpenAI not logged in
    (Log in)
    DeepSeek not logged in
    (Log in)
    |}];
  H.act h (Logout "anthropic");
  [%expect
    {| (Rpc (method_ logout) (params ((provider anthropic))) (tag Show_error)) |}];
  H.event h {|{"event":"auth","kind":"logged_out","provider":"anthropic"}|};
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 2500))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ list_models) (params ()) (tag Models))
    |}];
  H.reply h "auth_status" H.auth_json;
  H.act h Close_dialog;
  H.type_ h "/logout";
  H.act h Send;
  H.reply h "auth_status" H.auth_json;
  H.text h ~selector:".picker-items";
  [%expect
    {|
    (Focus editor)
    (Rpc (method_ push_pref) (params ((key history) (value /logout) (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Logout_picker)))
    (Focus picker-input)
    Anthropic oauth via auth.json
    |}];
  H.act h Picker_accept;
  [%expect
    {|
    (Focus editor)
    (Rpc (method_ logout) (params ((provider anthropic))) (tag Show_error))
    |}];
  H.type_ h "/logout";
  H.act h Send;
  H.reply h "auth_status" {|[]|};
  H.text h ~selector:".toast";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /logout) (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Logout_picker)))
    (Expire_toast (id 1) (after_ms 3420))
    (Focus editor)
    Logged out of anthropic
    No provider is logged in: /login logs in to one.
    |}]
;;

let%expect_test
    "toasts: notices expire, errors stay until dismissed, at most two"
  =
  let h = H.create () in
  H.event h {|{"event":"notice","text":"Compacted"}|};
  H.act h (Show_toast { text = "Upload failed"; error = true });
  H.text h ~selector:".toasts";
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 2500))
    Compacted
    Upload failed (Dismiss)
    |}];
  H.act h (Dismiss_toast 0);
  H.text h ~selector:".toasts";
  [%expect {| Upload failed (Dismiss) |}];
  List.iter [ "a"; "b"; "c"; "d" ] ~f:(fun text ->
    H.act h (Show_toast { text; error = true }));
  H.text h ~selector:".toasts";
  [%expect
    {|
    c (Dismiss)
    d (Dismiss)
    |}]
;;

let%expect_test
    "losing the connection: a banner with the attempt, then a fresh start"
  =
  let h = H.create () in
  H.event
    h
    {|{"event":"message_start","message":{"role":"user","text":"hello"}}|};
  H.act h Backend_closed;
  H.text h ~selector:".banner";
  [%expect
    {|
    (Reconnect (generation 1) (delay_ms 0) (session (s1)))
    Connection lost: reconnecting… (Retry now)
    |}];
  H.act h (Reply (Reconnect 1, Error "refused"));
  H.act h (Reply (Reconnect 1, Error "refused"));
  H.text h ~selector:".banner";
  [%expect
    {|
    (Reconnect (generation 1) (delay_ms 250) (session (s1)))
    (Reconnect (generation 1) (delay_ms 500) (session (s1)))
    Connection lost: reconnecting (attempt 3, next in 0.5s)… (Retry now)
    |}];
  (* A second close while reconnecting changes nothing. *)
  H.act h Backend_closed;
  [%expect {| |}];
  H.act h (Reply (Reconnect 1, Ok (Jsonaf.of_string {|{"client_id":"c9"}|})));
  [%expect
    {|
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ get_prefs)
     (params
      ((keys
        (web.theme web.accent web.notify web.verbosity web.terminal-open
         web.sidebar-folded history web.migrated web.sidebar-width
         web.files-width web.files-split web.agents-width web.terminal-height))))
     (tag Prefs))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s1.jsonl) (limit 1000000)))
     (tag (Resync (session s1) (from ()))))
    (Rpc (method_ get_pending) (params ()) (tag Resync_pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    |}];
  H.text h ~selector:".banner";
  H.text h ~selector:".toast";
  [%expect {| |}];
  H.reply h "get_state" (H.state_json ());
  [%expect
    {| |}];
  (* Replies to an older connection's reconnects are ignored. *)
  H.act h (Reply (Reconnect 0, Error "late"));
  [%expect {| |}];
  print_s [%sexp ((H.model h).connection : Prigh_web.App.Connection.t)];
  [%expect {| Connected |}]
;;

let%expect_test "the reconnect delay doubles up to ten seconds" =
  List.iter [ 0; 1; 2; 3; 6; 7; 8; 20 ] ~f:(fun attempt ->
    printf "%d: %dms\n" attempt (Prigh_web.App.Connection.delay_ms ~attempt));
  [%expect
    {|
    0: 0ms
    1: 250ms
    2: 500ms
    3: 1000ms
    6: 8000ms
    7: 16000ms
    8: 30000ms
    20: 30000ms
    |}]
;;

let%expect_test "signing out" =
  let h = H.create () in
  H.text h ~selector:".sidebar-footer";
  [%expect {| (Commands & keys) |}];
  H.act h Saved_login;
  H.text h ~selector:".sidebar-footer";
  [%expect {| (Commands & keys) (Sign out) |}];
  H.act
    h
    (Hello { client_id = "c1"; namespace = Some "bob"; user = Some "ann" });
  H.text h ~selector:".sidebar-footer";
  [%expect {| (A ann acting as bob) (Commands and keys (/help)) |}];
  H.act h Sign_out;
  H.type_ h "/signout";
  H.act h Send;
  [%expect
    {|
    Sign_out
    (Cache_prefs (((key history) (value ((/signout))))))
    Sign_out
    |}]
;;

let%expect_test "on a phone the agents panel is a sheet with the keyboard" =
  let h = H.create () in
  let inert () = H.elements h ~selector:"[inert]" in
  H.act h (Set_narrow true);
  H.act h Toggle_subagents;
  inert ();
  [%expect
    {|
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Focus agents-panel)
    <aside class="sidebar">
    <header class="topbar">
    <div class="chat-frame">
    <footer class="composer">
    |}];
  H.act h Toggle_subagents;
  inert ();
  [%expect {| <aside class="sidebar"> |}]
;;
