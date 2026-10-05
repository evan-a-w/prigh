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
    (Focus confirm)
    Allow bash?
    (Close (Esc))
    rm -rf build
    1 more waiting (Deny) (Allow)
    |}];
  (* Even with the editor focused and a popup open. *)
  H.type_ h "/";
  H.key h "Enter";
  [%expect
    {|
    (Respond_confirm (call_id c1) (allow true))
    (Rpc (method_ tool_confirm_respond) (params ((call_id c1) (allow true)))
     (tag Show_error))
    (Focus editor)
    |}];
  H.key h "ArrowDown";
  H.key h "Escape";
  [%expect
    {|
    (browser default)
    (Respond_confirm (call_id c2) (allow false))
    (Rpc (method_ tool_confirm_respond) (params ((call_id c2) (allow false)))
     (tag Show_error))
    (Focus editor)
    |}];
  H.text h ~selector:".modal";
  [%expect {| |}];
  (* A confirmation the agent no longer waits for goes away. *)
  H.event h (confirm "c3" "ls");
  H.event
    h
    {|{"event":"tool_end","call":{"id":"c3","name":"bash","arguments":"{\"command\":\"ls\"}"},"result":{"tool_call_id":"c3","tool_name":"bash","text":"cancelled","is_error":true}}|};
  H.text h ~selector:".modal";
  [%expect {| (Focus confirm) |}]
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
    (Respond_confirm (call_id c1) (allow true))
    (Rpc (method_ tool_confirm_respond) (params ((call_id c1) (allow true)))
     (tag Show_error))
    (Focus editor)
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
    Alt+Enter queue a follow-up for after the run
    Shift+Enter new line
    Esc stop the run; close a dialog, popup or side answer
    ↑ ↓ earlier prompts (in an empty editor or its first line)
    Alt+↑ take the last queued message back into the editor
    Tab complete a /command, its argument or an @path
    !cmd run a shell command (!!cmd: not added to the context; !&cmd: as a background job)
    Ctrl+L switch model
    Ctrl+P / Alt+P next / previous scoped model (/scoped-models)
    Alt+T cycle the thinking level
    Ctrl+O cycle the transcript verbosity
    Ctrl+X copy the last reply (when nothing is selected)
    Ctrl+↑ / Ctrl+↓ previous / next of your messages in the transcript
    PageUp / PageDown scroll the transcript
    Ctrl+K search sessions (↓ ↑ through them, Enter opens, Delete deletes)
    Ctrl+B show or hide the sidebar
    Alt+1…9 follow subagent or job N in the agents panel
    Alt+] Alt+[ the next or previous subagent or job
    Alt+0 close the agents panel
    Ctrl+` open or close the terminal (/terminal); in it, every other key goes to the shell
    Tab (in /scoped-models) check or uncheck the highlighted model
    Commands
    /help [command] show commands and keys, or a command's usage
    /hotkeys show the keyboard shortcuts
    /new start a new session
    /model [name] pick or switch the model
    /scoped-models pick the models Ctrl+P and Alt+P cycle through
    /thinking [off|low|on|high|max] pick or set the thinking level
    /change_default save the model and thinking level as the default for new sessions
    /verbosity [quiet|normal|verbose] how much of tool calls and thinking the transcript shows
    /confirm [on|off] ask before bash, write and edit run
    /compact [instructions] summarise older messages to free context
    /name [name] rename the session
    /session show the session's details and statistics
    /sessions search the saved sessions
    /switch [path] switch to a saved session
    /clone copy this session into a new one
    /fork start a new session from an earlier message
    /rewind go back to an earlier message in this session
    /tree show the session tree and move to any message in it
    /cd [path] change the working directory
    /host [name|backend] pick where tools run, and the directory there
    /export [path] export the transcript on the backend (markdown, or .jsonl)
    /import [path] import a session from a JSONL file on the backend
    /skills pick a skill to invoke (/skill:name)
    /skill:<name> [args] send a skill's instructions to the agent, with your arguments
    /mcp [reconnect] MCP servers: their tools, approve a project's, or restart failed ones
    /copy copy the last reply to the clipboard
    /btw <question> ask a side question without interrupting the run (not added to the conversation)
    /abort stop the current run
    /terminal open a shell where the tools run, in the session's directory (Ctrl+`)
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
    (Save_history (/login))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Login_picker)))
    (Focus picker-input)
    Anthropic Claude subscription · logged in via auth.json ✓
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
    (Expire_toast (id 0) (after_ms 4000))
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
    (Save_history (/auth))
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
    (Expire_toast (id 0) (after_ms 4000))
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
    (Save_history (/logout /auth))
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
    (Save_history (/logout /auth))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Logout_picker)))
    (Expire_toast (id 1) (after_ms 4000))
    Logged out of anthropic
    No provider is logged in: /login logs in to one.
    |}]
;;

let%expect_test
    "toasts: notices expire, errors stay until dismissed, at most four"
  =
  let h = H.create () in
  H.event h {|{"event":"notice","text":"Compacted"}|};
  H.act h (Show_toast { text = "Upload failed"; error = true });
  H.text h ~selector:".toasts";
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 4000))
    Compacted
    Upload failed
    |}];
  H.act h (Dismiss_toast 0);
  H.text h ~selector:".toasts";
  [%expect {| Upload failed |}];
  List.iter [ "a"; "b"; "c"; "d" ] ~f:(fun text ->
    H.act h (Show_toast { text; error = true }));
  H.text h ~selector:".toasts";
  [%expect
    {|
    a
    b
    c
    d
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
    (Expire_toast (id 0) (after_ms 4000))
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    |}];
  H.text h ~selector:".banner";
  H.text h ~selector:".toast";
  [%expect {| Reconnected |}];
  H.reply h "get_state" (H.state_json ());
  [%expect
    {|
    (Set_url_session s1)
    (Rpc (method_ get_messages) (params ()) (tag (Messages s1)))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    |}];
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
    7: 10000ms
    8: 10000ms
    20: 10000ms
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
    (Save_history (/signout))
    Sign_out
    |}]
;;
