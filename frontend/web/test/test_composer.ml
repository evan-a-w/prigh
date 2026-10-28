open! Core
module H = Harness

let running_state = H.state_json ~fields:[ "running", `True ] ()

let%expect_test
    "Enter sends, Shift+Enter is a newline, Alt+Enter queues a follow-up"
  =
  let h = H.create () in
  H.text h ~selector:".composer-box";
  [%expect
    {|
    []
    (Claude Opus 5.5)
    (Thinking on)
    (Auto: never asks)
    (backend)
    (Attach files (or drop them here)) (Send (Enter))
    |}];
  H.show h ~selector:".send";
  [%expect
    {|
    <button type="button"
            title="Send (Enter)"
            aria-label="Send"
            disabled=""
            class="btn primary send"
            @on_click>
      <icon class="send"> </icon>
    </button>
    <icon class="send"> </icon>
    |}];
  (* An empty editor sends nothing. *)
  H.key h "Enter";
  [%expect {| Send |}];
  H.type_ h "line one";
  H.key h "Enter" ~shift:true;
  [%expect {| (browser default) |}];
  H.type_ h "line one\nline two";
  H.key h "Enter";
  [%expect
    {|
    Send
    (Rpc (method_ push_pref)
     (params ((key history) (value  "line one\
                                   \nline two") (max 500)))
     (tag Ignore))
    Scroll_to_bottom
    (Rpc (method_ prompt) (params ((text  "line one\
                                         \nline two")))
     (tag (Sent (text  "line one\
                      \nline two") (images ()))))
    |}];
  print_s [%sexp ((H.model h).draft : string)];
  [%expect {| "" |}];
  H.event h (sprintf {|{"event":"state","state":%s}|} running_state);
  H.show h ~selector:"#editor";
  [%expect
    {|
    <textarea id="editor"
              placeholder="Steer (Enter) or queue a follow-up (Alt+Enter)…"
              aria-label="Message"
              dir="auto"
              rows="1"
              autocomplete="off"
              enterkeyhint="send"
              class="editor"
              #value=""
              @on_input> </textarea>
    |}];
  H.type_ h "use the other file";
  H.key h "Enter";
  H.type_ h "then run the tests";
  H.key h "Enter" ~alt:true;
  [%expect
    {|
    Send
    (Rpc (method_ push_pref)
     (params ((key history) (value "use the other file") (max 500)))
     (tag Ignore))
    Scroll_to_bottom
    (Rpc (method_ steer) (params ((text "use the other file")))
     (tag (Sent (text "use the other file") (images ()))))
    Send_follow_up
    (Rpc (method_ push_pref)
     (params ((key history) (value "then run the tests") (max 500)))
     (tag Ignore))
    Scroll_to_bottom
    (Rpc (method_ follow_up) (params ((text "then run the tests")))
     (tag (Sent (text "then run the tests") (images ()))))
    |}];
  (* Esc stops the run; when idle it does nothing. *)
  H.key h "Escape";
  [%expect
    {|
    Abort
    (Rpc (method_ abort) (params ()) (tag Restored))
    |}];
  H.event h (sprintf {|{"event":"state","state":%s}|} (H.state_json ()));
  H.key h "Escape";
  [%expect
    {|
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nbackend\
                            \n/work") (purpose (Sidebar 2)))))
    (browser default)
    |}]
;;

let image =
  { Prigh_protocol.Image.mime_type = "image/png"
  ; data = "iVBORw0KGgo="
  ; bytes = 8
  }
;;

let%expect_test "pending images: thumbnails, remove, sent with the prompt" =
  let h = H.create () in
  H.act h (Add_image image);
  H.act h (Add_image { image with mime_type = "image/jpeg" });
  H.show h ~selector:".pending-images";
  [%expect
    {|
    <div class="pending-images">
      <div class="pending-image">
        <img src="data:image/png;base64,iVBORw0KGgo="
             alt="[image: image/png, 8 B]"
             title="[image: image/png, 8 B]"/>
        <button type="button"
                title="Remove image"
                aria-label="Remove image"
                class="btn remove-image"
                @on_click>
          <icon class="close"> </icon>
        </button>
      </div>
      <div class="pending-image">
        <img src="data:image/jpeg;base64,iVBORw0KGgo="
             alt="[image: image/jpeg, 8 B]"
             title="[image: image/jpeg, 8 B]"/>
        <button type="button"
                title="Remove image"
                aria-label="Remove image"
                class="btn remove-image"
                @on_click>
          <icon class="close"> </icon>
        </button>
      </div>
    </div>
    |}];
  H.act h (Remove_image 1);
  (* An image alone can be sent. *)
  H.key h "Enter";
  [%expect
    {|
    Send
    Scroll_to_bottom
    (Rpc (method_ prompt)
     (params ((text "") (images (((mime_type image/png) (data iVBORw0KGgo=))))))
     (tag (Sent (text "") (images (((mime_type image/png) (bytes 8)))))))
    |}];
  H.show h ~selector:".pending-images";
  [%expect {| |}];
  (* A slash command with an image attached is a prompt. *)
  H.act h (Add_image image);
  H.type_ h "/help";
  H.act h Send;
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /help) (max 500)))
     (tag Ignore))
    Scroll_to_bottom
    (Rpc (method_ prompt)
     (params
      ((text /help) (images (((mime_type image/png) (data iVBORw0KGgo=))))))
     (tag (Sent (text /help) (images (((mime_type image/png) (bytes 8)))))))
    |}]
;;

let%expect_test "prompt history: Up and Down at the first and last line" =
  let h = H.create () in
  H.restore_history h [ "newest"; "older\ntwo lines"; "oldest" ];
  (* Down does nothing until browsing. *)
  H.key h "ArrowDown";
  H.type_ h "my draft";
  H.key h "ArrowUp" ~target:(Editor { cursor = 3 });
  print_s [%sexp ((H.model h).draft : string)];
  [%expect
    {|
    (Apply_theme ((name Original) (accent (Preset Orange))))
    (browser default)
    History_older
    newest
    |}];
  H.key h "ArrowUp" ~target:(Editor { cursor = 0 });
  print_s [%sexp ((H.model h).draft : string)];
  [%expect
    {|
    History_older
     "older\
    \ntwo lines"
    |}];
  (* Inside a multi-line entry the arrows move the caret. *)
  H.key h "ArrowDown" ~target:(Editor { cursor = 2 });
  H.key h "ArrowUp" ~target:(Editor { cursor = 8 });
  [%expect
    {|
    (browser default)
    (browser default)
    |}];
  H.key h "ArrowUp" ~target:(Editor { cursor = 0 });
  H.key h "ArrowUp" ~target:(Editor { cursor = 0 });
  print_s [%sexp ((H.model h).draft : string)];
  [%expect
    {|
    History_older
    (browser default)
    oldest
    |}];
  H.key h "ArrowDown";
  H.key h "ArrowDown";
  H.key h "ArrowDown";
  print_s [%sexp ((H.model h).draft : string)];
  [%expect
    {|
    History_newer
    History_newer
    History_newer
    "my draft"
    |}];
  H.key h "ArrowDown";
  [%expect {| (browser default) |}];
  (* Sending saves it, newest first, without repeating the last entry. *)
  H.act h Send;
  H.type_ h "my draft";
  H.act h Send;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "my draft") (max 500))) (tag Ignore))
    Scroll_to_bottom
    (Rpc (method_ prompt) (params ((text "my draft")))
     (tag (Sent (text "my draft") (images ()))))
    (Rpc (method_ push_pref)
     (params ((key history) (value "my draft") (max 500))) (tag Ignore))
    Scroll_to_bottom
    (Rpc (method_ prompt) (params ((text "my draft")))
     (tag (Sent (text "my draft") (images ()))))
    |}]
;;

let%expect_test "@ completes paths from the backend" =
  let h = H.create () in
  H.type_ h "look at @src/ma";
  [%expect
    {| (Rpc (method_ list_paths) (params ((prefix src/ma))) (tag (Paths src/ma))) |}];
  (* Nothing to show until the backend answers. *)
  H.text h ~selector:".popup";
  [%expect {| |}];
  H.reply h "list_paths" {|["src/main.ml","src/main.mli","src/macros/"]|};
  H.text h ~selector:".popup";
  [%expect
    {|
    Files ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    src/main.ml
    src/macros/
    src/main.mli
    |}];
  H.key h "ArrowDown";
  H.key h "ArrowDown";
  H.key h "Tab";
  print_s [%sexp ((H.model h).draft : string)];
  [%expect
    {|
    (Complete_move 1)
    (Complete_move 1)
    (Complete_accept (run false))
    (Set_caret 22)
    "look at @src/main.mli "
    |}];
  (* Directories keep completing; a stale answer is ignored. *)
  H.type_ h "look at @src/m";
  H.type_ h "look at @src/macros/";
  H.reply h "list_paths" {|["src/main.ml"]|};
  H.text h ~selector:".popup";
  [%expect
    {|
    (Rpc (method_ list_paths) (params ((prefix src/m))) (tag (Paths src/m)))
    (Rpc (method_ list_paths) (params ((prefix src/macros/)))
     (tag (Paths src/macros/)))
    |}];
  H.reply h "list_paths" {|["src/macros/x.ml","src/macros/y/"]|};
  H.text h ~selector:".popup-items";
  H.key h "Enter";
  print_s [%sexp ((H.model h).draft : string)];
  [%expect
    {|
    src/macros/y/
    src/macros/x.ml
    (Complete_accept (run true))
    (Rpc (method_ list_paths) (params ((prefix src/macros/y/)))
     (tag (Paths src/macros/y/)))
    (Set_caret 22)
    "look at @src/macros/y/"
    |}];
  (* Esc closes the popup; it does not stop anything. *)
  H.reply h "list_paths" {|["src/macros/y/z.ml"]|};
  H.key h "Escape";
  H.text h ~selector:".popup";
  [%expect {| Complete_close |}];
  (* A failed listing shows nothing. *)
  H.type_ h "see @zz";
  H.fail h "list_paths" "no such directory";
  H.text h ~selector:".toasts";
  [%expect
    {| (Rpc (method_ list_paths) (params ((prefix zz))) (tag (Paths zz))) |}]
;;

let%expect_test "Ctrl+Enter or Cmd+Enter sends what is typed, not the highlight"
  =
  let h = H.create () in
  H.type_ h "/cd new";
  H.reply h "list_dirs" {|["newer/"]|};
  H.text h ~selector:".popup";
  [%expect
    {|
    (Rpc (method_ list_dirs) (params ((prefix new))) (tag (Paths new)))
    Directories ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    newer/
    |}];
  H.key h "Enter" ~ctrl:true;
  let popup = Option.is_some (Prigh_web.App.Model.popup (H.model h)) in
  print_s [%sexp ((H.model h).draft : string), (popup : bool)];
  [%expect
    {|
    Send
    (Rpc (method_ push_pref) (params ((key history) (value "/cd new") (max 500)))
     (tag Ignore))
    (Rpc (method_ set_cwd) (params ((path new) (if_missing ask)))
     (tag (Dir_changed Cwd)))
    ("" false)
    |}];
  H.type_ h "look at @src/ma";
  H.reply h "list_paths" {|["src/main.ml"]|};
  H.key h "Enter" ~meta:true;
  [%expect
    {|
    (Rpc (method_ list_paths) (params ((prefix src/ma))) (tag (Paths src/ma)))
    Send
    (Rpc (method_ push_pref)
     (params ((key history) (value "look at @src/ma") (max 500))) (tag Ignore))
    Scroll_to_bottom
    (Rpc (method_ prompt) (params ((text "look at @src/ma")))
     (tag (Sent (text "look at @src/ma") (images ()))))
    |}];
  (* Without a popup it is Enter. *)
  H.type_ h "hello";
  H.key h "Enter" ~ctrl:true;
  [%expect
    {|
    Send
    (Rpc (method_ push_pref) (params ((key history) (value hello) (max 500)))
     (tag Ignore))
    Scroll_to_bottom
    (Rpc (method_ prompt) (params ((text hello)))
     (tag (Sent (text hello) (images ()))))
    |}]
;;

let%expect_test "slash commands: the popup, arguments, running, unknown ones" =
  let h = H.create () in
  H.type_ h "/";
  H.text h ~selector:".popup";
  [%expect
    {|
    Commands ↑↓ Tab Enter Esc   Ctrl+Enter as typed
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
    /filter [all|KIND...|-KIND|+KIND] show only some kinds in the transcript (your messages, replies, thinking, tools, subagents, notes); the model's context is unchanged
    /theme [original|dark|light] pick or set the theme, for you in every browser
    /accent [name|#rrggbb] pick or set the accent colour: orange, blue, green, purple, pink, teal, yellow or any #rrggbb
    /confirm [on|off] ask before bash, job_input, edits and MCP tools run (mode ask or auto, also for new sessions)
    /mode [ask|auto-edit|auto|plan] pick or set the mode: what asks first, or read-only planning (Alt+M cycles)
    /plan [on|off] toggle plan mode: the model investigates and plans, changing no files, until you approve its plan
    /todos show the model's plan (its update_plan steps)
    /permissions show the mode and the allow/ask/deny rules (config and this session)
    /notify [on|off] tell you when a turn ends or something waits for you while the tab is hidden
    /tabs [on|off] show the open sessions as tabs over the chat (saved for your account)
    /compact [instructions] summarise older messages to free context
    /compaction [at|model|mask ...] show when conversations are compacted and what summarises them; at TOKENS|off: also once the context exceeds TOKENS, not only at 80% of the window; model MODEL|off: the model that writes the summaries (off: the session's); mask on|off: replace old tool outputs with placeholders first
    /diff [path] show the uncommitted changes where the tools run, untracked files too
    /review [worktree] [--base REV] the review page, the changes file by file to approve, comment on and edit (Alt+R): of the session's worktree or another (its branch, path or name), against its base or REV; /review BASE [instructions] (HEAD: the uncommitted changes) has the model review them for prioritised findings
    /init write or improve the project's AGENTS.md
    /name [name] rename the session
    /session show the session's details and statistics
    /trace where the session's time went: model, tools, waiting on you, the slowest steps (prigh trace SESSION -perfetto FILE exports a timeline)
    /sessions search the saved sessions
    /switch [path] switch to a saved session
    /archive [session] archive a session (this one without an argument): it moves under Archived in the sidebar
    /unarchive [session] unarchive a session (this one without an argument)
    /delete [undo|session...] delete a message from the transcript and the model's context (a reply with its tool calls' results): pick it, newest first; undo brings the latest deletion back; with sessions, deletes them after a confirmation
    /delete-archived delete every archived session, after a confirmation
    /clone copy this session into a new one
    /fork start a new session from an earlier message
    /rewind go back to an earlier message in this session
    /trim [after|undo] drop everything before a message from the model's context (the transcript keeps it folded); after: everything after a message instead (what you send later stays); undo brings back the last trim
    /tree show the session tree and move to any message in it
    /cd [path] change the working directory (asks to create a missing one)
    /mkdir [path] create a directory (and its parents) and change to it
    /worktree [list|new|open|back|rm] pick a git worktree of the repository to work in (Alt+Enter: in a new session, Alt+R: review its changes, Delete removes it); new BRANCH [BASE] creates one and works there; open [-n] TARGET (-n: in a new session); back [main]: back where the session switched into worktrees from; rm [-f] TARGET (-f: with uncommitted changes)
    /default-dir [path|off] show or set the directory new sessions start in
    /host [name|backend] pick where tools run, and the directory there
    /export [path] export the transcript on the backend (markdown, or .jsonl)
    /import [path] import a session from a JSONL file on the backend
    /snippet [NAME|add|rm] insert a saved text at the caret (none: pick one, Alt+N); add NAME [TEXT] saves one (no TEXT: your last prompt), rm NAME deletes it
    /skills pick a skill to invoke (/skill:name)
    /skill:<name> [args] send a skill's instructions to the agent, with your arguments
    /mcp [reconnect] MCP servers: their tools, approve a project's, or restart failed ones
    |}];
  H.type_ h "/mod";
  H.key h "Tab";
  print_s [%sexp ((H.model h).draft : string)];
  H.text h ~selector:".popup";
  [%expect
    {|
    (Complete_accept (run false))
    (Set_caret 6)
    "/mode "
    Modes ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    ask bash, job_input, edits and MCP tools ask first
    auto-edit file edits run; bash, job_input and MCP tools ask first
    auto tools run without asking
    plan read-only planning: no file edits; bash, job_input and MCP tools ask first
    |}];
  H.type_ h "/model gpt";
  H.key h "Enter";
  [%expect
    {|
    (Complete_accept (run true))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/model openai/gpt-6") (max 500)))
     (tag Ignore))
    (Rpc (method_ set_model) (params ((model openai/gpt-6))) (tag Model_set))
    (Expire_toast (id 0) (after_ms 4140))
    |}];
  H.type_ h "/comp";
  H.key h "Enter";
  H.text h ~selector:".toast";
  [%expect
    {|
    (Complete_accept (run true))
    (Rpc (method_ push_pref) (params ((key history) (value /compact) (max 500)))
     (tag Ignore))
    (Expire_toast (id 1) (after_ms 2700))
    (Rpc (method_ compact) (params ())
     (tag (Notice "Compacted the conversation")))
    GPT-6 needs openai, which is not logged in: /login openai logs in.
    Compacting the conversation…
    |}];
  H.reply h "compact" {|{"summary":"..."}|};
  H.text h ~selector:".toast";
  [%expect
    {|
    (Expire_toast (id 2) (after_ms 2540))
    Compacting the conversation…
    Compacted the conversation
    |}];
  H.act h (Dismiss_toast 0);
  H.act h (Dismiss_toast 1);
  H.act h (Dismiss_toast 2);
  (* Unknown commands name the closest one, and stay in the editor to be
     corrected. *)
  H.type_ h "/compcat";
  H.act h Send;
  print_s [%sexp ((H.model h).draft : string)];
  H.text h ~selector:".toast";
  H.type_ h "/frobnicate";
  H.act h Send;
  print_s [%sexp ((H.model h).draft : string)];
  (* Sending again clears the errors about what was sent before. *)
  H.text h ~selector:".toast";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /compcat) (max 500)))
     (tag Ignore))
    /compcat
    Unknown command /compcat. Did you mean /compact? (/help lists them) (Dismiss)
    (Rpc (method_ push_pref)
     (params ((key history) (value /frobnicate) (max 500))) (tag Ignore))
    /frobnicate
    Unknown command /frobnicate: /help lists the commands. (Dismiss)
    |}];
  (* A path with a question is a prompt, not a command. *)
  H.type_ h "/etc/hosts has a typo?";
  H.act h Send;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/etc/hosts has a typo?") (max 500)))
     (tag Ignore))
    Scroll_to_bottom
    (Rpc (method_ prompt) (params ((text "/etc/hosts has a typo?")))
     (tag (Sent (text "/etc/hosts has a typo?") (images ()))))
    |}];
  H.type_ h "/help";
  H.key h "Enter";
  H.text h ~selector:".modal h2";
  [%expect
    {|
    (Complete_accept (run true))
    (Rpc (method_ push_pref) (params ((key history) (value /help) (max 500)))
     (tag Ignore))
    (Focus dialog)
    Commands and keys
    |}]
;;

let%expect_test "/cd completes directories" =
  let h = H.create () in
  H.type_ h "/cd ";
  [%expect
    {| (Rpc (method_ list_dirs) (params ((prefix ""))) (tag (Paths ""))) |}];
  H.reply h "list_dirs" {|["src/","test/"]|};
  H.key h "Enter";
  print_s [%sexp ((H.model h).draft : string)];
  [%expect
    {|
    (Complete_accept (run true))
    (Rpc (method_ list_dirs) (params ((prefix src/))) (tag (Paths src/)))
    (Set_caret 8)
    "/cd src/"
    |}];
  H.reply h "list_dirs" "[]";
  H.key h "Enter";
  [%expect
    {|
    Send
    (Rpc (method_ push_pref)
     (params ((key history) (value "/cd src/") (max 500))) (tag Ignore))
    (Rpc (method_ set_cwd) (params ((path src/) (if_missing ask)))
     (tag (Dir_changed Cwd)))
    |}];
  H.type_ h "/cd";
  H.act h Send;
  H.text h ~selector:".toast";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /cd) (max 500)))
     (tag Ignore))
    (Focus dialog-input)
    (Rpc (method_ list_dirs) (params ((prefix /work) (host backend)))
     (tag (Prompt_paths /work)))
    |}]
;;

let%expect_test "a message that cannot be sent comes back to the editor" =
  let h = H.create () in
  H.act
    h
    (Add_image { mime_type = "image/png"; bytes = 8; data = "iVBORw0KGgo=" });
  H.type_ h "look at this";
  H.key h "Enter";
  H.fail h "prompt" "a run is already in progress";
  print_s
    [%sexp ((H.model h).draft : string), (List.length (H.model h).images : int)];
  H.text h ~selector:".toasts";
  [%expect
    {|
    Send
    (Rpc (method_ push_pref)
     (params ((key history) (value "look at this") (max 500))) (tag Ignore))
    Scroll_to_bottom
    (Rpc (method_ prompt)
     (params
      ((text "look at this")
       (images (((mime_type image/png) (data iVBORw0KGgo=))))))
     (tag
      (Sent (text "look at this") (images (((mime_type image/png) (bytes 8)))))))
    ("look at this" 1)
    Couldn't send: a run is already in progress. Your message is back in the editor. (Dismiss)
    |}];
  (* Without a connection nothing is sent, and the draft stays. *)
  H.act h Backend_closed;
  H.key h "Enter";
  print_s [%sexp ((H.model h).draft : string)];
  H.text h ~selector:".toasts .error";
  [%expect
    {|
    (Reconnect (generation 1) (delay_ms 0) (session (s1)))
    Send
    "look at this"
    Not connected to the backend: your message stays here until it is back (/retry-backend-connection tries now). (Dismiss)
    |}]
;;
