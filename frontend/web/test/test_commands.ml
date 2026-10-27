open! Core
open Prigh_web
module H = Harness

(* The TUI's commands in prigh-web: each opens what the TUI opens (a picker,
   a dialog, a panel) or says why it makes no sense in a browser. *)

let run h command =
  H.type_ h command;
  H.act h Send
;;

let toasts h = H.text h ~selector:".toast"

let messages =
  {|[{"role":"user","text":"fix the build"},
     {"role":"assistant","content":[{"type":"text","text":"Fixed: the **flag** was wrong."}],"stop_reason":{"type":"end_turn"},"usage":{"input":10,"output":5,"cache_read":0},"model":"m"}]|}
;;

let with_messages h =
  H.act h (Reply (H.newest "s1", Ok (Jsonaf.of_string (H.page messages))))
;;

let%expect_test "every command does something (none is unknown)" =
  List.iter (Slash.all ~superuser:true) ~f:(fun spec ->
    let h = H.create () in
    let m = H.model h in
    let hello =
      { Prigh_protocol.Hello_reply.client_id = "c1"
      ; namespace = Some "alice"
      ; user = Some "alice"
      ; superuser = true
      }
    in
    let m', commands =
      App.update { m with draft = "/" ^ spec.name; hello = Some hello } Send
    in
    let what =
      match m'.dialog, m'.toasts, commands with
      | Some dialog, _, _ ->
        (match dialog with
         | Picker { picker; _ } -> "picker: " ^ Picker.title picker
         | Scoped_models _ -> "scoped models dialog"
         | Prompt _ -> "path prompt"
         | Text { title; _ } -> "text: " ^ title
         | (Loading _ | Pulls _) as d ->
           sprintf
             "%s: %s"
             (match d with
              | Loading title -> "loading " ^ title
              | _ -> "pull requests")
             (List.filter_map commands ~f:(function
                | Rpc { method_; _ } -> Some ("rpc " ^ method_)
                | _ -> None)
              |> String.concat ~sep:", ")
         | d -> String.prefix (Sexp.to_string (Dialog.sexp_of_t d)) 30)
      | None, _, _ when m'.agents.open_ -> "agents panel"
      | None, (_ :: _ as toasts), _ -> "toast: " ^ (List.last_exn toasts).text
      | None, [], _ ->
        List.filter_map commands ~f:(function
          | Rpc { method_ = "push_pref"; _ } -> None
          | Rpc { method_; _ } -> Some ("rpc " ^ method_)
          | Cache_prefs _ | Focus _ -> None
          | c -> Some (Sexp.to_string (App.Command.sexp_of_t c)))
        |> String.concat ~sep:", "
        |> fun s ->
        if String.is_empty s && m'.sidebar_open then "sidebar" else s
    in
    printf "/%-25s %s\n" spec.name what);
  [%expect
    {|
    /help                      Help
    /hotkeys                   Hotkeys
    /settings                  Settings
    /new                       rpc new_session
    /model                     picker: Switch model
    /scoped-models             scoped models dialog
    /fallback                  toast: No fallback models: /fallback MODEL [MODEL...] sets the chain that takes over when a model's usage runs out
    /thinking                  picker: Thinking level
    /change_default            rpc change_default
    /verbosity                 picker: Transcript verbosity
    /theme                     picker: Theme
    /accent                    picker: Accent colour (or type #rrggbb)
    /confirm                   picker: Tool confirmation
    /mode                      picker: When tools ask first (Alt+M cycles)
    /plan                      rpc set_mode
    /todos                     toast: No plan: the model has not made one (it makes one with update_plan).
    /permissions               text: Permissions
    /notify                    toast: Notifications are on, but the browser has not been asked yet: /notify on asks it
    /compact                   toast: Compacting the conversation…
    /compaction                toast: Compaction: at 200k tokens, or 80% of the model's window if that is less; summaries by the session's model; observation masking on (/compaction at TOKENS|off, /compaction model MODEL|off, /compaction mask on|off)
    /diff                      toast: Reading the changes…
    /review                    rpc git_diff, rpc git_status
    /init                      rpc init
    /name                      (Rename"")
    /session                   loading Session: rpc push_pref, rpc session_stats
    /sessions                  sidebar
    /switch                    sidebar
    /archive                   toast: This session isn't saved yet: send a message first.
    /unarchive                 toast: This session isn't saved yet: send a message first.
    /delete                    loading Delete a message: rpc push_pref, rpc get_message_points
    /delete-archived           toast: There are no archived sessions.
    /clone                     rpc clone
    /fork                      loading Fork from: rpc push_pref, rpc get_entries
    /rewind                    loading Rewind to: rpc push_pref, rpc get_entries
    /trim                      loading Trim the context: rpc push_pref, rpc get_trim_points
    /tree                      loading Session tree: rpc push_pref, rpc get_entries
    /cd                        path prompt
    /mkdir                     path prompt
    /worktree                  loading Worktrees: rpc push_pref, rpc list_worktrees
    /default-dir               toast: No default directory: new sessions start in the backend's working directory; /default-dir PATH sets one
    /host                      picker: Where tools run
    /export                    path prompt
    /import                    path prompt
    /snippet                   toast: no snippets yet: /snippet add NAME TEXT saves one
    /skills                    loading Skills: rpc push_pref, rpc list_skills
    /skill:                    loading Skills: rpc push_pref, rpc list_skills
    /mcp                       loading MCP servers: rpc push_pref, rpc list_mcp
    /budget                    toast: No budget ($0.00 spent): /budget DOLLARS stops the agent starting turns once the session has cost that much
    /goal                      toast: No goal: /goal TEXT sets one, and the agent keeps working until it judges it met
    /remind                    toast: No reminders yet: /remind stop TEXT or /remind every N TEXT adds one
    /resume                    toast: No resume is scheduled: one is when a run stops at a usage limit whose reset time the provider gives
    /copy                      toast: Nothing to copy yet: no reply in this session.
    /btw                       toast: Usage: /btw <question> (asked aside; the run goes on)
    /abort                     rpc abort
    /terminal                  rpc set_prefs, Focus_terminal
    /files                     rpc fs_list, (Files(Focus files-list)), rpc git_diff
    /agents                    agents panel
    /jobs                      agents panel
    /login                     loading Log in: rpc push_pref, rpc auth_status
    /logout                    loading Log out: rpc push_pref, rpc auth_status
    /auth                      loading Providers: rpc push_pref, rpc auth_status, rpc forge_status
    /pr                        pull requests: rpc push_pref, rpc pr_list
    /setusr                    loading Act as: rpc push_pref, rpc list_users
    /nix-gc                    rpc nix_store_status
    /signout                   Sign_out
    /retry-backend-connection  toast: The backend is connected.
    /state                     text: Session state
    /clear                     toast: Cleared the view; the conversation is kept (a reload shows it, /new starts afresh)
    /quit                      toast: Close the tab to leave: the session stays in the backend (/signout signs out).
    |}]
;;

let%expect_test "/help <command> and /hotkeys" =
  let h = H.create () in
  run h "/help compact";
  run h "/help /tree";
  run h "/help compcat";
  toasts h;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/help compact") (max 500))) (tag Ignore))
    (Expire_toast (id 0) (after_ms 4100))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/help /tree") (max 500))) (tag Ignore))
    (Expire_toast (id 1) (after_ms 3820))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/help compcat") (max 500))) (tag Ignore))
    /tree: show the session tree and move to any message in it
    No command /compcat. Did you mean /compact? (Dismiss)
    |}];
  run h "/hotkeys";
  H.text h ~selector:".modal";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /hotkeys) (max 500)))
     (tag Ignore))
    (Focus dialog)
    Keyboard shortcuts
    (Close (Esc))
    Enter send; while running, steer the agent
    Ctrl+Enter / ⌘+Enter send exactly what is typed, ignoring the completion popup
    Alt+Enter queue a follow-up for after the run
    Shift+Enter new line
    Esc stop the run; close a dialog, popup or side answer
    ↑ ↓ earlier prompts (in an empty editor or its first line)
    Alt+↑ take the last queued message back into the editor
    Tab complete a /command, its argument, an @path or a #snippet
    !cmd run a shell command (!!cmd: not added to the context; !&cmd: as a background job)
    Ctrl+L switch model: the model chip's menu
    ↑ ↓ / Enter / Esc (a chip's menu) move / choose / close it; type to filter the models
    ← → / Tab (a chip's menu) the next chip's menu (model, thinking, mode)
    Ctrl+P / Alt+P next / previous scoped model (/scoped-models)
    Alt+T cycle the thinking level
    Alt+M cycle the mode: ask → auto-edit → auto → plan (/mode, /plan)
    Alt+N insert a snippet at the caret: a list to filter (/snippet)
    Ctrl+O cycle the transcript verbosity
    Ctrl+X copy the last reply (when nothing is selected)
    Ctrl+↑ / Ctrl+↓ previous / next of your messages in the transcript (loading earlier ones)
    Ctrl+J jump to one of your messages: a list to filter (or the marks at the transcript's right edge)
    Delete (in the jump list) trim the model's context before the highlighted message (confirmed first; /trim undo brings it back)
    PageUp / PageDown scroll the transcript
    Ctrl+K search sessions (↓ ↑ through them, Enter opens, Delete deletes)
    Alt+Enter / Alt+R / Delete (/worktree) start a new session in the highlighted worktree / review its changes / remove it
    A (on a session) archive or unarchive it (archived sessions are listed last, muted)
    Shift+F10 / ContextMenu (on a session or worktree) its menu: open, rename, fork, new worktree, review, files, terminal, pull request, copy, delete or remove (also right-click, a long press or its ⋯)
    Space (selecting) mark a session (Shift: the range from the last one); Delete deletes the marked ones, Esc stops selecting
    Ctrl+B show or hide the sidebar
    Ctrl+, settings: notifications, transcript detail, your account (/settings)
    ← → / ↑ ↓ (on a panel's edge) resize the panel (Shift: faster; Home / End: smallest / largest; Enter: the default size); or drag the edge, double-click to reset
    Alt+1…9 follow subagent or job N in the agents panel
    Alt+] Alt+[ the next or previous subagent or job
    Alt+0 close the agents panel
    Enter / Alt+Enter (steering a subagent) send it a message: at its next step / when it ends its turn (its Steer button in the agents panel; /agents steer N TEXT sends one)
    Alt+↑ (steering a subagent) take its last queued message back
    Esc (steering a subagent) write to the main agent again
    Ctrl+Shift+F open or close the file browser (/files): ↑↓ select, Enter opens, Backspace goes up, F2 renames, Delete deletes, Esc closes
    Alt+R open or close the review page (/review): the changes file by file, beside the chat
    ] / [ (reviewing; also n p, j k) the next / previous changed file
    a / c / e / v / s (reviewing) approve the file; comment on the code selected (drag across it, in the review or a changed file's preview); edit or comment; the whole file or the changes; send the new comments to prigh
    r / b / d (reviewing) choose what to review (a worktree, or a branch read-only); what to compare it with (a branch, tag, commit or any revision typed); since the merge base or directly
    l / Shift+L (reviewing) what changed since your latest action on the file (or the hunks again); choose the action it compares with: Latest follows your newest
    Esc (reviewing) drop the comment or selection, then close the review
    Enter / Ctrl+S (reviewing) add the comment or reply (Shift+Enter: a new line); save the file being edited
    Ctrl+` open or close the terminal (/terminal); in it, every other key goes to the shell
    Esc (in a pull request) back to the list it was opened from (/pr); Enter closes it
    Alt+G (a pull request, commit message, branch or session name) ✦ Generate: the model drafts it from the changes, the commits and the session (Esc stops, keeping what came; Undo puts the text back)
    Ctrl+Enter (the review's commit message) commit the ticked files (approved files come ticked; untick to leave one out)
    Ctrl+Enter (in a dialog's text box) accept the dialog, e.g. create the pull request (Enter is a new line)
    Tab (in /scoped-models) check or uncheck the highlighted model
    Delete (in /remind's list) remove the highlighted reminder
    y / a / n / r (confirmation) allow once, always allow (its rules, this session), deny, deny saying why; Enter allows, Esc denies
    ↑↓ Space ←→ (question) choose, pick, previous / next question; type for your own answer; Enter accepts, Esc declines
    Left to the browser
    Ctrl+C / Ctrl+V / Ctrl+Z copy, paste, undo: the browser's (Esc stops a run)
    Ctrl+F find in the page, which has the whole transcript
    Ctrl+T / Ctrl+N / Ctrl+W the browser's tabs and windows: Alt+T cycles thinking
    Ctrl+R reload: the session comes back (?session=); Tab completes @paths
    Ctrl+G no $EDITOR in a browser: edit here (Shift+Enter for new lines)
    /help also lists the commands (Done)
    |}];
  H.key h "Escape" ~target:Page;
  [%expect
    {|
    Close_dialog
    (Focus editor)
    |}]
;;

let%expect_test "/scoped-models, Ctrl+P and Alt+P" =
  let h = H.create () in
  (* Without a scope, Ctrl+P cycles through the logged-in models. *)
  H.key h "p" ~ctrl:true;
  H.key h "p" ~ctrl:true;
  toasts h;
  [%expect
    {|
    (Cycle_model 1)
    (Expire_toast (id 0) (after_ms 2500))
    (Rpc (method_ set_model) (params ((model anthropic/claude-sonnet-5)))
     (tag Model_set))
    (Cycle_model 1)
    (Expire_toast (id 1) (after_ms 2500))
    (Rpc (method_ set_model) (params ((model anthropic/claude-opus-5-5)))
     (tag Model_set))
    Model: Claude Opus 5.5
    |}];
  run h "/scoped-models";
  H.text h ~selector:".modal";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value /scoped-models) (max 500))) (tag Ignore))
    (Focus picker-input)
    Scoped models
    (Close (Esc))
    Ctrl+P and Alt+P cycle through the checked models. With none checked they cycle through the logged-in ones.
    []
    ☐ GPT-6 openai
    ☑ Claude Opus 5.5 anthropic
    ☑ Claude Sonnet 5 anthropic
    ☐ DeepSeek Chat deepseek
    2 checked. Tab toggles, Enter saves (Cancel) (Save)
    |}];
  H.key h "Tab" ~target:Field;
  H.act h (Toggle_scoped "openai/gpt-6");
  H.act h (Picker_query "deep");
  H.key h "Tab" ~target:Field;
  H.text h ~selector:".modal-buttons";
  [%expect
    {|
    Dialog_toggle
    Dialog_toggle
    3 checked. Tab toggles, Enter saves (Cancel) (Save)
    |}];
  H.key h "Enter" ~target:Field;
  H.reply
    h
    "set_config"
    {|{"scoped_models":["anthropic/claude-opus-5-5","anthropic/claude-sonnet-5","deepseek/deepseek-chat"],"confirm_tools":false}|};
  toasts h;
  [%expect
    {|
    Dialog_accept
    (Focus editor)
    (Rpc (method_ set_config)
     (params
      ((config
        ((scoped_models
          (anthropic/claude-opus-5-5 anthropic/claude-sonnet-5
           deepseek/deepseek-chat))))))
     (tag (Config_saved "3 scoped models: Ctrl+P and Alt+P cycle through them")))
    (Expire_toast (id 2) (after_ms 3580))
    Model: Claude Opus 5.5
    3 scoped models: Ctrl+P and Alt+P cycle through them
    |}];
  (* Alt+P goes back; on a Mac Option+P types π, so the key's code counts. *)
  H.key h "π" ~alt:true ~code:"KeyP";
  H.key h "p" ~ctrl:true;
  [%expect
    {|
    (Cycle_model -1)
    (Expire_toast (id 3) (after_ms 2500))
    (Rpc (method_ set_model) (params ((model deepseek/deepseek-chat)))
     (tag Model_set))
    (Cycle_model 1)
    (Expire_toast (id 4) (after_ms 2500))
    (Rpc (method_ set_model) (params ((model anthropic/claude-opus-5-5)))
     (tag Model_set))
    |}];
  (* A dialog owns the keyboard: Ctrl+P does not cycle under it. *)
  run h "/scoped-models";
  H.key h "p" ~ctrl:true ~target:Field;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value /scoped-models) (max 500))) (tag Ignore))
    (Focus picker-input)
    (browser default)
    |}];
  H.key h "Escape" ~target:Field;
  print_s [%sexp ((H.model h).config : Prigh_protocol.Config.t option)];
  [%expect
    {|
    Close_dialog
    (Focus editor)
    (((scoped_models
       (anthropic/claude-opus-5-5 anthropic/claude-sonnet-5
        deepseek/deepseek-chat))
      (approval_mode Auto) (permissions ((allow ()) (ask ()) (deny ())))
      (default_model ()) (default_thinking ()) (fallback_models ())
      (default_cwd ()) (worktrees_dir ()) (archive_after_days 7) (notify bell)
      (snippets ()) (compact_at_tokens 200000) (compaction_model ())
      (mask_observations true)))
    |}]
;;

let%expect_test "/thinking and Alt+T" =
  let h = H.create () in
  H.key h "t" ~alt:true;
  H.key h "†" ~alt:true ~code:"KeyT";
  [%expect
    {|
    Cycle_thinking
    (Expire_toast (id 0) (after_ms 2500))
    (Rpc (method_ set_thinking) (params ((thinking high))) (tag Thinking_set))
    Cycle_thinking
    (Expire_toast (id 1) (after_ms 2500))
    (Rpc (method_ set_thinking) (params ((thinking max))) (tag Thinking_set))
    |}];
  let state = (H.model h).state |> Option.value_exn in
  print_endline state.thinking;
  [%expect {| max |}];
  H.act h (Set_model "deepseek/deepseek-chat");
  H.event
    h
    (sprintf
       {|{"event":"state","state":%s}|}
       (H.state_json
          ~fields:
            [ ( "model"
              , Jsonaf.of_string
                  {|{"id":"deepseek-chat","provider":"deepseek","key":"deepseek/deepseek-chat","name":"DeepSeek Chat","context_window":64000,"max_output":8000,"supports_thinking":false,"cost":{"input":0.3,"output":1,"cache_read":0.03}}|}
              )
            ]
          ()));
  H.key h "t" ~alt:true;
  toasts h;
  [%expect
    {|
    (Rpc (method_ set_model) (params ((model deepseek/deepseek-chat)))
     (tag Model_set))
    (Expire_toast (id 2) (after_ms 4620))
    Cycle_thinking
    DeepSeek Chat needs deepseek, which is not logged in: /login deepseek logs in.
    DeepSeek Chat has no thinking levels: switch to a model that thinks with /model (Dismiss)
    |}]
;;

let%expect_test "/change_default" =
  let h = H.create () in
  run h "/change_default";
  H.reply
    h
    "change_default"
    {|{"scoped_models":[],"confirm_tools":false,"default_model":"anthropic/claude-opus-5-5","default_thinking":"on"}|};
  toasts h;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value /change_default) (max 500))) (tag Ignore))
    (Rpc (method_ change_default) (params ()) (tag Default_saved))
    (Expire_toast (id 0) (after_ms 3980))
    New sessions start with anthropic/claude-opus-5-5, thinking on
    |}]
;;

let%expect_test "/verbosity and Ctrl+O" =
  let h = H.create () in
  with_messages h;
  run h "/verbosity";
  H.text h ~selector:".picker-items";
  [%expect
    {|
    Scroll_to_bottom
    (Rpc (method_ push_pref)
     (params ((key history) (value /verbosity) (max 500))) (tag Ignore))
    (Focus picker-input)
    Quiet tool calls without their output; no thinking
    Normal tool output and thinking folded ✓
    Verbose everything unfolded
    |}];
  H.key h "ArrowUp" ~target:Field;
  H.key h "Enter" ~target:Field;
  H.show h ~selector:"#chat";
  H.text h ~selector:".status";
  H.text h ~selector:".status-extras";
  [%expect
    {|
    (Dialog_move -1)
    Dialog_accept
    (Focus editor)
    (Cache_prefs (((key web.verbosity) (value (quiet)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.verbosity quiet)))))
     (tag Show_error))
    (Expire_toast (id 0) (after_ms 4660))
    <div id="chat" role="region" aria-label="Transcript" tabindex="0" class="chat verbosity-quiet">
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <div class="entries">
        <div data-entry="0" data-message-entry="0" class="msg user">
          <div dir="auto" class="bubble"> fix the build </div>
          <div class="msg-actions">
            <button type="button"
                    title="Delete this message from the transcript and the model's context"
                    data-delete-entry="0"
                    class="msg-delete">
              <span aria-hidden="true" class="glyph"> ✕ </span>
              Delete
            </button>
            <button type="button"
                    title="Trim everything after this from the model's context"
                    data-trim-after-entry="0"
                    class="trim-after">
              <span aria-hidden="true" class="glyph"> ✂ </span>
              Trim everything after this
            </button>
          </div>
        </div>
        <div data-message-entry="1" class="assistant msg">
          <div class="markdown">
            <div class="md-part">
              <p dir="auto">
                Fixed: the
                <strong> flag </strong>
                 was wrong.
              </p>
            </div>
          </div>
          <div class="meta">
            <span> m </span>
            <span> 10 in </span>
            <span> 5 out </span>
          </div>
          <div class="msg-actions">
            <button type="button"
                    title="Delete this reply from the transcript and the model's context"
                    data-delete-entry="1"
                    class="msg-delete">
              <span aria-hidden="true" class="glyph"> ✕ </span>
              Delete
            </button>
          </div>
        </div>
      </div>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
    </div>
    (0%) ↑0 ↓0 $0.0000 Ready
    (quiet)
    |}];
  H.key h "o" ~ctrl:true;
  H.key h "o" ~ctrl:true;
  run h "/verbosity loud";
  toasts h;
  [%expect
    {|
    Cycle_verbosity
    (Cache_prefs (((key web.verbosity) (value (normal)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.verbosity normal)))))
     (tag Show_error))
    (Expire_toast (id 1) (after_ms 4180))
    Cycle_verbosity
    (Cache_prefs (((key web.verbosity) (value (verbose)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.verbosity verbose)))))
     (tag Show_error))
    (Expire_toast (id 2) (after_ms 3740))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/verbosity loud") (max 500))) (tag Ignore))
    Transcript: verbose, everything unfolded (Ctrl+O cycles)
    Unknown verbosity “loud”: use quiet, normal or verbose. (Dismiss)
    |}];
  H.text h ~selector:".status";
  H.text h ~selector:".status-extras";
  [%expect
    {|
    (0%) ↑0 ↓0 $0.0000 Ready
    (verbose)
    |}]
;;

let%expect_test "/confirm" =
  let h = H.create () in
  run h "/confirm";
  H.text h ~selector:".picker-items";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /confirm) (max 500)))
     (tag Ignore))
    (Focus picker-input)
    On ask before bash, write and edit run
    Off run tools without asking ✓
    |}];
  H.act h (Picker_choose "on");
  H.reply
    h
    "set_config"
    {|{"scoped_models":[],"confirm_tools":true,"default_model":null,"default_thinking":null}|};
  H.text h ~selector:".mode-chip";
  run h "/confirm off";
  run h "/confirm maybe";
  toasts h;
  [%expect
    {|
    (Focus editor)
    (Rpc (method_ set_config) (params ((config ((approval_mode ask)))))
     (tag
      (Config_saved
       "Tool confirmation on (mode ask, also for new sessions): bash, job_input, edits and MCP tools ask first")))
    (Expire_toast (id 0) (after_ms 5580))
    (Asks: edits & commands)
    (Rpc (method_ push_pref)
     (params ((key history) (value "/confirm off") (max 500))) (tag Ignore))
    (Rpc (method_ set_config) (params ((config ((approval_mode auto)))))
     (tag
      (Config_saved
       "Tool confirmation off (mode auto, also for new sessions): tools run without asking")))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/confirm maybe") (max 500))) (tag Ignore))
    Tool confirmation on (mode ask, also for new sessions): bash, job_input, edits and MCP tools ask first
    Unknown setting “maybe”: use on or off. (Dismiss)
    |}]
;;

let%expect_test "/compact with instructions" =
  let h = H.create () in
  run h "/compact keep the file names";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/compact keep the file names") (max 500)))
     (tag Ignore))
    (Expire_toast (id 0) (after_ms 2700))
    (Rpc (method_ compact) (params ((instructions "keep the file names")))
     (tag (Notice "Compacted the conversation")))
    |}];
  H.type_ h "/comp";
  H.key h "Enter";
  [%expect
    {|
    (Complete_accept (run true))
    (Rpc (method_ push_pref) (params ((key history) (value /compact) (max 500)))
     (tag Ignore))
    (Expire_toast (id 1) (after_ms 2700))
    (Rpc (method_ compact) (params ())
     (tag (Notice "Compacted the conversation")))
    |}]
;;

let%expect_test "/session" =
  let h = H.create () in
  run h "/session";
  H.reply
    h
    "session_stats"
    {|{"message_count":12,"turns":4,"tool_calls":{"bash":3,"read":5},"usage":{"input":45000,"output":3200,"cache_read":30000},"cost_usd":0.4321,"context_percent":22.5,"model_changes":1,"compactions":0,"masks":2,"duration_seconds":754.2}|};
  H.text h ~selector:".modal";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /session) (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ session_stats) (params ()) (tag Session_stats))
    (Focus dialog)
    Session
    (Close (Esc))
    Name (unnamed) Id s1 File /sessions/s1.jsonl Directory /work Model Claude Opus 5.5 (anthropic/claude-opus-5-5) Thinking on
    12 Messages
    4 Turns
    $0.4321 Cost
    22.5% Context
    45.0k Input
    3.2k Output
    30.0k Cache read
    12m 34s Duration
    Tools bash 3, read 5 Model changes 1 Compactions 0 Maskings 2
    (Rename) (Export) (Tree) (Done)
    |}];
  H.act h (Run "/name");
  [%expect {| (Focus dialog-input) |}]
;;

let%expect_test "/switch" =
  let h =
    H.create
      ~sessions:(sprintf "[%s]" (H.session_json ~name:"Release notes" "s2"))
      ()
  in
  H.type_ h "/switch rel";
  H.text h ~selector:".popup";
  [%expect
    {|
    Sessions ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    Release notes /work
    |}];
  H.key h "Enter";
  [%expect
    {|
    (Complete_accept (run true))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/switch /sessions/s2.jsonl") (max 500)))
     (tag Ignore))
    (Rpc (method_ switch_session) (params ((path /sessions/s2.jsonl)))
     (tag Reload_state))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before ()))))
    (Focus editor)
    |}];
  H.act h Toggle_sidebar;
  run h "/switch";
  print_s [%sexp ((H.model h).sidebar_open : bool)];
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /switch) (max 500)))
     (tag Ignore))
    (Focus session-search)
    true
    |}]
;;

let entry ?parent id kind =
  sprintf
    {|{"id":"%s","parent":%s,%s}|}
    id
    (Option.value_map parent ~default:"null" ~f:(sprintf "%S"))
    kind
;;

let user text =
  sprintf {|"kind":"message","message":{"role":"user","text":"%s"}|} text
;;

let assistant text =
  sprintf
    {|"kind":"message","message":{"role":"assistant","content":[{"type":"text","text":"%s"}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}|}
    text
;;

let entries ~head list =
  sprintf {|{"head":"%s","entries":[%s]}|} head (String.concat ~sep:"," list)
;;

let linear =
  entries
    ~head:"e4"
    [ entry "e1" (user "first question")
    ; entry ~parent:"e1" "e2" (assistant "first answer")
    ; entry ~parent:"e2" "e3" (user "second question\\nwith detail")
    ; entry ~parent:"e3" "e4" (assistant "second answer")
    ]
;;

let%expect_test "/fork: pick a message, edit it in a new session" =
  let h = H.create () in
  run h "/fork";
  H.reply h "get_entries" linear;
  H.text h ~selector:".modal";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /fork) (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ get_entries) (params ()) (tag (Entries Fork)))
    (Focus picker-input)
    Fork from (edit and resend)
    (Close (Esc))
    []
    first question #1
    second question #2 ✓
    ↑↓ to move, Enter to choose, Esc to close
    |}];
  H.key h "ArrowUp" ~target:Field;
  H.key h "Enter" ~target:Field;
  print_s [%sexp ((H.model h).draft : string)];
  [%expect
    {|
    (Dialog_move -1)
    Dialog_accept
    (Focus editor)
    (Expire_toast (id 0) (after_ms 3700))
    (Rpc (method_ fork) (params ((at e1))) (tag Reload_state))
    "first question"
    |}];
  (* An empty session has nothing to fork. *)
  run h "/fork";
  H.reply h "get_entries" {|{"head":null,"entries":[]}|};
  toasts h;
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /fork) (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ get_entries) (params ()) (tag (Entries Fork)))
    (Expire_toast (id 1) (after_ms 3420))
    (Focus editor)
    Forked into a new session: edit the message and send it
    No messages yet: there is nothing to go back to.
    |}]
;;

let%expect_test "/rewind: pick, confirm, reload the messages" =
  let h = H.create () in
  run h "/rewind";
  H.reply h "get_entries" linear;
  H.key h "ArrowUp" ~target:Field;
  H.key h "Enter" ~target:Field;
  H.text h ~selector:".modal";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /rewind) (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ get_entries) (params ()) (tag (Entries Rewind)))
    (Focus picker-input)
    (Dialog_move -1)
    Dialog_accept
    (Focus dialog)
    Rewind here?
    (Close (Esc))
    The conversation goes back to:
    first question
    Later messages are kept as a branch: /tree returns to them.
    (Cancel) (Rewind)
    |}];
  H.key h "Enter" ~target:Page;
  H.reply h "rewind" "{}";
  [%expect
    {|
    Dialog_accept
    (Focus editor)
    (Expire_toast (id 0) (after_ms 2980))
    (Rpc (method_ rewind) (params ((to e1))) (tag Reload_messages))
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s1.jsonl) (limit 60)))
     (tag (Page (path /sessions/s1.jsonl) (before ()))))
    |}];
  (* Esc cancels without rewinding. *)
  run h "/rewind";
  H.reply h "get_entries" linear;
  H.key h "Enter" ~target:Field;
  H.key h "Escape" ~target:Page;
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /rewind) (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ get_entries) (params ()) (tag (Entries Rewind)))
    (Focus picker-input)
    Dialog_accept
    (Focus dialog)
    Close_dialog
    (Focus editor)
    |}]
;;

let%expect_test "/tree shows branches and moves the head" =
  let h = H.create () in
  run h "/tree";
  H.reply
    h
    "get_entries"
    (entries
       ~head:"e6"
       [ entry "e1" (user "first question")
       ; entry ~parent:"e1" "e2" (assistant "first answer")
       ; entry ~parent:"e2" "e3" (user "abandoned idea")
       ; entry ~parent:"e3" "e4" (assistant "abandoned answer")
       ; entry
           ~parent:"e2"
           "m1"
           {|"kind":"model","model":"openai/gpt-6","thinking":"on"|}
       ; entry ~parent:"m1" "e5" (user "better idea")
       ; entry ~parent:"e5" "e6" (assistant "better answer")
       ]);
  H.show h ~selector:".picker-items";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /tree) (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ get_entries) (params ((all true))) (tag (Entries Tree)))
    (Focus picker-input)
    <div role="listbox" id="picker-list" aria-label="Session tree" class="picker-items">
      <div role="option"
           id="picker-option-0"
           aria-selected="false"
           aria-checked="true"
           class="marked picker-item"
           @on_click>
        <span class="picker-label"> > first question </span>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        <span aria-hidden="true" class="mark picker-check">
          <icon class="check"> </icon>
        </span>
      </div>
      <div role="option"
           id="picker-option-1"
           aria-selected="false"
           aria-checked="true"
           class="marked picker-item"
           @on_click>
        <span class="picker-label"> · first answer </span>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        <span aria-hidden="true" class="mark picker-check">
          <icon class="check"> </icon>
        </span>
      </div>
      <div role="option" id="picker-option-2" aria-selected="false" class="picker-item" @on_click>
        <span class="picker-label">   > abandoned idea </span>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      </div>
      <div role="option" id="picker-option-3" aria-selected="false" class="picker-item" @on_click>
        <span class="picker-label">   · abandoned answer </span>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      </div>
      <div role="option"
           id="picker-option-4"
           aria-selected="false"
           aria-checked="true"
           class="marked picker-item"
           @on_click>
        <span class="picker-label">   > better idea </span>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        <span aria-hidden="true" class="mark picker-check">
          <icon class="check"> </icon>
        </span>
      </div>
      <div role="option"
           id="picker-option-5"
           aria-selected="true"
           aria-checked="true"
           class="marked picker-item selected"
           @on_click>
        <span class="picker-label">   · better answer </span>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        <span aria-hidden="true" class="mark picker-check">
          <icon class="check"> </icon>
        </span>
      </div>
    </div>
    |}];
  H.act h (Picker_choose "e4");
  [%expect
    {|
    (Focus editor)
    (Rpc (method_ rewind) (params ((to e4))) (tag Reload_messages))
    |}]
;;

let%expect_test "/tree of a very long conversation" =
  let n = 50_000 in
  let json =
    entries
      ~head:(sprintf "e%d" n)
      (List.init n ~f:(fun i ->
         entry
           ?parent:(if i = 0 then None else Some (sprintf "e%d" i))
           (sprintf "e%d" (i + 1))
           (if i % 2 = 0 then user "question" else assistant "answer")))
  in
  let head, list =
    Prigh_protocol.Json.parse json
    |> Or_error.ok_exn
    |> Prigh_web.Session_tree.of_json
    |> Or_error.ok_exn
  in
  let items = Prigh_web.Session_tree.tree_items list ~head in
  print_s
    [%sexp (List.length items : int), ((List.last_exn items).label : string)];
  [%expect {| (50000 "\194\183 answer") |}]
;;

let%expect_test "/cd: a prompt with directory completion, failures stay in it" =
  let h = H.create () in
  run h "/cd";
  H.reply h "list_dirs" {|["/work/src/","/work/test/"]|};
  H.text h ~selector:".modal";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /cd) (max 500)))
     (tag Ignore))
    (Focus dialog-input)
    (Rpc (method_ list_dirs) (params ((prefix /work) (host backend)))
     (tag (Prompt_paths /work)))
    Change directory
    (Close (Esc))
    The session's working directory [/work]
    /work/src/
    /work/test/
    Tab completes, ↑↓ choose (Cancel) (Change)
    |}];
  H.act h (Dialog_input "/work/s");
  H.reply h "list_dirs" {|["/work/src/"]|};
  H.key h "Tab" ~target:Field;
  print_s [%sexp ((H.model h).dialog : Dialog.t option)];
  [%expect
    {|
    (Rpc (method_ list_dirs) (params ((prefix /work/s) (host backend)))
     (tag (Prompt_paths /work/s)))
    Dialog_complete
    (Rpc (method_ list_dirs) (params ((prefix /work/src/) (host backend)))
     (tag (Prompt_paths /work/src/)))
    ((Prompt
      ((action Cd) (input /work/src/) (suggestions ()) (selected ()) (error ())
       (busy false))))
    |}];
  H.reply h "list_dirs" "[]";
  H.act h (Dialog_input "/nowhere");
  H.key h "Enter" ~target:Field;
  H.fail h "set_cwd" "no such directory: /nowhere";
  H.text h ~selector:".modal";
  [%expect
    {|
    (Rpc (method_ list_dirs) (params ((prefix /nowhere) (host backend)))
     (tag (Prompt_paths /nowhere)))
    Dialog_accept
    (Rpc (method_ set_cwd) (params ((path /nowhere) (if_missing ask)))
     (tag (Dir_changed Cwd)))
    Change directory
    (Close (Esc))
    The session's working directory [/nowhere]
    no such directory: /nowhere
    Tab completes, ↑↓ choose (Cancel) (Change)
    |}];
  H.act h (Dialog_input "/work/src");
  H.key h "Enter" ~target:Field;
  H.reply h "set_cwd" {|{"cwd":"/work/src","created":false}|};
  H.text h ~selector:".modal";
  toasts h;
  [%expect
    {|
    (Rpc (method_ list_dirs) (params ((prefix /work/src) (host backend)))
     (tag (Prompt_paths /work/src)))
    Dialog_accept
    (Rpc (method_ set_cwd) (params ((path /work/src) (if_missing ask)))
     (tag (Dir_changed Cwd)))
    (Focus editor)
    (Expire_toast (id 0) (after_ms 2620))
    Working directory: /work/src
    |}];
  (* An empty answer says what to do. *)
  run h "/cd";
  H.act h (Dialog_input "");
  H.key h "Enter" ~target:Field;
  H.text h ~selector:".dialog-error";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /cd) (max 500)))
     (tag Ignore))
    (Focus dialog-input)
    (Rpc (method_ list_dirs) (params ((prefix /work) (host backend)))
     (tag (Prompt_paths /work)))
    (Rpc (method_ list_dirs) (params ((prefix "") (host backend)))
     (tag (Prompt_paths "")))
    Dialog_accept
    Type a path (Esc cancels).
    |}]
;;

let hosts_state =
  H.state_json
    ~fields:
      [ ( "hosts"
        , Jsonaf.of_string
            {|[{"id":"backend","name":"backend","cwd":"/work"},
               {"id":"c7","name":"laptop","cwd":"/home/me/proj","session_id":"s1","session_name":null},
               {"id":"c9","name":"ci","cwd":"/build","session_id":"s3","session_name":"nightly"}]|}
        )
      ]
    ()
;;

let%expect_test "/host: pick a host, then the directory there" =
  let h = H.create ~state:hosts_state () in
  H.act
    h
    (Hello
       { client_id = "c7"; namespace = None; user = None; superuser = false });
  run h "/host";
  H.text h ~selector:".picker-items";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /host) (max 500)))
     (tag Ignore))
    (Focus picker-input)
    backend /work ✓
    laptop (this browser) /home/me/proj
    ci (in nightly) /build
    |}];
  H.key h "ArrowDown" ~target:Field;
  H.key h "Enter" ~target:Field;
  H.text h ~selector:".modal";
  [%expect
    {|
    (Dialog_move 1)
    Dialog_accept
    (Focus dialog-input)
    (Rpc (method_ list_dirs) (params ((prefix /work) (host c7)))
     (tag (Prompt_paths /work)))
    Run tools on laptop (this browser)
    (Close (Esc))
    Working directory there [/work]
    Tab completes, ↑↓ choose (Cancel) (Switch)
    |}];
  H.key h "Enter" ~target:Field;
  H.fail h "set_active_host" "laptop: /work: no such directory";
  H.text h ~selector:".dialog-error";
  H.act h (Dialog_input "/home/me/proj");
  H.key h "Enter" ~target:Field;
  H.reply h "set_active_host" {|{"cwd":"/home/me/proj","created":false}|};
  toasts h;
  [%expect
    {|
    Dialog_accept
    (Rpc (method_ set_active_host)
     (params ((host c7) (cwd /work) (if_missing ask)))
     (tag (Dir_changed (Host (id c7) (name "laptop (this browser)")))))
    laptop: /work: no such directory
    (Rpc (method_ list_dirs) (params ((prefix /home/me/proj) (host c7)))
     (tag (Prompt_paths /home/me/proj)))
    Dialog_accept
    (Rpc (method_ set_active_host)
     (params ((host c7) (cwd /home/me/proj) (if_missing ask)))
     (tag (Dir_changed (Host (id c7) (name "laptop (this browser)")))))
    (Focus editor)
    (Expire_toast (id 0) (after_ms 3580))
    Tools run on laptop (this browser), in /home/me/proj
    |}];
  run h "/host ci";
  print_s [%sexp ((H.model h).dialog : Dialog.t option)];
  run h "/host mainframe";
  toasts h;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/host ci") (max 500))) (tag Ignore))
    (Focus dialog-input)
    (Rpc (method_ list_dirs) (params ((prefix /work) (host c9)))
     (tag (Prompt_paths /work)))
    ((Prompt
      ((action (Host_cwd (host c9) (name "ci (in nightly)"))) (input /work)
       (suggestions ()) (selected ()) (error ()) (busy false))))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/host mainframe") (max 500))) (tag Ignore))
    Tools run on laptop (this browser), in /home/me/proj
    No tool host “mainframe”: one of backend, laptop, ci (/host picks one). (Dismiss)
    |}]
;;

let%expect_test "/cd and /host to a missing directory ask to create it" =
  let h = H.create ~state:hosts_state () in
  H.act
    h
    (Hello
       { client_id = "c7"; namespace = None; user = None; superuser = false });
  run h "/cd /work/new";
  H.reply h "set_cwd" {|{"missing":"/work/new"}|};
  H.text h ~selector:".modal";
  [%expect
    {|
    (Rpc (method_ list_dirs) (params ((prefix /work/new)))
     (tag (Paths /work/new)))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/cd /work/new") (max 500))) (tag Ignore))
    (Rpc (method_ set_cwd) (params ((path /work/new) (if_missing ask)))
     (tag (Dir_changed Cwd)))
    (Focus dialog)
    Create folder?
    (Close (Esc))
    This folder doesn't exist on backend:
    /work/new
    Missing parent folders are created too.
    Enter creates, Esc cancels (Cancel) (Create folder and switch)
    |}];
  (* Esc: nothing is created. *)
  H.key h "Escape" ~target:Page;
  print_s [%sexp ((H.model h).dialog : Dialog.t option)];
  [%expect
    {|
    Close_dialog
    (Focus editor)
    ()
    |}];
  run h "/cd /work/new";
  H.reply h "set_cwd" {|{"missing":"/work/new"}|};
  H.key h "Enter" ~target:Page;
  H.text h ~selector:".modal footer";
  H.fail h "set_cwd" "/work: permission denied";
  H.text h ~selector:".modal";
  [%expect
    {|
    (Rpc (method_ list_dirs) (params ((prefix /work/new)))
     (tag (Paths /work/new)))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/cd /work/new") (max 500))) (tag Ignore))
    (Rpc (method_ set_cwd) (params ((path /work/new) (if_missing ask)))
     (tag (Dir_changed Cwd)))
    (Focus dialog)
    Dialog_accept
    (Rpc (method_ set_cwd) (params ((path /work/new) (if_missing create)))
     (tag (Dir_changed Cwd)))
    Enter creates, Esc cancels (Cancel) (Creating…)
    Create folder?
    (Close (Esc))
    This folder doesn't exist on backend:
    /work/new
    Missing parent folders are created too.
    /work: permission denied
    Enter creates, Esc cancels (Cancel) (Create folder and switch)
    |}];
  H.click h ~selector:".modal .primary";
  H.reply h "set_cwd" {|{"cwd":"/work/new","created":true}|};
  print_s [%sexp ((H.model h).dialog : Dialog.t option)];
  toasts h;
  [%expect
    {|
    Dialog_accept
    (Rpc (method_ set_cwd) (params ((path /work/new) (if_missing create)))
     (tag (Dir_changed Cwd)))
    (Focus editor)
    (Expire_toast (id 0) (after_ms 2940))
    ()
    Created /work/new and switched to it
    |}];
  (* On another host, from /host's directory prompt. *)
  run h "/host ci";
  H.act h (Dialog_input "/build/x");
  H.key h "Enter" ~target:Field;
  H.reply h "set_active_host" {|{"missing":"/build/x"}|};
  H.text h ~selector:".modal";
  H.key h "Enter" ~target:Page;
  H.reply h "set_active_host" {|{"cwd":"/build/x","created":true}|};
  toasts h;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/host ci") (max 500))) (tag Ignore))
    (Focus dialog-input)
    (Rpc (method_ list_dirs) (params ((prefix /work) (host c9)))
     (tag (Prompt_paths /work)))
    (Rpc (method_ list_dirs) (params ((prefix /build/x) (host c9)))
     (tag (Prompt_paths /build/x)))
    Dialog_accept
    (Rpc (method_ set_active_host)
     (params ((host c9) (cwd /build/x) (if_missing ask)))
     (tag (Dir_changed (Host (id c9) (name "ci (in nightly)")))))
    (Focus dialog)
    Create folder?
    (Close (Esc))
    This folder doesn't exist on ci (in nightly):
    /build/x
    Missing parent folders are created too, then tools run on ci (in nightly) in it.
    Enter creates, Esc cancels (Cancel) (Create folder and switch)
    Dialog_accept
    (Rpc (method_ set_active_host)
     (params ((host c9) (cwd /build/x) (if_missing create)))
     (tag (Dir_changed (Host (id c9) (name "ci (in nightly)")))))
    (Focus editor)
    (Expire_toast (id 1) (after_ms 3620))
    Created /work/new and switched to it
    Created /build/x: tools run on ci (in nightly), in it
    |}];
  (* Another dialog opened meanwhile: the answer says how to create it. *)
  run h "/cd /elsewhere";
  run h "/name";
  H.reply h "set_cwd" {|{"missing":"/elsewhere"}|};
  toasts h;
  [%expect
    {|
    (Rpc (method_ list_dirs) (params ((prefix /elsewhere)))
     (tag (Paths /elsewhere)))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/cd /elsewhere") (max 500))) (tag Ignore))
    (Rpc (method_ set_cwd) (params ((path /elsewhere) (if_missing ask)))
     (tag (Dir_changed Cwd)))
    (Rpc (method_ push_pref) (params ((key history) (value /name) (max 500)))
     (tag Ignore))
    (Focus dialog-input)
    Created /build/x: tools run on ci (in nightly), in it
    /elsewhere doesn't exist: /mkdir /elsewhere creates it. (Dismiss)
    |}]
;;

let%expect_test "/mkdir creates a directory and changes to it" =
  let h = H.create () in
  run h "/mkdir /work/a/b";
  H.reply h "set_cwd" {|{"cwd":"/work/a/b","created":true}|};
  toasts h;
  run h "/mkdir";
  H.text h ~selector:".modal";
  H.act h (Dialog_input "/work/c");
  H.key h "Enter" ~target:Field;
  H.reply h "set_cwd" {|{"cwd":"/work/c","created":false}|};
  print_s [%sexp ((H.model h).dialog : Dialog.t option)];
  toasts h;
  [%expect
    {|
    (Rpc (method_ list_dirs) (params ((prefix /work/a/b)))
     (tag (Paths /work/a/b)))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/mkdir /work/a/b") (max 500))) (tag Ignore))
    (Rpc (method_ set_cwd) (params ((path /work/a/b) (if_missing create)))
     (tag (Dir_changed Cwd)))
    (Expire_toast (id 0) (after_ms 2940))
    Created /work/a/b and switched to it
    (Rpc (method_ push_pref) (params ((key history) (value /mkdir) (max 500)))
     (tag Ignore))
    (Focus dialog-input)
    (Rpc (method_ list_dirs) (params ((prefix /work/) (host backend)))
     (tag (Prompt_paths /work/)))
    Create directory
    (Close (Esc))
    Created with any missing parents, then made the working directory [/work/]
    Tab completes, ↑↓ choose (Cancel) (Create and switch)
    (Rpc (method_ list_dirs) (params ((prefix /work/c) (host backend)))
     (tag (Prompt_paths /work/c)))
    Dialog_accept
    (Rpc (method_ set_cwd) (params ((path /work/c) (if_missing create)))
     (tag (Dir_changed Cwd)))
    (Focus editor)
    (Expire_toast (id 1) (after_ms 2540))
    ()
    Created /work/a/b and switched to it
    Working directory: /work/c
    |}]
;;

(* Accepted while the connection is down: the dialog says so, and stops
   saying it once back. *)
let%expect_test "creating a folder while disconnected" =
  let h = H.create () in
  H.quiet h (fun () ->
    run h "/cd /work/new";
    H.reply h "set_cwd" {|{"missing":"/work/new"}|};
    H.key h "Enter" ~target:Page;
    H.act h Backend_closed;
    H.act h (Reply (Dir_changed Cwd, Error "not connected")));
  H.text h ~selector:".dialog-error";
  H.quiet h (fun () ->
    H.act h (Reply (Reconnect 1, Ok (Jsonaf.of_string {|{"client_id":"c2"}|}))));
  H.text h ~selector:".modal";
  [%expect
    {|
    Dialog_accept
    Not connected to the backend: try again once it reconnects.
    Create folder?
    (Close (Esc))
    This folder doesn't exist:
    /work/new
    Missing parent folders are created too.
    Enter creates, Esc cancels (Cancel) (Create folder and switch)
    |}];
  (* /mkdir offers the working directory to name a folder in. *)
  H.act h Close_dialog;
  H.quiet h (fun () -> run h "/mkdir");
  print_s [%sexp ((H.model h).dialog : Dialog.t option)];
  [%expect
    {|
    (Focus editor)
    ((Prompt
      ((action Mkdir) (input /work/) (suggestions ()) (selected ()) (error ())
       (busy false))))
    |}]
;;

let%expect_test "/export and /import" =
  let h = H.create () in
  run h "/export";
  H.key h "Enter" ~target:Field;
  H.reply h "export" {|{"path":"/home/me/.prigh/sessions/exports/s1.md"}|};
  run h "/export /tmp/s1.jsonl";
  H.reply h "export" {|{"path":"/tmp/s1.jsonl"}|};
  toasts h;
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /export) (max 500)))
     (tag Ignore))
    (Focus dialog-input)
    (Rpc (method_ list_paths) (params ((prefix ""))) (tag (Prompt_paths "")))
    Dialog_accept
    (Rpc (method_ export) (params ((format markdown))) (tag Exported))
    (Focus editor)
    (Expire_toast (id 0) (after_ms 3500))
    (Rpc (method_ list_paths) (params ((prefix /tmp/s1.jsonl)))
     (tag (Paths /tmp/s1.jsonl)))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/export /tmp/s1.jsonl") (max 500)))
     (tag Ignore))
    (Rpc (method_ export) (params ((format jsonl) (path /tmp/s1.jsonl)))
     (tag Exported))
    (Expire_toast (id 1) (after_ms 2500))
    Exported to /home/me/.prigh/sessions/exports/s1.md
    Exported to /tmp/s1.jsonl
    |}];
  run h "/import";
  H.act h (Dialog_input "old.jsonl");
  H.key h "Enter" ~target:Field;
  H.fail h "import" "old.jsonl: no such file";
  H.text h ~selector:".dialog-error";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /import) (max 500)))
     (tag Ignore))
    (Focus dialog-input)
    (Rpc (method_ list_paths) (params ((prefix ""))) (tag (Prompt_paths "")))
    (Rpc (method_ list_paths) (params ((prefix old.jsonl)))
     (tag (Prompt_paths old.jsonl)))
    Dialog_accept
    (Rpc (method_ import) (params ((path old.jsonl))) (tag Imported))
    old.jsonl: no such file
    |}];
  H.key h "Escape" ~target:Field;
  run h "/import /tmp/s1.jsonl";
  H.reply h "import" {|{"path":"/sessions/s9.jsonl"}|};
  toasts h;
  [%expect
    {|
    Close_dialog
    (Focus editor)
    (Rpc (method_ list_paths) (params ((prefix /tmp/s1.jsonl)))
     (tag (Paths /tmp/s1.jsonl)))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/import /tmp/s1.jsonl") (max 500)))
     (tag Ignore))
    (Rpc (method_ import) (params ((path /tmp/s1.jsonl))) (tag Imported))
    (Expire_toast (id 2) (after_ms 2700))
    (Rpc (method_ get_state) (params ()) (tag State))
    Exported to /tmp/s1.jsonl
    Imported as /sessions/s9.jsonl
    |}]
;;

let%expect_test "/copy and Ctrl+X copy the last reply" =
  let h = H.create () in
  run h "/copy";
  toasts h;
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /copy) (max 500)))
     (tag Ignore))
    Nothing to copy yet: no reply in this session. (Dismiss)
    |}];
  with_messages h;
  run h "/copy";
  [%expect
    {|
    Scroll_to_bottom
    (Rpc (method_ push_pref) (params ((key history) (value /copy) (max 500)))
     (tag Ignore))
    (Copy "Fixed: the **flag** was wrong.")
    (Expire_toast (id 1) (after_ms 3020))
    |}];
  H.key h "x" ~ctrl:true;
  [%expect
    {|
    Copy_last
    (Copy "Fixed: the **flag** was wrong.")
    (Expire_toast (id 2) (after_ms 3020))
    |}];
  (* With text selected Ctrl+X is the browser's cut. *)
  H.key h "x" ~ctrl:true ~selection:true;
  [%expect {| (browser default) |}]
;;

let%expect_test "/btw: a side answer streams into a panel" =
  let h = H.create () in
  run h "/btw";
  run h "/btw what does make check run?";
  H.event h {|{"event":"btw_delta","btw_id":"btw-1","delta":"It runs "}|};
  H.event h {|{"event":"btw_delta","btw_id":"btw-2","delta":"(stale)"}|};
  H.text h ~selector:"#btw";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /btw) (max 500)))
     (tag Ignore))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/btw what does make check run?") (max 500)))
     (tag Ignore))
    (Rpc (method_ btw)
     (params ((question "what does make check run?") (btw_id btw-1)))
     (tag (Btw btw-1)))
    btw what does make check run? (Close (Esc))
    It runs
    Answering…
    Not added to the conversation.
    |}];
  H.reply
    h
    "btw"
    {|{"btw_id":"btw-1","text":"It runs `dune build @runtest`.","cost_usd":0.001}|};
  H.text h ~selector:"#btw";
  [%expect
    {|
    btw what does make check run? (Close (Esc))
    It runs dune build @runtest .
    Answered
    Not added to the conversation.
    |}];
  (* A new question replaces the answer; Esc cancels and closes it. *)
  run h "/btw and lint?";
  H.key h "Escape";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/btw and lint?") (max 500))) (tag Ignore))
    (Rpc (method_ btw) (params ((question "and lint?") (btw_id btw-2)))
     (tag (Btw btw-2)))
    Close_btw
    (Rpc (method_ btw_cancel) (params ((btw_id btw-2))) (tag Ignore))
    (Focus editor)
    |}];
  H.text h ~selector:"#btw";
  run h "/btw again?";
  (* The cancelled question's failure is not this one's. *)
  H.fail h "btw" "cancelled";
  H.fail h "btw" "the model is unavailable";
  H.text h ~selector:"#btw";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/btw again?") (max 500))) (tag Ignore))
    (Rpc (method_ btw) (params ((question again?) (btw_id btw-3)))
     (tag (Btw btw-3)))
    btw again? (Close (Esc))
    Couldn't answer: the model is unavailable. /btw asks again.
    Not added to the conversation.
    |}]
;;

let now_ms = Time_ns.to_span_since_epoch H.now |> Time_ns.Span.to_ms

let subagent ?result id task =
  sprintf
    {|{"id":"%s","call_id":"call-%s","parent":null,"task":"%s","model":"m","state":"running","started_at_ms":%.0f,"updated_at_ms":0,"ended_at_ms":%s,"turns":1,"tool_calls":0,"current_tool":null,"current_tool_started_at_ms":null,"message_count":1,"stale":false,"result":%s}|}
    id
    id
    task
    (now_ms -. 30_000.)
    (if Option.is_some result
     then sprintf "%.0f" (now_ms -. 20_000.)
     else "null")
    (Option.value_map result ~default:"null" ~f:(fun text ->
       sprintf {|{"text":"%s","is_error":false}|} text))
;;

let%expect_test "/agents [n|id|cancel <n|id>]: the agents panel" =
  let h = H.create () in
  run h "/agents cancel 1";
  toasts h;
  H.reply
    h
    "list_subagents"
    (sprintf
       "[%s]"
       (String.concat
          ~sep:","
          [ subagent "a1" "read the tests"
          ; subagent "a2" "lint"
          ; subagent "a3" "count" ~result:"42"
          ]));
  H.reply h "list_jobs" "[]";
  run h "/agents";
  H.text h ~selector:".agents-panel";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/agents cancel 1") (max 500))) (tag Ignore))
    No subagent or job 1: none has run in this session. (Dismiss)
    (Rpc (method_ push_pref) (params ((key history) (value /agents) (max 500)))
     (tag Ignore))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    Agents
    2 agents running (Close (Alt+0))
    Subagents 2 running
    (Running read the tests 30s Thinking… a1  m  1 turn)
    (Running lint 30s Thinking… a2  m  1 turn)
    (✓ Done count 10s 42)
    |}];
  run h "/agents cancel 2";
  H.text h ~selector:".agents-panel";
  run h "/agents cancel a1";
  (* Each refusal replaces the last. *)
  List.iter
    [ "/agents cancel a3"; "/agents cancel 7"; "/agents cancel" ]
    ~f:(fun c ->
      run h c;
      toasts h);
  run h "/agents 2";
  H.text h ~selector:".toast";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/agents cancel 2") (max 500))) (tag Ignore))
    (Rpc (method_ get_subagent) (params ((id a2) (elide true)))
     (tag (Subagent a2)))
    (Rpc (method_ cancel_subagent) (params ((agent_id a2))) (tag Show_error))
    (All agents (Esc))
    Subagent a2
    (Close (Alt+0))
    Loading the transcript…
    lint
    (Steer) (Stopping…) (Show in chat) Running 30s m  1 turn
    (Rpc (method_ push_pref)
     (params ((key history) (value "/agents cancel a1") (max 500))) (tag Ignore))
    (Rpc (method_ get_subagent) (params ((id a1) (elide true)))
     (tag (Subagent a1)))
    (Rpc (method_ cancel_subagent) (params ((agent_id a1))) (tag Show_error))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/agents cancel a3") (max 500))) (tag Ignore))
    (Rpc (method_ get_subagent) (params ((id a3) (elide true)))
     (tag (Subagent a3)))
    Subagent a3 has already finished. (Dismiss)
    (Rpc (method_ push_pref)
     (params ((key history) (value "/agents cancel 7") (max 500))) (tag Ignore))
    No subagent or job 7: give its number (1-3) or id; /agents lists them. (Dismiss)
    (Rpc (method_ push_pref)
     (params ((key history) (value "/agents cancel") (max 500))) (tag Ignore))
    Usage: /agents cancel <n|id> (/agents lists them) (Dismiss)
    (Rpc (method_ push_pref)
     (params ((key history) (value "/agents 2") (max 500))) (tag Ignore))
    |}]
;;

let jobs =
  {|[{"id":"j1","command":"make test","running":true,"exit":null,"delivered":false,"elapsed":75.5,"bytes":120,"last_line":"ok 12 tests"},
     {"id":"j2","command":"sleep 1","running":false,"exit":"exited 0","delivered":false,"elapsed":1.0,"bytes":0,"last_line":null}]|}
;;

let%expect_test "/jobs [id|kill <id>]: jobs in the agents panel" =
  let h = H.create () in
  H.reply h "list_subagents" "[]";
  H.reply h "list_jobs" "[]";
  run h "/jobs j1";
  toasts h;
  run h "/jobs";
  H.reply h "list_subagents" "[]";
  H.reply h "list_jobs" jobs;
  H.text h ~selector:".agents-panel";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/jobs j1") (max 500))) (tag Ignore))
    No job j1: none has run in this session (!&command starts one). (Dismiss)
    (Rpc (method_ push_pref) (params ((key history) (value /jobs) (max 500)))
     (tag Ignore))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    Agents
    1 job running (Close (Alt+0))
    Jobs 1 running
    (Running make test 1m 15s ok 12 tests j1  120 B)
    (✓ Done sleep 1 1s)
    |}];
  run h "/jobs j2";
  H.reply h "job_output" {|{"text":""}|};
  H.text h ~selector:".agents-panel";
  run h "/jobs kill j1";
  H.text h ~selector:".agents-panel";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/jobs j2") (max 500))) (tag Ignore))
    (Rpc (method_ job_output) (params ((job_id j2))) (tag (Job_output j2)))
    (All agents (Esc))
    Job j2
    (Close (Alt+0))
    No output yet.
    ✓ sleep 1
    Done 1s
    (Rpc (method_ push_pref)
     (params ((key history) (value "/jobs kill j1") (max 500))) (tag Ignore))
    (Rpc (method_ job_output) (params ((job_id j1))) (tag (Job_output j1)))
    (Rpc (method_ kill_job) (params ((job_id j1))) (tag Show_error))
    (All agents (Esc))
    Job j1
    (Close (Alt+0))
    Loading the output…
    make test
    (Stopping…) Running 1m 15s 120 B
    |}];
  List.iter
    [ "/jobs kill j2"; "/jobs j9"; "/jobs kill"; "/jobs a b" ]
    ~f:(fun c ->
      run h c;
      toasts h);
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/jobs kill j2") (max 500))) (tag Ignore))
    (Rpc (method_ job_output) (params ((job_id j2))) (tag (Job_output j2)))
    Job j2 has already finished. (Dismiss)
    (Rpc (method_ push_pref)
     (params ((key history) (value "/jobs j9") (max 500))) (tag Ignore))
    No job j9: give its id (j1, j2); /jobs lists them. (Dismiss)
    (Rpc (method_ push_pref)
     (params ((key history) (value "/jobs kill") (max 500))) (tag Ignore))
    Usage: /jobs [id | kill <id>] (/jobs lists them) (Dismiss)
    (Rpc (method_ push_pref)
     (params ((key history) (value "/jobs a b") (max 500))) (tag Ignore))
    Usage: /jobs [id | kill <id>] (/jobs lists them) (Dismiss)
    |}]
;;

let%expect_test "/setusr: a superuser acts as another user, and back" =
  let h = H.create () in
  H.act
    h
    (Hello
       { client_id = "c1"
       ; namespace = Some "alice"
       ; user = Some "alice"
       ; superuser = false
       });
  run h "/setusr";
  H.reply h "list_users" {|["alice","bob","carol"]|};
  H.text h ~selector:".picker-items";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /setusr) (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ list_users) (params ()) (tag (Users Picker)))
    (Focus picker-input)
    alice you ✓
    bob
    carol
    |}];
  H.key h "ArrowDown" ~target:Field;
  H.key h "Enter" ~target:Field;
  H.reply h "set_user" {|{"client_id":"c1","namespace":"bob","user":"alice"}|};
  [%expect
    {|
    (Dialog_move 1)
    Dialog_accept
    (Focus editor)
    (Rpc (method_ set_user) (params ((user bob))) (tag User_switched))
    (Expire_toast (id 0) (after_ms 2500))
    (Focus editor)
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ forge_status) (params ()) (tag (Forge Status)))
    |}];
  H.reply h "get_state" (H.state_json ~fields:[ "session_id", `String "b1" ] ());
  H.text h ~selector:".sidebar-footer";
  H.text h ~selector:".status-item.account";
  [%expect
    {|
    (Set_url_session b1)
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/b1.jsonl) (limit 60)))
     (tag (Page (path /sessions/b1.jsonl) (before ()))))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Rpc (method_ get_review) (params ()) (tag (Review Review)))
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "b1\
                            \nbackend\
                            \n/work") (purpose (Sidebar 2)))))
    (A alice acting as bob) (Settings (Ctrl+, or /settings))
    |}];
  run h "/setusr alice";
  H.reply h "set_user" {|{"client_id":"c1","namespace":"alice","user":"alice"}|};
  toasts h;
  [%expect
    {|
    (Cache_prefs (((key history) (value (("/setusr alice" /setusr))))))
    (Rpc (method_ set_user) (params ((user alice))) (tag User_switched))
    (Expire_toast (id 1) (after_ms 2500))
    (Focus editor)
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ forge_status) (params ()) (tag (Forge Status)))
    (Rpc (method_ get_prefs)
     (params
      ((keys
        (web.theme web.accent web.notify web.verbosity web.terminal-open
         web.sidebar-folded web.review history web.migrated web.sidebar-width
         web.files-width web.files-preview-width web.agents-width
         web.terminal-height web.review-chat-width web.review-files-width
         web.review-margin-width))))
     (tag Prefs))
    Acting as bob
    Back to alice
    |}];
  run h "/setusr";
  H.fail h "list_users" "unauthorised: bob is not a superuser";
  toasts h;
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /setusr) (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ list_users) (params ()) (tag (Users Picker)))
    (Focus editor)
    Back to alice
    Unauthorised: bob is not a superuser (Dismiss)
    |}]
;;

let%expect_test "/retry-backend-connection, /state, /clear, /quit" =
  let h = H.create () in
  run h "/retry-backend-connection";
  H.act h Backend_closed;
  run h "/retry-backend-connection";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value /retry-backend-connection) (max 500)))
     (tag Ignore))
    (Expire_toast (id 0) (after_ms 2500))
    (Reconnect (generation 1) (delay_ms 0) (session (s1)))
    (Rpc (method_ push_pref)
     (params ((key history) (value /retry-backend-connection) (max 500)))
     (tag Ignore))
    (Expire_toast (id 1) (after_ms 2500))
    (Reconnect (generation 2) (delay_ms 0) (session (s1)))
    |}];
  (* The first attempt's late reply is stale. *)
  H.act h (Reply (Reconnect 1, Error "refused"));
  H.act h (Reply (Reconnect 2, Ok (Jsonaf.of_string {|{"client_id":"c2"}|})));
  [%expect
    {|
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ forge_status) (params ()) (tag (Forge Status)))
    (Rpc (method_ get_prefs)
     (params
      ((keys
        (web.theme web.accent web.notify web.verbosity web.terminal-open
         web.sidebar-folded web.review history web.migrated web.sidebar-width
         web.files-width web.files-preview-width web.agents-width
         web.terminal-height web.review-chat-width web.review-files-width
         web.review-margin-width))))
     (tag Prefs))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s1.jsonl) (limit 1000000)))
     (tag (Resync (session s1) (from ()))))
    (Rpc (method_ get_pending) (params ()) (tag Resync_pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    |}];
  H.reply h "get_state" (H.state_json ());
  run h "/state";
  H.text h ~selector:".modal";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /state) (max 500)))
     (tag Ignore))
    (Focus dialog)
    Session state
    (Close (Esc))
    ((session_id s1) (session_path /sessions/s1.jsonl) (session_name ())
     (session_description ()) (cwd /work) (home ()) (git_branch (main))
     (model
      ((id claude-opus-5-5) (provider anthropic) (key anthropic/claude-opus-5-5)
       (name "Claude Opus 5.5") (context_window 200000) (max_output 64000)
       (supports_thinking true) (cost ((input 5) (output 25) (cache_read 0.5)))))
     (thinking on) (running false) (message_count 0)
     (usage ((input 0) (output 0) (cache_read 0))) (cost_usd 0)
     (context_tokens 0) (active_host backend) (hosts ()) (subagents ()) (jobs ())
     (budget_usd ()) (goal ()) (reminders ()) (resume_at ()) (approval_mode Auto)
     (plan_mode false) (plan ()) (session_rules ()))
    (Done)
    |}];
  H.key h "Escape" ~target:Page;
  with_messages h;
  run h "/clear";
  H.text h ~selector:"#chat";
  run h "/quit";
  toasts h;
  [%expect
    {|
    Close_dialog
    (Focus editor)
    Scroll_to_bottom
    (Rpc (method_ push_pref) (params ((key history) (value /clear) (max 500)))
     (tag Ignore))
    (Expire_toast (id 2) (after_ms 4780))
    What are we building?
    /work
    / commands
    @ mention a file
    ! run a command
    Ctrl+L switch model
    Ctrl+K find a session
    (Rpc (method_ push_pref) (params ((key history) (value /quit) (max 500)))
     (tag Ignore))
    (Expire_toast (id 3) (after_ms 4620))
    Cleared the view; the conversation is kept (a reload shows it, /new starts afresh)
    Close the tab to leave: the session stays in the backend (/signout signs out).
    |}]
;;

let%expect_test "keys: Alt+Up, Ctrl+Up/Down, PageUp/PageDown, Ctrl+G" =
  let h = H.create () in
  H.key h "ArrowUp" ~alt:true;
  H.key h "ArrowUp" ~ctrl:true;
  H.key h "ArrowDown" ~ctrl:true;
  H.key h "PageUp";
  H.key h "PageDown";
  H.key h "g" ~ctrl:true;
  [%expect
    {|
    Dequeue
    (Rpc (method_ dequeue) (params ()) (tag Dequeued))
    (Jump_to_user_message -1)
    (Jump_to_user_message -1)
    (Jump_to_user_message 1)
    (Jump_to_user_message 1)
    (Scroll_chat -1)
    (Scroll_chat -1)
    (Scroll_chat 1)
    (Scroll_chat 1)
    (Show_toast
     (text
      "There is no $EDITOR in a browser: edit here (Shift+Enter for new lines).")
     (error false))
    (Expire_toast (id 0) (after_ms 4380))
    |}];
  (* Keys the browser keeps. *)
  H.key h "z" ~ctrl:true;
  H.key h "c" ~ctrl:true;
  H.key h "f" ~ctrl:true;
  H.key h "r" ~ctrl:true;
  H.key h "p" ~meta:true;
  [%expect
    {|
    (browser default)
    (browser default)
    (browser default)
    (browser default)
    (browser default)
    |}]
;;
