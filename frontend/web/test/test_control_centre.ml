open! Core
open Prigh_web
module H = Harness

let sessions =
  sprintf
    "[%s]"
    (String.concat
       ~sep:","
       [ H.session_json
           ~name:"Make the port configurable"
           ~first_prompt:"prigh serve should take its port from the config"
           ~updated_at:"2026-10-05 09:00:00Z"
           "s1"
       ; H.session_json
           ~first_prompt:"fix the parser bug"
           ~updated_at:"2026-10-05 08:00:00Z"
           "s2"
       ; H.session_json
           ~name:"Release notes"
           ~first_prompt:"write the release notes"
           ~updated_at:"2026-10-04 08:00:00Z"
           "s3"
       ])
;;

let palette h = H.text h ~selector:"#control-centre"

let%expect_test
    "Ctrl+K opens the control centre: the session's settings, recent sessions, \
     a few commands"
  =
  let h = H.create ~sessions () in
  H.key h "k" ~ctrl:true;
  palette h;
  [%expect
    {|
    Open_control
    (Focus control-input)
    [] (Close (Esc))
    This session
    Set model
    Claude Opus 5.5 Ctrl+L
    Set thinking
    provider's default Alt+T
    Set verbosity
    normal Ctrl+O
    Switch host
    none
    Set mode
    auto Alt+M
    Context and cost
    0% · $0.00
    Recent sessions
    fix the parser bug
    /work · 2h ago
    Release notes
    /work · 1d ago
    Commands
    /new
    start a new session
    /settings
    appearance, sessions, new sessions' defaults, models, git hosting and your account; a section opens at it (Ctrl+,)
    /review
    the review page, the changes file by file to approve, comment on and edit (Alt+R): of the session's worktree or another (its branch, path or name), against its base or REV; /review BASE [instructions] (HEAD: the uncommitted changes) has the model review them for prioritised findings
    /files
    browse, preview, upload and download files where the tools run (Ctrl+Shift+F)
    /help
    show commands and keys, or a command's usage
    ↑↓ move ↵ open ⌫ back esc close
    |}];
  H.show h ~selector:".control-input-row";
  [%expect
    {|
    <div class="control-input-row">
      <icon class="search"> </icon>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <input id="control-input"
             type="text"
             aria-label="Search sessions, settings and commands"
             role="combobox"
             aria-controls="control-list"
             aria-expanded="true"
             aria-autocomplete="list"
             aria-activedescendant="control-option-0"
             placeholder="Search sessions, settings and commands…"
             autocomplete="off"
             spellcheck="false"
             #value=""
             @on_input/>
      <button type="button"
              title="Close (Esc)"
              aria-label="Close"
              class="btn control-close ghost icon"
              @on_click>
        <icon class="close"> </icon>
      </button>
    </div>
    |}];
  (* Again: closed. *)
  H.key h "k" ~ctrl:true ~target:Field;
  [%expect
    {|
    Close_dialog
    (Focus editor)
    |}];
  print_s [%sexp (Option.is_some (H.model h).dialog : bool)];
  [%expect {| false |}]
;;

let%expect_test "the sidebar's Search opens it; Esc closes it, nothing changed" =
  let h = H.create ~sessions () in
  H.text h ~selector:".sidebar-actions";
  [%expect {| (New session) (Search) |}];
  H.click h ~selector:"#control-open";
  H.input h ~selector:"#control-input" "verbos";
  H.key h "Escape" ~target:Field;
  print_s
    [%message
      ""
        ~dialog:(Option.is_some (H.model h).dialog : bool)
        ~verbosity:(Prigh_ui.Verbosity.name (H.model h).verbosity)];
  [%expect
    {|
    Open_control
    (Focus control-input)
    (Control_query verbos)
    (Search_sessions_later (search 1) (after_ms 150))
    Close_dialog
    (Focus editor)
    ((dialog false) (verbosity normal))
    |}]
;;

let%expect_test "typing ranks the settings, their values, sessions and commands"
  =
  let h = H.create ~sessions () in
  H.act h Open_control;
  H.input h ~selector:"#control-input" "verbos";
  palette h;
  H.show h ~selector:".control-row.selected";
  [%expect
    {|
    (Focus control-input)
    (Control_query verbos)
    (Search_sessions_later (search 1) (after_ms 150))
    [verbos] (Close (Esc))
    Settings
    Set verbos ity
    normal Ctrl+O
    Verbos ity: Quiet
    tool calls without their output; no thinking
    Verbos ity: Normal
    current
    Verbos ity: Verbose
    everything unfolded
    Commands
    / verbos ity
    how much of tool calls and thinking the transcript shows
    ↑↓ move ↵ open ⌫ back esc close
    <div role="option"
         id="control-option-0"
         aria-selected="true"
         class="control-row selected"
         @on_click>
      <div class="control-main">
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        <span dir="auto" class="control-label">
          Set
          <mark> verbos </mark>
          ity
        </span>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      </div>
      <span class="control-detail"> normal </span>
      <kbd class="control-keys"> Ctrl+O </kbd>
    </div>
    |}];
  H.input h ~selector:"#control-input" "default mode";
  palette h;
  [%expect
    {|
    (Control_query "default mode")
    (Search_sessions_later (search 2) (after_ms 150))
    [default mode] (Close (Esc))
    Settings
    Default mode for new sessions
    auto
    Default mode l for new sessions
    the backend's
    Fallback models
    none
    Commands
    /settings
    appearance, sessions, new sessions' defaults, models, git hosting and your account; a section opens at it (Ctrl+,)
    /change_default
    save the model and thinking level as the default for new sessions
    ↑↓ move ↵ open ⌫ back esc close
    |}]
;;

let%expect_test "a value applies in one step" =
  let h = H.create ~sessions () in
  H.act h Open_control;
  H.input h ~selector:"#control-input" "verbosity verbose";
  H.key h "Enter" ~target:Field;
  print_s
    [%message
      ""
        ~dialog:(Option.is_some (H.model h).dialog : bool)
        ~verbosity:(Prigh_ui.Verbosity.name (H.model h).verbosity)];
  [%expect
    {|
    (Focus control-input)
    (Control_query "verbosity verbose")
    (Search_sessions_later (search 1) (after_ms 150))
    Dialog_accept
    (Focus editor)
    (Cache_prefs (((key web.verbosity) (value (verbose)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.verbosity verbose)))))
     (tag Show_error))
    (Expire_toast (id 0) (after_ms 3740))
    ((dialog false) (verbosity verbose))
    |}]
;;

let%expect_test
    "Enter on a setting opens its values; ⌫ in the empty filter goes back; \
     Enter sets one and closes"
  =
  let h = H.create ~sessions () in
  H.act h Open_control;
  H.input h ~selector:"#control-input" "verbos";
  H.key h "Enter" ~target:Field;
  palette h;
  H.show h ~selector:".control-input-row";
  [%expect
    {|
    (Focus control-input)
    (Control_query verbos)
    (Search_sessions_later (search 1) (after_ms 150))
    Dialog_accept
    (Focus control-input)
    (Verbosity) [] (Close (Esc))
    Quiet tool calls without their output; no thinking
    Normal tool output and thinking folded
    current
    Verbose everything unfolded
    ↑↓ move ↵ set ⌫ back esc close
    <div class="control-input-row">
      <icon class="search"> </icon>
      <button type="button" title="Back (⌫ in an empty filter)" class="btn control-crumb" @on_click> Verbosity </button>
      <input id="control-input"
             type="text"
             aria-label="Search sessions, settings and commands"
             role="combobox"
             aria-controls="control-list"
             aria-expanded="true"
             aria-autocomplete="list"
             aria-activedescendant="control-option-1"
             placeholder="Filter verbosity…"
             autocomplete="off"
             spellcheck="false"
             #value=""
             @on_input/>
      <button type="button"
              title="Close (Esc)"
              aria-label="Close"
              class="btn control-close ghost icon"
              @on_click>
        <icon class="close"> </icon>
      </button>
    </div>
    |}];
  H.key h "Backspace" ~target:Field;
  palette h;
  [%expect
    {|
    Control_back
    (Focus control-input)
    [verbos] (Close (Esc))
    Settings
    Set verbos ity
    normal Ctrl+O
    Verbos ity: Quiet
    tool calls without their output; no thinking
    Verbos ity: Normal
    current
    Verbos ity: Verbose
    everything unfolded
    Commands
    / verbos ity
    how much of tool calls and thinking the transcript shows
    ↑↓ move ↵ open ⌫ back esc close
    |}];
  H.key h "Enter" ~target:Field;
  H.key h "ArrowDown" ~target:Field;
  H.key h "ArrowDown" ~target:Field;
  H.show h ~selector:".control-row.selected";
  H.key h "Enter" ~target:Field;
  print_s
    [%message
      ""
        ~dialog:(Option.is_some (H.model h).dialog : bool)
        ~verbosity:(Prigh_ui.Verbosity.name (H.model h).verbosity)];
  [%expect
    {|
    Dialog_accept
    (Focus control-input)
    (Dialog_move 1)
    (Dialog_move 1)
    <div role="option"
         id="control-option-2"
         aria-selected="true"
         class="control-row selected"
         @on_click>
      <div class="control-main">
        <span aria-hidden="true" class="control-dot"> </span>
        <span class="control-label"> Verbose </span>
        <span class="control-about"> everything unfolded </span>
      </div>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
    </div>
    Dialog_accept
    (Focus editor)
    (Cache_prefs (((key web.verbosity) (value (verbose)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.verbosity verbose)))))
     (tag Show_error))
    (Expire_toast (id 0) (after_ms 3740))
    ((dialog false) (verbosity verbose))
    |}]
;;

let%expect_test "a setting's values filter; a click sets one" =
  let h = H.create ~sessions () in
  H.act h Open_control;
  H.input h ~selector:"#control-input" "set thinking";
  H.key h "Enter" ~target:Field;
  H.input h ~selector:"#control-input" "hi";
  palette h;
  H.click h ~selector:".control-row";
  [%expect
    {|
    (Focus control-input)
    (Control_query "set thinking")
    (Search_sessions_later (search 1) (after_ms 150))
    Dialog_accept
    (Focus control-input)
    (Control_query hi)
    (Thinking) [hi] (Close (Esc))
    high more
    xhigh a lot, for long agentic work
    provider's default the provider picks: medium
    current
    ↑↓ move ↵ set ⌫ back esc close
    (Control_choose 0)
    (Focus editor)
    (Rpc (method_ set_thinking) (params ((thinking high))) (tag Thinking_set))
    |}]
;;

let%expect_test "the session's mode and a default through it" =
  let h = H.create ~sessions () in
  H.act h Open_control;
  H.input h ~selector:"#control-input" "set mode";
  H.key h "Enter" ~target:Field;
  palette h;
  [%expect
    {|
    (Focus control-input)
    (Control_query "set mode")
    (Search_sessions_later (search 1) (after_ms 150))
    Dialog_accept
    (Focus control-input)
    (Mode) [] (Close (Esc))
    Asks: edits & commands bash, job_input, edits and MCP tools ask first
    Asks: commands file edits run; bash, job_input and MCP tools ask first
    Smart: a model decides edits in the project run; the approval model lets other calls run or asks you, saying why (/approval-model)
    Auto: never asks tools run without asking
    current
    Plan mode read-only planning, until you leave it (/plan off or Alt+M): no file edits; bash, job_input and MCP tools ask first
    Chat: no tool host a chat without the tool host: no files, commands or project, only the web and other host-free tools, until you leave it (/chat off or Alt+M)
    ↑↓ move ↵ set ⌫ back esc close
    |}];
  H.key h "Escape" ~target:Field;
  H.act h Open_control;
  H.input h ~selector:"#control-input" "new sessions' mode: auto";
  palette h;
  H.key h "Enter" ~target:Field;
  [%expect
    {|
    Close_dialog
    (Focus editor)
    (Focus control-input)
    (Control_query "new sessions' mode: auto")
    (Search_sessions_later (search 2) (after_ms 150))
    [new sessions' mode: auto] (Close (Esc))
    Settings
    New sessions' mode: Auto
    current
    New sessions' mode: Auto -edit
    file edits run; bash, job_input and MCP tools ask first
    ↑↓ move ↵ open ⌫ back esc close
    Dialog_accept
    (Focus editor)
    (Rpc (method_ set_config)
     (params
      ((config ((approval_mode auto) (default_chat_mode false)))
       (defaults_only true)))
     (tag (Default_set (Mode (Approval Auto)))))
    |}]
;;

let%expect_test "a setting kept on the settings page opens it there" =
  let h = H.create ~sessions () in
  H.act h Open_control;
  H.input h ~selector:"#control-input" "archive after";
  H.key h "Enter" ~target:Field;
  print_s
    [%message
      ""
        ~dialog:
          (Option.map (H.model h).dialog ~f:(fun d ->
             Sexp.to_string_hum ([%sexp_of: Dialog.t] d)
             |> Fn.flip String.prefix 40)
           : string option)
        ~section:((H.model h).settings_section : Settings_section.t)];
  [%expect
    {|
    (Focus control-input)
    (Control_query "archive after")
    (Search_sessions_later (search 1) (after_ms 150))
    Dialog_accept
    (Focus dialog)
    ((dialog (Settings)) (section Sessions))
    |}]
;;

let%expect_test
    "sessions: titles at once, the messages once the typing pauses; Enter \
     switches"
  =
  let h = H.create ~sessions () in
  H.act h Open_control;
  H.input h ~selector:"#control-input" "port";
  H.input h ~selector:"#control-input" "port config";
  H.text h ~selector:".control-list";
  [%expect
    {|
    (Focus control-input)
    (Control_query port)
    (Search_sessions_later (search 1) (after_ms 150))
    (Control_query "port config")
    (Search_sessions_later (search 2) (after_ms 150))
    Sessions
    Make the port config urable
    /work · 1h ago
    |}];
  (* The first pause came too soon: the query changed since. *)
  H.act h (Search_sessions 1);
  H.act h (Search_sessions 2);
  H.reply
    h
    "search_sessions"
    {|{"query":"port config","hits":[
      {"id":"s2","path":"/sessions/s2.jsonl","score":44,
       "snippet":{"text":"…then make the port configurable via the config file","ranges":[[17,4],[22,6],[43,6]]}},
      {"id":"s1","path":"/sessions/s1.jsonl","score":30,"snippet":null}]}|};
  H.text h ~selector:".control-list";
  H.show h ~selector:".control-snippet";
  [%expect
    {|
    (Rpc (method_ search_sessions) (params ((query "port config") (limit 50)))
     (tag (Session_hits 2)))
    Sessions
    Make the port config urable
    /work · 1h ago
    fix the parser bug …then make the port config urable via the config file
    /work · 2h ago
    <span dir="auto" class="control-snippet">
      …then make the
      <mark> port </mark>

      <mark> config </mark>
      urable via the
      <mark> config </mark>
       file
    </span>
    |}];
  H.key h "ArrowDown" ~target:Field;
  H.key h "Enter" ~target:Field;
  [%expect
    {|
    (Dialog_move 1)
    Dialog_accept
    (Focus editor)
    (Rpc (method_ switch_session) (params ((path /sessions/s2.jsonl)))
     (tag Reload_state))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before ()))))
    (Focus editor)
    |}]
;;

let%expect_test "nothing found says what to do; a command runs" =
  let h = H.create ~sessions () in
  H.act h Open_control;
  H.input h ~selector:"#control-input" "zzzz";
  palette h;
  [%expect
    {|
    (Focus control-input)
    (Control_query zzzz)
    (Search_sessions_later (search 1) (after_ms 150))
    [zzzz] (Close (Esc))
    Nothing matches “zzzz”: Backspace widens the search.
    ↑↓ move ↵ open ⌫ back esc close
    |}];
  H.input h ~selector:"#control-input" "/hotkeys";
  H.key h "Enter" ~target:Field;
  [%expect
    {|
    (Control_query /hotkeys)
    (Search_sessions_later (search 2) (after_ms 150))
    Dialog_accept
    (Focus editor)
    (Focus dialog)
    |}];
  H.act h Close_dialog;
  H.act h Open_control;
  H.input h ~selector:"#control-input" "rename";
  palette h;
  H.key h "Enter" ~target:Field;
  print_s [%sexp ((H.model h).draft : string)];
  [%expect
    {|
    (Focus editor)
    (Focus control-input)
    (Control_query rename)
    (Search_sessions_later (search 3) (after_ms 150))
    [rename] (Close (Esc))
    Commands
    /name
    rename the session
    ↑↓ move ↵ open ⌫ back esc close
    Dialog_accept
    (Focus editor)
    (Focus dialog-input)
    ""
    |}]
;;

let%expect_test "switching sessions resets it" =
  let h = H.create ~sessions () in
  H.act h Open_control;
  H.input h ~selector:"#control-input" "release";
  H.key h "Enter" ~target:Field;
  print_s [%sexp (Option.is_some (H.model h).dialog : bool)];
  H.act h Open_control;
  palette h;
  [%expect
    {|
    (Focus control-input)
    (Control_query release)
    (Search_sessions_later (search 1) (after_ms 150))
    Dialog_accept
    (Focus editor)
    (Rpc (method_ switch_session) (params ((path /sessions/s3.jsonl)))
     (tag Reload_state))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s3.jsonl) (limit 60)))
     (tag (Page (path /sessions/s3.jsonl) (before ()))))
    (Focus editor)
    false
    (Focus control-input)
    [] (Close (Esc))
    This session
    Set model
    Claude Opus 5.5 Ctrl+L
    Set thinking
    provider's default Alt+T
    Set verbosity
    normal Ctrl+O
    Switch host
    none
    Set mode
    auto Alt+M
    Context and cost
    0% · $0.00
    Recent sessions
    fix the parser bug
    /work · 2h ago
    Release notes
    /work · 1d ago
    Commands
    /new
    start a new session
    /settings
    appearance, sessions, new sessions' defaults, models, git hosting and your account; a section opens at it (Ctrl+,)
    /review
    the review page, the changes file by file to approve, comment on and edit (Alt+R): of the session's worktree or another (its branch, path or name), against its base or REV; /review BASE [instructions] (HEAD: the uncommitted changes) has the model review them for prioritised findings
    /files
    browse, preview, upload and download files where the tools run (Ctrl+Shift+F)
    /help
    show commands and keys, or a command's usage
    ↑↓ move ↵ open ⌫ back esc close
    |}]
;;

(* Every row of the settings page is in the control centre's table. *)
let%expect_test "each setting has an entry" =
  let h = H.create ~sessions () in
  H.act
    h
    (Hello
       { client_id = "c1"
       ; namespace = None
       ; user = None
       ; superuser = true
       ; features = []
       });
  let covered =
    List.filter_map Control_setting.all ~f:Control_setting.page
    |> List.map ~f:(fun (section, name) -> Settings_section.name section, name)
  in
  let missing = ref [] in
  List.iter (Settings_section.available ~superuser:true) ~f:(fun section ->
    H.quiet h (fun () -> H.act h (Settings_section section));
    let names = H.texts h ~selector:".setting-name" in
    let covers name =
      List.mem
        covered
        (Settings_section.name section, name)
        ~equal:[%equal: string * string]
    in
    List.iter names ~f:(fun name ->
      if not (covers name || covers (Settings_section.name section))
      then missing := (Settings_section.name section, name) :: !missing));
  print_s [%sexp (List.rev !missing : (string * string) list)];
  [%expect {| () |}]
;;

let%expect_test
    "the composer's row: the model, host, thinking and mode chips, no \
     verbosity, no divider"
  =
  let h = H.create ~sessions () in
  H.names ~selector:".composer-row button" h;
  H.elements ~selector:".row-chips > *" h;
  [%expect
    {|
    button "Add files or images" -- Add files or images (or paste, or drop them here)
    button "Claude Opus 5.5" -- Claude Opus 5.5: switch model (Ctrl+L)
    button "backend" -- Tools run on the backend (/host switches)
    button "Thinking: provider's default" -- Thinking: provider's default: how much the model reasons before it answers (Alt+T cycles, /thinking)
    button "Auto: never asks" -- Tools run without asking (Alt+M cycles, /mode picks)
    button "Context 0% full: the session's details"
    button "Dictate" -- Dictate (speak, then click again to stop)
    button "Send" -- Send (Enter)
    <div class="chip-anchor thinking">
    <div class="chip-anchor mode">
    |}]
;;
