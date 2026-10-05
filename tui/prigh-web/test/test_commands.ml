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
  H.act h (Reply (Messages "s1", Ok (Jsonaf.of_string messages)))
;;

let%expect_test "every command does something (none is unknown)" =
  List.iter Slash.all ~f:(fun spec ->
    let h = H.create () in
    let m = H.model h in
    let m', commands = App.update { m with draft = "/" ^ spec.name } Send in
    let what =
      match m'.dialog, m'.toasts, commands with
      | Some dialog, _, _ ->
        (match dialog with
         | Picker { picker; _ } -> "picker: " ^ Picker.title picker
         | Scoped_models _ -> "scoped models dialog"
         | Prompt _ -> "path prompt"
         | Text { title; _ } -> "text: " ^ title
         | d -> String.prefix (Sexp.to_string (Dialog.sexp_of_t d)) 30)
      | None, _, _ when m'.agents.open_ -> "agents panel"
      | None, (_ :: _ as toasts), _ -> "toast: " ^ (List.last_exn toasts).text
      | None, [], _ ->
        List.filter_map commands ~f:(function
          | Rpc { method_; _ } -> Some ("rpc " ^ method_)
          | Save_history _ | Focus _ -> None
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
    /new                       rpc new_session
    /model                     picker: Switch model
    /scoped-models             scoped models dialog
    /thinking                  picker: Thinking level
    /change_default            rpc change_default
    /verbosity                 picker: Transcript verbosity
    /confirm                   picker: Tool confirmation
    /compact                   toast: Compacting the conversation…
    /name                      (Rename"")
    /session                   rpc session_stats
    /sessions                  sidebar
    /switch                    sidebar
    /clone                     rpc clone
    /fork                      rpc get_entries
    /rewind                    rpc get_entries
    /tree                      rpc get_entries
    /cd                        path prompt
    /host                      picker: Where tools run
    /export                    path prompt
    /import                    path prompt
    /copy                      toast: Nothing to copy yet: no reply in this session.
    /btw                       toast: Usage: /btw <question> (asked aside; the run goes on)
    /abort                     rpc abort
    /agents                    agents panel
    /jobs                      agents panel
    /login                     rpc auth_status
    /logout                    rpc auth_status
    /auth                      rpc auth_status
    /setusr                    rpc list_users
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
    (Save_history ("/help compact"))
    (Expire_toast (id 0) (after_ms 4000))
    (Save_history ("/help /tree" "/help compact"))
    (Expire_toast (id 1) (after_ms 4000))
    (Save_history ("/help compcat" "/help /tree" "/help compact"))
    /compact [instructions] — summarise older messages to free context
    /tree — show the session tree and move to any message in it
    No command /compcat. Did you mean /compact?
    |}];
  run h "/hotkeys";
  H.text h ~selector:".modal";
  [%expect
    {|
    (Save_history (/hotkeys "/help compcat" "/help /tree" "/help compact"))
    (Focus dialog)
    Keyboard shortcuts
    (Close (Esc))
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
    Tab (in /scoped-models) check or uncheck the highlighted model
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
    (Expire_toast (id 0) (after_ms 4000))
    (Rpc (method_ set_model) (params ((model anthropic/claude-sonnet-5)))
     (tag Show_error))
    (Cycle_model 1)
    (Expire_toast (id 1) (after_ms 4000))
    (Rpc (method_ set_model) (params ((model anthropic/claude-opus-5-5)))
     (tag Show_error))
    Model: Claude Opus 5.5
    |}];
  run h "/scoped-models";
  H.text h ~selector:".modal";
  [%expect
    {|
    (Save_history (/scoped-models))
    (Focus picker-input)
    Scoped models
    (Close (Esc))
    Ctrl+P and Alt+P cycle through the checked models. With none checked they cycle through the logged-in ones.
    []
    GPT-6 openai
    ✓ Claude Opus 5.5 anthropic
    ✓ Claude Sonnet 5 anthropic
    DeepSeek Chat deepseek
    2 checked · Tab toggles · Enter saves (Cancel) (Save)
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
    3 checked · Tab toggles · Enter saves (Cancel) (Save)
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
           deepseek/deepseek-chat))
         (confirm_tools false) (default_model null) (default_thinking null)))))
     (tag (Config_saved "3 scoped models: Ctrl+P and Alt+P cycle through them")))
    (Expire_toast (id 2) (after_ms 4000))
    Model: Claude Opus 5.5
    3 scoped models: Ctrl+P and Alt+P cycle through them
    |}];
  (* Alt+P goes back; on a Mac Option+P types π, so the key's code counts. *)
  H.key h "π" ~alt:true ~code:"KeyP";
  H.key h "p" ~ctrl:true;
  [%expect
    {|
    (Cycle_model -1)
    (Expire_toast (id 3) (after_ms 4000))
    (Rpc (method_ set_model) (params ((model deepseek/deepseek-chat)))
     (tag Show_error))
    (Cycle_model 1)
    (Expire_toast (id 4) (after_ms 4000))
    (Rpc (method_ set_model) (params ((model anthropic/claude-opus-5-5)))
     (tag Show_error))
    |}];
  (* A dialog owns the keyboard: Ctrl+P does not cycle under it. *)
  run h "/scoped-models";
  H.key h "p" ~ctrl:true ~target:Field;
  [%expect
    {|
    (Save_history (/scoped-models))
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
      (confirm_tools false) (default_model ()) (default_thinking ())))
    |}]
;;

let%expect_test "/thinking and Alt+T" =
  let h = H.create () in
  H.key h "t" ~alt:true;
  H.key h "†" ~alt:true ~code:"KeyT";
  [%expect
    {|
    Cycle_thinking
    (Expire_toast (id 0) (after_ms 4000))
    (Rpc (method_ set_thinking) (params ((thinking high))) (tag Show_error))
    Cycle_thinking
    (Expire_toast (id 1) (after_ms 4000))
    (Rpc (method_ set_thinking) (params ((thinking max))) (tag Show_error))
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
     (tag Show_error))
    Cycle_thinking
    Thinking: max
    DeepSeek Chat has no thinking levels: switch to a model that thinks with /model
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
    (Save_history (/change_default))
    (Rpc (method_ change_default) (params ()) (tag Default_saved))
    (Expire_toast (id 0) (after_ms 4000))
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
    Follow_chat
    (Save_history (/verbosity))
    (Focus picker-input)
    Quiet tool calls without their output; no thinking
    Normal tool output and thinking folded ✓
    Verbose everything unfolded
    |}];
  H.key h "ArrowUp" ~target:Field;
  H.key h "Enter" ~target:Field;
  H.show h ~selector:"#chat";
  H.text h ~selector:".status";
  [%expect
    {|
    (Dialog_move -1)
    Dialog_accept
    (Focus editor)
    (Expire_toast (id 0) (after_ms 4000))
    <div id="chat" class="chat verbosity-quiet">
      <div class="entries">
        <div class="msg user">
          <div class="bubble"> fix the build </div>
        </div>
        <div class="assistant msg">
          <div class="markdown">
            <p>
              Fixed: the
              <strong> flag </strong>
               was wrong.
            </p>
          </div>
          <div class="meta"> m · 10 in · 5 out </div>
        </div>
      </div>
    </div>
    Ready 0% ↑0 ↓0 $0.0000 (quiet)
    |}];
  H.key h "o" ~ctrl:true;
  H.key h "o" ~ctrl:true;
  run h "/verbosity loud";
  toasts h;
  [%expect
    {|
    Cycle_verbosity
    (Expire_toast (id 1) (after_ms 4000))
    Cycle_verbosity
    (Expire_toast (id 2) (after_ms 4000))
    (Save_history ("/verbosity loud" /verbosity))
    Transcript: verbose, everything unfolded (Ctrl+O cycles)
    Unknown verbosity "loud": use quiet, normal or verbose.
    |}];
  H.text h ~selector:".status";
  [%expect {| Ready 0% ↑0 ↓0 $0.0000 (verbose) |}]
;;

let%expect_test "/confirm" =
  let h = H.create () in
  run h "/confirm";
  H.text h ~selector:".picker-items";
  [%expect
    {|
    (Save_history (/confirm))
    (Focus picker-input)
    On ask before bash, write and edit run
    Off run tools without asking ✓
    |}];
  H.act h (Picker_choose "on");
  H.reply
    h
    "set_config"
    {|{"scoped_models":[],"confirm_tools":true,"default_model":null,"default_thinking":null}|};
  H.text h ~selector:".status";
  run h "/confirm off";
  run h "/confirm maybe";
  toasts h;
  [%expect
    {|
    (Focus editor)
    (Rpc (method_ set_config)
     (params
      ((config
        ((scoped_models ()) (confirm_tools true) (default_model null)
         (default_thinking null)))))
     (tag (Config_saved "Tool confirmation on: bash, write and edit ask first")))
    (Expire_toast (id 0) (after_ms 4000))
    Ready 0% ↑0 ↓0 $0.0000 (confirm)
    (Save_history ("/confirm off" /confirm))
    (Rpc (method_ set_config)
     (params
      ((config
        ((scoped_models ()) (confirm_tools false) (default_model null)
         (default_thinking null)))))
     (tag (Config_saved "Tool confirmation off: tools run without asking")))
    (Save_history ("/confirm maybe" "/confirm off" /confirm))
    Tool confirmation on: bash, write and edit ask first
    Unknown setting "maybe": use on or off.
    |}]
;;

let%expect_test "/compact with instructions" =
  let h = H.create () in
  run h "/compact keep the file names";
  [%expect
    {|
    (Save_history ("/compact keep the file names"))
    (Expire_toast (id 0) (after_ms 4000))
    (Rpc (method_ compact) (params ((instructions "keep the file names")))
     (tag (Notice "Compacted the conversation")))
    |}];
  H.type_ h "/comp";
  H.key h "Enter";
  [%expect
    {|
    (Complete_accept (run true))
    (Save_history (/compact "/compact keep the file names"))
    (Expire_toast (id 1) (after_ms 4000))
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
    {|{"message_count":12,"turns":4,"tool_calls":{"bash":3,"read":5},"usage":{"input":45000,"output":3200,"cache_read":30000},"cost_usd":0.4321,"context_percent":22.5,"model_changes":1,"compactions":0,"duration_seconds":754.2}|};
  H.text h ~selector:".modal";
  [%expect
    {|
    (Save_history (/session))
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
    Tools bash 3, read 5 Model changes 1 Compactions 0
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
    Sessions ↑↓ Tab Enter Esc
    Release notes /work
    |}];
  H.key h "Enter";
  [%expect
    {|
    (Complete_accept (run true))
    (Save_history ("/switch /sessions/s2.jsonl"))
    (Rpc (method_ switch_session) (params ((path /sessions/s2.jsonl)))
     (tag Reload_state))
    |}];
  H.act h Toggle_sidebar;
  run h "/switch";
  print_s [%sexp ((H.model h).sidebar_open : bool)];
  [%expect
    {|
    (Save_history (/switch "/switch /sessions/s2.jsonl"))
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
    (Save_history (/fork))
    (Rpc (method_ get_entries) (params ()) (tag (Entries Fork)))
    (Focus picker-input)
    Fork from (edit and resend)
    (Close (Esc))
    []
    first question #1
    second question #2 ✓
    ↑↓ move · Enter choose · Esc close
    |}];
  H.key h "ArrowUp" ~target:Field;
  H.key h "Enter" ~target:Field;
  print_s [%sexp ((H.model h).draft : string)];
  [%expect
    {|
    (Dialog_move -1)
    Dialog_accept
    (Focus editor)
    (Expire_toast (id 0) (after_ms 4000))
    (Rpc (method_ fork) (params ((at e1))) (tag Reload_state))
    "first question"
    |}];
  (* An empty session has nothing to fork. *)
  run h "/fork";
  H.reply h "get_entries" {|{"head":null,"entries":[]}|};
  toasts h;
  [%expect
    {|
    (Save_history (/fork))
    (Rpc (method_ get_entries) (params ()) (tag (Entries Fork)))
    (Expire_toast (id 1) (after_ms 4000))
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
    (Save_history (/rewind))
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
    (Expire_toast (id 0) (after_ms 4000))
    (Rpc (method_ rewind) (params ((to e1))) (tag Reload_messages))
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ get_messages) (params ()) (tag (Messages s1)))
    |}];
  (* Esc cancels without rewinding. *)
  run h "/rewind";
  H.reply h "get_entries" linear;
  H.key h "Enter" ~target:Field;
  H.key h "Escape" ~target:Page;
  [%expect
    {|
    (Save_history (/rewind))
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
    (Save_history (/tree))
    (Rpc (method_ get_entries) (params ((all true))) (tag (Entries Tree)))
    (Focus picker-input)
    <div role="listbox" class="picker-items">
      <div role="option" class="marked picker-item" @on_click>
        <span class="picker-label"> > first question </span>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        <span class="picker-check"> ✓ </span>
      </div>
      <div role="option" class="marked picker-item" @on_click>
        <span class="picker-label"> · first answer </span>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        <span class="picker-check"> ✓ </span>
      </div>
      <div role="option" class="picker-item" @on_click>
        <span class="picker-label">   > abandoned idea </span>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      </div>
      <div role="option" class="picker-item" @on_click>
        <span class="picker-label">   · abandoned answer </span>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      </div>
      <div role="option" class="marked picker-item" @on_click>
        <span class="picker-label">   > better idea </span>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        <span class="picker-check"> ✓ </span>
      </div>
      <div role="option" class="marked picker-item selected" @on_click>
        <span class="picker-label">   · better answer </span>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        <span class="picker-check"> ✓ </span>
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

let%expect_test "/cd: a prompt with directory completion, failures stay in it" =
  let h = H.create () in
  run h "/cd";
  H.reply h "list_dirs" {|["/work/src/","/work/test/"]|};
  H.text h ~selector:".modal";
  [%expect
    {|
    (Save_history (/cd))
    (Focus dialog-input)
    (Rpc (method_ list_dirs) (params ((prefix /work) (host backend)))
     (tag (Prompt_paths /work)))
    Change directory
    (Close (Esc))
    The session's working directory [/work]
    /work/src/
    /work/test/
    Tab completes · ↑↓ choose (Cancel) (Change)
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
    (Rpc (method_ set_cwd) (params ((path /nowhere)))
     (tag (Prompt_done "Working directory: /nowhere")))
    Change directory
    (Close (Esc))
    The session's working directory [/nowhere]
    no such directory: /nowhere
    Tab completes · ↑↓ choose (Cancel) (Change)
    |}];
  H.act h (Dialog_input "/work/src");
  H.key h "Enter" ~target:Field;
  H.reply h "set_cwd" "{}";
  H.text h ~selector:".modal";
  toasts h;
  [%expect
    {|
    (Rpc (method_ list_dirs) (params ((prefix /work/src) (host backend)))
     (tag (Prompt_paths /work/src)))
    Dialog_accept
    (Rpc (method_ set_cwd) (params ((path /work/src)))
     (tag (Prompt_done "Working directory: /work/src")))
    (Focus editor)
    (Expire_toast (id 0) (after_ms 4000))
    Working directory: /work/src
    |}];
  (* An empty answer says what to do. *)
  run h "/cd";
  H.act h (Dialog_input "");
  H.key h "Enter" ~target:Field;
  H.text h ~selector:".dialog-error";
  [%expect
    {|
    (Save_history (/cd))
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
  H.act h (Hello { client_id = "c7"; namespace = None; user = None });
  run h "/host";
  H.text h ~selector:".picker-items";
  [%expect
    {|
    (Save_history (/host))
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
    Tab completes · ↑↓ choose (Cancel) (Switch)
    |}];
  H.key h "Enter" ~target:Field;
  H.fail h "set_active_host" "laptop: /work: no such directory";
  H.text h ~selector:".dialog-error";
  H.act h (Dialog_input "/home/me/proj");
  H.key h "Enter" ~target:Field;
  H.reply h "set_active_host" "{}";
  toasts h;
  [%expect
    {|
    Dialog_accept
    (Rpc (method_ set_active_host) (params ((host c7) (cwd /work)))
     (tag (Prompt_done "Tools run on laptop (this browser), in /work")))
    laptop: /work: no such directory
    (Rpc (method_ list_dirs) (params ((prefix /home/me/proj) (host c7)))
     (tag (Prompt_paths /home/me/proj)))
    Dialog_accept
    (Rpc (method_ set_active_host) (params ((host c7) (cwd /home/me/proj)))
     (tag (Prompt_done "Tools run on laptop (this browser), in /home/me/proj")))
    (Focus editor)
    (Expire_toast (id 0) (after_ms 4000))
    Tools run on laptop (this browser), in /home/me/proj
    |}];
  run h "/host ci";
  print_s [%sexp ((H.model h).dialog : Dialog.t option)];
  run h "/host mainframe";
  toasts h;
  [%expect
    {|
    (Save_history ("/host ci" /host))
    (Focus dialog-input)
    (Rpc (method_ list_dirs) (params ((prefix /work) (host c9)))
     (tag (Prompt_paths /work)))
    ((Prompt
      ((action (Host_cwd (host c9) (name "ci (in nightly)"))) (input /work)
       (suggestions ()) (selected ()) (error ()) (busy false))))
    (Save_history ("/host mainframe" "/host ci" /host))
    Tools run on laptop (this browser), in /home/me/proj
    No tool host "mainframe": one of backend, laptop, ci (/host picks one).
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
    (Save_history (/export))
    (Focus dialog-input)
    (Rpc (method_ list_paths) (params ((prefix ""))) (tag (Prompt_paths "")))
    Dialog_accept
    (Rpc (method_ export) (params ((format markdown))) (tag Exported))
    (Focus editor)
    (Expire_toast (id 0) (after_ms 4000))
    (Rpc (method_ list_paths) (params ((prefix /tmp/s1.jsonl)))
     (tag (Paths /tmp/s1.jsonl)))
    (Save_history ("/export /tmp/s1.jsonl" /export))
    (Rpc (method_ export) (params ((format jsonl) (path /tmp/s1.jsonl)))
     (tag Exported))
    (Expire_toast (id 1) (after_ms 4000))
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
    (Save_history (/import "/export /tmp/s1.jsonl" /export))
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
    (Save_history
     ("/import /tmp/s1.jsonl" /import "/export /tmp/s1.jsonl" /export))
    (Rpc (method_ import) (params ((path /tmp/s1.jsonl))) (tag Imported))
    (Expire_toast (id 2) (after_ms 4000))
    (Rpc (method_ get_state) (params ()) (tag State))
    Exported to /home/me/.prigh/sessions/exports/s1.md
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
    (Save_history (/copy))
    Nothing to copy yet: no reply in this session.
    |}];
  with_messages h;
  run h "/copy";
  [%expect
    {|
    Follow_chat
    (Save_history (/copy))
    (Copy "Fixed: the **flag** was wrong.")
    (Expire_toast (id 1) (after_ms 4000))
    |}];
  H.key h "x" ~ctrl:true;
  [%expect
    {|
    Copy_last
    (Copy "Fixed: the **flag** was wrong.")
    (Expire_toast (id 2) (after_ms 4000))
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
    (Save_history (/btw))
    (Save_history ("/btw what does make check run?" /btw))
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
    Not added to the conversation.
    |}];
  (* A new question replaces the answer; Esc cancels and closes it. *)
  run h "/btw and lint?";
  H.key h "Escape";
  [%expect
    {|
    (Save_history ("/btw and lint?" "/btw what does make check run?" /btw))
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
    (Save_history
     ("/btw again?" "/btw and lint?" "/btw what does make check run?" /btw))
    (Rpc (method_ btw) (params ((question again?) (btw_id btw-3)))
     (tag (Btw btw-3)))
    btw again? (Close (Esc))
    Failed: the model is unavailable
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
    (Save_history ("/agents cancel 1"))
    No subagent or job 1: none has run in this session.
    (Save_history (/agents "/agents cancel 1"))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    Agents 2 agents running (Close (Alt+0))
    Subagents 2 running
    (1 read the tests 30s a1 · m · 1 turn thinking…) (2 lint 30s a2 · m · 1 turn thinking…) (3 ✓ count 10s a3 · m · 1 turn 42)
    |}];
  run h "/agents cancel 2";
  H.text h ~selector:".agents-panel";
  run h "/agents cancel a1";
  run h "/agents cancel a3";
  run h "/agents cancel 7";
  run h "/agents cancel";
  run h "/agents 2";
  H.text h ~selector:".toast";
  [%expect
    {|
    (Save_history ("/agents cancel 2" /agents "/agents cancel 1"))
    (Rpc (method_ get_subagent) (params ((id a2))) (tag (Subagent a2)))
    (Rpc (method_ cancel_subagent) (params ((agent_id a2))) (tag Show_error))
    (All agents (Esc)) Subagent a2 2 agents running (Close (Alt+0))
    lint
    running 30s a2 m 1 turn
    (Stopping…) (Show in chat)
    Loading the transcript…
    (Save_history
     ("/agents cancel a1" "/agents cancel 2" /agents "/agents cancel 1"))
    (Rpc (method_ get_subagent) (params ((id a1))) (tag (Subagent a1)))
    (Rpc (method_ cancel_subagent) (params ((agent_id a1))) (tag Show_error))
    (Save_history
     ("/agents cancel a3" "/agents cancel a1" "/agents cancel 2" /agents
      "/agents cancel 1"))
    (Rpc (method_ get_subagent) (params ((id a3))) (tag (Subagent a3)))
    (Save_history
     ("/agents cancel 7" "/agents cancel a3" "/agents cancel a1"
      "/agents cancel 2" /agents "/agents cancel 1"))
    (Save_history
     ("/agents cancel" "/agents cancel 7" "/agents cancel a3" "/agents cancel a1"
      "/agents cancel 2" /agents "/agents cancel 1"))
    (Save_history
     ("/agents 2" "/agents cancel" "/agents cancel 7" "/agents cancel a3"
      "/agents cancel a1" "/agents cancel 2" /agents "/agents cancel 1"))
    (Rpc (method_ get_subagent) (params ((id a2))) (tag (Subagent a2)))
    No subagent or job 1: none has run in this session.
    Subagent a3 has already finished.
    No subagent or job 7: give its number (1-3) or id; /agents lists them.
    Usage: /agents cancel <n|id> (/agents lists them)
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
    (Save_history ("/jobs j1"))
    No job j1: none has run in this session (!&command starts one).
    (Save_history (/jobs "/jobs j1"))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    Agents 1 job running (Close (Alt+0))
    Jobs 1 running
    (1 make test 1m 15s j1 · 120 B ok 12 tests)
    (2 ✓ sleep 1 1s j2 · exited 0)
    |}];
  run h "/jobs j2";
  H.reply h "job_output" {|{"text":""}|};
  H.text h ~selector:".agents-panel";
  run h "/jobs kill j1";
  H.text h ~selector:".agents-panel";
  [%expect
    {|
    (Save_history ("/jobs j2" /jobs "/jobs j1"))
    (Rpc (method_ job_output) (params ((job_id j2))) (tag (Job_output j2)))
    (All agents (Esc)) Job j2 1 job running (Close (Alt+0))
    ✓ sleep 1
    exited 0 1s j2
    No output yet.
    (Save_history ("/jobs kill j1" "/jobs j2" /jobs "/jobs j1"))
    (Rpc (method_ job_output) (params ((job_id j1))) (tag (Job_output j1)))
    (Rpc (method_ kill_job) (params ((job_id j1))) (tag Show_error))
    (All agents (Esc)) Job j1 1 job running (Close (Alt+0))
    make test
    running 1m 15s j1 120 B
    (Stopping…)
    Loading the output…
    Its output refreshes while it runs.
    |}];
  run h "/jobs kill j2";
  run h "/jobs j9";
  run h "/jobs kill";
  run h "/jobs a b";
  H.text h ~selector:".toast";
  [%expect
    {|
    (Save_history ("/jobs kill j2" "/jobs kill j1" "/jobs j2" /jobs "/jobs j1"))
    (Rpc (method_ job_output) (params ((job_id j2))) (tag (Job_output j2)))
    (Save_history
     ("/jobs j9" "/jobs kill j2" "/jobs kill j1" "/jobs j2" /jobs "/jobs j1"))
    (Save_history
     ("/jobs kill" "/jobs j9" "/jobs kill j2" "/jobs kill j1" "/jobs j2" /jobs
      "/jobs j1"))
    (Save_history
     ("/jobs a b" "/jobs kill" "/jobs j9" "/jobs kill j2" "/jobs kill j1"
      "/jobs j2" /jobs "/jobs j1"))
    No job j1: none has run in this session (!&command starts one).
    Job j2 has already finished.
    No job j9: give its id (j1, j2); /jobs lists them.
    Usage: /jobs [id | kill <id>] (/jobs lists them)
    |}]
;;

let%expect_test "/setusr: a superuser acts as another user, and back" =
  let h = H.create () in
  H.act
    h
    (Hello { client_id = "c1"; namespace = Some "alice"; user = Some "alice" });
  run h "/setusr";
  H.reply h "list_users" {|["alice","bob","carol"]|};
  H.text h ~selector:".picker-items";
  [%expect
    {|
    (Save_history (/setusr))
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
    (Expire_toast (id 0) (after_ms 4000))
    (Focus editor)
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    |}];
  H.reply h "get_state" (H.state_json ~fields:[ "session_id", `String "b1" ] ());
  H.text h ~selector:".sidebar-footer";
  H.text h ~selector:".status-item.account";
  [%expect
    {|
    (Set_url_session b1)
    (Rpc (method_ get_messages) (params ()) (tag (Messages b1)))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (A alice acting as bob) (Commands and keys (/help))
    (alice as bob)
    |}];
  run h "/setusr alice";
  H.reply h "set_user" {|{"client_id":"c1","namespace":"alice","user":"alice"}|};
  toasts h;
  [%expect
    {|
    (Save_history ("/setusr alice" /setusr))
    (Rpc (method_ set_user) (params ((user alice))) (tag User_switched))
    (Expire_toast (id 1) (after_ms 4000))
    (Focus editor)
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    Acting as bob
    Back to alice
    |}];
  run h "/setusr";
  H.fail h "list_users" "unauthorised: bob is not a superuser";
  toasts h;
  [%expect
    {|
    (Save_history (/setusr "/setusr alice" /setusr))
    (Rpc (method_ list_users) (params ()) (tag (Users Picker)))
    Acting as bob
    Back to alice
    unauthorised: bob is not a superuser
    |}]
;;

let%expect_test "/retry-backend-connection, /state, /clear, /quit" =
  let h = H.create () in
  run h "/retry-backend-connection";
  H.act h Backend_closed;
  run h "/retry-backend-connection";
  [%expect
    {|
    (Save_history (/retry-backend-connection))
    (Expire_toast (id 0) (after_ms 4000))
    (Reconnect (generation 1) (delay_ms 0) (session (s1)))
    (Save_history (/retry-backend-connection))
    (Expire_toast (id 1) (after_ms 4000))
    (Reconnect (generation 2) (delay_ms 0) (session (s1)))
    |}];
  (* The first attempt's late reply is stale. *)
  H.act h (Reply (Reconnect 1, Error "refused"));
  H.act h (Reply (Reconnect 2, Ok (Jsonaf.of_string {|{"client_id":"c2"}|})));
  [%expect
    {|
    (Expire_toast (id 2) (after_ms 4000))
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    |}];
  H.reply h "get_state" (H.state_json ());
  run h "/state";
  H.text h ~selector:".modal";
  [%expect
    {|
    (Set_url_session s1)
    (Rpc (method_ get_messages) (params ()) (tag (Messages s1)))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Save_history (/state /retry-backend-connection))
    (Focus dialog)
    Session state
    (Close (Esc))
    ((session_id s1) (session_path /sessions/s1.jsonl) (session_name ())
     (session_description ()) (cwd /work) (git_branch (main))
     (model
      ((id claude-opus-5-5) (provider anthropic) (key anthropic/claude-opus-5-5)
       (name "Claude Opus 5.5") (context_window 200000) (max_output 64000)
       (supports_thinking true) (cost ((input 5) (output 25) (cache_read 0.5)))))
     (thinking on) (running false) (message_count 0)
     (usage ((input 0) (output 0) (cache_read 0))) (cost_usd 0)
     (context_tokens 0) (active_host backend) (hosts ()) (subagents ())
     (jobs ()))
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
    Follow_chat
    (Save_history (/clear /state /retry-backend-connection))
    (Expire_toast (id 3) (after_ms 4000))
    What are we building?
    /work
    / commands
    @ mention a file
    ! run a command
    Ctrl+L switch model
    Ctrl+K find a session
    (Save_history (/quit /clear /state /retry-backend-connection))
    (Expire_toast (id 4) (after_ms 4000))
    Reconnecting now…
    Reconnected
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
    (Expire_toast (id 0) (after_ms 4000))
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
