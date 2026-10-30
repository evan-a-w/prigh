open! Core
open! Expect_test_helpers_core
open Prigh_ui
open Fixtures
module H = Test_app.H

(* The rows that must never move: the composer's rule, the editor and the
   status line, found in the plain screen. *)
module Fixed = struct
  type t =
    { rule : int
    ; editor : int
    ; status : int
    ; cursor : (int * int) option
    ; transcript_items : int
    }
  [@@deriving sexp_of, equal]

  let of_ (h : H.t) =
    let screen = Render.screen h.model in
    let rows = List.map screen.lines ~f:Content.Line.to_plain in
    let editor =
      Option.value_map
        (List.findi rows ~f:(fun _ row ->
           String.is_prefix row ~prefix:"> " || String.is_prefix row ~prefix:"? "))
        ~default:(-1)
        ~f:fst
    in
    { rule = editor - 1
    ; editor
    ; status = List.length rows - 1
    ; cursor = screen.cursor
    ; transcript_items = List.length (Transcript.items h.model.transcript)
    }
  ;;
end

(* Shows the screen before and after [f], then whether anything that must
   stay put moved. The status line's text may change: it shows state. *)
let no_shift ?(cursor_moves = false) (h : H.t) ~f =
  let before = Fixed.of_ h in
  H.show h;
  print_endline "--- after ---";
  f ();
  H.show h;
  let after = Fixed.of_ h in
  let same name a b = if not a then printf "MOVED: %s\n" name else ignore b in
  same "rule row" (before.rule = after.rule) ();
  same "editor row" (before.editor = after.editor) ();
  same "status row" (before.status = after.status) ();
  same
    "transcript (items appended)"
    (before.transcript_items = after.transcript_items)
    ();
  if not cursor_moves
  then
    same "cursor" ([%equal: (int * int) option] before.cursor after.cursor) ();
  print_endline "fixed rows unchanged"
;;

let%expect_test "no shift: opening the command autocomplete" =
  let h = Test_app.connected ~width:70 ~height:16 () in
  no_shift h ~cursor_moves:true ~f:(fun () -> H.key h (Key.plain Tab));
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    --- after ---
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ /help                              show commands and keys
      /hotkeys                           show keyboard shortcuts
      /model [name|id|provider/id]       pick or switch the model
      /scoped-models                     pick the models Ctrl+P cycles th…
      /change_default                    save the current model and think…
      /fallback [off|model...]           show or set the models that take…
      /default-dir [off|path]            show or set the directory new se…
      /login [provider] [api_key|oauth]  log in to a provider, or a git h…
      ↕ 1–8 of 70
    ──────────────────────────────────────────────────────────────────────
    > /▏
    deepseek-flash  ctx:0.1%/1.0M  Tab/Enter accept · Alt+S as typed
    fixed rows unchanged
    |}]
;;

let%expect_test "no shift: a confirmation dialog arrives" =
  let h = Test_app.connected ~width:70 ~height:16 () in
  H.keys h "half-typed";
  no_shift h ~f:(fun () ->
    H.event
      h
      (Tool_confirm
         { call_id = "c1"
         ; agent_id = None
         ; name = "bash"
         ; summary = "rm -rf _build"
         ; rules = [ "bash(rm:*)" ]
         }));
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────────────────────────────────────
    > half-typed▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    --- after ---


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ──────────────────────────────────────────────────────────┐
    │ Run bash: rm -rf _build?                                           │
    │ y  allow once                                                      │
    │ a  always allow bash(rm:*) this session                            │
    │ n  deny                                                            │
    │ r  deny, saying why                                                │
    └────────────────────────────────────────────────────────────────────┘
    ──────────────────────────────────────────────────────────────────────
    ? half-typed▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  confirm: y/a/n/r
    fixed rows unchanged
    |}];
  H.mode h;
  [%expect {| confirm |}]
;;

let%expect_test "no shift: a picker opens over the transcript" =
  let h = Test_app.connected ~width:70 ~height:16 () in
  (* The cursor goes to the picker's filter, where typing goes. *)
  no_shift h ~cursor_moves:true ~f:(fun () ->
    H.key h (Key.ctrl 'l');
    H.reply ~quiet:true h (Models_for_picker "") models_json);
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    --- after ---
    (Rpc (method_ list_models) (params ()) (tag (Models_for_picker "")))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Model  (4)
    / ▏
       Claude Fable 5       anthropic/claude-fable-5  ctx 1.0M  $10/$50 p…
       Claude Fable 5.1     anthropic/claude-fable-5-1  ctx 1.0M  $10/$50…
       GPT-5.5              openai/gpt-5.5  ctx 1.0M  $10/$50 per M  not …
    ▸* DeepSeek V4.1 Flash  deepseek/deepseek-flash ◆  ctx 1.0M  $10/$50 …
    ──────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  Enter selects · Esc closes · Ctrl+N logged-in only
    fixed rows unchanged
    |}];
  H.esc h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test
    "no shift: a command's acknowledgement goes to the rule and expires"
  =
  let h = Test_app.connected ~width:70 ~height:16 () in
  no_shift h ~f:(fun () -> H.key h (Key.ctrl 'o'));
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    --- after ---









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────── view: verbose — everything is shown ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    fixed rows unchanged
    |}];
  (* Eight seconds of ticks later it is gone (or at the next key press). *)
  for _ = 1 to 80 do
    H.step h Tick
  done;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "no shift: Ctrl+C once, and a usage error too long for the rule"
  =
  let h = Test_app.connected ~width:70 ~height:16 () in
  no_shift h ~f:(fun () -> H.key h (Key.ctrl 'c'));
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    --- after ---









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01  Ctrl+C again quits
    fixed rows unchanged
    |}];
  H.esc h;
  H.keys h "/notify sometimes";
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────── usage: /notify [off|bell|desktop] ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  (* Esc dismisses it at once. *)
  H.esc h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  (* So does the next key press. *)
  H.keys h "/notify sometimes";
  H.enter h;
  H.keys h "x";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────────────────────────────────────
    > x▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "no shift: queued messages while running" =
  let h = Test_app.connected ~width:110 ~height:16 () in
  H.event h (State (state ~running:true ()));
  H.event h (Message_start (user "go"));
  H.event
    h
    (Message_update { partial; delta = Text_delta "Working on it, step one" });
  no_shift h ~f:(fun () ->
    H.keys h "and then this";
    H.key h (Key.alt Enter);
    H.keys h "also that";
    H.enter h);
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ go
      Working on it, step one
    ──────────────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  ⠋ writing · Esc aborts · Enter steers
    --- after ---
    (Rpc (method_ follow_up) (params ((text "and then this"))) (tag Show_error))
    (Append_history "and then this")
    (Rpc
      (method_ push_pref)
      (params (
        (key   history)
        (value "and then this")
        (max   500)))
      (tag Ignore))
    (Rpc (method_ steer) (params ((text "also that"))) (tag Show_error))
    (Append_history "also that")
    (Rpc
      (method_ push_pref)
      (params (
        (key   history)
        (value "also that")
        (max   500)))
      (tag Ignore))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ go
      Working on it, step one
    queued (2) · Alt+Up: edit the last · /queue: edit or remove
      follow-up  and then this
      steer      also that
    ──────────────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  queued:2  ⠋ writing · Esc aborts · Enter steers
    fixed rows unchanged
    |}]
;;

let%expect_test "/help is a scrollable box; typing closes it into the editor" =
  let h = Test_app.connected ~width:70 ~height:16 () in
  H.keys h "/hotkeys";
  no_shift h ~f:(fun () -> H.enter h) ~cursor_moves:true;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ /hotkeys  show keyboard shortcuts
    ──────────────────────────────────────────────────────────────────────
    > /hotkeys▏
    deepseek-flash  ctx:0.1%/1.0M  Tab/Enter accept · Alt+S as typed
    --- after ---
    ┌─ Keys ─────────────────────────────────────────────────────────────┐
    │ Keys                                                               │
    │ Enter                   send the prompt (steer a focused subagent) │
    │ / accept the highlighted item                                      │
    │ Alt+Enter               queue a follow-up to run after the current │
    │ turn (a focused subagent's: before it reports)                     │
    │ Alt+S                   send exactly what is typed, ignoring the   │
    │ completion popup                                                   │
    │ Ctrl+J / Alt+J          insert a newline                           │
    │ Esc                     close the dialog, back to the main agent   │
    │ from a subagent, or abort the running turn                         │
    │ Tab                     complete a slash command, @path or         │
    └──────────────────── 1–11 of 89 · ↑↓ PgUp PgDn scroll · Esc closes ─┘
    ──────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ↑↓ scroll · Esc closes · typing goes to the editor
    fixed rows unchanged
    |}];
  H.key h (Key.plain Page_down);
  H.show h;
  [%expect
    {|
    ┌─ Keys ─────────────────────────────────────────────────────────────┐
    │ #snippet / open the command picker / the next field of the sign-in │
    │ and pull request dialogs                                           │
    │ Up                      move up (editor line, history, or list     │
    │ row)                                                               │
    │ Down                    move down (editor line, history, or list   │
    │ row)                                                               │
    │ Alt+Up                  pop the last queued steer/follow-up (the   │
    │ focused agent's) back into the editor                              │
    │ Left                    move the cursor left                       │
    │ Right                   move the cursor right                      │
    │ Alt+B / Ctrl+Left       move the cursor back one word              │
    └─────────────────── 12–22 of 89 · ↑↓ PgUp PgDn scroll · Esc closes ─┘
    ──────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ↑↓ scroll · Esc closes · typing goes to the editor
    |}];
  (* Esc closes it, nothing else changes. *)
  H.esc h;
  H.mode h;
  [%expect {| editing |}];
  (* So does Enter. *)
  H.keys h "/hotkeys";
  H.enter h;
  H.mode h;
  H.enter h;
  H.mode h;
  [%expect
    {|
    output
    editing
    |}];
  H.keys h "/hotkeys";
  H.enter h;
  H.keys h "/m";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ /mcp [reconnect]                 list MCP servers (Enter approves o…
      /mode [ask|auto-edit|auto|plan]  pick or set the mode: what asks fi…
      /model [name|id|provider/id]     pick or switch the model
      /mkdir [path]                    create a directory (and its parent…
      /scoped-models                   pick the models Ctrl+P cycles thro…
      /trim [after|undo|show|hide]     drop everything before a message f…
      /name [text]                     set the session name
      /remind [stop|every|rm|off ...]  stop [N] TEXT: remind the agent wh…
      ↕ 1–8 of 16
    ──────────────────────────────────────────────────────────────────────
    > /m▏
    deepseek-flash  ctx:0.1%/1.0M  Tab/Enter accept · Alt+S as typed
    |}]
;;

let%expect_test "status line: whole segments give way, the model never" =
  let h = Test_app.connected () in
  H.event
    h
    (State
       (state
          ~running:true
          ~subagents:[ "a1", "count files", true ]
          ~jobs:[ "j1", "make", None ]
          ()));
  List.iter [ 160; 120; 100; 80; 60; 40; 24; 12 ] ~f:(fun width ->
    H.step ~quiet:true h (Resize { width; height = 12 });
    printf "%3d |%s|\n" width (Content.Line.to_plain (Render.status h.model)));
  [%expect
    {|
    (Rpc (method_ get_subagent) (params ((id a1))) (tag Subagent_loaded))
    160 |/work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01  agents:[main] 1⠋  jobs:1  ⠋ working · Esc aborts · Enter steers|
    120 |deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  agents:[main] 1⠋  jobs:1  ⠋ working · Esc aborts · Enter steers|
    100 |deepseek-flash  ctx:0.1%/1.0M  agents:[main] 1⠋  jobs:1  ⠋ working · Esc aborts · Enter steers|
     80 |deepseek-flash  ctx:0.1%/1.0M  jobs:1  ⠋ working · Esc aborts · Enter steers|
     60 |deepseek-flash  $0.01  ⠋ working · Esc aborts · Enter steers|
     40 |deepseek-flash  ctx:0.1%/1.0M  jobs:1|
     24 |deepseek-flash  jobs:1|
     12 |deepseek-fl…|
    |}]
;;

let%expect_test
    "welcome line: home-relative cwd and session name; the server in the \
     status line"
  =
  let h = H.create ~width:80 ~height:8 () in
  H.step ~quiet:true h (Set_home "/home/me");
  H.step
    ~quiet:true
    h
    (Server_link { address = Some "box:7000"; user = None; saved = None });
  H.step ~quiet:true h Start;
  H.reply
    ~quiet:true
    h
    Initial_state
    (state_json
       ~cwd:"/home/me/src/prigh"
       ~git_branch:"main"
       ~session_name:"tui redesign"
       ());
  H.show h;
  [%expect
    {|
      prigh in ~/src/prigh "tui redesign" · /help · Esc aborts · Ctrl+C twice quits
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  server:box:7000
    |}];
  H.step ~quiet:true h Backend_closed;
  H.step ~quiet:true h (Resize { width = 80; height = 8 });
  H.show h;
  [%expect
    {|
      prigh in ~/src/prigh "tui redesign" · /help · Esc aborts · Ctrl+C twice quits
      backend connection lost; reconnecting (/retry-backend-connection to retry now,
      Ctrl+C quits)
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01  backend gone · retry 1 in 0s
    |}]
;;

let%expect_test "screen: 100x30 and 160x40" =
  List.iter
    [ 100, 30; 160, 40 ]
    ~f:(fun (width, height) ->
      let h = Test_app.connected ~width ~height () in
      H.event h (State (state ~running:true ()));
      H.event h (Message_start (user "list the files"));
      H.event
        h
        (Tool_start
           (tool_call ~name:"bash" ~arguments:{|{"command":"ls"}|} "t1"));
      H.event
        h
        (Tool_end
           { call = tool_call ~name:"bash" ~arguments:{|{"command":"ls"}|} "t1"
           ; result = tool_result ~id:"t1" ~name:"bash" "a.ml\nb.ml\n"
           });
      H.event
        h
        (Message_update
           { partial; delta = Text_delta "Two files: a.ml and b.ml." });
      H.show h;
      print_endline "";
      H.keys h "/mo";
      H.show h;
      print_endline "");
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ list the files
    ✓ bash $ ls
        a.ml
        b.ml
      Two files: a.ml and b.ml.
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  ⠋ writing · Esc aborts · Enter steers












      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ list the files
    ✓ bash $ ls
        a.ml
        b.ml
      Two files: a.ml and b.ml.
    ▸ /mode [ask|auto-edit|auto|plan]  pick or set the mode: what asks first, or read-only planning (Al…
      /model [name|id|provider/id]     pick or switch the model
      /scoped-models                   pick the models Ctrl+P cycles through
      /import [path]                   import a session from a JSONL file
      /compaction [at|model|mask ...]  show when conversations are compacted and what summarises them; …
      /permissions                     show the mode and the allow/ask/deny rules (config and this sess…
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > /mo▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  Tab/Enter accept · Alt+S as typed




























      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ list the files
    ✓ bash $ ls
        a.ml
        b.ml
      Two files: a.ml and b.ml.
    ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01  ⠋ writing · Esc aborts · Enter steers






















      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ list the files
    ✓ bash $ ls
        a.ml
        b.ml
      Two files: a.ml and b.ml.
    ▸ /mode [ask|auto-edit|auto|plan]  pick or set the mode: what asks first, or read-only planning (Alt+M cycles)
      /model [name|id|provider/id]     pick or switch the model
      /scoped-models                   pick the models Ctrl+P cycles through
      /import [path]                   import a session from a JSONL file
      /compaction [at|model|mask ...]  show when conversations are compacted and what summarises them; at TOKENS|off: also once the context exceeds TOKENS, not onl…
      /permissions                     show the mode and the allow/ask/deny rules (config and this session)
    ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    > /mo▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01  Tab/Enter accept · Alt+S as typed
    |}]
;;
