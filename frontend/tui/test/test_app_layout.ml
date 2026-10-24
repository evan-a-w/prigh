open! Core
open! Expect_test_helpers_core
open Prigh_ui
open Fixtures
module H = Test_app.H

(* The rows that must never move: the composer's rule, the editor and the
   status bar, found in the plain screen. *)
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
    let find prefix =
      Option.value_map
        (List.findi rows ~f:(fun _ row -> String.is_prefix row ~prefix))
        ~default:(-1)
        ~f:fst
    in
    let editor = find "> " in
    { rule = editor - 1
    ; editor
    ; status = List.length rows - 1
    ; cursor = screen.cursor
    ; transcript_items = List.length (Transcript.items h.model.transcript)
    }
  ;;
end

(* Shows the screen before and after [f], then whether anything that must
   stay put moved. The status bar's text may change: it shows state. *)
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
    prigh  new session                                               /work










    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ───
    > ▏
    deepseek-flash  think:off  mode:auto        ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    --- after ---
    prigh  new session                                               /work


    ╭─ Commands ─────────────────────────────────────────────────────────╮
    │ ▸ /help                              show commands and keys        │
    │   /hotkeys                           show keyboard shortcuts       │
    │   /model [name|id|provider/id]       pick or switch the model      │
    │   /scoped-models                     pick the models Ctrl+P cycle… │
    │   /change_default                    save the current model and t… │
    │   /fallback [off|model...]           show or set the models that … │
    │   /default-dir [off|path]            show or set the directory ne… │
    │   /login [provider] [api_key|oauth]  log in to a provider          │
    ╰──────────────────────────────────────────────────────── 1–8 of 61 ─╯
    ── Tab/Enter accept · Alt+S as typed ─────────────────────────────────
    > /▏
    deepseek-flash  think:off  mode:auto        ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                               /work










    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ───
    > half-typed▏
    deepseek-flash  think:off  mode:auto        ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    --- after ---
    prigh  new session                                               /work





    ╭─ Confirm ──────────────────────────────────────────────────────────╮
    │ Run bash: rm -rf _build?                                           │
    │ y  allow once                                                      │
    │ a  always allow bash(rm:*) this session                            │
    │ n  deny                                                            │
    │ r  deny, saying why                                                │
    ╰────────────────────────────────────────────────────────────────────╯
    ── confirm: y/a/n/r ──────────────────────────────────────────────────
    > half-typed▏
    deepseek-flash  think:off  mode:auto        ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                               /work










    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ───
    > ▏
    deepseek-flash  think:off  mode:auto        ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    --- after ---
    (Rpc (method_ list_models) (params ()) (tag (Models_for_picker "")))
    prigh  new session                                               /work





    ╭─ Model (4) ────────────────────────────────────────────────────────╮
    │ / ▏                                                                │
    │    Claude Fable 5       anthropic/claude-fable-5  ctx 1.0M  $10/$… │
    │    Claude Fable 5.1     anthropic/claude-fable-5-1  ctx 1.0M  $10… │
    │    GPT-5.5              openai/gpt-5.5  ctx 1.0M  $10/$50 per M  … │
    │ ▸* DeepSeek V4.1 Flash  deepseek/deepseek-flash ◆  ctx 1.0M  $10/… │
    ╰────────────────────────────────────────────────────────────────────╯
    ── Enter selects · Esc closes · Ctrl+N logged-in only ────────────────
    >
    deepseek-flash  think:off  mode:auto        ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    fixed rows unchanged
    |}];
  H.esc h;
  H.show h;
  [%expect
    {|
    prigh  new session                                               /work










    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ───
    > ▏
    deepseek-flash  think:off  mode:auto        ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}]
;;

let%expect_test
    "no shift: a command's acknowledgement goes to the rule and expires"
  =
  let h = Test_app.connected ~width:70 ~height:16 () in
  no_shift h ~f:(fun () -> H.key h (Key.ctrl 'o'));
  [%expect
    {|
    prigh  new session                                               /work










    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ───
    > ▏
    deepseek-flash  think:off  mode:auto        ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    --- after ---
    prigh  new session                                               /work










    ▌ earlier question
      earlier answer
    ── /help · Tab commands ─────── view: verbose — everything is shown ──
    > ▏
    deepseek-flash  think:off  mode:auto        ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    fixed rows unchanged
    |}];
  (* Eight seconds of ticks later it is gone (or at the next key press). *)
  for _ = 1 to 80 do
    H.step h Tick
  done;
  H.show h;
  [%expect
    {|
    prigh  new session                                               /work










    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ───
    > ▏
    deepseek-flash  think:off  mode:auto        ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "no shift: Ctrl+C once, and a usage error too long for the rule"
  =
  let h = Test_app.connected ~width:70 ~height:16 () in
  no_shift h ~f:(fun () -> H.key h (Key.ctrl 'c'));
  [%expect
    {|
    prigh  new session                                               /work










    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ───
    > ▏
    deepseek-flash  think:off  mode:auto        ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    --- after ---
    prigh  new session                                               /work










    ▌ earlier question
      earlier answer
    ── Ctrl+C again quits ────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto        ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    fixed rows unchanged
    |}];
  H.esc h;
  H.keys h "/notify sometimes";
  H.enter h;
  H.show h;
  [%expect
    {|
    prigh  new session                                               /work










    ▌ earlier question
      earlier answer
    ── /help · Tab commands ───────── usage: /notify [off|bell|desktop] ──
    > ▏
    deepseek-flash  think:off  mode:auto        ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  (* Esc dismisses it at once. *)
  H.esc h;
  H.show h;
  [%expect
    {|
    prigh  new session                                               /work










    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ───
    > ▏
    deepseek-flash  think:off  mode:auto        ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  (* So does the next key press. *)
  H.keys h "/notify sometimes";
  H.enter h;
  H.keys h "x";
  H.show h;
  [%expect
    {|
    prigh  new session                                               /work










    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ───
    > x▏
    deepseek-flash  think:off  mode:auto        ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                                                                       /work







    ▌ earlier question
      earlier answer

    ▌ go
      Working on it, step one
    ── ⠋ writing · Esc aborts · Enter steers ─────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  view:normal                                   ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    --- after ---
    (Rpc (method_ follow_up) (params ((text "and then this"))) (tag Show_error))
    (Append_history "and then this")
    (Rpc (method_ steer) (params ((text "also that"))) (tag Show_error))
    (Append_history "also that")
    prigh  new session                                                                                       /work







    ▌ earlier question
      earlier answer                                       ╭─ Queued (2) ────────────────────────────────────────╮
                                                           │ follow-up  and then this                            │
    ▌ go                                                   │ steer      also that                                │
      Working on it, step one                              ╰─── Alt+Up edits the last · /queue edits or removes ─╯
    ── ⠋ writing · Esc aborts · Enter steers ─────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  view:normal                         queued:2  ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    fixed rows unchanged
    |}]
;;

let%expect_test "/help is a scrollable box; typing closes it into the editor" =
  let h = Test_app.connected ~width:70 ~height:16 () in
  H.keys h "/hotkeys";
  no_shift h ~f:(fun () -> H.enter h) ~cursor_moves:true;
  [%expect
    {|
    prigh  new session                                               /work









    ╭─ Commands ─────────────────────────────────────────────────────────╮
    │ ▸ /hotkeys  show keyboard shortcuts                                │
    ╰────────────────────────────────────────────────────────────────────╯
    ── Tab/Enter accept · Alt+S as typed ─────────────────────────────────
    > /hotkeys▏
    deepseek-flash  think:off  mode:auto        ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    --- after ---
    prigh  new session                                               /work
    ╭─ Keys ─────────────────────────────────────────────────────────────╮
    │ Keys                                                               │
    │ Enter                   send the prompt / accept the highlighted   │
    │ item                                                               │
    │ Alt+Enter               queue a follow-up to run after the current │
    │ turn                                                               │
    │ Alt+S                   send exactly what is typed, ignoring the   │
    │ completion popup                                                   │
    │ Ctrl+J / Alt+J          insert a newline                           │
    │ Esc                     close the dialog, or abort the running     │
    │ turn                                                               │
    ╰──────────────────── 1–10 of 79 · ↑↓ PgUp PgDn scroll · Esc closes ─╯
    ── ↑↓ scroll · Esc closes · typing goes to the editor ────────────────
    > ▏
    deepseek-flash  think:off  mode:auto        ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    fixed rows unchanged
    |}];
  H.key h (Key.plain Page_down);
  H.show h;
  [%expect
    {|
    prigh  new session                                               /work
    ╭─ Keys ─────────────────────────────────────────────────────────────╮
    │ Tab                     complete a slash command, @path or         │
    │ #snippet / open the command picker / the next field of the sign-in │
    │ dialog                                                             │
    │ Up                      move up (editor line, history, or list     │
    │ row)                                                               │
    │ Down                    move down (editor line, history, or list   │
    │ row)                                                               │
    │ Alt+Up                  pop the last queued steer/follow-up back   │
    │ into the editor                                                    │
    │ Left                    move the cursor left                       │
    ╰─────────────────── 11–20 of 79 · ↑↓ PgUp PgDn scroll · Esc closes ─╯
    ── ↑↓ scroll · Esc closes · typing goes to the editor ────────────────
    > ▏
    deepseek-flash  think:off  mode:auto        ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                               /work


    ╭─ Commands ─────────────────────────────────────────────────────────╮
    │ ▸ /mcp [reconnect]                 list MCP servers (Enter approv… │
    │   /mode [ask|auto-edit|auto|plan]  pick or set the mode: what ask… │
    │   /model [name|id|provider/id]     pick or switch the model        │
    │   /mkdir [path]                    create a directory (and its pa… │
    │   /scoped-models                   pick the models Ctrl+P cycles … │
    │   /name [text]                     set the session name            │
    │   /remind [stop|every|rm|off ...]  stop [N] TEXT: remind the agen… │
    │   /resume [now|cancel]             show, run now or cancel the au… │
    ╰──────────────────────────────────────────────────────── 1–8 of 12 ─╯
    ── Tab/Enter accept · Alt+S as typed ─────────────────────────────────
    > /m▏
    deepseek-flash  think:off  mode:auto        ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "status bar: whole segments give way, the model never" =
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
    160 |deepseek-flash  think:off  mode:auto  view:normal                                                           agents:1 running  jobs:1  ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01|
    120 |deepseek-flash  think:off  mode:auto  view:normal                   agents:1 running  jobs:1  ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01|
    100 |deepseek-flash  think:off  mode:auto            agents:1 running  jobs:1  ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01|
     80 |deepseek-flash  mode:auto   agents:1 running  jobs:1  ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01|
     60 |deepseek-flash         agents:1 running  ctx ▱▱▱▱▱ 0.1%/1.0M|
     40 |deepseek-flash       ctx ▱▱▱▱▱ 0.1%/1.0M|
     24 |deepseek-flash|
     12 |deepseek-fl…|
    |}]
;;

let%expect_test "header: session title, home-relative cwd, the server" =
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
    prigh  tui redesign                                ~/src/prigh (main) · box:7000




    ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────
    > ▏
    deepseek-flash  think:off  mode:auto  view:normal     ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  List.iter [ 50; 30 ] ~f:(fun width ->
    H.step ~quiet:true h (Resize { width; height = 8 });
    print_endline
      (List.hd_exn
         (String.split_lines (Screen.to_plain (Render.screen h.model)))));
  [%expect
    {|
    prigh  tui redesign  ~/src/prigh (main) · box:7000
    prigh  tui redesign
    |}];
  H.step ~quiet:true h Backend_closed;
  H.step ~quiet:true h (Resize { width = 80; height = 8 });
  H.show h;
  [%expect
    {|
    prigh  tui redesign                            ~/src/prigh (main) · reconnecting


      backend connection lost; reconnecting (/retry-backend-connection to retry now,
      Ctrl+C quits)
    ── backend gone · retry 1 in 0s ────────────────────────────────────────────────
    > ▏
    reconnecting  deepseek-flash  think:off  mode:auto    ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                                                             /work


















    ▌ earlier question
      earlier answer

    ▌ list the files
    ✓ bash $ ls
        a.ml
        b.ml
      Two files: a.ml and b.ml.
    ── ⠋ writing · Esc aborts · Enter steers ───────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  view:normal                         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01

    prigh  new session                                                                             /work


















    ▌ earlier question
    ╭─ Commands ───────────────────────────────────────────────────────────────────────────────────────╮
    │ ▸ /mode [ask|auto-edit|auto|plan]  pick or set the mode: what asks first, or read-only planning… │
    │   /model [name|id|provider/id]     pick or switch the model                                      │
    │   /scoped-models                   pick the models Ctrl+P cycles through                         │
    │   /import [path]                   import a session from a JSONL file                            │
    │   /permissions                     show the mode and the allow/ask/deny rules (config and this … │
    ╰──────────────────────────────────────────────────────────────────────────────────────────────────╯
    ── Tab/Enter accept · Alt+S as typed ───────────────────────────────────────────────────────────────
    > /mo▏
    deepseek-flash  think:off  mode:auto  view:normal                         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01

    prigh  new session                                                                                                                                         /work
     Agents                     │
    ▌● main              running│
       writing                  │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │ ▌ earlier question
                                │   earlier answer
                                │
                                │ ▌ list the files
                                │ ✓ bash $ ls
                                │     a.ml
                                │     b.ml
     Alt+L hides · ⇧Tab focuses │   Two files: a.ml and b.ml.
    ── ⠋ writing · Esc aborts · Enter steers ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  view:normal                                                                                     ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01

    prigh  new session                                                                                                                                         /work
     Agents                     │
    ▌● main              running│
       writing                  │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │
                                │ ▌ earlier question
                                │ ╭─ Commands ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────╮
                                │ │ ▸ /mode [ask|auto-edit|auto|plan]  pick or set the mode: what asks first, or read-only planning (Alt+M cycles)                 │
                                │ │   /model [name|id|provider/id]     pick or switch the model                                                                    │
                                │ │   /scoped-models                   pick the models Ctrl+P cycles through                                                       │
                                │ │   /import [path]                   import a session from a JSONL file                                                          │
                                │ │   /permissions                     show the mode and the allow/ask/deny rules (config and this session)                        │
     Alt+L hides · ⇧Tab focuses │ ╰────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────╯
    ── Tab/Enter accept · Alt+S as typed ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    > /mo▏
    deepseek-flash  think:off  mode:auto  view:normal                                                                                     ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}]
;;
