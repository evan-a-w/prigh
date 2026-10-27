open! Core
open! Expect_test_helpers_core
open Prigh_ui
module H = Test_app.H

(* One of each kind /filter knows: a message of yours, thinking, a reply, a
   tool call with its output, a subagent and a notice (the reply was
   aborted). *)
let messages_json =
  {|[{"role":"user","text":"fix the build"},
     {"role":"assistant","content":[{"type":"thinking","text":"the Makefile looks off"},{"type":"text","text":"Let me look."},{"type":"tool_call","id":"c1","name":"bash","arguments":"{\"command\":\"make\"}"},{"type":"tool_call","id":"c2","name":"subagent","arguments":"{\"task\":\"read the docs\"}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m"},
     {"role":"tool_result","tool_call_id":"c1","tool_name":"bash","text":"error: no rule","is_error":true},
     {"role":"tool_result","tool_call_id":"c2","tool_name":"subagent","text":"the docs say make all","is_error":false},
     {"role":"assistant","content":[{"type":"text","text":"Fixed it."}],"stop_reason":{"type":"aborted"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m"}]|}
;;

let connected () =
  let h = Test_app.connected ~width:80 ~height:24 () in
  H.reply ~quiet:true h Initial_messages messages_json;
  h
;;

let%expect_test
    "/filter opens a checklist; Space toggles, Enter applies, the status line \
     says what is hidden"
  =
  let h = connected () in
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ fix the build
    ┆ the Makefile looks off
      Let me look.
    ✕ bash $ make
        error: no rule
    ✓ subagent read the docs
        the docs say make all
      Fixed it.
      [aborted]
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  (* The first Enter completes the command, the second runs it bare. *)
  H.keys h "/filter";
  H.enter h;
  H.enter h;
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer

    ▌ fix the build
    ┆ the Makefile looks off
      Let me look.
    ✕ bash $ make
        error: no rule
    ✓ subagent read the docs
        the docs say make all
      Fixed it.
      [aborted]
    Show in the transcript  (6)
    / ▏
    ▸[x] Your messages  what you sent, and the skills you ran
     [x] Replies        the model's answers
     [x] Thinking       the model's reasoning
     [x] Tool calls     commands, edits, searches and their output
     [x] Subagents      the agents it started, with their work
     [x] Notices        errors, compactions, background reports
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Space toggles · Enter applies · Esc cancels
    |}];
  (* Thinking is the third row; tool calls the fourth. *)
  H.key h (Key.plain Down);
  H.key h (Key.plain Down);
  H.keys h " ";
  H.key h (Key.plain Down);
  H.keys h " ";
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer

    ▌ fix the build
    ┆ the Makefile looks off
      Let me look.
    ✕ bash $ make
        error: no rule
    ✓ subagent read the docs
        the docs say make all
      Fixed it.
      [aborted]
    Show in the transcript  (6)
    / ▏
     [x] Your messages  what you sent, and the skills you ran
     [x] Replies        the model's answers
     [ ] Thinking       the model's reasoning
    ▸[ ] Tool calls     commands, edits, searches and their output
     [x] Subagents      the agents it started, with their work
     [x] Notices        errors, compactions, background reports
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Space toggles · Enter applies · Esc cancels
    |}];
  H.enter h;
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ set_prefs)
      (params ((prefs ((tui.transcript-filter thinking,tools)))))
      (tag Ignore))










      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ fix the build
      Let me look.
    ✓ subagent read the docs
        the docs say make all
      Fixed it.
      [aborted]
    ──────────────────── filter: −thinking −tools — /filter all shows everything ───
    > ▏
    deepseek-flash  think:off  mode:auto  filter: −thinking −tools  ctx:0.1%/1.0M
    |}]
;;

let%expect_test "Esc leaves the filter as it was" =
  let h = connected () in
  (* The first Enter completes the command, the second runs it bare. *)
  H.keys h "/filter";
  H.enter h;
  H.enter h;
  H.keys h " ";
  H.esc h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ fix the build
    ┆ the Makefile looks off
      Let me look.
    ✕ bash $ make
        error: no rule
    ✓ subagent read the docs
        the docs say make all
      Fixed it.
      [aborted]
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test
    "/filter with kinds: only those, -KIND, +KIND, all, and unknown kinds"
  =
  let h = connected () in
  let run command =
    H.keys h command;
    H.enter h;
    H.show h
  in
  run "/filter user assistant";
  [%expect
    {|
    (Rpc
      (method_ set_prefs)
      (params ((prefs ((tui.transcript-filter thinking,tools,subagents,notes)))))
      (tag Ignore))















    ▌ earlier question
      earlier answer

    ▌ fix the build
      Let me look.
      Fixed it.
    ── filter: −thinking −tools −subagents −notes — /filter all shows everything ───
    > ▏
    deepseek-flash  filter: −thinking −tools −subagents −notes  ctx:0.1%/1.0M  $0.01
    |}];
  run "/filter +tools -user";
  [%expect
    {|
    (Rpc
      (method_ set_prefs)
      (params ((prefs ((tui.transcript-filter user,thinking,subagents,notes)))))
      (tag Ignore))
















      earlier answer
      Let me look.
    ✕ bash $ make
        error: no rule
      Fixed it.
    ─── filter: −user −thinking −subagents −notes — /filter all shows everything ───
    > ▏
    deepseek-flash  filter: −user −thinking −subagents −notes  ctx:0.1%/1.0M  $0.01
    |}];
  run "/filter +agents -notices";
  [%expect
    {|
    (Rpc
      (method_ set_prefs)
      (params ((prefs ((tui.transcript-filter user,thinking,notes)))))
      (tag Ignore))














      earlier answer
      Let me look.
    ✕ bash $ make
        error: no rule
    ✓ subagent read the docs
        the docs say make all
      Fixed it.
    ────────────── filter: −user −thinking −notes — /filter all shows everything ───
    > ▏
    deepseek-flash  think:off  filter: −user −thinking −notes  ctx:0.1%/1.0M  $0.01
    |}];
  H.esc h;
  run "/filter all";
  [%expect
    {|
    (Rpc
      (method_ set_prefs)
      (params ((prefs ((tui.transcript-filter null)))))
      (tag Ignore))







      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ fix the build
    ┆ the Makefile looks off
      Let me look.
    ✕ bash $ make
        error: no rule
    ✓ subagent read the docs
        the docs say make all
      Fixed it.
      [aborted]
    ─────────────────────────────────────────────────── filter: everything shown ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  (* Typing completes the kinds; a word nothing completes is an error that
     says what would work. *)
  run "/filter -thinkng";
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ fix the build
    ┆ the Makefile looks off
      Let me look.
    ✕ bash $ make
        error: no rule
    ✓ subagent read the docs
        the docs say make all
      Fixed it.
      [aborted]
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ Unknown kind “thinkng”. Did you mean thinking? Use all, or kinds (user,      │
    │ assistant, thinking, tools, subagents, notes) to show only those, -KIND to   │
    │ hide one, +KIND to show it.                                                  │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "each kind hidden on its own" =
  List.iter Transcript_filter.Kind.all ~f:(fun kind ->
    let h = connected () in
    print_endline ("-- " ^ Transcript_filter.Kind.name kind);
    H.keys h ("/filter -" ^ Transcript_filter.Kind.name kind);
    H.enter h;
    H.esc h;
    H.show h);
  [%expect
    {|
    -- user
    (Rpc
      (method_ set_prefs)
      (params ((prefs ((tui.transcript-filter user)))))
      (tag Ignore))











      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
      earlier answer
    ┆ the Makefile looks off
      Let me look.
    ✕ bash $ make
        error: no rule
    ✓ subagent read the docs
        the docs say make all
      Fixed it.
      [aborted]
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  filter: −user  ctx:0.1%/1.0M  $0.01
    -- assistant
    (Rpc
      (method_ set_prefs)
      (params ((prefs ((tui.transcript-filter assistant)))))
      (tag Ignore))










      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question

    ▌ fix the build
    ┆ the Makefile looks off
    ✕ bash $ make
        error: no rule
    ✓ subagent read the docs
        the docs say make all
      [aborted]
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  filter: −assistant  ctx:0.1%/1.0M  $0.01
    -- thinking
    (Rpc
      (method_ set_prefs)
      (params ((prefs ((tui.transcript-filter thinking)))))
      (tag Ignore))








      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ fix the build
      Let me look.
    ✕ bash $ make
        error: no rule
    ✓ subagent read the docs
        the docs say make all
      Fixed it.
      [aborted]
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  filter: −thinking  ctx:0.1%/1.0M  $0.01
    -- tools
    (Rpc
      (method_ set_prefs)
      (params ((prefs ((tui.transcript-filter tools)))))
      (tag Ignore))









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ fix the build
    ┆ the Makefile looks off
      Let me look.
    ✓ subagent read the docs
        the docs say make all
      Fixed it.
      [aborted]
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  filter: −tools  ctx:0.1%/1.0M  $0.01
    -- subagents
    (Rpc
      (method_ set_prefs)
      (params ((prefs ((tui.transcript-filter subagents)))))
      (tag Ignore))









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ fix the build
    ┆ the Makefile looks off
      Let me look.
    ✕ bash $ make
        error: no rule
      Fixed it.
      [aborted]
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  filter: −subagents  ctx:0.1%/1.0M  $0.01
    -- notes
    (Rpc
      (method_ set_prefs)
      (params ((prefs ((tui.transcript-filter notes)))))
      (tag Ignore))










    ▌ earlier question
      earlier answer

    ▌ fix the build
    ┆ the Makefile looks off
      Let me look.
    ✕ bash $ make
        error: no rule
    ✓ subagent read the docs
        the docs say make all
      Fixed it.
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  filter: −notes  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test
    "the backend's preference sets it, at startup and when another frontend \
     changes it"
  =
  let h = connected () in
  H.reply
    h
    Prefs_history
    {|{"prefs":{"history":[],"tui.transcript-filter":"thinking,tools"}}|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ fix the build
      Let me look.
    ✓ subagent read the docs
        the docs say make all
      Fixed it.
      [aborted]
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  filter: −thinking −tools  ctx:0.1%/1.0M
    |}];
  H.event h (Prefs_changed [ { key = "tui.transcript-filter"; value = None } ]);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ fix the build
    ┆ the Makefile looks off
      Let me look.
    ✕ bash $ make
        error: no rule
    ✓ subagent read the docs
        the docs say make all
      Fixed it.
      [aborted]
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test
    "your messages hidden: Ctrl+Up and Ctrl+Down still go from turn to turn, \
     to the first line shown of each"
  =
  let h = Test_app.connected ~width:60 ~height:9 () in
  H.reply
    ~quiet:true
    h
    Initial_messages
    {|[{"role":"user","text":"first question"},
       {"role":"assistant","content":[{"type":"text","text":"first answer\nwith\nmany\nlines"}],"stop_reason":{"type":"end_turn"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m"},
       {"role":"user","text":"second question"},
       {"role":"assistant","content":[{"type":"text","text":"second answer\nwith\nmany\nlines"}],"stop_reason":{"type":"end_turn"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m"},
       {"role":"user","text":"third question"},
       {"role":"assistant","content":[{"type":"text","text":"third answer\nwith\nmany\nlines"}],"stop_reason":{"type":"end_turn"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m"}]|};
  H.keys h "/filter -user";
  H.enter h;
  H.enter h;
  let up () = H.key h { (Key.plain Up) with ctrl = true } in
  let down () = H.key h { (Key.plain Down) with ctrl = true } in
  up ();
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ set_prefs)
      (params ((prefs ((tui.transcript-filter user)))))
      (tag Ignore))
      second answer
      with
      many
      lines
      third answer
      with                                        ↓ bottom · End
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  filter: −user  ctx:0.1%/1.0M
    |}];
  up ();
  H.show h;
  [%expect
    {|
      first answer
      with
      many
      lines
      second answer
      with                                        ↓ bottom · End
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  filter: −user  ctx:0.1%/1.0M
    |}];
  down ();
  H.show h;
  [%expect
    {|
      second answer
      with
      many
      lines
      third answer
      with                                        ↓ bottom · End
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  filter: −user  ctx:0.1%/1.0M
    |}]
;;

let%expect_test "parse: what /filter's arguments do from a filter" =
  let from = Transcript_filter.of_hidden [ Thinking ] in
  List.iter
    [ ""
    ; "all"
    ; "tools"
    ; "user assistant"
    ; "-tools"
    ; "+thinking"
    ; "−notes +thinking"
    ; "replies -thoughts"
    ; "all -agents"
    ; "thinkng"
    ; "zzz"
    ]
    ~f:(fun args ->
      printf
        "%-18s → %s\n"
        ("[" ^ args ^ "]")
        (match Transcript_filter.parse from args with
         | Ok filter ->
           Option.value (Transcript_filter.summary filter) ~default:"all"
         | Error e -> e));
  [%expect
    {|
    []                 → −thinking
    [all]              → all
    [tools]            → −user −assistant −thinking −subagents −notes
    [user assistant]   → −thinking −tools −subagents −notes
    [-tools]           → −thinking −tools
    [+thinking]        → all
    [−notes +thinking] → −notes
    [replies -thoughts] → −user −thinking −tools −subagents −notes
    [all -agents]      → −subagents
    [thinkng]          → Unknown kind “thinkng”. Did you mean thinking? Use all, or kinds (user, assistant, thinking, tools, subagents, notes) to show only those, -KIND to hide one, +KIND to show it.
    [zzz]              → Unknown kind “zzz”. Use all, or kinds (user, assistant, thinking, tools, subagents, notes) to show only those, -KIND to hide one, +KIND to show it.
    |}]
;;
