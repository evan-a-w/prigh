open! Core
open! Expect_test_helpers_core
open Prigh_ui
open Fixtures
module H = Test_app.H
module P = Prigh_protocol

let host_note =
  "[Environment: the tool host is now laptop, so tools run there and its \
   filesystem may differ from the one described above. Re-reading \
   AGENTS.md/CLAUDE.md there is at your discretion.]"
;;

let cwd_note =
  "[Environment: the working directory is now /work/sub. Project instructions \
   for it may differ from the ones above.]"
;;

(* What the backend sends: the text the model saw, and how it is made up. *)
let noted_json =
  sprintf
    {|{"role":"user","text":%s,"origin":{"body":"fix the build","notes":[{"kind":"host","title":"Tools now run on laptop","text":%s},{"kind":"cwd","title":"Working directory is now /work/sub","text":%s}],"attachments":[{"path":"Makefile"}]}}|}
    (P.Json.to_string
       (P.Json.str
          (host_note
           ^ "\n"
           ^ cwd_note
           ^ "\n\nfix the build\n\n<file path=\"Makefile\">\nall:\n</file>")))
    (P.Json.to_string (P.Json.str host_note))
    (P.Json.to_string (P.Json.str cwd_note))
;;

let source_json ~kind ~title text =
  sprintf
    {|{"role":"user","text":%s,"origin":{"source":{"kind":"%s","title":"%s"}}}|}
    (P.Json.to_string (P.Json.str text))
    kind
    title
;;

let goal_json =
  source_json
    ~kind:"goal_continuation"
    ~title:"Goal: continuing (3/20)"
    "[prigh goal: continuation 3 of 20] Continue working toward the goal:\n\n\
     <goal>\n\
     all tests pass\n\
     </goal>"
;;

let reminder_json =
  source_json
    ~kind:"reminder"
    ~title:"Reminder: keep-going (1/5)"
    "[prigh reminder: keep-going, as you ended your turn (1 of 5)] Don't stop \
     until the tests pass."
;;

let review_json =
  source_json
    ~kind:"review"
    ~title:"Review of uncommitted changes"
    "[prigh review] Review the uncommitted changes in /work and give \
     prioritised findings."
;;

let summary_json =
  source_json
    ~kind:"summary"
    ~title:"Summary of the earlier conversation"
    "Summary of the conversation so far:\nwe fixed the parser"
;;

let reply text =
  sprintf
    {|{"role":"assistant","content":[{"type":"text","text":%s}],"stop_reason":{"type":"end_turn"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m"}|}
    (P.Json.to_string (P.Json.str text))
;;

let history =
  sprintf
    "[%s]"
    (String.concat
       ~sep:","
       [ summary_json
       ; noted_json
       ; reply "done"
       ; goal_json
       ; reply "still going"
       ; reminder_json
       ; reply "carrying on"
       ; review_json
       ; reply "**[P1] Fix the count** `a.ml:2`"
       ])
;;

let%expect_test
    "notes, attachments and the harness's messages: quiet lines, not the \
     user's words; verbose shows what the model saw"
  =
  let h = Test_app.connected ~width:72 ~height:35 () in
  H.reply ~quiet:true h Initial_messages history;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      context compacted

    ⇄ Tools now run on laptop
    ⌂ Working directory is now /work/sub
    ▌ fix the build
    ▌ @Makefile  attached
      done

    ◷ Goal: continuing (3/20)
      still going

    ※ Reminder: keep-going (1/5)
      carrying on

    ⚑ Review of uncommitted changes
      [P1] Fix the count a.ml:2
    ────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.keys h "/verbosity verbose";
  H.enter h;
  H.show h;
  [%expect
    {|
      earlier answer
      context compacted
      we fixed the parser

    ⇄ Tools now run on laptop
      [Environment: the tool host is now laptop, so tools run there and its
      filesystem may differ from the one described above. Re-reading
      AGENTS.md/CLAUDE.md there is at your discretion.]
    ⌂ Working directory is now /work/sub
      [Environment: the working directory is now /work/sub. Project
      instructions for it may differ from the ones above.]
    ▌ fix the build
    ▌ @Makefile  attached
      done

    ◷ Goal: continuing (3/20)
      [prigh goal: continuation 3 of 20] Continue working toward the goal:

      <goal>
      all tests pass
      </goal>
      still going

    ※ Reminder: keep-going (1/5)
      [prigh reminder: keep-going, as you ended your turn (1 of 5)] Don't
      stop until the tests pass.
      carrying on

    ⚑ Review of uncommitted changes
      [prigh review] Review the uncommitted changes in /work and give
      prioritised findings.
      [P1] Fix the count a.ml:2
    ──────────────────────────────── view: verbose — everything is shown ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "a note arriving live, before the user's text" =
  let h = Test_app.connected ~width:72 ~height:12 () in
  H.event
    h
    (Message_start
       (P.Message.of_json (P.Json.parse noted_json |> Or_error.ok_exn)
        |> Or_error.ok_exn));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ⇄ Tools now run on laptop
    ⌂ Working directory is now /work/sub
    ▌ fix the build
    ▌ @Makefile  attached
    ────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test
    "/fork lists the harness's messages by their notes and restores only what \
     the user typed"
  =
  let h = Test_app.connected ~width:72 ~height:12 () in
  H.keys h "/fork";
  H.enter h;
  H.reply
    h
    Entries_for_fork
    (sprintf
       {|{"head":"u3","entries":[{"id":"u1","parent":null,"kind":"message","message":%s},{"id":"u2","parent":"u1","kind":"message","message":%s},{"id":"u3","parent":"u2","kind":"message","message":%s}]}|}
       noted_json
       goal_json
       review_json);
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_entries) (params ()) (tag Entries_for_fork))
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Fork at  (3)
    / ▏
       fix the build                    #1
       ◷ Goal: continuing (3/20)        #2
    ▸* ⚑ Review of uncommitted changes  #3
    ────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  H.step ~quiet:true h (Key (Key.plain Up));
  H.step ~quiet:true h (Key (Key.plain Up));
  H.enter h;
  H.show h;
  [%expect
    {|
    (Rpc (method_ fork) (params ((at u1))) (tag Reload_messages))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────────────────
    > fix the build▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let plan_note ~on =
  if on
  then "[Plan mode is on: you are planning, not implementing.]"
  else "[Plan mode is off: you can change files again.]"
;;

let plan_note_json ~on =
  sprintf
    {|{"kind":"plan_mode","title":"%s","text":%s}|}
    (if on
     then "Plan mode on: the model was told to plan, not change files"
     else "Plan mode off: the model was told it can change files")
    (P.Json.to_string (P.Json.str (plan_note ~on)))
;;

(* Plan mode noted with a prompt, alone between turns, and before a goal's
   message, as the backend records them. *)
let plan_history =
  sprintf
    "[%s]"
    (String.concat
       ~sep:","
       [ sprintf
           {|{"role":"user","text":%s,"origin":{"body":"look around","notes":[%s]}}|}
           (P.Json.to_string
              (P.Json.str (plan_note ~on:true ^ "\n\nlook around")))
           (plan_note_json ~on:true)
       ; reply "looked"
       ; sprintf
           {|{"role":"user","text":%s,"origin":{"body":"","notes":[%s]}}|}
           (P.Json.to_string (P.Json.str (plan_note ~on:false)))
           (plan_note_json ~on:false)
       ; reply "editing"
       ; sprintf
           {|{"role":"user","text":%s,"origin":{"source":{"kind":"goal","title":"Goal: ship it"},"notes":[%s]}}|}
           (P.Json.to_string
              (P.Json.str (plan_note ~on:true ^ "\n\n[prigh goal] ship it")))
           (plan_note_json ~on:true)
       ; reply "planning the goal"
       ])
;;

let%expect_test "plan mode notes: before a prompt, alone, before a goal" =
  let h = Test_app.connected ~width:72 ~height:20 () in
  H.reply ~quiet:true h Initial_messages plan_history;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ◇ Plan mode on: the model was told to plan, not change files
    ▌ look around
      looked

    ◇ Plan mode off: the model was told it can change files
      editing

    ◇ Plan mode on: the model was told to plan, not change files
    ◎ Goal: ship it
      planning the goal
    ────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.keys h "/fork";
  H.enter h;
  H.reply
    h
    Entries_for_fork
    (sprintf
       {|{"head":"u2","entries":[{"id":"u1","parent":null,"kind":"message","message":%s},{"id":"u2","parent":"u1","kind":"message","message":%s}]}|}
       (sprintf
          {|{"role":"user","text":"x","origin":{"body":"look around","notes":[%s]}}|}
          (plan_note_json ~on:true))
       (sprintf
          {|{"role":"user","text":"x","origin":{"body":"","notes":[%s]}}|}
          (plan_note_json ~on:false)));
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_entries) (params ()) (tag Entries_for_fork))
    ▌ earlier question
      earlier answer

    ◇ Plan mode on: the model was told to plan, not change files
    ▌ look around
      looked

    ◇ Plan mode off: the model was told it can change files
      editing

    ◇ Plan mode on: the model was told to plan, not change files
    ◎ Goal: ship it
      planning the goal
    Fork at  (2)
    / ▏
       look around                           #1
    ▸* ◇ Plan mode off: the model was told it can change files  #2
    ────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}]
;;

let note_json kind title =
  sprintf
    {|{"kind":"%s","title":"%s","text":%s}|}
    kind
    title
    (P.Json.to_string (P.Json.str ("[" ^ title ^ "]")))
;;

(* Notes alone between turns (two messages in a row), a hand-over and a
   resume carrying notes, then a prompt with notes: each group starts with
   one blank line, and its lines follow each other without gaps. *)
let%expect_test "every kind of note: groups read as one block" =
  let h = Test_app.connected ~width:72 ~height:28 () in
  let notes_only notes =
    sprintf
      {|{"role":"user","text":"x","origin":{"body":"","notes":[%s]}}|}
      (String.concat ~sep:"," notes)
  in
  let with_source ~kind ~title notes =
    sprintf
      {|{"role":"user","text":"x","origin":{"source":{"kind":"%s","title":"%s"},"notes":[%s]}}|}
      kind
      title
      (String.concat ~sep:"," notes)
  in
  let history =
    sprintf
      "[%s]"
      (String.concat
         ~sep:","
         [ notes_only
             [ note_json "host" "Tools now run on laptop"
             ; note_json "cwd" "Working directory is now /work/sub"
             ]
         ; notes_only [ note_json "tools" "Tools changed: web_search removed" ]
         ; with_source
             ~kind:"handover"
             ~title:"Hand-over from deepseek-flash"
             [ note_json "plan_mode" "Plan mode off" ]
         ; reply "taking over"
         ; with_source
             ~kind:"resume"
             ~title:"Resumed after the usage limit"
             [ note_json "shell" "Shell output: make" ]
         ; reply "resumed"
         ; sprintf
             {|{"role":"user","text":"x","origin":{"body":"go on","notes":[%s,%s]}}|}
             (note_json "plan_mode" "Plan mode on")
             (note_json "goal_continuation" "Goal: continuing (2/20)")
         ; reply "going"
         ])
  in
  H.reply ~quiet:true h Initial_messages history;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ⇄ Tools now run on laptop
    ⌂ Working directory is now /work/sub
    ± Tools changed: web_search removed
    ◇ Plan mode off
    ↪ Hand-over from deepseek-flash
      taking over

    $ Shell output: make
    ↻ Resumed after the usage limit
      resumed

    ◇ Plan mode on
    ◷ Goal: continuing (2/20)
    ▌ go on
      going
    ────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let diff_json =
  {|{"root":"/work","against":"HEAD","truncated":false,"diff":"diff --git a/src/a.ml b/src/a.ml\nindex 1..2 100644\n--- a/src/a.ml\n+++ b/src/a.ml\n@@ -1,2 +1,2 @@\n let x = 1\n-let y = 2\n+let y = 3\ndiff --git a/new file.txt b/new file.txt\nnew file mode 100644\nindex 0..1\n--- /dev/null\n+++ b/new file.txt\n@@ -0,0 +1 @@\n+hello\n"}|}
;;

let%expect_test "/diff: the changes in colour, a path, none, not a repository" =
  let h = Test_app.connected ~width:72 ~height:16 () in
  (* The first Enter takes the command and offers paths; the second runs it. *)
  H.keys h "/diff";
  H.step ~quiet:true h (Key (Key.plain Enter));
  H.enter h;
  H.reply h Git_diff diff_json;
  H.show h;
  [%expect
    {|
    (Rpc (method_ git_diff) (params ()) (tag Git_diff))
    ▌ earlier question
      earlier answer
    ┌─ Changes ────────────────────────────────────────────────────────────┐
    │ diff /work against HEAD  2 files changed, +2 −1                      │
    │ src/a.ml  +1 −1                                                      │
    │   @@ -1,2 +1,2 @@                                                    │
    │    let x = 1                                                         │
    │   -let y = 2                                                         │
    │   +let y = 3                                                         │
    │ new file.txt  new  +1                                                │
    │   @@ -0,0 +1 @@                                                      │
    │   +hello                                                             │
    └───────────────────────────────────────────────────────── Esc closes ─┘
    ─────────────────────────────────────────────── reading the changes… ───
    > ▏
    deepseek-flash  ↑↓ scroll · Esc closes · typing goes to the editor
    |}];
  print_endline
    (Content.to_styled
       (Diff_files.render
          (P.Git_diff.of_json (P.Json.parse diff_json |> Or_error.ok_exn)
           |> Or_error.ok_exn)));
  [%expect
    {|
    [bold]diff [/]/work[dim] against HEAD[/][gray]  2 files changed, +2 −1[/]
    [bold]src/a.ml[/][dim]  +1 −1[/]
    [cyan]  @@ -1,2 +1,2 @@[/]
    [gray]   let x = 1[/]
    [red]  -let y = 2[/]
    [green]  +let y = 3[/]
    [bold]new file.txt[/][dim]  new  +1[/]
    [cyan]  @@ -0,0 +1 @@[/]
    [green]  +hello[/]
    |}];
  H.keys h "/diff src";
  H.enter h;
  H.reply
    h
    Git_diff
    {|{"root":"/work","against":"4b825dc642cb6eb9a060e54bf8d69288fbee4904","truncated":false,"diff":""}|};
  H.keys h "/diff";
  H.step ~quiet:true h (Key (Key.plain Enter));
  H.enter h;
  H.reply_error
    h
    Git_diff
    "not a git repository: /work (git init starts one, or /cd into one)";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ list_paths)
      (params ((prefix "")))
      (tag (Paths_for_autocomplete "")))
    (Rpc
      (method_ list_paths)
      (params ((prefix s)))
      (tag (Paths_for_autocomplete s)))
    (Rpc
      (method_ list_paths)
      (params ((prefix sr)))
      (tag (Paths_for_autocomplete sr)))
    (Rpc
      (method_ list_paths)
      (params ((prefix src)))
      (tag (Paths_for_autocomplete src)))
    (Rpc (method_ git_diff) (params ((path src))) (tag Git_diff))
    (Rpc (method_ git_diff) (params ()) (tag Git_diff))









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ─ not a git repository: /work (git init starts one, or /cd into one) ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test
    "/review and /init: sent to the backend; refusals go back to the editor; a \
     busy agent queues them"
  =
  let h = Test_app.connected ~width:72 ~height:10 () in
  H.keys h "/review main look at errors";
  H.enter h;
  H.reply_error
    h
    (Retry_in_editor
       { command = "/review main look at errors"
       ; hint = ""
       ; then_ = Review_started
       })
    "nothing to review: no changes against main in /work";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ review)
      (params ((args "main look at errors")))
      (tag (
        Retry_in_editor
        (command "/review main look at errors")
        (hint    "")
        (then_   Review_started))))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────── nothing to review: no changes against main in /work ───
    > /review main look at errors▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.step ~quiet:true h (Key (Key.ctrl 'u'));
  H.event h (State (state ~running:true ()));
  H.keys h "/init";
  H.enter h;
  H.reply h Review_started "{}";
  H.show h;
  [%expect
    {|
    (Rpc (method_ init) (params ()) (tag Review_started))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────── queued: it starts when the current run ends ───
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  ⠋ working · Esc aborts · Enter steers
    |}]
;;

let%expect_test "/review alone: the files' approvals and the review comments" =
  let h = Test_app.connected ~width:84 ~height:22 () in
  let hash path =
    let diff =
      P.Git_diff.of_json (P.Json.parse diff_json |> Or_error.ok_exn)
      |> Or_error.ok_exn
    in
    (List.find_exn (Diff_files.parse diff.diff) ~f:(fun f ->
       String.equal f.path path))
      .hash
  in
  H.keys h "/review";
  H.step ~quiet:true h (Key (Key.plain Enter));
  H.enter h;
  let review_json =
    sprintf
      {|{"comments":[
          {"id":"c1","root":"/work","path":"src/a.ml","first":2,"last":2,"side":"new","start_col":4,"end_col":5,"anchor":"let y = 3","status":"open",
           "thread":[{"author":"user","text":"Why 3?","at_ms":1,"delivered":true},
                     {"author":"agent","text":"It was off by one.","at_ms":2,"delivered":true,
                      "proposal":{"edits":[{"old_text":"3","new_text":"4"}],"state":"pending"}}]},
          {"id":"c2","root":"/work","path":"gone.ml","first":1,"last":3,"side":"old","anchor":"x","status":"resolved",
           "thread":[{"author":"user","text":"Drop it","at_ms":3,"delivered":false}]}],
         "approvals":[{"path":"src/a.ml","hash":%S},{"path":"new file.txt","hash":"older"}]}|}
      (hash "src/a.ml")
  in
  H.reply ~quiet:true h Review_comments review_json;
  H.reply
    h
    (Review_status
       (P.Review.of_json (P.Json.parse review_json |> Or_error.ok_exn)
        |> Or_error.ok_exn))
    diff_json;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Review ─────────────────────────────────────────────────────────────────────────┐
    │ review /work  1 of 2 files approved                                              │
    │   ✓ src/a.ml  +1 −1  approved                                                    │
    │     c1 line 2 “y”, open                                                          │
    │       you: Why 3?                                                                │
    │       prigh: It was off by one. [proposes a change]                              │
    │   ● new file.txt  +1 −0  changed since approved                                  │
    │   on files no longer changed:                                                    │
    │   gone.ml                                                                        │
    │     c2 old lines 1–3, resolved                                                   │
    │       you: Drop it                                                               │
    │ approve files and comment on code you select in prigh-web's review page; /review │
    │ HEAD has prigh review the changes itself                                         │
    └───────────────────────────────────────────────────────────────────── Esc closes ─┘
    ────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  ↑↓ scroll · Esc closes · typing goes to the editor
    |}]
;;

let%expect_test
    "/notify: the setting, and notifications only while the terminal is not \
     focused"
  =
  let h = Test_app.connected ~width:100 ~height:10 () in
  H.reply ~quiet:true h Config config_json;
  let command text =
    H.keys h text;
    (* The first Enter takes the command (and offers its arguments). *)
    if not (String.mem text ' ')
    then H.step ~quiet:true h (Key (Key.plain Enter));
    H.enter h
  in
  command "/notify";
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────────────────────────────────────────────┐
    │ notifications: the bell when a turn ends, a tool waits for you or background work finishes,      │
    │ while this terminal is not focused (it has not reported focus yet: in tmux, set -g focus-events  │
    │ on)                                                                                              │
    └────────────────────────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  let finish () =
    H.event h (Agent_end [ assistant "All tests pass.\nDetails follow." ])
  in
  (* The terminal has not said where the focus is, or it is focused: nothing. *)
  finish ();
  H.step h (Focus true);
  finish ();
  [%expect {| |}];
  H.step h (Focus false);
  finish ();
  H.event
    h
    (Tool_confirm
       { call_id = "c1"
       ; agent_id = None
       ; name = "bash"
       ; summary = "rm -rf build"
       ; rules = []
       });
  H.step ~quiet:true h (Key (Key.char 'n'));
  H.event h (Message_start (delivery "[job j1 exited 0] make test\nok"));
  (* A subagent's end is not the turn's. *)
  H.event
    h
    (Subagent
       { call_id = "c2"
       ; agent_id = "a1"
       ; event = Agent_end [ assistant "sub done" ]
       });
  (* The model's questions and its plan wait for the user too. *)
  H.event
    h
    (Ask_user
       { call_id = "q1"
       ; questions =
           [ { question = "Which database?"
             ; header = None
             ; choices = []
             ; multiple = false
             }
           ]
       ; deadline = None
       });
  H.step ~quiet:true h (Key (Key.plain Escape));
  H.event
    h
    (Tool_confirm
       { call_id = "p1"
       ; agent_id = None
       ; name = "exit_plan_mode"
       ; summary = "1. Write the schema\n2. Test it"
       ; rules = []
       });
  H.step ~quiet:true h (Key (Key.char 'n'));
  [%expect
    {|
    (Notify
      (mode Bell)
      (notification (
        (title "prigh: done in work")
        (body  "All tests pass."))))
    (Notify
      (mode Bell)
      (notification (
        (title "prigh: waiting for you in work")
        (body  "Allow bash: rm -rf build?"))))
    (Notify
      (mode Bell)
      (notification (
        (title "prigh: background work finished in work")
        (body  Reports))))
    (Notify
      (mode Bell)
      (notification (
        (title "prigh: a question for you in work")
        (body  "Which database?"))))
    (Notify
      (mode Bell)
      (notification (
        (title "prigh: waiting for you in work")
        (body  "Approve the plan: 1. Write the schema"))))
    |}];
  command "/notify desktop";
  H.reply ~quiet:true h (Notice_on_success "notifications: desktop") "{}";
  finish ();
  command "/notify off";
  finish ();
  command "/notify loud";
  command "/notify";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ set_config)
      (params ((config ((notify desktop)))))
      (tag (
        Notice_on_success
        "notifications: a desktop notification when a turn ends, a tool waits for you or background work finishes, while this terminal is not focused")))
    (Notify
      (mode Desktop)
      (notification (
        (title "prigh: done in work")
        (body  "All tests pass."))))
    (Rpc
      (method_ set_config)
      (params ((config ((notify off)))))
      (tag (
        Notice_on_success
        "notifications off (/notify bell or /notify desktop turns them on)")))

    ▌ earlier question
      earlier answer
      denied bash
    ↩ job j1 "make test"  exited 0
        ok
      plan not approved: still planning
    ────────────────────────────── notifications off (/notify bell or /notify desktop turns them on) ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;
