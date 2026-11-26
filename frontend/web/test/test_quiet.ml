open! Core
open Prigh_web
module H = Harness

(* Quiet verbosity: every run of steps between messages (thinking, tool
   calls, reports of background work) is one line saying what it is doing
   or what it did, which opens the steps, each a line that opens its
   detail. *)

let ev fields = sprintf {|{"type":"event",%s}|} fields

(* 2026-10-05 13:00:00Z and [s] seconds after. *)
let at s = sprintf "%.0f" (1791205200000. +. (Float.of_int s *. 1000.))
let json s = Jsonaf.to_string (`String s)

let user ?(source = "") s text =
  ev
    (sprintf
       {|"event":"message_start","message":{"role":"user","text":%s,"at":%s%s}|}
       (json text)
       (at s)
       source)
;;

let message ?(stop = {|{"type":"tool_use"}|}) ?s content =
  sprintf
    {|{"role":"assistant","content":[%s],"stop_reason":%s,"usage":{"input":0,"output":0,"cache_read":0},"model":"m"%s}|}
    (String.concat ~sep:"," content)
    stop
    (Option.value_map s ~default:"" ~f:(fun s -> sprintf {|,"at":%s|} (at s)))
;;

let assistant ?stop ?s content =
  ev (sprintf {|"event":"message_end","message":%s|} (message ?stop ?s content))
;;

let streaming content =
  ev
    (sprintf
       {|"event":"message_update","partial":%s,"delta":{"type":"text_delta","text":""}|}
       (message content))
;;

let reply ?(s = 30) text =
  assistant
    ~stop:{|{"type":"end_turn"}|}
    ~s
    [ sprintf {|{"type":"text","text":%s}|} (json text) ]
;;

let thinking text = sprintf {|{"type":"thinking","text":%s}|} (json text)

let call id name args =
  sprintf
    {|{"type":"tool_call","id":"%s","name":"%s","arguments":%s}|}
    id
    name
    (json args)
;;

let call_json id name args =
  sprintf {|{"id":"%s","name":"%s","arguments":%s}|} id name (json args)
;;

let start id name args =
  ev (sprintf {|"event":"tool_start","call":%s|} (call_json id name args))
;;

let finish ?(is_error = false) ?s id name args text =
  ev
    (sprintf
       {|"event":"tool_end","call":%s,"result":{"role":"tool_result","tool_call_id":"%s","tool_name":"%s","text":%s,"is_error":%b%s}|}
       (call_json id name args)
       id
       name
       (json text)
       is_error
       (Option.value_map s ~default:"" ~f:(fun s -> sprintf {|,"at":%s|} (at s))))
;;

let bash = {|{"command":"make"}|}
let read = {|{"path":"src/a.ml"}|}
let edit = {|{"path":"src/a.ml","old_text":"a","new_text":"b"}|}

(* Asked to fix a bug: it thinks, builds, reads, edits a file twice, then
   says it is done. *)
let fixed =
  [ user 0 "fix the bug"
  ; assistant
      ~s:2
      [ thinking "Let me build it first."
      ; call "c1" "bash" bash
      ; call "c2" "read" read
      ]
  ; finish
      ~is_error:true
      ~s:5
      "c1"
      "bash"
      bash
      "Error: a is not b\n[exit code 2]"
  ; finish ~s:5 "c2" "read" read "let a = 1"
  ; assistant ~s:8 [ call "c3" "edit" edit ]
  ; finish ~s:9 "c3" "edit" edit "edited src/a.ml"
  ; assistant ~s:10 [ call "c4" "edit" edit ]
  ; finish ~s:12 "c4" "edit" edit "edited src/a.ml"
  ; reply ~s:14 "Fixed it."
  ]
;;

let quiet ?(toggled = []) () =
  Fold.create ~verbosity:Quiet ~toggled:(String.Set.of_list toggled) ()
;;

let text ?toggled ?selector chat =
  Chat_harness.text ~fold:(quiet ?toggled ()) ?selector chat
;;

let show ?toggled ?selector chat =
  Chat_harness.show ~fold:(quiet ?toggled ()) ?selector chat
;;

let box = "run:think:1:0"

let%expect_test "a run of steps is one line: what it did and how long it took" =
  let chat = Chat_harness.chat fixed in
  text chat;
  [%expect
    {| fix the bug 15:00 ✕ Worked for 12s · ran a command, read a file, edited a file 1 failed Fixed it. 15:00 |}];
  show ~selector:".run" chat;
  [%expect
    {|
    <div data-slot="collapsible" data-state="closed" class="error run">
      <button data-slot="collapsible-trigger"
              type="button"
              data-fold="run:think:1:0"
              aria-expanded="false"
              class="run-head">
        <span title="A step failed" role="img" aria-label="A step failed" class="failed mark">
          <icon class="cross"> </icon>
        </span>
        <span class="run-label"> Worked for </span>
        <span class="run-time"> 12s </span>
        <span class="run-did"> · ran a command, read a file, edited a file </span>
        <span class="run-failed"> 1 failed </span>
        <span class="chev">  </span>
      </button>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
    </div>
    |}];
  (* Normal shows each step as before. *)
  Chat_harness.text ~fold:Fold.default chat;
  [%expect
    {| fix the bug 15:00 Thought Let me build it first. Let me build it first. ✕ bash make exit code 2 Error: a is not b ✓ read src/a.ml ✓ edit src/a.ml ✓ edit src/a.ml Fixed it. 15:00 |}]
;;

let%expect_test "opened, it lists the steps; a step opens its detail" =
  let chat = Chat_harness.chat fixed in
  text ~toggled:[ box ] ~selector:".run" chat;
  [%expect
    {| ✕ Worked for 12s · ran a command, read a file, edited a file 1 failed ✓ Thinking Let me build it first. ✕ bash make exit code 2 Error: a is not b ✓ read src/a.ml ✓ edit src/a.ml ✓ edit src/a.ml |}];
  text ~toggled:[ box; "c1#open"; "think:1:0" ] ~selector:".run" chat;
  [%expect
    {| ✕ Worked for 12s · ran a command, read a file, edited a file 1 failed ✓ Thinking Let me build it first. ✕ bash make exit code 2 ✓ read src/a.ml ✓ edit src/a.ml ✓ edit src/a.ml |}]
;;

let%expect_test "messages break runs; a failed step marks its run" =
  let chat =
    Chat_harness.chat
      [ user 0 "look around"
      ; assistant
          ~s:3
          [ call "c1" "ls" {|{"path":"."}|}
          ; sprintf {|{"type":"text","text":"Now the tests."}|}
          ; call "c2" "bash" {|{"command":"dune test"}|}
          ]
      ; finish ~s:2 "c1" "ls" {|{"path":"."}|} "src\ntest"
      ; finish ~s:6 "c2" "bash" {|{"command":"dune test"}|} "ok"
      ; reply ~s:7 "All good."
      ]
  in
  text chat;
  [%expect
    {| look around 15:00 ✓ Worked for 2s · listed a directory Now the tests. ✓ Worked for 6s · ran a command All good. 15:00 |}]
;;

let%expect_test "while it runs, the line says what it is doing" =
  let chat =
    Chat_harness.chat
      ~running:true
      [ user 0 "fix the bug"
      ; assistant ~s:2 [ call "c1" "bash" bash ]
      ; start "c1" "bash" bash
      ]
  in
  text ~selector:".run" chat;
  [%expect {| Running a command |}];
  let chat = Chat_harness.apply chat (finish ~s:3 "c1" "bash" bash "ok") in
  (* Between steps: the model is at work. *)
  text ~selector:".run" chat;
  [%expect {| Thinking |}];
  let chat =
    Chat_harness.apply
      chat
      (streaming [ call "c2" "edit" {|{"path":"src/b.ml"|} ])
  in
  text ~selector:".run" chat;
  [%expect {| Editing src/b.ml |}];
  let chat = Chat_harness.apply chat (streaming [ thinking "Hmm, b or c" ]) in
  text ~selector:".run" chat;
  [%expect {| Thinking |}];
  (* Text ends the run: what it did. *)
  let chat =
    Chat_harness.apply
      chat
      (streaming [ sprintf {|{"type":"text","text":"Done"}|} ])
  in
  text chat;
  [%expect {| fix the bug 15:00 ✓ Worked for 3s · ran a command Done |}]
;;

let%expect_test "reports of background work join the run; errors stay out" =
  let chat =
    Chat_harness.chat
      [ user 0 "build it in the background"
      ; assistant
          ~s:1
          [ call "c1" "bash" {|{"command":"make","background":true}|} ]
      ; finish ~s:1 "c1" "bash" bash "started job j1: make"
      ; reply ~s:2 "Started."
      ; user
          ~source:{|,"origin":{"source":{"kind":"delivery","title":"Reports"}}|}
          9
          "[job j1 exited 0] make\nbuilt"
      ; assistant ~s:10 [ call "c2" "job_output" {|{"id":"j1"}|} ]
      ; finish ~s:11 "c2" "job_output" {|{"id":"j1"}|} "built"
      ; assistant
          ~stop:{|{"type":"error","message":"overloaded"}|}
          ~s:12
          [ thinking "It built." ]
      ]
  in
  text chat;
  [%expect
    {| build it in the background 15:00 ✓ Worked for 1s · ran a command Started. 15:00 ✓ Worked for 9s · got a report, read a job's output The model returned an error overloaded Send a message to retry, or switch model. 15:00 |}];
  text ~toggled:[ "run:report:4" ] ~selector:".run-steps" chat;
  [%expect {| |}]
;;

let%expect_test "every tool the backend has reads in plain words" =
  (* The tools of backend/lib (grep [name = "] and [~name:"] in tools/,
     agent/ and computer/), and the [!command]'s [shell]: a new one needs a
     phrase in frontend/ui/tool_phrase.ml. *)
  let backend =
    [ "answer_session"
    ; "apply_patch"
    ; "ask_session"
    ; "ask_user"
    ; "bash"
    ; "computer"
    ; "custom_instructions"
    ; "edit"
    ; "find"
    ; "goal_complete"
    ; "grep"
    ; "issue"
    ; "job_input"
    ; "job_kill"
    ; "job_output"
    ; "job_status"
    ; "ls"
    ; "memory"
    ; "on_host"
    ; "pull_request"
    ; "read"
    ; "render_ui"
    ; "reply_to_comment"
    ; "repo"
    ; "sessions"
    ; "shell"
    ; "subagent"
    ; "subagent_cancel"
    ; "subagent_continue"
    ; "subagent_message"
    ; "subagent_status"
    ; "update_plan"
    ; "web_fetch"
    ; "web_search"
    ; "worktree"
    ; "write"
    ]
  in
  let missing =
    List.filter backend ~f:(fun name ->
      not (List.mem Prigh_ui.Tool_phrase.known name ~equal:String.equal))
  in
  print_s [%message (missing : string list)];
  let call name args =
    Prigh_ui.Tool_phrase.Step.Call { id = "x"; name; arguments = args }
  in
  List.iter Prigh_ui.Tool_phrase.known ~f:(fun name ->
    let step = call name "{}" in
    printf
      "%-20s %-28s | %s | %s\n"
      name
      (Prigh_ui.Tool_phrase.doing step)
      (Prigh_ui.Tool_phrase.did [ step ])
      (Prigh_ui.Tool_phrase.did [ step; step; step ]));
  [%expect
    {|
    (missing ())
    answer_session       Answering another session    | Answered another session | Answered 3 questions
    apply_patch          Editing a file               | Edited a file | Edited 3 files
    ask_session          Asking another session       | Asked another session | Asked other sessions 3 times
    ask_user             Asking you                   | Asked you a question | Asked you 3 questions
    bash                 Running a command            | Ran a command | Ran 3 commands
    computer             Using the computer           | Used the computer | Used the computer 3 times
    custom_instructions  Updating your instructions   | Updated your instructions | Updated your instructions 3 times
    edit                 Editing a file               | Edited a file | Edited 3 files
    find                 Searching the code           | Searched the code | Searched the code 3 times
    goal_complete        Completing the goal          | Completed the goal | Completed the goal 3 times
    grep                 Searching the code           | Searched the code | Searched the code 3 times
    issue                Working on an issue          | Worked on an issue | Worked on issues 3 times
    job_input            Sending input to a job       | Sent input to a job | Sent input to jobs 3 times
    job_kill             Stopping a job               | Stopped a job | Stopped 3 jobs
    job_output           Reading a job's output       | Read a job's output | Read jobs' output 3 times
    job_status           Checking on jobs             | Checked on jobs | Checked on jobs 3 times
    ls                   Listing a directory          | Listed a directory | Listed 3 directories
    memory               Using its memory             | Used its memory | Used its memory 3 times
    on_host              Listing the machines         | Listed the machines | Listed the machines 3 times
    pull_request         Working on a pull request    | Worked on a pull request | Worked on pull requests 3 times
    read                 Reading a file               | Read a file | Read 3 files
    render_ui            Showing you a view           | Showed a view | Showed 3 views
    reply_to_comment     Replying to a comment        | Replied to a comment | Replied to 3 comments
    repo                 Checking the repository      | Checked the repository | Checked the repository 3 times
    sessions             Looking through sessions     | Looked through sessions | Looked through sessions 3 times
    shell                Running a command            | Ran a command | Ran 3 commands
    subagent             Starting a subagent          | Started a subagent | Started 3 subagents
    subagent_cancel      Cancelling a subagent        | Cancelled a subagent | Cancelled 3 subagents
    subagent_continue    Continuing a subagent        | Continued a subagent | Continued 3 subagents
    subagent_message     Messaging a subagent         | Messaged a subagent | Messaged subagents 3 times
    subagent_status      Checking on subagents        | Checked on subagents | Checked on subagents 3 times
    update_plan          Updating the plan            | Updated the plan | Updated the plan 3 times
    web_fetch            Fetching a page              | Fetched a page | Fetched 3 pages
    web_search           Searching the web            | Searched the web | Searched the web 3 times
    worktree             Managing worktrees           | Managed worktrees | Managed worktrees 3 times
    write                Writing a file               | Wrote a file | Wrote 3 files
    |}];
  let open Prigh_ui.Tool_phrase in
  print_endline (doing (call "edit" edit));
  print_endline (doing (call "mcp__linear__create" "{}"));
  print_endline
    (doing
       (call "on_host" {|{"host":"laptop","tool":"read","args":{"path":"x"}}|}));
  print_endline (did [ Thinking; Thinking ]);
  print_endline
    (did
       [ Thinking
       ; call "edit" edit
       ; call "edit" edit
       ; call "write" {|{"path":"b"}|}
       ; call "edit" {|{"path":"c"}|}
       ; call "mcp__x__y" "{}"
       ; Report
       ]);
  List.iter [ 0.4; 14.; 125.; 3600.; 3780. ] ~f:(fun s ->
    print_endline (duration (Time_ns.Span.of_sec s)));
  [%expect
    {|
    Editing src/a.ml
    Using mcp__linear__create
    Reading a file
    Thought
    Edited 2 files, wrote a file, used mcp__x__y, got a report
    <1s
    14s
    2m 5s
    1h
    1h 3m
    |}]
;;

(* The app: the box's line toggles it; its open state survives new steps
   arriving; what waits on the user is outside it. *)
let%expect_test
    "in the app: toggled by its line, open while steps arrive, approvals \
     outside"
  =
  let h = H.create () in
  H.quiet h (fun () ->
    H.act h Cycle_verbosity;
    H.act h Cycle_verbosity);
  H.elements h ~selector:".chat";
  [%expect {| <div class="chat verbosity-quiet"> |}];
  H.event h {|{"event":"agent_start"}|};
  List.iter
    [ user 0 "fix the bug"
    ; assistant ~s:2 [ call "c1" "bash" bash ]
    ; finish ~s:3 "c1" "bash" bash "ok"
    ]
    ~f:(H.event h);
  H.show h ~selector:".run-head";
  H.act h (Toggle_fold "run:c1#open");
  H.text h ~selector:".run";
  [%expect
    {|
    <button data-slot="collapsible-trigger"
            type="button"
            data-fold="run:c1#open"
            aria-expanded="false"
            class="run-head">
      <span aria-hidden="true" class="lamp"> </span>
      <span class="run-label"> Thinking </span>
      <span class="chev">  </span>
    </button>
    (Thinking)
    (✓ bash make)
    |}];
  H.event
    h
    (assistant ~s:4 [ call "c2" "write" {|{"path":"x.ml","content":"x"}|} ]);
  H.event h (start "c2" "write" {|{"path":"x.ml","content":"x"}|});
  H.event
    h
    {|{"event":"tool_confirm","call_id":"c2","agent_id":null,"name":"write","summary":"x.ml","rules":[]}|};
  H.text h ~selector:".run";
  H.elements h ~selector:".confirm";
  [%expect
    {|
    (Focus confirm)
    (Writing x.ml)
    (✓ bash make)
    (write x.ml 1 line)
    x
    |}]
;;

(* An agent CLI's web searches are recorded as [web_search] calls (its code
   searches as [grep]): the line counts each kind apart. *)
let%expect_test "web searches and code searches are counted apart" =
  let web q = sprintf {|{"query":%s}|} (json q) in
  let grep = {|{"pattern":"usage_limit","path":"backend"}|} in
  let chat =
    Chat_harness.chat
      [ user 0 "does Ultra include API credits?"
      ; assistant
          ~s:2
          [ call
              "w1"
              "web_search"
              (web "\"Google AI Ultra\" Gemini API credits")
          ; call "w2" "web_search" (web "site:ai.google.dev Ultra credits")
          ; call "w3" "web_search" (web "Gemini API free tier")
          ; call "g1" "grep" grep
          ; call "o1" "Looking things up" "{}"
          ]
      ; finish ~s:4 "w1" "web_search" (web "a") "results"
      ; finish ~s:5 "w2" "web_search" (web "b") "results"
      ; finish ~s:6 "w3" "web_search" (web "c") "results"
      ; finish ~s:6 "g1" "grep" grep "a.ml:1"
      ; finish ~s:6 "o1" "Looking things up" "{}" "done"
      ; reply ~s:8 "No."
      ]
  in
  text ~selector:".run" chat;
  [%expect
    {| ✓ Worked for 6s · searched the web 3 times, searched the code, used Looking things up |}]
;;
