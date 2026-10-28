open! Core
open! Expect_test_helpers_core
open Prigh_ui
open Fixtures
module H = Test_app.H
module P = Prigh_protocol

let started ?(height = 14) () =
  let h = Test_app.connected ~width:90 ~height () in
  H.event
    h
    (Subagent_start
       { call_id = "c1"
       ; agent_id = "a1"
       ; task = "index the repo"
       ; model = "claude-haiku"
       ; tools = [ "bash" ]
       ; worktree = None
       ; forked = false
       ; continued = false
       });
  H.event h (State (state ~subagents:[ "a1", "index the repo", true ] ()));
  h
;;

let alt_enter h = H.key h (Key.alt Enter)
let alt_up h = H.key h (Key.alt Up)
let queues h = print_s [%sexp (h.H.model.agent_queues : Agent_queues.t)]

let queue_event agents =
  P.Event.Agent_queues
    (List.map agents ~f:(fun (agent_id, steer, follow_up) ->
       { P.Agent_queue.agent_id; steer; follow_up }))
;;

(* What the backend sends when the subagent takes a message. *)
let delivered ?(follow_up = false) text =
  let note : P.Origin.Note.t =
    { kind = Other "steer"
    ; title = (if follow_up then "You (follow-up)" else "You (steer)")
    ; text = "[The user sends you this while you work: take it into account.]"
    }
  in
  let message =
    user
      ~origin:{ P.Origin.empty with body = Some text; notes = [ note ] }
      (note.text ^ "\n\n" ^ text)
  in
  [ P.Event.Subagent
      { call_id = "c1"; agent_id = "a1"; event = Message_start message }
  ; Subagent { call_id = "c1"; agent_id = "a1"; event = Message_end message }
  ]
;;

let%expect_test
    "a focused subagent: Enter steers it, Alt+Enter follows up, Alt+Up takes \
     back, its queue shows"
  =
  let h = started () in
  H.next_agent h;
  H.show h;
  [%expect
    {|
    ◆ subagent 1/1  claude-haiku  ⠋ running 0 turns  "index the repo"










    ──────────────────────────────────────────────────────────────────────────────────────────
    > ▏Steer a1 · index the repo (Enter: steer, Alt+Enter: follow-up)
    deepseek-flash  think:off  ctx:0.1%/1.0M  agents:main [1⠋] bg  → a1 · Esc: main agent
    |}];
  H.keys h "use the tests";
  H.enter h;
  H.keys h "then summarise";
  alt_enter h;
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ steer_agent)
      (params (
        (agent_id a1)
        (text     "use the tests")
        (mode     steer)))
      (tag (
        Agent_message
        (agent_id a1)
        (kind     Steer)
        (text     "use the tests")
        (retry    "use the tests"))))
    (Append_history "use the tests")
    (Rpc
      (method_ push_pref)
      (params (
        (key   history)
        (value "use the tests")
        (max   500)))
      (tag Ignore))
    (Rpc
      (method_ steer_agent)
      (params (
        (agent_id a1)
        (text     "then summarise")
        (mode     follow_up)))
      (tag (
        Agent_message
        (agent_id a1)
        (kind     Follow_up)
        (text     "then summarise")
        (retry    "then summarise"))))
    (Append_history "then summarise")
    (Rpc
      (method_ push_pref)
      (params (
        (key   history)
        (value "then summarise")
        (max   500)))
      (tag Ignore))
    ◆ subagent 1/1  claude-haiku  ⠋ running 0 turns  "index the repo"







    queued for a1 (2) · Alt+Up: edit the last · /queue: edit or remove
      steer      use the tests
      follow-up  then summarise
    ──────────────────────────────────────────────────────────────────────────────────────────
    > ▏Steer a1 · index the repo (Enter: steer, Alt+Enter: follow-up)
    deepseek-flash  ctx:0.1%/1.0M  queued:2  agents:main [1⠋] bg  → a1 · Esc: main agent
    |}];
  H.event h (queue_event [ "a1", [ "use the tests" ], [ "then summarise" ] ]);
  queues h;
  [%expect
    {|
    ((
      (agent_id a1)
      (steer     ("use the tests"))
      (follow_up ("then summarise"))))
    |}];
  (* Alt+Up takes the latest back, into the editor. *)
  alt_up h;
  [%expect
    {| (Rpc (method_ dequeue_agent) (params ((agent_id a1))) (tag Dequeued)) |}];
  H.reply h Dequeued {|{"text":"then summarise"}|};
  H.event h (queue_event [ "a1", [ "use the tests" ], [] ]);
  H.show h;
  [%expect
    {|
    ◆ subagent 1/1  claude-haiku  ⠋ running 0 turns  "index the repo"








    queued for a1 (1) · Alt+Up: edit the last · /queue: edit or remove
      steer      use the tests
    ──────────────────────────────────────────────────────────────────────────────────────────
    > then summarise▏
    deepseek-flash  ctx:0.1%/1.0M  queued:1  agents:main [1⠋] bg  → a1 · Esc: main agent
    |}];
  (* /queue lists the focused agent's; Ctrl+D removes from its queue. *)
  H.key h (Key.ctrl 'u');
  H.keys h "/queue";
  H.enter h;
  H.key h (Key.ctrl 'd');
  [%expect
    {|
    (Rpc
      (method_ remove_agent_queued)
      (params (
        (agent_id a1)
        (queue    steer)
        (index    0)
        (text     "use the tests")))
      (tag (Notice_on_success "removed from the queue: use the tests")))
    |}];
  H.event h (queue_event []);
  (* Delivered: the agent's transcript shows it as the user's steer, the
     main one a quiet line. *)
  List.iter (delivered "use the tests") ~f:(H.event h);
  List.iter (delivered ~follow_up:true "and summarise") ~f:(H.event h);
  H.show h;
  [%expect
    {|
    ◆ subagent 1/1  claude-haiku  ⠋ running 0 turns  "index the repo"





    → You (steer)
    ▌ use the tests

    → You (follow-up)
    ▌ and summarise
    ──────────────────────────────────────────────────────────────────────────────────────────
    > ▏Steer a1 · index the repo (Enter: steer, Alt+Enter: follow-up)
    deepseek-flash  think:off  ctx:0.1%/1.0M  agents:main [1⠋] bg  → a1 · Esc: main agent
    |}];
  H.esc h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    → a1: use the tests
    → a1 (follow-up): and summarise
    ──────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  agents:[main] 1⠋ bg
    |}]
;;

let%expect_test
    "a refused steer: the error shows, the text is back in the editor, no \
     longer queued"
  =
  let h = started () in
  H.focus_agent h 1;
  H.keys h "hurry up";
  H.step ~quiet:true h (Key (Key.plain Enter));
  queues h;
  [%expect {| (((agent_id a1) (steer ("hurry up")) (follow_up ()))) |}];
  H.reply_error
    h
    (Agent_message
       { agent_id = "a1"; kind = Steer; text = "hurry up"; retry = "hurry up" })
    "a1 has finished and its report was delivered to the main agent: tell the \
     main agent instead, or have it start a new subagent";
  queues h;
  H.show h;
  [%expect
    {|
    ()
    ◆ subagent 1/1  claude-haiku  ⠋ running 0 turns  "index the repo"






    ┌─ Error ────────────────────────────────────────────────────────────────────────────────┐
    │ a1 has finished and its report was delivered to the main agent: tell the main agent    │
    │ instead, or have it start a new subagent                                               │
    └──────────────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ──────────────────────────────────────────────────────────────────────────────────────────
    > hurry up▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  agents:main [1⠋] bg  → a1 · Esc: main agent
    |}]
;;

let%expect_test
    "a finished subagent takes nothing: the text stays, the error says what to \
     do"
  =
  let h = started () in
  H.event
    h
    (Subagent_end
       { call_id = "c1"
       ; agent_id = "a1"
       ; usage = { input = 1; output = 1; cache_read = 0 }
       ; turns = 2
       ; cost_usd = 0.
       ; result = { text = "indexed"; is_error = false }
       });
  H.next_agent h;
  H.show h;
  H.keys h "one more thing";
  H.enter h;
  H.show h;
  [%expect
    {|
    ◆ subagent 1/1  claude-haiku  ✓ done 2 turns $0.00  "index the repo"










    ──────────────────────────────────────────────────────────────────────────────────────────
    > ▏a1 has finished · Esc goes back to the main agent
    deepseek-flash  ctx:0.1%/1.0M  agents:main [1✓] bg  → a1 (finished) · Esc: main agent
    ◆ subagent 1/1  claude-haiku  ✓ done 2 turns $0.00  "index the repo"










    ─────────────────── a1 has finished: tell the main agent instead (Esc goes back to it) ───
    > one more thing▏
    deepseek-flash  ctx:0.1%/1.0M  agents:main [1✓] bg  → a1 (finished) · Esc: main agent
    |}]
;;

let%expect_test
    "/agents steer and follow-up: without focusing; errors say what to do"
  =
  let h = started () in
  let run text =
    H.keys h text;
    H.key h (Key.alt (Char "s"));
    print_endline
      (Option.value_map h.model.notice ~default:"" ~f:(fun n -> n.text));
    print_endline ("editor: " ^ Editor.text h.model.editor);
    H.key h (Key.ctrl 'u')
  in
  run "/agents steer 1 look at lib/ first";
  [%expect
    {|
    (Rpc
      (method_ steer_agent)
      (params (
        (agent_id a1)
        (text     "look at lib/ first")
        (mode     steer)))
      (tag (
        Agent_message
        (agent_id a1)
        (kind     Steer)
        (text     "look at lib/ first")
        (retry    "/agents steer 1 look at lib/ first"))))
    (Append_history "look at lib/ first")
    (Rpc
      (method_ push_pref)
      (params (
        (key   history)
        (value "look at lib/ first")
        (max   500)))
      (tag Ignore))
    steer for a1 queued: it reads it at its next step
    editor:
    |}];
  run "/agents follow-up a1 then the docs";
  [%expect
    {|
    (Rpc
      (method_ steer_agent)
      (params (
        (agent_id a1)
        (text     "then the docs")
        (mode     follow_up)))
      (tag (
        Agent_message
        (agent_id a1)
        (kind     Follow_up)
        (text     "then the docs")
        (retry    "/agents follow-up a1 then the docs"))))
    (Append_history "then the docs")
    (Rpc
      (method_ push_pref)
      (params (
        (key   history)
        (value "then the docs")
        (max   500)))
      (tag Ignore))
    follow-up for a1 queued: it reads it before it reports
    editor:
    |}];
  (* Shown in the main view's status, the queue only in the agent's. *)
  queues h;
  [%expect
    {|
    ((
      (agent_id a1)
      (steer     ("look at lib/ first"))
      (follow_up ("then the docs"))))
    |}];
  H.reply_error
    h
    (Agent_message
       { agent_id = "a1"
       ; kind = Follow_up
       ; text = "then the docs"
       ; retry = "/agents follow-up a1 then the docs"
       })
    "a1 has finished and its report was delivered to the main agent: tell the \
     main agent instead, or have it start a new subagent";
  print_endline ("editor: " ^ Editor.text h.model.editor);
  queues h;
  H.key h (Key.ctrl 'u');
  [%expect
    {|
    editor: /agents follow-up a1 then the docs
    (((agent_id a1) (steer ("look at lib/ first")) (follow_up ())))
    |}];
  run "/agents steer 3 hello";
  run "/agents steer a1";
  run "/agents stear 1 hi";
  run "/agents cancel";
  [%expect
    {|
    no subagent 3; known: 1 (a1)
    editor: /agents steer 3 hello
    usage: /agents steer <n|id> TEXT
    editor: /agents steer a1
    unknown /agents "stear"; did you mean steer? usage: /agents [cancel <n|id> | steer <n|id> TEXT | follow-up <n|id> TEXT]
    editor: /agents stear 1 hi
    usage: /agents cancel <n|id>
    editor: /agents cancel
    |}];
  (* Completions: the subcommands, then the agents. *)
  H.keys h "/agents ";
  H.show h;
  H.keys h "st";
  H.key h (Key.plain Tab);
  H.keys h " ";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ cancel     cancel subagent n
      steer      tell subagent n something at its next step
      follow-up  tell subagent n something when it would report
    ──────────────────────────────────────────────────────────────────────────────────────────
    > /agents ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01  Tab accepts · Enter as typed · Esc hides






      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ 1  a1 · index the repo
    ──────────────────────────────────────────────────────────────────────────────────────────
    > /agents steer ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01  Tab accepts · Enter as typed · Esc hides
    |}]
;;

let%expect_test
    "get_pending fills the queues; a session switch resets them and the focus"
  =
  let h = started () in
  H.reply
    h
    Pending
    {|{"steer_texts":[],"follow_up_texts":[],"agents":[{"agent_id":"a1","steer_texts":["use the tests"],"follow_up_texts":[]}],"confirms":[],"questions":[]}|};
  H.next_agent h;
  queues h;
  H.show h;
  [%expect
    {|
    (((agent_id a1) (steer ("use the tests")) (follow_up ())))
    ◆ subagent 1/1  claude-haiku  ⠋ running 0 turns  "index the repo"








    queued for a1 (1) · Alt+Up: edit the last · /queue: edit or remove
      steer      use the tests
    ──────────────────────────────────────────────────────────────────────────────────────────
    > ▏Steer a1 · index the repo (Enter: steer, Alt+Enter: follow-up)
    deepseek-flash  ctx:0.1%/1.0M  queued:1  agents:main [1⠋] bg  → a1 · Esc: main agent
    |}];
  H.reply h (Reload_messages_notice "switched session") "{}";
  queues h;
  print_s [%sexp (h.model.focus : [ `Main | `Agent of string ])];
  [%expect
    {|
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    ()
    Main
    |}]
;;
