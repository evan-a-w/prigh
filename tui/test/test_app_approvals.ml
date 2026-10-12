open! Core
open! Expect_test_helpers_core
open Prigh_ui
open Fixtures
module H = Test_app.H
module P = Prigh_protocol

let alt_m h = H.key h (Key.alt (Char "m"))

(* Types a command and runs it: the first Enter may only accept the
   completion. *)
let command (h : H.t) text =
  H.keys h text;
  H.enter h;
  if Option.is_some h.model.autocomplete then H.enter h
;;

let with_mode ?(plan_mode = false) ?plan approval_mode =
  { (state ()) with approval_mode; plan_mode; plan }
;;

let plan : P.Plan.t =
  { explanation = None
  ; steps =
      [ { step = "read the code"; status = Completed }
      ; { step = "change it"; status = In_progress }
      ; { step = "test"; status = Pending }
      ]
  }
;;

let confirm ?(rules = []) ?agent_id ?(name = "bash") call_id summary
  : P.Pending.Confirm.t
  =
  { call_id; agent_id; name; summary; rules }
;;

let%expect_test "Alt+M cycles the mode; the status line shows it at once" =
  let h = Test_app.connected () in
  H.event h (State (with_mode Ask));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  mode:ask  ctx:0.1%/1.0M  $0.01
    |}];
  alt_m h;
  [%expect
    {|
    (Rpc
      (method_ set_mode)
      (params (
        (approval_mode auto-edit)
        (plan_mode     false)))
      (tag (
        Mode_set
        "mode: auto-edit \226\128\148 file edits run; bash, job_input and MCP tools ask first")))
    |}];
  H.reply
    h
    (Mode_set "mode: auto-edit — file edits run; bash and MCP tools ask first")
    (state_json ());
  alt_m h;
  alt_m h;
  alt_m h;
  [%expect
    {|
    (Rpc
      (method_ set_mode)
      (params (
        (approval_mode auto)
        (plan_mode     true)))
      (tag (
        Mode_set
        "mode: plan \226\128\148 read-only planning: no file edits; bash, job_input and MCP tools ask first")))
    (Rpc
      (method_ set_mode)
      (params (
        (approval_mode ask)
        (plan_mode     false)))
      (tag (
        Mode_set
        "mode: ask \226\128\148 bash, job_input, edits and MCP tools ask first")))
    (Rpc
      (method_ set_mode)
      (params (
        (approval_mode auto-edit)
        (plan_mode     false)))
      (tag (
        Mode_set
        "mode: auto-edit \226\128\148 file edits run; bash, job_input and MCP tools ask first")))
    |}];
  (* A refusal puts the backend's mode back. *)
  H.reply_error h (Mode_set "x") "unknown approval mode";
  [%expect {| (Rpc (method_ get_state) (params ()) (tag Refresh_state)) |}];
  H.reply h Refresh_state (state_json ());
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      mode: auto-edit — file edits run; bash and MCP tools ask
      first
      unknown approval mode
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/mode picks or sets the mode; /plan toggles plan mode" =
  let h = Test_app.connected ~height:16 () in
  H.event h (State (with_mode Auto_edit));
  command h "/mode";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Mode (Alt+M cycles)  (4)
    / ▏
       ask        bash, job_input, edits and MCP tools ask first
    ▸* auto-edit  file edits run; bash, job_input and MCP tools…
       auto       tools run without asking
       plan       read-only planning: no file edits; bash, job_…
    ────────────────────────────────────────────────────────────
    …deepseek-flash  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  H.keys h "plan";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ set_mode)
      (params (
        (approval_mode auto-edit)
        (plan_mode     true)))
      (tag (
        Mode_set
        "mode: plan \226\128\148 read-only planning: no file edits; bash, job_input and MCP tools ask first")))
    |}];
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  mode:plan  ctx:0.1%/1.0M  $0.01
    |}];
  command h "/plan";
  command h "/plan on";
  command h "/mode ask";
  command h "/mode careful";
  command h "/plan maybe";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ set_mode)
      (params (
        (approval_mode auto-edit)
        (plan_mode     false)))
      (tag (
        Mode_set
        "mode: auto-edit \226\128\148 file edits run; bash, job_input and MCP tools ask first")))
    (Rpc
      (method_ set_mode)
      (params (
        (approval_mode auto-edit)
        (plan_mode     true)))
      (tag (
        Mode_set
        "mode: plan \226\128\148 read-only planning: no file edits; bash, job_input and MCP tools ask first")))
    (Rpc
      (method_ set_mode)
      (params (
        (approval_mode ask)
        (plan_mode     false)))
      (tag (
        Mode_set
        "mode: ask \226\128\148 bash, job_input, edits and MCP tools ask first")))






      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      unknown mode "careful"; use ask, auto-edit, auto, plan
      unknown argument "maybe"; use on or off (or nothing to
      toggle)
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  mode:ask  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "the plan in the status line, /todos and /permissions" =
  let h = Test_app.connected ~width:90 ~height:20 () in
  command h "/todos";
  H.event h (State (with_mode ~plan Ask));
  H.event
    h
    (Config_changed
       { P.Config.default with
         permissions =
           { allow = [ "bash(git status:*)" ]
           ; ask = []
           ; deny = [ "bash(rm:*)" ]
           }
       });
  H.event h (State { (with_mode ~plan Ask) with session_rules = [ "edit" ] });
  command h "/todos";
  command h "/permissions";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      no plan: the model has not made one (it uses update_plan)
      plan 1/3
      ✓ read the code
      ▸ change it
      ○ test
      mode: ask — bash, job_input, edits and MCP tools ask first
      allowed this session: edit
      allow (config): bash(git status:*)
      deny (config): bash(rm:*)
      rules: permissions in ~/.prigh/config.json, e.g. {"allow": ["bash(git status:*)"],
      "deny": ["bash(rm:*)"]}; /mode or Alt+M switches the mode
    ──────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:ask  ctx:0.1%/1.0M  $0.01  plan 1/3: change it
    |}]
;;

let%expect_test
    "confirmations: always allow shows its rules; deny with a reason; Esc in \
     the reason goes back"
  =
  let h = Test_app.connected ~height:16 () in
  H.event h (State (with_mode Ask));
  H.keys h "draft";
  H.event
    h
    (Tool_confirm (confirm ~rules:[ "bash(git status:*)" ] "c1" "git status -s"));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────────────────────────┐
    │ Run bash: git status -s?                                 │
    │ y  allow once                                            │
    │ a  always allow bash(git status:*) this session          │
    │ n  deny                                                  │
    │ r  deny, saying why                                      │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? draft▏
    …deepseek-flash  mode:ask  ctx:0.1%/1.0M  confirm: y/a/n/r
    |}];
  H.keys h "a";
  [%expect
    {|
    (Rpc
      (method_ tool_confirm_respond)
      (params (
        (call_id c1)
        (allow   true)
        (always  true)))
      (tag Show_error))
    |}];
  H.event h (Tool_confirm (confirm ~agent_id:"a1" "s1" "make"));
  H.keys h "r";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      allowed bash; always: bash(git status:*)
    ┌─ Why not? (the model sees it) ───────────────────────────┐
    │ [a1] Run bash: make?                                     │
    │ › ▏                                                      │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    …deepseek-flash  ctx:0.1%/1.0M  Enter denies · Esc goes back
    |}];
  (* Esc goes back to the question, the draft kept. *)
  H.esc h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      allowed bash; always: bash(git status:*)
    ┌─ Confirm ────────────────────────────────────────────────┐
    │ [a1] Run bash: make?                                     │
    │ y  allow once                                            │
    │ n  deny                                                  │
    │ r  deny, saying why                                      │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? draft▏
    …deepseek-flash  mode:ask  ctx:0.1%/1.0M  confirm: y/n/r
    |}];
  H.keys h "r";
  H.keys h "use dune";
  H.enter h;
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ tool_confirm_respond)
      (params (
        (call_id  s1)
        (allow    false)
        (agent_id a1)
        (reason   "use dune")))
      (tag Show_error))







      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      allowed bash; always: bash(git status:*)
      denied bash
    ────────────────────────────────────────────────────────────
    > draft▏
    …deepseek-flash  think:off  mode:ask  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test
    "a confirmation answered elsewhere closes; a queued one is dropped"
  =
  let h = Test_app.connected () in
  H.event h (Tool_confirm (confirm "c1" "make"));
  H.event h (Tool_confirm (confirm "c2" "make install"));
  H.mode h;
  H.event h (Tool_confirm_done { call_id = "c2"; agent_id = None });
  H.event h (Tool_confirm_done { call_id = "c1"; agent_id = None });
  H.mode h;
  print_s [%sexp (List.length h.model.pending_confirms : int)];
  H.show h;
  [%expect
    {|
    confirm
    editing
    0




      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      confirmation answered elsewhere
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "the plan to approve" =
  let h = Test_app.connected ~height:18 () in
  H.event h (State (with_mode ~plan_mode:true Auto));
  H.event
    h
    (Tool_confirm
       (confirm
          ~name:"exit_plan_mode"
          "p1"
          "1. Add the flag to main.ml\n2. Test it"));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Plan ───────────────────────────────────────────────────┐
    │ Approve the plan and start implementing?                 │
    │ 1. Add the flag to main.ml                               │
    │ 2. Test it                                               │
    │                                                          │
    │ y  approve: leave plan mode and implement                │
    │ n  keep planning                                         │
    │ r  keep planning, saying what to change                  │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    …deepseek-flash  mode:plan  ctx:0.1%/1.0M  confirm: y/n/r
    |}];
  (* No "always" for a plan. *)
  H.keys h "a";
  H.mode h;
  H.keys h "r";
  H.keys h "also update the docs";
  H.enter h;
  [%expect
    {|
    confirm
    (Rpc
      (method_ tool_confirm_respond)
      (params (
        (call_id p1)
        (allow   false)
        (reason  "also update the docs")))
      (tag Show_error))
    |}]
;;

let questions : P.Question.t list =
  [ { question = "Which database?"
    ; header = Some "Storage"
    ; choices =
        [ { label = "Postgres"; description = Some "what prod uses" }
        ; { label = "SQLite"; description = None }
        ]
    ; multiple = false
    }
  ; { question = "Which features?"
    ; header = None
    ; choices =
        [ { label = "auth"; description = None }
        ; { label = "billing"; description = None }
        ]
    ; multiple = true
    }
  ; { question = "Anything else?"
    ; header = None
    ; choices = []
    ; multiple = false
    }
  ]
;;

let%expect_test "ask_user: a dialog per question; Enter answers, Esc declines" =
  let h = Test_app.connected ~height:16 () in
  H.event h (Ask_user { call_id = "q1"; questions });
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Storage (1/3) ──────────────────────────────────────────┐
    │ Which database?                                          │
    │ › ( ) Postgres▏ what prod uses                           │
    │   ( ) SQLite                                             │
    │   Other:                                                 │
    │ ↑↓ choose · type your own · Enter picks · ←→ questions   │
    │ Esc declines (the model is told)                         │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    …deepseek-flash  ctx:0.1%/1.0M  Enter accepts · Esc declines
    |}];
  H.key h (Key.plain Down);
  H.enter h;
  (* Several to pick, and an answer of one's own. *)
  H.keys h " ";
  H.key h (Key.plain Down);
  H.keys h " ";
  H.keys h "and sso";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Question 2/3 ───────────────────────────────────────────┐
    │ Which features?                                          │
    │   [x] auth                                               │
    │   [x] billing                                            │
    │ › Other: and sso▏                                        │
    │ ↑↓ choose · Space toggles · type your own                │
    │ Enter accepts · ←→ questions                             │
    │ Esc declines (the model is told)                         │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    …deepseek-flash  ctx:0.1%/1.0M  Enter accepts · Esc declines
    |}];
  (* Back to the first: its answer is kept. *)
  H.key h (Key.plain Left);
  H.show h;
  H.key h (Key.plain Right);
  H.enter h;
  H.keys h "keep it small";
  H.enter h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Storage (1/3) ──────────────────────────────────────────┐
    │ Which database?                                          │
    │   ( ) Postgres  what prod uses                           │
    │ › (•) SQLite▏                                            │
    │   Other:                                                 │
    │ ↑↓ choose · type your own · Enter picks · ←→ questions   │
    │ Esc declines (the model is told)                         │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    …deepseek-flash  ctx:0.1%/1.0M  Enter accepts · Esc declines
    (Rpc
      (method_ ask_user_respond)
      (params (
        (call_id q1)
        (answers ((SQLite) (auth billing "and sso") ("keep it small")))))
      (tag Show_error))
    |}];
  H.mode h;
  H.event
    h
    (Ask_user { call_id = "q2"; questions = [ List.last_exn questions ] });
  H.esc h;
  H.mode h;
  [%expect
    {|
    editing
    (Rpc
      (method_ ask_user_respond)
      (params (
        (call_id q2)
        (decline true)))
      (tag Show_error))
    editing
    |}]
;;

let%expect_test
    "ask_user: answered elsewhere closes; get_pending brings back what is \
     waiting"
  =
  let h = Test_app.connected () in
  H.event h (Ask_user { call_id = "q1"; questions });
  H.event h (Ask_user_done "q1");
  H.mode h;
  [%expect {| editing |}];
  (* After a reconnect: what the backend still waits for, without
     duplicates. *)
  H.event h (Tool_confirm (confirm "c1" "make"));
  H.reply
    h
    Pending
    {|{"steer_texts":[],"follow_up_texts":[],
       "confirms":[{"call_id":"c1","agent_id":null,"name":"bash","summary":"make","rules":[]},
                   {"call_id":"c2","agent_id":"a1","name":"write","summary":"x","rules":["edit"]}],
       "questions":[{"call_id":"q3","questions":[{"question":"Sure?"}]}]}|};
  let shown () =
    match h.model.mode with
    | Confirm { question; _ } -> print_endline ("confirm: " ^ question)
    | Question { call_id; _ } -> print_endline ("question " ^ call_id)
    | mode -> print_endline (Mode.name mode)
  in
  shown ();
  H.keys h "y";
  shown ();
  H.keys h "n";
  shown ();
  H.esc h;
  shown ();
  [%expect
    {|
    confirm: Run bash: make?
    (Rpc
      (method_ tool_confirm_respond)
      (params (
        (call_id c1)
        (allow   true)))
      (tag Show_error))
    confirm: [a1] Write x?
    (Rpc
      (method_ tool_confirm_respond)
      (params (
        (call_id  c2)
        (allow    false)
        (agent_id a1)))
      (tag Show_error))
    question q3
    (Rpc
      (method_ ask_user_respond)
      (params (
        (call_id q3)
        (decline true)))
      (tag Show_error))
    editing
    |}]
;;

let%expect_test "the plan, questions and plan approval in the transcript" =
  let h = Test_app.connected ~width:70 ~height:30 () in
  let plan_args =
    {|{"explanation":"first pass","plan":[{"step":"read the code","status":"completed"},{"step":"change it","status":"in_progress"},{"step":"test","status":"pending"}]}|}
  in
  let run name ?(is_error = false) arguments id result =
    let call = tool_call ~name ~arguments id in
    H.event h (Tool_start call);
    Option.iter result ~f:(fun text ->
      H.event
        h
        (Tool_end { call; result = tool_result ~name ~is_error ~id text }))
  in
  run
    "update_plan"
    plan_args
    "t1"
    (Some "Plan updated (1 of 3 steps completed)");
  run
    "ask_user"
    {|{"questions":[{"question":"Which database?"}]}|}
    "t2"
    (Some "The user answered:\nQ: Which database?\nA: Postgres\n");
  run
    "exit_plan_mode"
    {|{"plan":"1. Change main.ml\n2. Test"}|}
    "t3"
    ~is_error:true
    (Some
       "[the user did not approve the plan; their feedback: docs too] Stay in \
        plan mode.");
  run "ask_user" {|{"questions":[{"question":"Ready?"}]}|} "t4" None;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ✓ update_plan  1/3 done
        first pass
        ✓ read the code
        ▸ change it
        ○ test
    ✓ ask_user
        Q: Which database?
        A: Postgres
    ✕ exit_plan_mode  not approved
        1. Change main.ml
        2. Test
        [the user did not approve the plan; their feedback: docs too] Stay
        in plan mode.
    … ask_user  waiting for your answer
        ? Ready?
    ──────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/confirm shows the mode and sets ask or auto as the default" =
  let h = Test_app.connected () in
  command h "/confirm";
  command h "/confirm on";
  [%expect
    {|
    (Rpc
      (method_ set_config)
      (params ((config ((approval_mode ask)))))
      (tag (
        Notice_on_success
        "tool confirmation on (approval mode ask, also for new sessions)")))
    |}];
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      mode: auto — tools run without asking (/confirm on|off
      sets ask or auto; /mode has all)
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "job_input's confirmation says which job gets what" =
  let h = Test_app.connected ~height:14 () in
  H.event h (State (with_mode Ask));
  H.event
    h
    (Tool_confirm
       (confirm ~rules:[ "job_input" ] ~name:"job_input" "c1" {|j1: "y\n"|}));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────────────────────────┐
    │ Send to job j1: "y\n"?                                   │
    │ y  allow once                                            │
    │ a  always allow job_input this session                   │
    │ n  deny                                                  │
    │ r  deny, saying why                                      │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    …deepseek-flash  mode:ask  ctx:0.1%/1.0M  confirm: y/a/n/r
    |}]
;;

let%expect_test
    "the reason is typed in its box, under the call it denies; long ones wrap \
     there"
  =
  let h = Test_app.connected ~width:50 ~height:16 () in
  H.event h (State (with_mode Ask));
  H.event h (Tool_confirm (confirm "c1" "rm -rf _build && dune build @runtest"));
  H.keys h "r";
  H.keys h "use dune clean instead, it knows what the build owns";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C
      twice quits

    ▌ earlier question
      earlier answer
    ┌─ Why not? (the model sees it) ─────────────────┐
    │ Run bash: rm -rf _build && dune build          │
    │ @runtest?                                      │
    │ › use dune clean instead, it knows what the bu │
    │   ild owns▏                                    │
    └────────────────────────────────────────────────┘
    ──────────────────────────────────────────────────
    …deepseek-flash  Enter denies · Esc goes back
    |}]
;;

let%expect_test "long choice descriptions and answers wrap in the question box" =
  let h = Test_app.connected ~width:50 ~height:20 () in
  H.event
    h
    (Ask_user
       { call_id = "q1"
       ; questions =
           [ { question = "Which database?"
             ; header = None
             ; choices =
                 [ { label = "Postgres"
                   ; description =
                       Some
                         "what production uses, with the extensions we rely \
                          on for search"
                   }
                 ; { label = "SQLite"; description = Some "embedded" }
                 ]
             ; multiple = false
             }
           ]
       });
  H.key h (Key.plain Down);
  H.show h;
  H.keys h "neither: a flat file is enough for this small tool";
  H.show h;
  [%expect {|
      prigh in /work · /help · Esc aborts · Ctrl+C
      twice quits

    ▌ earlier question
      earlier answer
    ┌─ Question ─────────────────────────────────────┐
    │ Which database?                                │
    │   ( ) Postgres                                 │
    │       what production uses, with the           │
    │       extensions we rely on for search         │
    │ › ( ) SQLite▏ embedded                         │
    │   Other:                                       │
    │ ↑↓ choose · type your own · Enter picks        │
    │ Esc declines (the model is told)               │
    └────────────────────────────────────────────────┘
    ──────────────────────────────────────────────────
    …deepseek-flash  Enter accepts · Esc declines


      prigh in /work · /help · Esc aborts · Ctrl+C
      twice quits

    ▌ earlier question
      earlier answer
    ┌─ Question ─────────────────────────────────────┐
    │ Which database?                                │
    │   ( ) Postgres                                 │
    │       what production uses, with the           │
    │       extensions we rely on for search         │
    │   ( ) SQLite  embedded                         │
    │ › Other: neither: a flat file is enough for th │
    │          is small tool▏                        │
    │ ↑↓ choose · type your own · Enter picks        │
    │ Esc declines (the model is told)               │
    └────────────────────────────────────────────────┘
    ──────────────────────────────────────────────────
    …deepseek-flash  Enter accepts · Esc declines
    |}]
;;
