open! Core
open! Expect_test_helpers_core
open Prigh_ui
open Fixtures
module H = Test_app.H
module P = Prigh_protocol

(* /budget, /goal and /resume, and what the status line shows of them. *)

let connected () = Test_app.connected ~width:100 ~height:10 ()

let goal_json ?(status = "active") ?(continuations = 3) ?summary ?reason text =
  sprintf
    {|{"text":%s,"status":%S,"continuations":%d,"max_continuations":20%s%s}|}
    (P.Json.to_string (P.Json.str text))
    status
    continuations
    (Option.value_map summary ~default:"" ~f:(sprintf {|,"summary":%S|}))
    (Option.value_map reason ~default:"" ~f:(sprintf {|,"reason":%S|}))
;;

(* The last line of the screen. *)
let status h =
  print_endline
    (List.last_exn
       (String.split_lines
          (Screen.to_plain ~show_cursor:false (Render.screen h.H.model))))
;;

(* As typed, closing the completion first so that Enter runs it. *)
let run h text =
  String.iter text ~f:(fun c -> H.step ~quiet:true h (Key (Key.char c)));
  if Option.is_some h.H.model.autocomplete then H.esc h;
  H.enter h
;;

let%expect_test
    "the status line shows budget and spend, the goal and a scheduled resume"
  =
  let h = connected () in
  status h;
  H.event h (State (state ~extra:[ "budget_usd", "5" ] ()));
  status h;
  H.event h (State (state ~cost_usd:4.2 ~extra:[ "budget_usd", "5" ] ()));
  status h;
  H.event
    h
    (State
       (state
          ~cost_usd:5.01
          ~extra:[ "budget_usd", "5"; "goal", goal_json "tests pass" ]
          ()));
  status h;
  H.event
    h
    (State
       (state
          ~extra:
            [ "goal", goal_json ~status:"done" ~summary:"they pass" "tests pass"
            ; "resume_at_ms", "1760191500000"
            ]
          ()));
  status h;
  (* In the frontend's time zone. *)
  H.step h (Set_utc_offset (Time_ns.Span.of_hr 2.));
  status h;
  [%expect
    {|
    /work  deepseek-flash  think:off  view:normal  ctx:0.1%/1.0M  $0.01
    /work  deepseek-flash  think:off  view:normal  ctx:0.1%/1.0M  $0.01/$5.00
    /work  deepseek-flash  think:off  view:normal  ctx:0.1%/1.0M  $4.20/$5.00
    /work  deepseek-flash  think:off  view:normal  ctx:0.1%/1.0M  $5.01/$5.00  goal:3/20
    /work  deepseek-flash  think:off  view:normal  ctx:0.1%/1.0M  $0.01  goal:done  resumes 14:05
    /work  deepseek-flash  think:off  view:normal  ctx:0.1%/1.0M  $0.01  goal:done  resumes 16:05
    |}]
;;

let%expect_test
    "/budget shows, sets and clears the budget; a bad amount says what to type"
  =
  let h = connected () in
  run h "/budget";
  run h "/budget 5";
  H.reply h (Notice_on_success "budget set to $5.00") "{}";
  run h "/budget $2.50";
  run h "/budget off";
  run h "/budget lots";
  H.event h (State (state ~extra:[ "budget_usd", "5" ] ()));
  run h "/budget";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ set_budget)
      (params ((usd 5)))
      (tag (Notice_on_success "budget set to $5.00")))
    (Rpc
      (method_ set_budget)
      (params ((usd 2.5)))
      (tag (Notice_on_success "budget set to $2.50")))
    (Rpc
      (method_ set_budget)
      (params ((usd null)))
      (tag (Notice_on_success "budget removed")))
    ▌ earlier question
      earlier answer
      no budget ($0.01 spent): /budget DOLLARS stops the agent starting turns once the session has cost
      that much
      budget set to $5.00
      "lots" is not a budget: /budget DOLLARS (e.g. /budget 5) or /budget off
      budget $5.00, $0.01 spent: /budget DOLLARS changes it, /budget off removes it
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  view:normal  ctx:0.1%/1.0M  $0.01/$5.00
    |}]
;;

let%expect_test "/goal sets, shows, clears and resumes the goal" =
  let h = connected () in
  run h "/goal";
  run h "/goal all tests pass";
  run h "/goal off";
  run h "/goal resume";
  H.event
    h
    (State
       (state
          ~extra:
            [ ( "goal"
              , goal_json
                  ~status:"stopped"
                  ~reason:"it used all 20 automatic continuations"
                  "all tests pass" )
            ]
          ()));
  run h "/goal";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ set_goal)
      (params ((text "all tests pass")))
      (tag (
        Retry_in_editor
        (command "/goal all tests pass")
        (hint    "")
        (then_ (Notice_on_success "goal set: the agent works until it is met")))))
    (Rpc
      (method_ clear_goal)
      (params ())
      (tag (Notice_on_success "goal cleared")))
    (Rpc
      (method_ resume_goal)
      (params ())
      (tag (Notice_on_success "goal resumed")))
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      no goal: /goal TEXT sets one, and the agent keeps working until it judges it met
      goal: all tests pass — stopped (it used all 20 automatic continuations); /goal resume continues
      it, /goal off clears it
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  view:normal  ctx:0.1%/1.0M  $0.01  goal:stopped
    |}]
;;

let%expect_test "a refused /goal goes back into the editor" =
  let h = connected () in
  run h "/goal ship it";
  H.reply_error
    h
    (Retry_in_editor
       { command = "/goal ship it"
       ; hint = ""
       ; then_ = Notice_on_success "goal set: the agent works until it is met"
       })
    "the session's budget of $1.00 is spent ($1.20 so far): /budget 3 raises \
     it, /budget off removes it";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ set_goal)
      (params ((text "ship it")))
      (tag (
        Retry_in_editor
        (command "/goal ship it")
        (hint    "")
        (then_ (Notice_on_success "goal set: the agent works until it is met")))))


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      the session's budget of $1.00 is spent ($1.20 so far): /budget 3 raises it, /budget off removes it
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > /goal ship it▏
    /work  deepseek-flash  think:off  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/resume shows, runs now or cancels the automatic resume" =
  let h = connected () in
  run h "/resume";
  H.event h (State (state ~extra:[ "resume_at_ms", "1760191500000" ] ()));
  run h "/resume";
  run h "/resume now";
  run h "/resume cancel";
  run h "/resume later";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ resume_now)
      (params ())
      (tag (Notice_on_success "resuming now")))
    (Rpc
      (method_ cancel_resume)
      (params ())
      (tag (Notice_on_success "automatic resume cancelled")))
    ▌ earlier question
      earlier answer
      no resume is scheduled: one is when a run stops at a usage limit whose reset time the provider
      gives
      the run stopped at a usage limit and resumes at 14:05: /resume now retries at once, /resume cancel
      stays stopped
      usage: /resume [now|cancel]
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  view:normal  ctx:0.1%/1.0M  $0.01  resumes 14:05
    |}]
;;

let%expect_test "completion offers the keywords, but not over a goal's text" =
  let h = connected () in
  let complete text =
    H.step ~quiet:true h (Key (Key.ctrl 'u'));
    String.iter text ~f:(fun c -> H.step ~quiet:true h (Key (Key.char c)));
    H.show h
  in
  complete "/goal ";
  complete "/goal re";
  complete "/goal refactor";
  complete "/resume ";
  complete "/budget o";
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > /goal ▏
    ▸ off     clear the goal
      resume  make a stopped goal active again
    /work  deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01  Tab accepts · Enter runs as typed · Esc


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > /goal re▏
    ▸ resume  make a stopped goal active again
    /work  deepseek-flash  think:off  view:normal  ctx:0.1%/1.0M  $0.01  Tab/Enter accept · Esc close



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > /goal refactor▏
    /work  deepseek-flash  think:off  view:normal  ctx:0.1%/1.0M  $0.01

      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > /resume ▏
    ▸ now     retry the run at once
      cancel  stay stopped until you send a message
    /work  deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01  Tab accepts · Enter runs as typed · Esc


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > /budget o▏
    ▸ off  no budget: turns start whatever the session has cost
    /work  deepseek-flash  think:off  view:normal  ctx:0.1%/1.0M  $0.01  Tab/Enter accept · Esc close
    |}]
;;
