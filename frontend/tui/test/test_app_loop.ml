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
    deepseek-flash  think:off  mode:auto  view:normal                         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    deepseek-flash  think:off  mode:auto  view:normal                   ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01/$5.00
    deepseek-flash  think:off  mode:auto  view:normal                   ctx ▱▱▱▱▱ 0.1%/1.0M  $4.20/$5.00
    deepseek-flash  think:off  mode:auto  view:normal  goal:3/20        ctx ▱▱▱▱▱ 0.1%/1.0M  $5.01/$5.00
    deepseek-flash  think:off  mode:auto  goal:done  resumes 14:05            ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    deepseek-flash  think:off  mode:auto  goal:done  resumes 16:05            ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                                                             /work




    ▌ earlier question
      earlier answer
    ── /help ────────── budget $5.00, $0.01 spent: /budget DOLLARS changes it, /budget off removes it ──
    > ▏
    deepseek-flash  think:off  mode:auto  view:normal                   ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01/$5.00
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
    prigh  new session                                                                             /work

                                                      ╭─ Notice ───────────────────────────────────────╮
                                                      │ goal: all tests pass — stopped (it used all 20 │
                                                      │ automatic continuations); /goal resume         │
    ▌ earlier question                                │ continues it, /goal off clears it              │
      earlier answer                                  ╰──────────────────────────────── Esc dismisses ─╯
    ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  view:normal  goal:stopped           ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                                                             /work

                                                      ╭─ Error ────────────────────────────────────────╮
                                                      │ the session's budget of $1.00 is spent ($1.20  │
                                                      │ so far): /budget 3 raises it, /budget off      │
    ▌ earlier question                                │ removes it                                     │
      earlier answer                                  ╰──────────────────────────────── Esc dismisses ─╯
    ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
    > /goal ship it▏
    deepseek-flash  think:off  mode:auto  view:normal                         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                                                             /work




    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ── usage: /resume [now|cancel] ──
    > ▏
    deepseek-flash  think:off  mode:auto  view:normal  resumes 14:05          ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                                                             /work


    ╭─ /goal ──────────────────────────────────────────────────────────────────────────────────────────╮
    │ ▸ off     clear the goal                                                                         │
    │   resume  make a stopped goal active again                                                       │
    ╰──────────────────────────────────────────────────────────────────────────────────────────────────╯
    ── Tab accepts · Enter as typed · Esc hides ────────────────────────────────────────────────────────
    > /goal ▏
    deepseek-flash  think:off  mode:auto  view:normal                         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    prigh  new session                                                                             /work



    ╭─ /goal ──────────────────────────────────────────────────────────────────────────────────────────╮
    │ ▸ resume  make a stopped goal active again                                                       │
    ╰──────────────────────────────────────────────────────────────────────────────────────────────────╯
    ── Tab/Enter accept · Alt+S as typed ───────────────────────────────────────────────────────────────
    > /goal re▏
    deepseek-flash  think:off  mode:auto  view:normal                         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    prigh  new session                                                                             /work




    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
    > /goal refactor▏
    deepseek-flash  think:off  mode:auto  view:normal                         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    prigh  new session                                                                             /work


    ╭─ /resume ────────────────────────────────────────────────────────────────────────────────────────╮
    │ ▸ now     retry the run at once                                                                  │
    │   cancel  stay stopped until you send a message                                                  │
    ╰──────────────────────────────────────────────────────────────────────────────────────────────────╯
    ── Tab accepts · Enter as typed · Esc hides ────────────────────────────────────────────────────────
    > /resume ▏
    deepseek-flash  think:off  mode:auto  view:normal                         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    prigh  new session                                                                             /work



    ╭─ /budget ────────────────────────────────────────────────────────────────────────────────────────╮
    │ ▸ off  no budget: turns start whatever the session has cost                                      │
    ╰──────────────────────────────────────────────────────────────────────────────────────────────────╯
    ── Tab/Enter accept · Alt+S as typed ───────────────────────────────────────────────────────────────
    > /budget o▏
    deepseek-flash  think:off  mode:auto  view:normal                         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}]
;;
