open! Core
module H = Harness

(* /budget, /goal and /resume, and the status line items that show them. *)

let run h command =
  H.type_ h command;
  H.act h Send
;;

let toasts h = H.text h ~selector:".toast"

let goal ?(status = "active") ?(continuations = 3) ?summary ?reason text =
  `Object
    ([ "text", `String text
     ; "status", `String status
     ; "continuations", `Number (Int.to_string continuations)
     ; "max_continuations", `Number "20"
     ]
     @ Option.value_map summary ~default:[] ~f:(fun s ->
       [ "summary", `String s ])
     @ Option.value_map reason ~default:[] ~f:(fun r -> [ "reason", `String r ])
    )
;;

let state fields =
  sprintf
    {|{"type":"event","event":"state","state":%s}|}
    (H.state_json ~fields ())
;;

let%expect_test
    "the status line shows spend against the budget, the goal and a scheduled \
     resume; each opens its command"
  =
  let h = H.create () in
  H.text h ~selector:".status";
  H.event h (state [ "budget_usd", `Number "5"; "cost_usd", `Number "4.2" ]);
  H.text h ~selector:".status";
  H.elements h ~selector:".status .budget";
  H.event
    h
    (state
       [ "budget_usd", `Number "5"
       ; "cost_usd", `Number "5.5"
       ; "goal", goal "all tests pass"
       ; "resume_at_ms", `Number "1791295500000"
       ]);
  H.text h ~selector:".status";
  List.iter
    [ ".status .budget"; ".status .goal"; ".status .resume" ]
    ~f:(fun selector -> H.elements h ~selector);
  (* In the page's time zone. *)
  H.act h (Set_utc_offset (Time_ns.Span.of_hr 2.));
  H.text h ~selector:".status .resume";
  [%expect
    {|
    Ready 0% ↑0 ↓0 $0.0000
    (Notifications not allowed yet: click to ask the browser)
    Ready 0% ↑0 ↓0 ($4.20/$5.00)
    (Notifications not allowed yet: click to ask the browser)
    <button class="budget cost link status-item warm">
    Ready 0% ↑0 ↓0 ($5.50/$5.00) (Goal 3/20) (Resumes 14:05)
    (Notifications not allowed yet: click to ask the browser)
    <button class="budget cost hot link status-item">
    <button class="goal link status-item warm">
    <button class="link resume status-item warm">
    (Resumes 16:05)
    |}];
  H.act h (Run "/goal");
  toasts h;
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 5100))
    Goal: all tests pass — active, 3 of 20 automatic continuations used; /goal off clears it
    |}]
;;

let%expect_test "/budget shows, sets and clears the budget" =
  let h = H.create () in
  run h "/budget";
  toasts h;
  run h "/budget $2.50";
  H.reply h "set_budget" "{}";
  toasts h;
  run h "/budget off";
  H.reply h "set_budget" "{}";
  run h "/budget lots";
  toasts h;
  [%expect
    {|
    (Save_history (/budget))
    (Expire_toast (id 0) (after_ms 5780))
    No budget ($0.00 spent): /budget DOLLARS stops the agent starting turns once the session has cost that much
    (Save_history ("/budget $2.50" /budget))
    (Rpc (method_ set_budget) (params ((usd 2.5)))
     (tag
      (Notice
       "Budget set to $2.50: no turn starts once the session has cost that")))
    (Expire_toast (id 1) (after_ms 4140))
    No budget ($0.00 spent): /budget DOLLARS stops the agent starting turns once the session has cost that much
    Budget set to $2.50: no turn starts once the session has cost that
    (Save_history ("/budget off" "/budget $2.50" /budget))
    (Rpc (method_ set_budget) (params ((usd null)))
     (tag (Notice "Budget removed: turns start whatever it costs")))
    (Expire_toast (id 2) (after_ms 3300))
    (Save_history ("/budget lots" "/budget off" "/budget $2.50" /budget))
    Budget removed: turns start whatever it costs
    "lots" is not a budget: /budget DOLLARS (e.g. /budget 5) or /budget off
    |}]
;;

let%expect_test
    "/goal sets, shows, clears and resumes the goal; refused, it goes back \
     into the editor"
  =
  let h = H.create () in
  run h "/goal";
  toasts h;
  run h "/goal all tests pass";
  H.fail
    h
    "set_goal"
    "the session's budget of $1.00 is spent ($1.20 so far): /budget 3 raises \
     it, /budget off removes it";
  print_s [%sexp ((H.model h).draft : string)];
  H.quiet h (fun () -> H.act h (Set_draft ""));
  run h "/goal off";
  H.reply h "clear_goal" "{}";
  toasts h;
  H.event
    h
    (state
       [ ( "goal"
         , goal
             ~status:"stopped"
             ~reason:"it used all 20 automatic continuations"
             "all tests pass" )
       ]);
  run h "/goal";
  run h "/goal resume";
  H.reply h "resume_goal" "{}";
  toasts h;
  [%expect
    {|
    (Save_history (/goal))
    (Expire_toast (id 0) (after_ms 4700))
    No goal: /goal TEXT sets one, and the agent keeps working until it judges it met
    (Save_history ("/goal all tests pass" /goal))
    (Rpc (method_ set_goal) (params ((text "all tests pass")))
     (tag (Sent (text "/goal all tests pass") (images ()))))
    "/goal all tests pass"
    (Save_history ("/goal off" "/goal all tests pass" /goal))
    (Rpc (method_ clear_goal) (params ()) (tag (Notice "Goal cleared")))
    (Expire_toast (id 2) (after_ms 2500))
    No goal: /goal TEXT sets one, and the agent keeps working until it judges it met
    Goal cleared
    (Save_history (/goal "/goal off" "/goal all tests pass" /goal))
    (Expire_toast (id 3) (after_ms 6000))
    (Save_history
     ("/goal resume" /goal "/goal off" "/goal all tests pass" /goal))
    (Rpc (method_ resume_goal) (params ()) (tag (Notice "Goal resumed")))
    (Expire_toast (id 4) (after_ms 2500))
    Goal: all tests pass — stopped (it used all 20 automatic continuations); /goal resume continues it, /goal off clears it
    Goal resumed
    |}]
;;

let%expect_test "/resume shows, runs now or cancels the automatic resume" =
  let h = H.create () in
  run h "/resume";
  H.event h (state [ "resume_at_ms", `Number "1791295500000" ]);
  run h "/resume";
  toasts h;
  run h "/resume now";
  H.reply h "resume_now" "{}";
  toasts h;
  run h "/resume cancel";
  H.reply h "cancel_resume" "{}";
  run h "/resume later";
  toasts h;
  [%expect
    {|
    (Save_history (/resume))
    (Expire_toast (id 0) (after_ms 5500))
    (Save_history (/resume))
    (Expire_toast (id 1) (after_ms 5980))
    No resume is scheduled: one is when a run stops at a usage limit whose reset time the provider gives
    The run stopped at a usage limit and resumes at 14:05: /resume now retries at once, /resume cancel stays stopped
    (Save_history ("/resume now" /resume))
    (Rpc (method_ resume_now) (params ()) (tag (Notice "Resuming now")))
    (Expire_toast (id 2) (after_ms 2500))
    The run stopped at a usage limit and resumes at 14:05: /resume now retries at once, /resume cancel stays stopped
    Resuming now
    (Save_history ("/resume cancel" "/resume now" /resume))
    (Rpc (method_ cancel_resume) (params ())
     (tag (Notice "Automatic resume cancelled: send a message to retry")))
    (Expire_toast (id 3) (after_ms 3540))
    (Save_history ("/resume later" "/resume cancel" "/resume now" /resume))
    Automatic resume cancelled: send a message to retry
    Unknown /resume argument "later": /resume now retries at once, /resume cancel stays stopped.
    |}]
;;

let%expect_test "completion offers the keywords, but not over a goal's text" =
  let h = H.create () in
  let complete text =
    H.quiet h (fun () -> H.type_ h text);
    H.text h ~selector:".popup"
  in
  complete "/goal ";
  complete "/goal re";
  complete "/goal refactor";
  complete "/resume ";
  complete "/budget ";
  [%expect
    {|
    Goal ↑↓ Tab Enter Esc
    off clear the goal
    resume make a stopped goal active
    Goal ↑↓ Tab Enter Esc
    resume make a stopped goal active
    Automatic resume ↑↓ Tab Enter Esc
    now retry the run at once
    cancel stay stopped until you send a message
    Budget ↑↓ Tab Enter Esc
    off no budget: turns start whatever it costs
    |}]
;;
