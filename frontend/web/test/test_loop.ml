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
  H.text h ~selector:".context-card .cost";
  H.text h ~selector:".status-notes";
  H.event h (state [ "budget_usd", `Number "5"; "cost_usd", `Number "4.2" ]);
  H.text h ~selector:".context-card .cost";
  H.text h ~selector:".status-notes";
  H.elements h ~selector:".context-card .cost";
  H.event
    h
    (state
       [ "budget_usd", `Number "5"
       ; "cost_usd", `Number "5.5"
       ; "goal", goal "all tests pass"
       ; "resume_at_ms", `Number "1791295500000"
       ]);
  H.text h ~selector:".context-card .cost";
  H.text h ~selector:".status-notes";
  List.iter
    [ ".context-card .cost"; ".status-notes .goal"; ".status-notes .resume" ]
    ~f:(fun selector -> H.elements h ~selector);
  (* In the page's time zone. *)
  H.act h (Set_utc_offset (Time_ns.Span.of_hr 2.));
  H.text h ~selector:".status-notes .resume";
  [%expect
    {|
    This session $4.20 · $4.20/$5.00
    <div class="cost status-detail warm">
    This session $5.50 · $5.50/$5.00
    (Goal 3/20) (Resumes 14:05)
    <div class="cost hot status-detail">
    <button class="[&>svg]:pointer-events-none [&>svg]:size-3! [a]:hover:bg-muted [a]:hover:text-muted-foreground aria-invalid:border-destructive aria-invalid:ring-destructive/20 border border-primary/40 cursor-pointer dark:aria-invalid:ring-destructive/40 focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 font-medium gap-1 goal group/badge h-5 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center link loon max-w-[24ch] narrow:h-11 narrow:text-sm overflow-hidden px-2 py-0.5 rounded-4xl shrink-0 status-item text-primary text-xs transition-all w-fit warm whitespace-nowrap">
    <button class="[&>svg]:pointer-events-none [&>svg]:size-3! [a]:hover:bg-muted [a]:hover:text-muted-foreground aria-invalid:border-destructive aria-invalid:ring-destructive/20 border border-primary/40 cursor-pointer dark:aria-invalid:ring-destructive/40 focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 font-medium gap-1 group/badge h-5 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center link loon max-w-[24ch] narrow:h-11 narrow:text-sm overflow-hidden px-2 py-0.5 resume rounded-4xl shrink-0 status-item text-primary text-xs transition-all w-fit warm whitespace-nowrap">
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
    (Rpc (method_ push_pref) (params ((key history) (value /budget) (max 500)))
     (tag Ignore))
    (Expire_toast (id 0) (after_ms 5780))
    No budget ($0.00 spent): /budget DOLLARS stops the agent starting turns once the session has cost that much
    (Rpc (method_ push_pref)
     (params ((key history) (value "/budget $2.50") (max 500))) (tag Ignore))
    (Rpc (method_ set_budget) (params ((usd 2.5)))
     (tag
      (Notice
       "Budget set to $2.50: no turn starts once the session has cost that")))
    (Expire_toast (id 1) (after_ms 4140))
    No budget ($0.00 spent): /budget DOLLARS stops the agent starting turns once the session has cost that much
    Budget set to $2.50: no turn starts once the session has cost that
    (Rpc (method_ push_pref)
     (params ((key history) (value "/budget off") (max 500))) (tag Ignore))
    (Rpc (method_ set_budget) (params ((usd null)))
     (tag (Notice "Budget removed: turns start whatever it costs")))
    (Expire_toast (id 2) (after_ms 3300))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/budget lots") (max 500))) (tag Ignore))
    Budget removed: turns start whatever it costs
    "lots" is not a budget: /budget DOLLARS (e.g. /budget 5) or /budget off (Dismiss)
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
    (Rpc (method_ push_pref) (params ((key history) (value /goal) (max 500)))
     (tag Ignore))
    (Expire_toast (id 0) (after_ms 4700))
    No goal: /goal TEXT sets one, and the agent keeps working until it judges it met
    (Rpc (method_ push_pref)
     (params ((key history) (value "/goal all tests pass") (max 500)))
     (tag Ignore))
    (Rpc (method_ set_goal) (params ((text "all tests pass")))
     (tag (Sent (text "/goal all tests pass") (images ()))))
    "/goal all tests pass"
    (Rpc (method_ push_pref)
     (params ((key history) (value "/goal off") (max 500))) (tag Ignore))
    (Rpc (method_ clear_goal) (params ()) (tag (Notice "Goal cleared")))
    (Expire_toast (id 2) (after_ms 2500))
    No goal: /goal TEXT sets one, and the agent keeps working until it judges it met
    Goal cleared
    (Rpc (method_ push_pref) (params ((key history) (value /goal) (max 500)))
     (tag Ignore))
    (Expire_toast (id 3) (after_ms 6000))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/goal resume") (max 500))) (tag Ignore))
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
    (Rpc (method_ push_pref) (params ((key history) (value /resume) (max 500)))
     (tag Ignore))
    (Expire_toast (id 0) (after_ms 6000))
    (Rpc (method_ push_pref) (params ((key history) (value /resume) (max 500)))
     (tag Ignore))
    (Expire_toast (id 1) (after_ms 6000))
    No resume is scheduled: one is when a run stops at a usage limit whose reset time the provider gives; runs stopped by a usage limit resume by themselves when it resets (/resume off keeps them stopped)
    The run stopped at a usage limit and resumes at 14:05: /resume now retries at once, /resume cancel stays stopped; runs stopped by a usage limit resume by themselves when it resets (/resume off keeps them stopped)
    (Rpc (method_ push_pref)
     (params ((key history) (value "/resume now") (max 500))) (tag Ignore))
    (Rpc (method_ resume_now) (params ()) (tag (Notice "Resuming now")))
    (Expire_toast (id 2) (after_ms 2500))
    The run stopped at a usage limit and resumes at 14:05: /resume now retries at once, /resume cancel stays stopped; runs stopped by a usage limit resume by themselves when it resets (/resume off keeps them stopped)
    Resuming now
    (Rpc (method_ push_pref)
     (params ((key history) (value "/resume cancel") (max 500))) (tag Ignore))
    (Rpc (method_ cancel_resume) (params ())
     (tag (Notice "Automatic resume cancelled: send a message to retry")))
    (Expire_toast (id 3) (after_ms 3540))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/resume later") (max 500))) (tag Ignore))
    Automatic resume cancelled: send a message to retry
    Unknown /resume argument "later": now retries at once, cancel stays stopped, on|off turns the automatic resume after a usage limit on or off; usage: /resume [now|cancel|on|off] (Dismiss)
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
    Goal ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    off clear the goal
    resume make a stopped goal active
    Goal ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    resume make a stopped goal active
    Automatic resume ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    now retry the run at once
    cancel stay stopped until you send a message
    on resume runs by themselves when a usage limit resets
    off never resume by themselves after a usage limit
    Budget ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    off no budget: turns start whatever it costs
    |}]
;;
