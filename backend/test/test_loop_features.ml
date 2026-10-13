open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

(* The agent-level parts of the loop features: compaction during a run, the
   session budget, [/goal] and resuming after a usage limit. *)

let iso_re = Re.compile (Re.Perl.re {|\d{4}-\d{2}-\d{2}T[\d:.]+Z|})

let usage ?(input = 100) ?(output = 10) () =
  { Usage.input; output; cache_read = 0 }
;;

let call ?input ?output id size =
  { (Reply.tool_call
       ~id
       ~name:"filler"
       ~arguments:(sprintf {|{"size":%d}|} size)
       ())
    with
    usage = usage ?input ?output ()
  }
;;

let text ?input ?output s =
  { (Reply.text s) with usage = usage ?input ?output () }
;;

let last_user (r : Provider.Request.t) =
  List.find_map (List.rev r.messages) ~f:(function
    | Message.User u -> Some u.text
    | Assistant _ | Tool_result _ -> None)
  |> Option.value ~default:""
;;

let print_request (r : Provider.Request.t) =
  match r.system with
  | Some system when String.is_prefix system ~prefix:"Summarise" ->
    print_endline "summarise"
  | _ ->
    printf
      "request: %d messages, last user %S%s\n"
      (List.length r.messages)
      (String.prefix (List.hd_exn (String.split_lines (last_user r))) 60)
      (if List.exists r.tools ~f:(fun t -> String.equal t.name Goal.tool_name)
       then " (+goal_complete)"
       else "")
;;

let with_agent
      ?(config = [])
      ?(tools = [ filler ])
      ?max_budget_usd
      ?sleep_until
      ?provider
      ?session
      replies
      f
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  write t ".prigh/config.json" (Jsonaf.to_string (`Object config));
  let provider =
    match provider with
    | Some p -> p
    | None -> Faux_provider.create ~on_request:print_request replies
  in
  let make ?session () =
    let agent =
      Agent.create
        ~env:t.env
        ~sw
        ~provider
        ~tools
        ~sessions_dir:(Filename.concat t.dir "sessions")
        ~home:t.dir
        ?max_budget_usd
        ?sleep_until
        ?session
        ~cwd:t.dir
        ()
    in
    Agent.subscribe agent ~f:(fun event ->
      let line =
        match event with
        | Notice n -> Some ("notice: " ^ n)
        | Compacted { summary } -> Some ("compacted: " ^ summary)
        | Loop (Message_end (Assistant a)) ->
          Some
            (sprintf
               "assistant: %s%s"
               (Message.Assistant.text a)
               (match a.stop_reason with
                | Error e -> " ERROR " ^ e
                | _ -> ""))
        | Loop (Tool_end { call; result }) ->
          Some (sprintf "tool %s: %s" call.name (String.prefix result.text 80))
        | _ -> None
      in
      Option.iter line ~f:(fun l ->
        print_endline (Re.replace_string iso_re ~by:"<time>" (mask t l))));
    agent
  in
  f t (make ?session:(Option.map session ~f:(fun f -> f t)) ()) make
;;

let show_state agent =
  let s = Agent.state agent in
  print_s
    [%sexp
      { running = (s.running : bool)
      ; cost_usd = (Float.round_decimal s.cost_usd ~decimal_digits:4 : float)
      ; budget_usd = (s.budget_usd : float option)
      ; goal = (s.goal : Goal.t option)
      ; resume_scheduled = (Option.is_some s.resume_at : bool)
      }]
;;

(* ---- compaction during a run ------------------------------------------ *)

let%expect_test
    "the main agent compacts mid-run into its session and carries on"
  =
  with_agent
    [ call "c1" 40_000
    ; call ~input:900_000 "c2" 100
    ; text "SUMMARY"
    ; text "done"
    ]
  @@ fun _t agent _make ->
  let states = ref [] in
  Agent.subscribe agent ~f:(function
    | State_changed s -> states := s.running :: !states
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "the task");
  Agent.wait_idle agent;
  (* It never looked idle in between. *)
  print_s [%sexp (List.rev !states : bool list)];
  let session = Agent.session agent in
  let reloaded = Or_error.ok_exn (Session.load (Session.path session)) in
  print_s
    [%sexp
      (List.map (Session.messages reloaded) ~f:(fun m ->
         match m with
         | User u -> "U " ^ String.prefix u.text 40
         | Assistant a -> "A " ^ Message.Assistant.text a
         | Tool_result r -> "T " ^ r.tool_call_id)
       : string list)];
  print_s
    [%sexp
      (List.filter_map (Session.active_path reloaded) ~f:(fun e ->
         match e.payload with
         | Compaction { summary; pinned; _ } ->
           Some (summary, Option.is_some pinned)
         | _ -> None)
       : (string * bool) list)];
  [%expect
    {|
    request: 1 messages, last user "the task"
    assistant:
    tool filler: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    request: 3 messages, last user "the task"
    assistant:
    tool filler: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    summarise
    compacted: SUMMARY
    request: 4 messages, last user "Summary of the conversation so far:"
    assistant: done
    (true true false)
    ("U the task"  "U Summary of the conversation so far:\
                  \nSUMM"
     "A " "T c2" "A done")
    ((SUMMARY true))
    |}]
;;

let%expect_test "an overflowing request in a session is compacted and retried" =
  with_agent
    [ call "c1" 40_000
    ; call "c2" 100
    ; Reply.context_overflow ()
    ; text "SUMMARY"
    ; text "fits now"
    ]
  @@ fun _t agent _make ->
  Or_error.ok_exn (Agent.prompt agent "the task");
  Agent.wait_idle agent;
  (* The failed reply is not part of the conversation. *)
  print_s
    [%sexp
      (List.count (Agent.messages agent) ~f:(function
         | Assistant { stop_reason = Error _; _ } -> true
         | _ -> false)
       : int)];
  [%expect
    {|
    request: 1 messages, last user "the task"
    assistant:
    tool filler: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    request: 3 messages, last user "the task"
    assistant:
    tool filler: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    request: 5 messages, last user "the task"
    summarise
    compacted: SUMMARY
    request: 4 messages, last user "Summary of the conversation so far:"
    assistant: fits now
    0
    |}]
;;

(* ---- budget ------------------------------------------------------------ *)

let%expect_test "the budget stops a run between turns and refuses prompts" =
  (* deepseek-flash: $1.20 per million output tokens. *)
  with_agent
    [ call ~output:250_000 "c1" 10
    ; call ~output:250_000 "c2" 10
    ; call ~output:250_000 "c3" 10
    ; text "done"
    ]
  @@ fun t agent make ->
  Or_error.ok_exn (Agent.set_budget agent (Some 0.5));
  Or_error.ok_exn (Agent.prompt agent "spend");
  Agent.wait_idle agent;
  show_state agent;
  print_s [%sexp (Agent.prompt agent "more" : unit Or_error.t)];
  print_s [%sexp (Agent.follow_up agent "more" : unit Or_error.t)];
  print_s [%sexp (Agent.set_budget agent (Some 0.) : unit Or_error.t)];
  (* Recorded in the session. *)
  let reloaded = make ~session:(Agent.session agent) () in
  show_state reloaded;
  Or_error.ok_exn (Agent.set_budget agent (Some 2.));
  Or_error.ok_exn (Agent.prompt agent "more");
  Agent.wait_idle agent;
  Or_error.ok_exn (Agent.set_budget agent None);
  show_state agent;
  ignore t;
  [%expect
    {|
    request: 1 messages, last user "spend"
    assistant:
    tool filler: xxxxxxxxxx
    request: 3 messages, last user "spend"
    assistant:
    tool filler: xxxxxxxxxx
    notice: the session's budget of $0.50 is spent ($0.60 so far): /budget 2 raises it, /budget off removes it
    ((running false) (cost_usd 0.6001) (budget_usd (0.5)) (goal ())
     (resume_scheduled false))
    (Error
     "the session's budget of $0.50 is spent ($0.60 so far): /budget 2 raises it, /budget off removes it")
    (Error
     "the session's budget of $0.50 is spent ($0.60 so far): /budget 2 raises it, /budget off removes it")
    (Error
     "a budget must be a positive number of dollars, e.g. /budget 5 (or /budget off)")
    ((running false) (cost_usd 0.6001) (budget_usd (0.5)) (goal ())
     (resume_scheduled false))
    request: 6 messages, last user "more"
    assistant:
    tool filler: xxxxxxxxxx
    request: 8 messages, last user "more"
    assistant: done
    ((running false) (cost_usd 0.9001) (budget_usd ()) (goal ())
     (resume_scheduled false))
    |}]
;;

let%expect_test
    "the default budget: the CLI's over the config's, /budget off over both"
  =
  with_agent ~config:[ "max_budget_usd", `Number "3" ] []
  @@ fun _t agent _make ->
  show_state agent;
  [%expect
    {|
    ((running false) (cost_usd 0) (budget_usd (3)) (goal ())
     (resume_scheduled false))
    |}];
  with_agent ~config:[ "max_budget_usd", `Number "3" ] ~max_budget_usd:1.5 []
  @@ fun _t agent _make ->
  show_state agent;
  Or_error.ok_exn (Agent.set_budget agent None);
  show_state agent;
  [%expect
    {|
    ((running false) (cost_usd 0) (budget_usd (1.5)) (goal ())
     (resume_scheduled false))
    ((running false) (cost_usd 0) (budget_usd ()) (goal ())
     (resume_scheduled false))
    |}]
;;

let%expect_test
    "a subagent's spend counts while it runs, the budget stops it, and it is \
     recorded in the session"
  =
  let provider =
    Routed_provider.create
      ~main:
        [ Reply.tool_call
            ~id:"s1"
            ~name:"subagent"
            ~arguments:{|{"task":"dig"}|}
            ()
        ; text "waiting for it"
        ]
      [ ( "dig"
        , [ call ~output:500_000 "c1" 10
          ; call ~output:500_000 "c2" 10
          ; text "dug"
          ] )
      ]
  in
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let subagent =
    Tool_subagent.create
      ~provider
      ~current_model:(fun () -> Model.default)
      ~current_thinking:(fun () -> Thinking.Off)
      ~home:t.dir
      ()
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:([ filler; subagent ] @ Tool_subagent.control_tools)
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:t.dir
      ()
  in
  Agent.subscribe agent ~f:(function
    | Notice n -> print_endline ("notice: " ^ n)
    | Loop (Message_end (User u))
      when String.is_prefix u.text ~prefix:"[subagent" ->
      print_endline (List.hd_exn (String.split_lines u.text))
    | _ -> ());
  Or_error.ok_exn (Agent.set_budget agent (Some 0.5));
  Or_error.ok_exn (Agent.prompt agent "delegate");
  Agent.wait_idle agent;
  let cost agent =
    print_s
      [%sexp
        (Float.round_decimal (Agent.state agent).cost_usd ~decimal_digits:4
         : float)]
  in
  cost agent;
  (* As after a backend restart: the session read back from its file. *)
  let reloaded =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:[]
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~session:
        (Or_error.ok_exn (Session.load (Session.path (Agent.session agent))))
      ~cwd:t.dir
      ()
  in
  cost reloaded;
  print_s [%sexp (Agent.prompt reloaded "more" : unit Or_error.t)];
  [%expect
    {|
    [subagent a1 failed] dig
    notice: the session's budget of $0.50 is spent ($0.60 so far): /budget 2 raises it, /budget off removes it
    0.6001
    0.6001
    (Error
     "the session's budget of $0.50 is spent ($0.60 so far): /budget 2 raises it, /budget off removes it")
    |}]
;;

(* ---- goal -------------------------------------------------------------- *)

let complete summary =
  Reply.tool_call
    ~id:"g1"
    ~name:Goal.tool_name
    ~arguments:(sprintf {|{"summary":%S}|} summary)
    ()
;;

let%expect_test "a goal keeps the agent working until it marks it complete" =
  with_agent
    [ text "step one"
    ; text "step two"
    ; complete "all tests pass"
    ; text "finished"
    ]
  @@ fun _t agent make ->
  Or_error.ok_exn (Agent.set_goal agent "all tests pass");
  Agent.wait_idle agent;
  show_state agent;
  (* Recorded in the session. *)
  show_state (make ~session:(Agent.session agent) ());
  [%expect
    {|
    request: 1 messages, last user "[prigh goal] Work on this goal until it is fully achieved:" (+goal_complete)
    assistant: step one
    request: 3 messages, last user "[prigh goal: continuation 1 of 20] Continue working toward t" (+goal_complete)
    assistant: step two
    request: 5 messages, last user "[prigh goal: continuation 2 of 20] Continue working toward t" (+goal_complete)
    assistant:
    notice: goal complete: all tests pass
    tool goal_complete: goal marked complete
    request: 7 messages, last user "[prigh goal: continuation 2 of 20] Continue working toward t" (+goal_complete)
    assistant: finished
    ((running false) (cost_usd 0.0001) (budget_usd ())
     (goal
      (((text "all tests pass") (status (Done (summary "all tests pass")))
        (continuations 2))))
     (resume_scheduled false))
    ((running false) (cost_usd 0.0001) (budget_usd ())
     (goal
      (((text "all tests pass") (status (Done (summary "all tests pass")))
        (continuations 2))))
     (resume_scheduled false))
    |}]
;;

let%expect_test
    "a goal stops after its automatic continuations; /goal resume continues"
  =
  with_agent
    ~config:[ "goal_max_continuations", `Number "2" ]
    [ text "a"; text "b"; text "c"; complete "done"; text "ok" ]
  @@ fun _t agent _make ->
  Or_error.ok_exn (Agent.set_goal agent "ship it");
  Agent.wait_idle agent;
  show_state agent;
  print_s [%sexp (Agent.resume_goal agent : unit Or_error.t)];
  Agent.wait_idle agent;
  show_state agent;
  print_s [%sexp (Agent.resume_goal agent : unit Or_error.t)];
  [%expect
    {|
    request: 1 messages, last user "[prigh goal] Work on this goal until it is fully achieved:" (+goal_complete)
    assistant: a
    request: 3 messages, last user "[prigh goal: continuation 1 of 2] Continue working toward th" (+goal_complete)
    assistant: b
    request: 5 messages, last user "[prigh goal: continuation 2 of 2] Continue working toward th" (+goal_complete)
    assistant: c
    notice: goal stopped: it used all 2 automatic continuations; /goal resume continues it
    ((running false) (cost_usd 0.0001) (budget_usd ())
     (goal
      (((text "ship it")
        (status (Stopped (reason "it used all 2 automatic continuations")))
        (continuations 2))))
     (resume_scheduled false))
    request: 7 messages, last user "[prigh goal] Work on this goal until it is fully achieved:" (+goal_complete)
    (Ok ())
    assistant:
    notice: goal complete: done
    tool goal_complete: goal marked complete
    request: 9 messages, last user "[prigh goal] Work on this goal until it is fully achieved:" (+goal_complete)
    assistant: ok
    ((running false) (cost_usd 0.0002) (budget_usd ())
     (goal (((text "ship it") (status (Done (summary done))) (continuations 0))))
     (resume_scheduled false))
    request: 11 messages, last user "[prigh goal] Work on this goal until it is fully achieved:" (+goal_complete)
    assistant:  ERROR faux provider: no scripted reply
    notice: goal stopped: the run failed: faux provider: no scripted reply; /goal resume continues it
    (Ok ())
    |}]
;;

let%expect_test "a failed run, an abort or the budget stop a goal" =
  with_agent [ Reply.error "HTTP 400: bad request" ]
  @@ fun _t agent _make ->
  Or_error.ok_exn (Agent.set_goal agent "x");
  Agent.wait_idle agent;
  show_state agent;
  [%expect
    {|
    request: 1 messages, last user "[prigh goal] Work on this goal until it is fully achieved:" (+goal_complete)
    assistant:  ERROR HTTP 400: bad request
    notice: goal stopped: the run failed: HTTP 400: bad request; /goal resume continues it
    ((running false) (cost_usd 0) (budget_usd ())
     (goal
      (((text x)
        (status (Stopped (reason "the run failed: HTTP 400: bad request")))
        (continuations 0))))
     (resume_scheduled false))
    |}];
  with_agent
    ~tools:Tools.all
    [ Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"sleep 5"}|}
        ()
    ]
  @@ fun t agent _make ->
  Or_error.ok_exn (Agent.set_goal agent "y");
  Eio.Time.sleep (Eio.Stdenv.clock t.env) 0.3;
  ignore (Agent.abort agent : string list);
  Agent.wait_idle agent;
  show_state agent;
  [%expect
    {|
    request: 1 messages, last user "[prigh goal] Work on this goal until it is fully achieved:" (+goal_complete)
    assistant:
    tool bash: [cancelled]
    notice: goal stopped: the run was aborted; /goal resume continues it
    ((running false) (cost_usd 0) (budget_usd ())
     (goal
      (((text y) (status (Stopped (reason "the run was aborted")))
        (continuations 0))))
     (resume_scheduled false))
    |}];
  with_agent [ text ~output:500_000 "expensive" ]
  @@ fun _t agent _make ->
  Or_error.ok_exn (Agent.set_budget agent (Some 0.5));
  Or_error.ok_exn (Agent.set_goal agent "z");
  Agent.wait_idle agent;
  show_state agent;
  print_s [%sexp (Agent.set_goal agent "again" : unit Or_error.t)];
  [%expect
    {|
    request: 1 messages, last user "[prigh goal] Work on this goal until it is fully achieved:" (+goal_complete)
    assistant: expensive
    notice: goal stopped: the session's budget is spent; /goal resume continues it
    ((running false) (cost_usd 0.6) (budget_usd (0.5))
     (goal
      (((text z) (status (Stopped (reason "the session's budget is spent")))
        (continuations 0))))
     (resume_scheduled false))
    (Error
     "the session's budget of $0.50 is spent ($0.60 so far): /budget 2 raises it, /budget off removes it")
    |}]
;;

let%expect_test "/goal off; queued messages come before a continuation" =
  with_agent [ text "first"; text "for the follow-up"; text "continued" ]
  @@ fun _t agent _make ->
  print_s [%sexp (Agent.set_goal agent "  " : unit Or_error.t)];
  print_s [%sexp (Agent.clear_goal agent : unit Or_error.t)];
  Or_error.ok_exn (Agent.set_goal agent "g");
  Or_error.ok_exn (Agent.follow_up agent "a question");
  Agent.wait_idle agent;
  (* The script ran out: the second continuation's request failed. *)
  Or_error.ok_exn (Agent.clear_goal agent);
  show_state agent;
  [%expect
    {|
    (Error
     "say what done means, e.g. /goal all tests pass (/goal off clears it)")
    (Error "there is no goal; /goal TEXT sets one")
    request: 1 messages, last user "[prigh goal] Work on this goal until it is fully achieved:" (+goal_complete)
    assistant: first
    request: 3 messages, last user "a question" (+goal_complete)
    assistant: for the follow-up
    request: 5 messages, last user "[prigh goal: continuation 1 of 20] Continue working toward t" (+goal_complete)
    assistant: continued
    request: 7 messages, last user "[prigh goal: continuation 2 of 20] Continue working toward t" (+goal_complete)
    assistant:  ERROR faux provider: no scripted reply
    notice: goal stopped: the run failed: faux provider: no scripted reply; /goal resume continues it
    ((running false) (cost_usd 0.0001) (budget_usd ()) (goal ())
     (resume_scheduled false))
    |}]
;;

let%expect_test "subagents never get goal_complete" =
  let tools =
    Or_error.ok_exn
      (Tools.for_context
         ~parent:
           [ filler; Tool_goal.create ~complete:(fun ~summary:_ -> Ok ()) ]
         ~depth:1
         ())
  in
  print_s [%sexp (List.map tools ~f:Tool.name : string list)];
  [%expect {| (filler) |}]
;;

(* ---- resuming after a usage limit -------------------------------------- *)

let%expect_test "reset times from bodies and headers" =
  let now = 1_000_000. in
  let show ?(headers = []) ?(body = "") () =
    print_s
      [%sexp
        (Option.map (Usage_limit.reset_time ~now ~headers ~body) ~f:(fun at ->
           at -. now)
         : float option)]
  in
  show
    ~body:
      {|{"error":{"type":"usage_limit_reached","resets_at":1003600,"message":"limit"}}|}
    ();
  show
    ~body:{|{"error":{"type":"usage_limit_reached","resets_in_seconds":90}}|}
    ();
  show
    ~headers:
      [ "x-codex-primary-used-percent", "100"
      ; "x-codex-primary-reset-at", "1000300"
      ; "x-codex-secondary-used-percent", "40"
      ; "x-codex-secondary-reset-at", "1100000"
      ]
    ();
  show
    ~headers:
      [ "x-codex-primary-reset-at", "1000300"
      ; "x-codex-secondary-reset-at", "1100000"
      ]
    ();
  show ~headers:[ "Anthropic-Ratelimit-Unified-Reset", "1000600" ] ();
  show
    ~headers:
      [ "anthropic-ratelimit-unified-5h-reset", "1000600"
      ; "anthropic-ratelimit-unified-5h-status", "allowed"
      ; "anthropic-ratelimit-unified-7d-reset", "1500000"
      ; "anthropic-ratelimit-unified-7d-status", "rejected"
      ]
    ();
  show ~headers:[ "retry-after", "120" ] ();
  show ~body:"<html>busy</html>" ();
  [%expect
    {|
    (3600)
    (90)
    (300)
    (100000)
    (600)
    (500000)
    (120)
    ()
    |}];
  let message =
    Sse_request.error_message_of_body
      ~resets_at:1_760_191_500.
      ~status:429
      {|{"error":{"type":"usage_limit_reached","message":"The usage limit has been reached"}}|}
  in
  print_endline message;
  print_s [%sexp (Usage_limit.resets_at message : float option)];
  print_s [%sexp (Usage_limit.resets_at "HTTP 429: slow down" : float option)];
  [%expect
    {|
    HTTP 429: The usage limit has been reached (usage limit reached) (resets at 2025-10-11T14:05:00Z)
    (1760191500)
    ()
    |}]
;;

(* [sleep_until] waits on a promise the test resolves, standing in for the
   clock. *)
let manual_clock () =
  let waiting = ref None in
  let sleep_until at =
    let p, u = Eio.Promise.create () in
    waiting := Some (at, u);
    Eio.Promise.await p
  in
  let fire () =
    match !waiting with
    | Some (_, u) ->
      waiting := None;
      Eio.Promise.resolve u ()
    | None -> print_endline "nothing waiting"
  in
  sleep_until, fire
;;

let%expect_test "a run stopped by a usage limit resumes when it resets" =
  let sleep_until, fire = manual_clock () in
  let resets_at = Core_unix.gettimeofday () +. 3630. in
  with_agent
    ~sleep_until
    [ call "c1" 10; Reply.usage_limit ~resets_at (); text "carried on" ]
  @@ fun t agent _make ->
  Or_error.ok_exn (Agent.prompt agent "work");
  Agent.wait_idle agent;
  show_state agent;
  (* To the second. *)
  print_s
    [%sexp
      (Float.( < )
         (Float.abs
            (Option.value_exn (Agent.state agent).resume_at -. resets_at))
         1.
       : bool)];
  fire ();
  Eio.Time.sleep (Eio.Stdenv.clock t.env) 0.05;
  Agent.wait_idle agent;
  show_state agent;
  [%expect
    {|
    request: 1 messages, last user "work"
    assistant:
    tool filler: xxxxxxxxxx
    request: 3 messages, last user "work"
    assistant:  ERROR HTTP 429: The usage limit has been reached (usage limit reached) (resets at <time>)
    notice: deepseek/deepseek-flash: the usage limit is reached; the run resumes when it resets at <time> (in 1h 01m): /resume now retries at once, /resume cancel stays stopped
    ((running false) (cost_usd 0) (budget_usd ()) (goal ())
     (resume_scheduled true))
    true
    request: 5 messages, last user "[prigh: the usage limit has reset. Carry on with the interru"
    assistant: carried on
    ((running false) (cost_usd 0.0001) (budget_usd ()) (goal ())
     (resume_scheduled false))
    |}]
;;

let%expect_test
    "a reset time already past backs off rather than retrying at once"
  =
  let sleep_until, fire = manual_clock () in
  let resets_at = Core_unix.gettimeofday () -. 10. in
  with_agent
    ~sleep_until
    [ Reply.usage_limit ~resets_at ()
    ; Reply.usage_limit ~resets_at ()
    ; text "carried on"
    ]
  @@ fun t agent _make ->
  let wait () =
    let at = Option.value_exn (Agent.state agent).resume_at in
    printf "resume in %.0fs\n" (at -. Core_unix.gettimeofday ())
  in
  Or_error.ok_exn (Agent.prompt agent "work");
  Agent.wait_idle agent;
  wait ();
  fire ();
  Eio.Time.sleep (Eio.Stdenv.clock t.env) 0.05;
  Agent.wait_idle agent;
  wait ();
  fire ();
  Eio.Time.sleep (Eio.Stdenv.clock t.env) 0.05;
  Agent.wait_idle agent;
  show_state agent;
  [%expect
    {|
    request: 1 messages, last user "work"
    assistant:  ERROR HTTP 429: The usage limit has been reached (usage limit reached) (resets at <time>)
    notice: deepseek/deepseek-flash: the usage limit is reached; the run resumes in 1m (the provider says it reset at <time>, which has passed): /resume now retries at once, /resume cancel stays stopped
    resume in 60s
    request: 3 messages, last user "[prigh: the usage limit has reset. Carry on with the interru"
    assistant:  ERROR HTTP 429: The usage limit has been reached (usage limit reached) (resets at <time>)
    notice: deepseek/deepseek-flash: the usage limit is reached; the run resumes in 2m (the provider says it reset at <time>, which has passed): /resume now retries at once, /resume cancel stays stopped
    resume in 120s
    request: 5 messages, last user "[prigh: the usage limit has reset. Carry on with the interru"
    assistant: carried on
    ((running false) (cost_usd 0) (budget_usd ()) (goal ())
     (resume_scheduled false))
    |}]
;;

let%expect_test
    "a scheduled resume is recorded in the session: loaded again it is \
     scheduled again, or dropped once its time has passed"
  =
  let sleep_until, fire = manual_clock () in
  let resets_at = Core_unix.gettimeofday () +. 600. in
  with_agent ~sleep_until [ Reply.usage_limit ~resets_at (); text "carried on" ]
  @@ fun t agent make ->
  Or_error.ok_exn (Agent.prompt agent "work");
  Agent.wait_idle agent;
  let path = Session.path (Agent.session agent) in
  let recorded () =
    List.filter_map
      (Session.entries (Or_error.ok_exn (Session.load path)))
      ~f:(fun e ->
        match e.payload with
        | Resume_at { at } ->
          Some (Option.map at ~f:(fun at -> Float.(abs (at - resets_at) < 1.)))
        | _ -> None)
  in
  print_s [%sexp (recorded () : bool option list)];
  print_s
    [%sexp
      (List.map
         (Session.list ~dir:(Filename.concat t.dir "sessions"))
         ~f:(fun s -> Option.is_some s.resume_at)
       : bool list)];
  (* The backend stops: the file as it was then, without what ending this
     agent's timer records, is loaded by a new one. *)
  let snapshot = In_channel.read_all path in
  Or_error.ok_exn (Agent.cancel_resume agent);
  Out_channel.write_all path ~data:snapshot;
  let reloaded = make ~session:(Or_error.ok_exn (Session.load path)) () in
  show_state reloaded;
  fire ();
  Eio.Time.sleep (Eio.Stdenv.clock t.env) 0.05;
  Agent.wait_idle reloaded;
  show_state reloaded;
  print_s [%sexp (recorded () : bool option list)];
  [%expect
    {|
    request: 1 messages, last user "work"
    assistant:  ERROR HTTP 429: The usage limit has been reached (usage limit reached) (resets at <time>)
    notice: deepseek/deepseek-flash: the usage limit is reached; the run resumes when it resets at <time> (in 10m): /resume now retries at once, /resume cancel stays stopped
    ((true))
    (true)
    ((running false) (cost_usd 0) (budget_usd ()) (goal ())
     (resume_scheduled true))
    request: 3 messages, last user "[prigh: the usage limit has reset. Carry on with the interru"
    assistant: carried on
    ((running false) (cost_usd 0) (budget_usd ()) (goal ())
     (resume_scheduled false))
    ((true) ())
    |}];
  (* A time that passed while nothing ran is dropped. *)
  let session = Or_error.ok_exn (Session.load path) in
  Session.set_resume_at session (Some (Core_unix.gettimeofday () -. 60.));
  let late = make ~session:(Or_error.ok_exn (Session.load path)) () in
  show_state late;
  print_s
    [%sexp
      (Session.resume_at (Or_error.ok_exn (Session.load path)) : float option)];
  [%expect
    {|
    ((running false) (cost_usd 0) (budget_usd ()) (goal ())
     (resume_scheduled false))
    ()
    |}]
;;

let%expect_test "a resume whose session was deleted meanwhile does not run" =
  let sleep_until, fire = manual_clock () in
  let resets_at = Core_unix.gettimeofday () +. 600. in
  with_agent ~sleep_until [ Reply.usage_limit ~resets_at (); text "never" ]
  @@ fun t agent _make ->
  Or_error.ok_exn (Agent.prompt agent "work");
  Agent.wait_idle agent;
  Session.delete_files (Session.path (Agent.session agent));
  fire ();
  Eio.Time.sleep (Eio.Stdenv.clock t.env) 0.05;
  Agent.wait_idle agent;
  show_state agent;
  print_s
    [%sexp
      (Sys_unix.file_exists_exn (Session.path (Agent.session agent)) : bool)];
  [%expect
    {|
    request: 1 messages, last user "work"
    assistant:  ERROR HTTP 429: The usage limit has been reached (usage limit reached) (resets at <time>)
    notice: deepseek/deepseek-flash: the usage limit is reached; the run resumes when it resets at <time> (in 10m): /resume now retries at once, /resume cancel stays stopped
    notice: the usage limit has reset, but this session was deleted, so its run does not resume
    ((running false) (cost_usd 0) (budget_usd ()) (goal ())
     (resume_scheduled false))
    false
    |}]
;;

let%expect_test
    "an automatic resume is cancelled by /resume cancel, an abort or a new \
     prompt; /resume now runs at once"
  =
  let sleep_until, fire = manual_clock () in
  let resets_at = Core_unix.gettimeofday () +. 600. in
  with_agent
    ~sleep_until
    [ Reply.usage_limit ~resets_at ()
    ; Reply.usage_limit ~resets_at ()
    ; Reply.usage_limit ~resets_at ()
    ; text "now"
    ]
  @@ fun t agent _make ->
  Or_error.ok_exn (Agent.prompt agent "one");
  Agent.wait_idle agent;
  Or_error.ok_exn (Agent.cancel_resume agent);
  fire ();
  Eio.Time.sleep (Eio.Stdenv.clock t.env) 0.05;
  show_state agent;
  print_s [%sexp (Agent.cancel_resume agent : unit Or_error.t)];
  Or_error.ok_exn (Agent.prompt agent "two");
  Agent.wait_idle agent;
  ignore (Agent.abort agent : string list);
  show_state agent;
  Or_error.ok_exn (Agent.prompt agent "three");
  Agent.wait_idle agent;
  Or_error.ok_exn (Agent.resume_now agent);
  Agent.wait_idle agent;
  show_state agent;
  [%expect
    {|
    request: 1 messages, last user "one"
    assistant:  ERROR HTTP 429: The usage limit has been reached (usage limit reached) (resets at <time>)
    notice: deepseek/deepseek-flash: the usage limit is reached; the run resumes when it resets at <time> (in 10m): /resume now retries at once, /resume cancel stays stopped
    ((running false) (cost_usd 0) (budget_usd ()) (goal ())
     (resume_scheduled false))
    (Error
     "no run is waiting for a usage limit to reset (one is scheduled when a run stops at a limit whose reset time the provider gives)")
    request: 3 messages, last user "two"
    assistant:  ERROR HTTP 429: The usage limit has been reached (usage limit reached) (resets at <time>)
    notice: deepseek/deepseek-flash: the usage limit is reached; the run resumes when it resets at <time> (in 10m): /resume now retries at once, /resume cancel stays stopped
    notice: the automatic resume is cancelled
    ((running false) (cost_usd 0) (budget_usd ()) (goal ())
     (resume_scheduled false))
    request: 5 messages, last user "three"
    assistant:  ERROR HTTP 429: The usage limit has been reached (usage limit reached) (resets at <time>)
    notice: deepseek/deepseek-flash: the usage limit is reached; the run resumes when it resets at <time> (in 10m): /resume now retries at once, /resume cancel stays stopped
    request: 7 messages, last user "[prigh: the usage limit has reset. Carry on with the interru"
    assistant: now
    ((running false) (cost_usd 0) (budget_usd ()) (goal ())
     (resume_scheduled false))
    |}]
;;

let%expect_test
    "no resume without a reset time, or with resume_on_limit_reset off; a goal \
     waits for the resume"
  =
  with_agent [ Reply.usage_limit () ]
  @@ fun _t agent _make ->
  Or_error.ok_exn (Agent.prompt agent "x");
  Agent.wait_idle agent;
  show_state agent;
  [%expect
    {|
    request: 1 messages, last user "x"
    assistant:  ERROR HTTP 429: The usage limit has been reached (usage limit reached)
    notice: deepseek/deepseek-flash: the usage limit is reached and the provider did not say when it resets, so the run will not resume by itself; send a message to try again, or /model picks another model
    ((running false) (cost_usd 0) (budget_usd ()) (goal ())
     (resume_scheduled false))
    |}];
  let resets_at = Core_unix.gettimeofday () +. 600. in
  with_agent
    ~config:[ "resume_on_limit_reset", `False ]
    [ Reply.usage_limit ~resets_at () ]
  @@ fun _t agent _make ->
  Or_error.ok_exn (Agent.prompt agent "x");
  Agent.wait_idle agent;
  show_state agent;
  [%expect
    {|
    request: 1 messages, last user "x"
    assistant:  ERROR HTTP 429: The usage limit has been reached (usage limit reached) (resets at <time>)
    ((running false) (cost_usd 0) (budget_usd ()) (goal ())
     (resume_scheduled false))
    |}];
  let sleep_until, fire = manual_clock () in
  with_agent
    ~sleep_until
    [ Reply.usage_limit ~resets_at ()
    ; text "resumed"
    ; complete "did it"
    ; text "ok"
    ]
  @@ fun t agent _make ->
  Or_error.ok_exn (Agent.set_goal agent "g");
  Agent.wait_idle agent;
  show_state agent;
  fire ();
  Eio.Time.sleep (Eio.Stdenv.clock t.env) 0.05;
  Agent.wait_idle agent;
  show_state agent;
  [%expect
    {|
    request: 1 messages, last user "[prigh goal] Work on this goal until it is fully achieved:" (+goal_complete)
    assistant:  ERROR HTTP 429: The usage limit has been reached (usage limit reached) (resets at <time>)
    notice: deepseek/deepseek-flash: the usage limit is reached; the run resumes when it resets at <time> (in 10m): /resume now retries at once, /resume cancel stays stopped
    ((running false) (cost_usd 0) (budget_usd ())
     (goal (((text g) (status Active) (continuations 0))))
     (resume_scheduled true))
    request: 3 messages, last user "[prigh: the usage limit has reset. Carry on with the interru" (+goal_complete)
    assistant: resumed
    request: 5 messages, last user "[prigh goal: continuation 1 of 20] Continue working toward t" (+goal_complete)
    assistant:
    notice: goal complete: did it
    tool goal_complete: goal marked complete
    request: 7 messages, last user "[prigh goal: continuation 1 of 20] Continue working toward t" (+goal_complete)
    assistant: ok
    ((running false) (cost_usd 0.0001) (budget_usd ())
     (goal (((text g) (status (Done (summary "did it"))) (continuations 1))))
     (resume_scheduled false))
    |}]
;;
