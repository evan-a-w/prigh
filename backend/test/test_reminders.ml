open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

(* The config's [reminders]: on-stop ones when the main agent ends its turn
   with nothing to wait for, periodic ones every so many turns, never to
   subagents. *)

let text = Test_loop_features.text
let call = Test_loop_features.call

(* The user messages a request ends with: what the model got since its last
   message or tool result. *)
let trailing_users (r : Provider.Request.t) =
  List.rev r.messages
  |> List.take_while ~f:(function
    | Message.User _ -> true
    | Assistant _ | Tool_result _ -> false)
  |> List.rev_map ~f:(function
    | Message.User u ->
      let title =
        match u.origin with
        | Some { source = Some source; _ } ->
          sprintf
            " <%s: %s>"
            (Injected.Source.kind source)
            (Injected.Source.title source)
        | Some _ | None -> ""
      in
      sprintf
        "%S%s"
        (String.prefix (List.hd_exn (String.split_lines u.text)) 90)
        title
    | Assistant _ | Tool_result _ -> assert false)
;;

let print_request (r : Provider.Request.t) =
  printf "request: %d messages\n" (List.length r.messages);
  List.iter (trailing_users r) ~f:(fun u -> printf "  user %s\n" u)
;;

let reminders list = [ "reminders", `Array list ]

let on_stop ?max_repeats name text =
  `Object
    ([ "name", `String name; "text", `String text; "on", `String "stop" ]
     @ Option.value_map max_repeats ~default:[] ~f:(fun n ->
       [ "max_repeats", `Number (Int.to_string n) ]))
;;

let every n text =
  `Object [ "text", `String text; "every_turns", `Number (Int.to_string n) ]
;;

let with_agent ?(config = []) ?tools ?provider replies f =
  let provider =
    match provider with
    | Some p -> p
    | None -> Faux_provider.create ~on_request:print_request replies
  in
  Test_loop_features.with_agent ~config ?tools ~provider [] (fun t agent _ ->
    f t agent)
;;

let keep_going = on_stop ~max_repeats:2 "keep-going" "Don't stop until done."

let%expect_test "labels, messages and the tracker" =
  let parse s = Or_error.ok_exn (Reminder.of_json (Jsonaf.of_string s)) in
  let stop =
    parse
      {|{"name": "go", "text": "Keep at it.", "on": "stop", "max_repeats": 2}|}
  in
  let disk =
    parse {|{"text": "Check disk usage.\nDelete artifacts.", "every_turns": 2}|}
  in
  let each = parse {|{"text": "Breathe.", "every_turns": 1}|} in
  List.iter
    [ stop, Some 2; disk, None; each, None ]
    ~f:(fun (r, repeat) ->
      print_endline (Reminder.label r);
      print_endline (Reminder.message_text r ~repeat));
  [%expect
    {|
    go
    [prigh reminder: go, as you ended your turn (2 of 2)] Keep at it.
    Check disk usage.
    [prigh reminder, every 2 turns] Check disk usage.
    Delete artifacts.
    Breathe.
    [prigh reminder, every turn] Breathe.
    |}];
  let reminders = [ stop; disk; each ] in
  let tracker = Reminder.Tracker.create () in
  let show_due () =
    print_s
      [%sexp
        (List.map (Reminder.Tracker.due tracker reminders) ~f:Reminder.label
         : string list)]
  in
  let show_stop () =
    let { Reminder.Tracker.Stop.fire; used_up } =
      Reminder.Tracker.stopped tracker reminders
    in
    print_s
      [%sexp
        { fire =
            (List.map fire ~f:(fun (r, n) -> Reminder.label r, n)
             : (string * int) list)
        ; used_up = (List.map used_up ~f:Reminder.label : string list)
        }]
  in
  show_due ();
  Reminder.Tracker.turn_ended tracker;
  show_due ();
  show_due ();
  Reminder.Tracker.turn_ended tracker;
  show_due ();
  show_stop ();
  show_stop ();
  show_stop ();
  show_stop ();
  Reminder.Tracker.user_spoke tracker;
  show_stop ();
  Reminder.Tracker.reset tracker;
  Reminder.Tracker.turn_ended tracker;
  show_due ();
  [%expect
    {|
    ()
    (Breathe.)
    ()
    ("Check disk usage." Breathe.)
    ((fire ((go 1))) (used_up ()))
    ((fire ((go 2))) (used_up ()))
    ((fire ()) (used_up (go)))
    ((fire ()) (used_up ()))
    ((fire ((go 1))) (used_up ()))
    (Breathe.)
    |}]
;;

let%expect_test
    "an on-stop reminder keeps the agent going up to max_repeats, then waits \
     for the user"
  =
  with_agent
    ~config:(reminders [ keep_going ])
    [ text "a"; text "b"; text "c"; text "d"; text "e"; text "f" ]
  @@ fun _t agent ->
  let states = ref [] in
  Agent.subscribe agent ~f:(function
    | State_changed s -> states := s.running :: !states
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  (* It never looked idle in between. *)
  print_s [%sexp (List.rev !states : bool list)];
  Or_error.ok_exn (Agent.prompt agent "more");
  Agent.wait_idle agent;
  [%expect
    {|
    request: 1 messages
      user "go"
    assistant: a
    request: 3 messages
      user "[prigh reminder: keep-going, as you ended your turn (1 of 2)] Don't stop until done." <reminder: Reminder: keep-going (1/2)>
    assistant: b
    request: 5 messages
      user "[prigh reminder: keep-going, as you ended your turn (2 of 2)] Don't stop until done." <reminder: Reminder: keep-going (2/2)>
    assistant: c
    notice: reminder keep-going fired 2 times in a row; it fires again after your next message
    (true true true false)
    request: 7 messages
      user "more"
    assistant: d
    request: 9 messages
      user "[prigh reminder: keep-going, as you ended your turn (1 of 2)] Don't stop until done." <reminder: Reminder: keep-going (1/2)>
    assistant: e
    request: 11 messages
      user "[prigh reminder: keep-going, as you ended your turn (2 of 2)] Don't stop until done." <reminder: Reminder: keep-going (2/2)>
    assistant: f
    notice: reminder keep-going fired 2 times in a row; it fires again after your next message
    |}]
;;

let%expect_test "a user message sent while it works starts the count over" =
  let agent = ref None in
  let sent = ref false in
  let provider =
    Faux_provider.create
      ~on_request:(fun r ->
        print_request r;
        (* While it answers the first reminder. *)
        if
          (not !sent)
          && List.exists
               (trailing_users r)
               ~f:(String.is_prefix ~prefix:"\"[prigh reminder")
        then (
          sent := true;
          Or_error.ok_exn (Agent.follow_up (Option.value_exn !agent) "and this")))
      [ text "a"; text "b"; text "c"; text "d" ]
  in
  with_agent
    ~config:(reminders [ on_stop ~max_repeats:1 "keep-going" "Go on." ])
    ~provider
    []
  @@ fun _t a ->
  agent := Some a;
  Or_error.ok_exn (Agent.prompt a "go");
  Agent.wait_idle a;
  [%expect
    {|
    request: 1 messages
      user "go"
    assistant: a
    request: 3 messages
      user "[prigh reminder: keep-going, as you ended your turn (1 of 1)] Go on." <reminder: Reminder: keep-going (1/1)>
    assistant: b
    request: 5 messages
      user "and this"
    assistant: c
    request: 7 messages
      user "[prigh reminder: keep-going, as you ended your turn (1 of 1)] Go on." <reminder: Reminder: keep-going (1/1)>
    assistant: d
    notice: reminder keep-going fired once; it fires again after your next message
    |}]
;;

let%expect_test "not after an error, an abort, in plan mode or past the budget" =
  with_agent
    ~config:(reminders [ keep_going ])
    [ Reply.error "HTTP 400: bad request" ]
  @@ fun _t agent ->
  Or_error.ok_exn (Agent.prompt agent "fail");
  Agent.wait_idle agent;
  [%expect
    {|
    request: 1 messages
      user "fail"
    assistant:  ERROR HTTP 400: bad request
    |}];
  with_agent
    ~config:(reminders [ keep_going ])
    ~tools:Tools.all
    [ Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"sleep 5"}|}
        ()
    ]
  @@ fun t agent ->
  Or_error.ok_exn (Agent.prompt agent "abort me");
  Eio.Time.sleep (Eio.Stdenv.clock t.env) 0.3;
  ignore (Agent.abort agent : string list);
  Agent.wait_idle agent;
  [%expect
    {|
    request: 1 messages
      user "abort me"
    assistant:
    tool bash: [cancelled]
    |}];
  with_agent ~config:(reminders [ keep_going ]) [ text "the plan" ]
  @@ fun _t agent ->
  Agent.set_mode agent ~plan:true;
  Or_error.ok_exn (Agent.prompt agent "plan it");
  Agent.wait_idle agent;
  [%expect
    {|
    request: 1 messages
      user "[Plan mode is on: you are planning, not implementing. Investigate with read-only tools (ba"
    assistant: the plan
    |}];
  with_agent
    ~config:(reminders [ keep_going ])
    [ text ~output:500_000 "expensive" ]
  @@ fun _t agent ->
  Or_error.ok_exn (Agent.set_budget agent (Some 0.5));
  Or_error.ok_exn (Agent.prompt agent "spend");
  Agent.wait_idle agent;
  [%expect
    {|
    request: 1 messages
      user "spend"
    assistant: expensive
    |}]
;;

let%expect_test "it waits for a running background job, then reminds" =
  with_agent
    ~config:(reminders [ keep_going ])
    ~tools:Tools.all
    [ Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"sleep 0.3","background":true}|}
        ()
    ; text "waiting for it"
    ; text "it finished"
    ; text "really done"
    ; text "truly done"
    ]
  @@ fun _t agent ->
  Or_error.ok_exn (Agent.prompt agent "build");
  Agent.wait_idle agent;
  [%expect
    {|
    request: 1 messages
      user "build"
    assistant:
    tool bash: started job j1: sleep 0.3; its output is being recorded; you will be notified wh
    request: 3 messages
    assistant: waiting for it
    request: 5 messages
      user "[job j1 exited 0] sleep 0.3" <delivery: Job j1 exited 0>
    assistant: it finished
    request: 7 messages
      user "[prigh reminder: keep-going, as you ended your turn (1 of 2)] Don't stop until done." <reminder: Reminder: keep-going (1/2)>
    assistant: really done
    request: 9 messages
      user "[prigh reminder: keep-going, as you ended your turn (2 of 2)] Don't stop until done." <reminder: Reminder: keep-going (2/2)>
    assistant: truly done
    notice: reminder keep-going fired 2 times in a row; it fires again after your next message
    |}]
;;

let%expect_test "a goal's continuation comes first; the reminder after it stops"
  =
  with_agent
    ~config:(("goal_max_continuations", `Number "1") :: reminders [ keep_going ])
    [ text "a"; text "b"; text "c"; text "d" ]
  @@ fun _t agent ->
  Or_error.ok_exn (Agent.set_goal agent "ship it");
  Agent.wait_idle agent;
  [%expect
    {|
    request: 1 messages
      user "[prigh goal] Work on this goal until it is fully achieved:" <goal: Goal: ship it>
    assistant: a
    request: 3 messages
      user "[prigh goal: continuation 1 of 1] Continue working toward the goal:" <goal_continuation: Goal: continuing (1/1)>
    assistant: b
    notice: goal stopped: it used all 1 automatic continuations; /goal resume continues it
    request: 5 messages
      user "[prigh reminder: keep-going, as you ended your turn (1 of 2)] Don't stop until done." <reminder: Reminder: keep-going (1/2)>
    assistant: c
    request: 7 messages
      user "[prigh reminder: keep-going, as you ended your turn (2 of 2)] Don't stop until done." <reminder: Reminder: keep-going (2/2)>
    assistant: d
    notice: reminder keep-going fired 2 times in a row; it fires again after your next message
    |}]
;;

let%expect_test
    "a periodic reminder: between turns of a run and at a run's start"
  =
  with_agent
    ~config:(reminders [ every 2 "Check disk usage." ])
    [ call "c1" 1; call "c2" 1; call "c3" 1; text "done"; text "again" ]
  @@ fun _t agent ->
  Or_error.ok_exn (Agent.prompt agent "work");
  Agent.wait_idle agent;
  Or_error.ok_exn (Agent.prompt agent "more");
  Agent.wait_idle agent;
  [%expect
    {|
    request: 1 messages
      user "work"
    assistant:
    tool filler: x
    request: 3 messages
    assistant:
    tool filler: x
    request: 6 messages
      user "[prigh reminder, every 2 turns] Check disk usage." <reminder: Reminder: Check disk usage.>
    assistant:
    tool filler: x
    request: 8 messages
    assistant: done
    request: 11 messages
      user "more"
      user "[prigh reminder, every 2 turns] Check disk usage." <reminder: Reminder: Check disk usage.>
    assistant: again
    |}]
;;

let%expect_test "a new session starts the counts over" =
  with_agent
    ~config:(reminders [ every 2 "Check disk usage." ])
    [ text "a"; text "b"; text "c"; text "d" ]
  @@ fun _t agent ->
  Or_error.ok_exn (Agent.prompt agent "one");
  Agent.wait_idle agent;
  Agent.new_session agent;
  List.iter [ "two"; "three"; "four" ] ~f:(fun prompt ->
    Or_error.ok_exn (Agent.prompt agent prompt);
    Agent.wait_idle agent);
  [%expect
    {|
    request: 1 messages
      user "one"
    assistant: a
    request: 1 messages
      user "two"
    assistant: b
    request: 3 messages
      user "three"
    assistant: c
    request: 6 messages
      user "four"
      user "[prigh reminder, every 2 turns] Check disk usage." <reminder: Reminder: Check disk usage.>
    assistant: d
    |}]
;;

let%expect_test "subagents never get reminders" =
  let subagent_users = ref [] in
  let provider =
    Routed_provider.create
      ~on_request:(fun ~key r ->
        if String.equal key "main"
        then print_request r
        else subagent_users := !subagent_users @ trailing_users r)
      ~main:
        [ Reply.tool_call
            ~id:"s1"
            ~name:"subagent"
            ~arguments:{|{"task":"dig"}|}
            ()
        ; text "done"
        ; text "still done"
        ; text "ok"
        ]
      [ "dig", [ call "d1" 1; call "d2" 1; text "dug" ] ]
  in
  with_agent
    ~config:
      (reminders
         [ every 1 "Check disk usage."
         ; on_stop ~max_repeats:1 "keep-going" "Go on."
         ])
    ~tools:([ filler ] @ Test_background_subagents.subagent_tools ~provider)
    ~provider
    []
  @@ fun _t agent ->
  Or_error.ok_exn (Agent.prompt agent "delegate");
  Agent.wait_idle agent;
  print_s [%sexp (!subagent_users : string list)];
  [%expect
    {|
    request: 1 messages
      user "delegate"
    assistant:
    tool subagent: started agent a1 (dig); its result will be delivered to you when it finishes: ke
    request: 4 messages
      user "[prigh reminder, every turn] Check disk usage." <reminder: Reminder: Check disk usage.>
    assistant: done
    request: 7 messages
      user "[subagent a1 finished] dig" <delivery: Subagent a1 finished>
      user "[prigh reminder, every turn] Check disk usage." <reminder: Reminder: Check disk usage.>
    assistant: still done
    request: 10 messages
      user "[prigh reminder: keep-going, as you ended your turn (1 of 1)] Go on." <reminder: Reminder: keep-going (1/1)>
      user "[prigh reminder, every turn] Check disk usage." <reminder: Reminder: Check disk usage.>
    assistant: ok
    notice: reminder keep-going fired once; it fires again after your next message
    ("\"dig\"")
    |}]
;;
