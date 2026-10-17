open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

(* A session's reminders: on-stop ones when the main agent ends its turn
   with nothing to wait for, periodic ones every so many turns, never to
   subagents; recorded in the session. *)

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

let on_stop ?(max_repeats = Reminder.default_max_repeats) text =
  text, Reminder.Trigger.On_stop { max_repeats }
;;

let every n text = text, Reminder.Trigger.Every_turns n

let add agent (text, trigger) =
  ignore (Or_error.ok_exn (Agent.add_reminder agent ~text trigger) : Reminder.t)
;;

let with_agent ?(config = []) ?(reminders = []) ?tools ?provider replies f =
  let provider =
    match provider with
    | Some p -> p
    | None -> Faux_provider.create ~on_request:print_request replies
  in
  Test_loop_features.with_agent ~config ?tools ~provider [] (fun t agent make ->
    List.iter reminders ~f:(add agent);
    f t agent make)
;;

let keep_going = on_stop ~max_repeats:2 "Don't stop until done."

let show_reminders agent =
  List.iter (Agent.reminders agent) ~f:(fun r ->
    print_s [%sexp (r : Reminder.t)])
;;

let%expect_test "names, labels, messages and the tracker" =
  let create ~taken (text, trigger) = Reminder.create ~taken ~text trigger in
  let stop =
    Or_error.ok_exn (create ~taken:[] (on_stop ~max_repeats:2 "Keep at it."))
  in
  let disk =
    Or_error.ok_exn
      (create ~taken:[ "r1" ] (every 2 "Check disk usage.\nDelete artifacts."))
  in
  let each =
    Or_error.ok_exn
      (create ~taken:[ "r1"; "r7"; "x"; "r2" ] (every 1 "Breathe."))
  in
  List.iter
    [ stop, Some 2; disk, None; each, None ]
    ~f:(fun (r, repeat) ->
      print_endline (Reminder.label r);
      print_endline (Reminder.message_text r ~repeat));
  List.iter
    [ on_stop "  "; on_stop ~max_repeats:0 "x"; every 0 "x"; every (-1) "x" ]
    ~f:(fun r -> print_s [%sexp (create ~taken:[] r : Reminder.t Or_error.t)]);
  [%expect
    {|
    r1: Keep at it.
    [prigh reminder: r1, as you ended your turn (2 of 2)] Keep at it.
    r2: Check disk usage.
    [prigh reminder: r2, every 2 turns] Check disk usage.
    Delete artifacts.
    r8: Breathe.
    [prigh reminder: r8, every turn] Breathe.
    (Error
     "say what to remind the agent of, e.g. /remind stop run the tests before you stop")
    (Error "the number of repeats must be 1 or more, e.g. /remind stop 5 TEXT")
    (Error "the number of turns must be 1 or more, e.g. /remind every 20 TEXT")
    (Error "the number of turns must be 1 or more, e.g. /remind every 20 TEXT")
    |}];
  let reminders = [ stop; disk; each ] in
  let tracker = Reminder.Tracker.create () in
  let label (r : Reminder.t) = r.name in
  let show_due () =
    print_s
      [%sexp
        (List.map (Reminder.Tracker.due tracker reminders) ~f:label
         : string list)]
  in
  let show_stop () =
    let { Reminder.Tracker.Stop.fire; used_up } =
      Reminder.Tracker.stopped tracker reminders
    in
    print_s
      [%sexp
        { fire =
            (List.map fire ~f:(fun (r, n) -> label r, n) : (string * int) list)
        ; used_up = (List.map used_up ~f:label : string list)
        ; stops = (Reminder.Tracker.stops tracker : int)
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
  (* After a reset (another session) each counts its turns afresh. *)
  show_due ();
  Reminder.Tracker.turn_ended tracker;
  show_due ();
  [%expect
    {|
    ()
    (r8)
    ()
    (r2 r8)
    ((fire ((r1 1))) (used_up ()) (stops 1))
    ((fire ((r1 2))) (used_up ()) (stops 2))
    ((fire ()) (used_up (r1)) (stops 3))
    ((fire ()) (used_up ()) (stops 4))
    ((fire ((r1 1))) (used_up ()) (stops 1))
    ()
    (r8)
    |}]
;;

let%expect_test
    "an on-stop reminder keeps the agent going up to max_repeats, then waits \
     for the user"
  =
  with_agent
    ~reminders:[ keep_going ]
    [ text "a"; text "b"; text "c"; text "d"; text "e"; text "f" ]
  @@ fun _t agent _ ->
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
      user "[prigh reminder: r1, as you ended your turn (1 of 2)] Don't stop until done." <reminder: Reminder r1: Don't stop until done. (1/2)>
    assistant: b
    request: 5 messages
      user "[prigh reminder: r1, as you ended your turn (2 of 2)] Don't stop until done." <reminder: Reminder r1: Don't stop until done. (2/2)>
    assistant: c
    notice: reminder r1 fired 2 times in a row; it fires again after your next message
    (true true true false)
    request: 7 messages
      user "more"
    assistant: d
    request: 9 messages
      user "[prigh reminder: r1, as you ended your turn (1 of 2)] Don't stop until done." <reminder: Reminder r1: Don't stop until done. (1/2)>
    assistant: e
    request: 11 messages
      user "[prigh reminder: r1, as you ended your turn (2 of 2)] Don't stop until done." <reminder: Reminder r1: Don't stop until done. (2/2)>
    assistant: f
    notice: reminder r1 fired 2 times in a row; it fires again after your next message
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
  with_agent ~reminders:[ on_stop ~max_repeats:1 "Go on." ] ~provider []
  @@ fun _t a _ ->
  agent := Some a;
  Or_error.ok_exn (Agent.prompt a "go");
  Agent.wait_idle a;
  [%expect
    {|
    request: 1 messages
      user "go"
    assistant: a
    request: 3 messages
      user "[prigh reminder: r1, as you ended your turn (1 of 1)] Go on." <reminder: Reminder r1: Go on. (1/1)>
    assistant: b
    request: 5 messages
      user "and this"
    assistant: c
    request: 7 messages
      user "[prigh reminder: r1, as you ended your turn (1 of 1)] Go on." <reminder: Reminder r1: Go on. (1/1)>
    assistant: d
    notice: reminder r1 fired once; it fires again after your next message
    |}]
;;

let%expect_test "not after an error, an abort, in plan mode or past the budget" =
  with_agent ~reminders:[ keep_going ] [ Reply.error "HTTP 400: bad request" ]
  @@ fun _t agent _ ->
  Or_error.ok_exn (Agent.prompt agent "fail");
  Agent.wait_idle agent;
  [%expect
    {|
    request: 1 messages
      user "fail"
    assistant:  ERROR HTTP 400: bad request
    |}];
  with_agent
    ~reminders:[ keep_going ]
    ~tools:Tools.all
    [ Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"sleep 5"}|}
        ()
    ]
  @@ fun t agent _ ->
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
  with_agent ~reminders:[ keep_going ] [ text "the plan" ]
  @@ fun _t agent _ ->
  Agent.set_mode agent ~plan:true;
  Or_error.ok_exn (Agent.prompt agent "plan it");
  Agent.wait_idle agent;
  [%expect
    {|
    request: 1 messages
      user "[Plan mode is on: you are planning, not implementing. Investigate with read-only tools (ba"
    assistant: the plan
    |}];
  with_agent ~reminders:[ keep_going ] [ text ~output:500_000 "expensive" ]
  @@ fun _t agent _ ->
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
    ~reminders:[ keep_going ]
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
  @@ fun _t agent _ ->
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
      user "[prigh reminder: r1, as you ended your turn (1 of 2)] Don't stop until done." <reminder: Reminder r1: Don't stop until done. (1/2)>
    assistant: really done
    request: 9 messages
      user "[prigh reminder: r1, as you ended your turn (2 of 2)] Don't stop until done." <reminder: Reminder r1: Don't stop until done. (2/2)>
    assistant: truly done
    notice: reminder r1 fired 2 times in a row; it fires again after your next message
    |}]
;;

let%expect_test "a goal's continuation comes first; the reminder after it stops"
  =
  with_agent
    ~config:[ "goal_max_continuations", `Number "1" ]
    ~reminders:[ keep_going ]
    [ text "a"; text "b"; text "c"; text "d" ]
  @@ fun _t agent _ ->
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
      user "[prigh reminder: r1, as you ended your turn (1 of 2)] Don't stop until done." <reminder: Reminder r1: Don't stop until done. (1/2)>
    assistant: c
    request: 7 messages
      user "[prigh reminder: r1, as you ended your turn (2 of 2)] Don't stop until done." <reminder: Reminder r1: Don't stop until done. (2/2)>
    assistant: d
    notice: reminder r1 fired 2 times in a row; it fires again after your next message
    |}]
;;

let%expect_test
    "a periodic reminder: between turns of a run and at a run's start"
  =
  with_agent
    ~reminders:[ every 2 "Check disk usage." ]
    [ call "c1" 1; call "c2" 1; call "c3" 1; text "done"; text "again" ]
  @@ fun _t agent _ ->
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
      user "[prigh reminder: r1, every 2 turns] Check disk usage." <reminder: Reminder r1: Check disk usage.>
    assistant:
    tool filler: x
    request: 8 messages
    assistant: done
    request: 11 messages
      user "more"
      user "[prigh reminder: r1, every 2 turns] Check disk usage." <reminder: Reminder r1: Check disk usage.>
    assistant: again
    |}]
;;

let%expect_test "a fork keeps the reminders and starts the counts over" =
  with_agent
    ~reminders:[ every 2 "Check disk usage." ]
    [ text "a"; text "b"; text "c"; text "d" ]
  @@ fun _t agent _ ->
  Or_error.ok_exn (Agent.prompt agent "one");
  Agent.wait_idle agent;
  Or_error.ok_exn (Agent.fork agent ());
  show_reminders agent;
  List.iter [ "two"; "three"; "four" ] ~f:(fun prompt ->
    Or_error.ok_exn (Agent.prompt agent prompt);
    Agent.wait_idle agent);
  [%expect
    {|
    request: 1 messages
      user "one"
    assistant: a
    ((name r1) (text "Check disk usage.") (trigger (Every_turns 2)))
    request: 3 messages
      user "two"
    assistant: b
    request: 5 messages
      user "three"
    assistant: c
    request: 8 messages
      user "four"
      user "[prigh reminder: r1, every 2 turns] Check disk usage." <reminder: Reminder r1: Check disk usage.>
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
    ~reminders:[ every 1 "Check disk usage."; on_stop ~max_repeats:1 "Go on." ]
    ~tools:([ filler ] @ Test_background_subagents.subagent_tools ~provider)
    ~provider
    []
  @@ fun _t agent _ ->
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
      user "[prigh reminder: r1, every turn] Check disk usage." <reminder: Reminder r1: Check disk usage.>
    assistant: done
    request: 7 messages
      user "[subagent a1 finished] dig" <delivery: Subagent a1 finished>
      user "[prigh reminder: r1, every turn] Check disk usage." <reminder: Reminder r1: Check disk usage.>
    assistant: still done
    request: 10 messages
      user "[prigh reminder: r2, as you ended your turn (1 of 1)] Go on." <reminder: Reminder r2: Go on. (1/1)>
      user "[prigh reminder: r1, every turn] Check disk usage." <reminder: Reminder r1: Check disk usage.>
    assistant: ok
    notice: reminder r2 fired once; it fires again after your next message
    ("\"dig\"")
    |}]
;;

let%expect_test
    "recorded in the session: restored on reload and switch, none in a new \
     session; names are never reused"
  =
  with_agent [] ~reminders:[ keep_going; every 3 "Check disk usage." ]
  @@ fun _t agent make ->
  (* A name makes the session worth saving, so that it has a file. *)
  Agent.set_session_name agent "reminded";
  let states = ref [] in
  Agent.subscribe agent ~f:(function
    | State_changed s -> states := List.length s.reminders :: !states
    | _ -> ());
  show_reminders agent;
  [%expect
    {|
    ((name r1) (text "Don't stop until done.")
     (trigger (On_stop (max_repeats 2))))
    ((name r2) (text "Check disk usage.") (trigger (Every_turns 3)))
    |}];
  print_s [%sexp (Agent.remove_reminder agent "r2" : unit Or_error.t)];
  add agent (on_stop "Run the tests.");
  print_s [%sexp (Agent.remove_reminder agent "r9" : unit Or_error.t)];
  print_s [%sexp (Agent.remove_reminder agent " r3 " : unit Or_error.t)];
  add agent (every 5 "Commit.");
  show_reminders agent;
  print_s [%sexp (List.rev !states : int list)];
  [%expect
    {|
    (Ok ())
    (Error "no reminder \"r9\"; the session's are r1, r3 (/remind lists them)")
    (Ok ())
    ((name r1) (text "Don't stop until done.")
     (trigger (On_stop (max_repeats 2))))
    ((name r4) (text Commit.) (trigger (Every_turns 5)))
    (1 2 1 2)
    |}];
  let first = Agent.session agent in
  (* A restart: the session file as a new agent loads it. *)
  let reloaded =
    make ~session:(Or_error.ok_exn (Session.load (Session.path first))) ()
  in
  show_reminders reloaded;
  print_s [%sexp (Agent.clear_reminders reloaded : unit Or_error.t)];
  print_s [%sexp (Agent.clear_reminders reloaded : unit Or_error.t)];
  print_s [%sexp (Agent.remove_reminder reloaded "r1" : unit Or_error.t)];
  add reloaded (every 2 "Again.");
  show_reminders reloaded;
  [%expect
    {|
    ((name r1) (text "Don't stop until done.")
     (trigger (On_stop (max_repeats 2))))
    ((name r4) (text Commit.) (trigger (Every_turns 5)))
    (Ok ())
    (Error "there are no reminders; /remind stop TEXT adds one")
    (Error
     "no reminder \"r1\": there are no reminders; /remind stop TEXT adds one")
    ((name r5) (text Again.) (trigger (Every_turns 2)))
    |}];
  Agent.new_session agent;
  show_reminders agent;
  print_s [%sexp ((Agent.state agent).reminders : Reminder.t list)];
  add agent (on_stop "Fresh.");
  show_reminders agent;
  Or_error.ok_exn (Agent.switch_session agent ~path:(Session.path first));
  show_reminders agent;
  [%expect
    {|
    ()
    ((name r1) (text Fresh.) (trigger (On_stop (max_repeats 3))))
    ((name r5) (text Again.) (trigger (Every_turns 2)))
    |}]
;;

let%expect_test "the RPC methods and the state" =
  Test_rpc.with_agent []
  @@ fun t agent h ->
  let show_state () =
    match Rpc_json.state (Agent.state agent) with
    | `Object fields ->
      print_endline
        (Jsonaf.to_string
           (List.Assoc.find_exn fields ~equal:String.equal "reminders"))
    | _ -> ()
  in
  let call = Test_rpc.call t h in
  call ~params:{|{"text": "Keep going.", "on": "stop"}|} "add_reminder";
  call ~params:{|{"text": "Check disk.", "every_turns": 10}|} "add_reminder";
  call
    ~params:{|{"text": "Twice.", "on": "stop", "max_repeats": 2}|}
    "add_reminder";
  call ~params:{|{"text": "x"}|} "add_reminder";
  call ~params:{|{"text": "x", "on": "start"}|} "add_reminder";
  call ~params:{|{"text": "x", "every_turns": 0}|} "add_reminder";
  call ~params:{|{"text": " ", "on": "stop"}|} "add_reminder";
  show_state ();
  call ~params:{|{"name": "r2"}|} "remove_reminder";
  call ~params:{|{"name": "r2"}|} "remove_reminder";
  show_state ();
  call "clear_reminders";
  call "clear_reminders";
  show_state ();
  (* Frontends see each change in a state event. *)
  print_s
    [%sexp
      (Queue.count h.sent ~f:(fun json ->
         match Jsonaf.member "event" json with
         | Some (`String "state") -> true
         | _ -> false)
       : int)];
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{"name":"r1","text":"Keep going.","on":"stop","max_repeats":3,"repeats":0}}
    {"type":"response","id":"r1","ok":true,"result":{"name":"r2","text":"Check disk.","every_turns":10}}
    {"type":"response","id":"r1","ok":true,"result":{"name":"r3","text":"Twice.","on":"stop","max_repeats":2,"repeats":0}}
    {"type":"response","id":"r1","ok":false,"error":"add_reminder needs \"on\": \"stop\" (with an optional \"max_repeats\") or \"every_turns\": N"}
    {"type":"response","id":"r1","ok":false,"error":"add_reminder needs \"on\": \"stop\" (with an optional \"max_repeats\") or \"every_turns\": N"}
    {"type":"response","id":"r1","ok":false,"error":"the number of turns must be 1 or more, e.g. /remind every 20 TEXT"}
    {"type":"response","id":"r1","ok":false,"error":"say what to remind the agent of, e.g. /remind stop run the tests before you stop"}
    [{"name":"r1","text":"Keep going.","on":"stop","max_repeats":3,"repeats":0},{"name":"r2","text":"Check disk.","every_turns":10},{"name":"r3","text":"Twice.","on":"stop","max_repeats":2,"repeats":0}]
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":false,"error":"no reminder \"r2\"; the session's are r1, r3 (/remind lists them)"}
    [{"name":"r1","text":"Keep going.","on":"stop","max_repeats":3,"repeats":0},{"name":"r3","text":"Twice.","on":"stop","max_repeats":2,"repeats":0}]
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":false,"error":"there are no reminders; /remind stop TEXT adds one"}
    []
    5
    |}]
;;
