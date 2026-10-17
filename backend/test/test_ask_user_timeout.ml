open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

let%expect_test "timeout: the config's, the call's override, bounds, 0" =
  List.iter
    [ Some 600., None
    ; Some 600., Some 120
    ; Some 600., Some 5
    ; Some 600., Some 1_000_000
    ; Some 600., Some 0
    ; None, None
    ; None, Some 60
    ]
    ~f:(fun (config, requested) ->
      print_s
        [%sexp
          (config : float option)
        , (requested : int option)
        , "->"
        , (User_question.timeout ~config ~requested : float option)]);
  [%expect
    {|
    ((600) () -> (600))
    ((600) (120) -> (120))
    ((600) (5) -> (30))
    ((600) (1000000) -> (86400))
    ((600) (0) -> ())
    (() () -> ())
    (() (60) -> ())
    |}];
  List.iter [ 0.; 45.; 60.; 570.; 600.; 3600.; 3900.; 86400. ] ~f:(fun s ->
    print_endline (User_question.duration_hum s));
  [%expect
    {|
    0s
    45s
    1m
    9m 30s
    10m
    1h
    1h 05m
    24h
    |}]
;;

(* [sleep_until] waits until [fire]; cancelling it (an answer came first, or
   the run was aborted) is printed. *)
let manual_clock t =
  let waiting = ref None in
  let sleep_until at =
    let p, u = Eio.Promise.create () in
    waiting := Some u;
    printf
      "timer: due in %.0fs\n"
      (at -. Eio.Time.now (Eio.Stdenv.clock t.env));
    match Eio.Promise.await p with
    | () -> ()
    | exception (Eio.Cancel.Cancelled _ as exn) ->
      waiting := None;
      print_endline "timer: cancelled";
      raise exn
  in
  let fire () =
    match !waiting with
    | Some u ->
      waiting := None;
      print_endline "timer: fires";
      Eio.Promise.resolve u ()
    | None -> print_endline "timer: nothing waiting"
  in
  sleep_until, fire
;;

let ask ?timeout_seconds id =
  Reply.tool_call
    ~id
    ~name:"ask_user"
    ~arguments:
      (sprintf
         {|{"questions":[{"question":"Which database?","options":[{"label":"Postgres"},{"label":"SQLite"}]}]%s}|}
         (Option.value_map timeout_seconds ~default:"" ~f:(fun s ->
            sprintf {|,"timeout_seconds":%d|} s)))
    ()
;;

(* An agent with [ask_user] on a manual clock. The events about questions,
   tool results and what the model is sent back are printed; nobody answers
   unless the test does. *)
let with_agent ?config replies f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  Option.iter config ~f:(fun json -> write t ".prigh/config.json" json);
  let sleep_until, fire = manual_clock t in
  let provider =
    Faux_provider.create
      ~on_request:(fun r ->
        match List.last r.messages with
        | Some (Tool_result r) ->
          printf
            "model reads: %s%s\n"
            (if r.is_error then "ERROR " else "")
            r.text
        | _ -> ())
      replies
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:Tools.interaction
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:t.dir
      ~sleep_until
      ()
  in
  let now () = Eio.Time.now (Eio.Stdenv.clock t.env) in
  Agent.subscribe agent ~f:(function
    | Loop (Ask_user { call_id; questions = _; deadline }) ->
      printf
        "ask_user %s, %s\n"
        call_id
        (match deadline with
         | None -> "no deadline"
         | Some at -> sprintf "deadline in %.0fs" (at -. now ()));
      print_s
        [%sexp
          (List.map (Agent.pending_questions agent) ~f:(fun q ->
             ( q.call_id
             , Option.map q.deadline ~f:(fun at -> Float.round (at -. now ())) ))
           : (string * float option) list)]
    | Loop (Ask_user_done { call_id; timed_out }) ->
      printf "ask_user_done %s, timed_out=%b\n" call_id timed_out
    | _ -> ());
  (* Lets the run reach the next wait (the timer, or the run's end). *)
  let settle () = Eio.Time.sleep (Eio.Stdenv.clock t.env) 0.05 in
  f agent ~fire ~settle
;;

let%expect_test "unanswered, the question times out and the model carries on" =
  with_agent [ ask "q1"; Reply.text "went with Postgres" ]
  @@ fun agent ~fire ~settle ->
  Or_error.ok_exn (Agent.prompt agent "go");
  settle ();
  fire ();
  Agent.wait_idle agent;
  print_s
    [%sexp
      (Agent.answer_question agent ~call_id:"q1" (Answered [ [ "SQLite" ] ])
       : unit Or_error.t)];
  [%expect
    {|
    ask_user q1, deadline in 600s
    ((q1 (600)))
    timer: due in 600s
    timer: fires
    ask_user_done q1, timed_out=true
    model reads: ERROR [the user did not answer within 10m; carry on with your best judgement and say what you assumed]
    (Error
     "no pending question for tool call \"q1\": it was answered or withdrawn")
    |}]
;;

let%expect_test "answered before the timeout: the timer is cancelled" =
  with_agent [ ask "q1"; Reply.text "ok" ]
  @@ fun agent ~fire ~settle ->
  Or_error.ok_exn (Agent.prompt agent "go");
  settle ();
  print_s
    [%sexp
      (Agent.answer_question agent ~call_id:"q1" (Answered [ [ "SQLite" ] ])
       : unit Or_error.t)];
  Agent.wait_idle agent;
  fire ();
  [%expect
    {|
    ask_user q1, deadline in 600s
    ((q1 (600)))
    timer: due in 600s
    (Ok ())
    timer: cancelled
    ask_user_done q1, timed_out=false
    model reads: The user answered:
    Q: Which database?
    A: SQLite
    timer: nothing waiting
    |}]
;;

let%expect_test "declining is not timing out" =
  with_agent [ ask "q1"; Reply.text "ok" ]
  @@ fun agent ~fire:_ ~settle ->
  Or_error.ok_exn (Agent.prompt agent "go");
  settle ();
  print_s
    [%sexp
      (Agent.answer_question agent ~call_id:"q1" Declined : unit Or_error.t)];
  Agent.wait_idle agent;
  [%expect
    {|
    ask_user q1, deadline in 600s
    ((q1 (600)))
    timer: due in 600s
    (Ok ())
    timer: cancelled
    ask_user_done q1, timed_out=false
    model reads: ERROR [the user declined to answer; carry on with your best judgement or ask differently]
    |}]
;;

let%expect_test "aborting withdraws the question and stops the timer" =
  with_agent [ ask "q1" ]
  @@ fun agent ~fire ~settle ->
  Or_error.ok_exn (Agent.prompt agent "go");
  settle ();
  ignore (Agent.abort agent : string list);
  Agent.wait_idle agent;
  fire ();
  [%expect
    {|
    ask_user q1, deadline in 600s
    ((q1 (600)))
    timer: due in 600s
    timer: cancelled
    ask_user_done q1, timed_out=false
    timer: nothing waiting
    |}]
;;

let%expect_test "the call's timeout_seconds replaces the config's" =
  with_agent
    [ ask ~timeout_seconds:120 "q1"
    ; ask ~timeout_seconds:1 "q2"
    ; ask ~timeout_seconds:(-5) "q3"
    ; Reply.text "ok"
    ]
  @@ fun agent ~fire ~settle ->
  Or_error.ok_exn (Agent.prompt agent "go");
  settle ();
  fire ();
  settle ();
  fire ();
  Agent.wait_idle agent;
  [%expect
    {|
    ask_user q1, deadline in 120s
    ((q1 (120)))
    timer: due in 120s
    timer: fires
    ask_user_done q1, timed_out=true
    model reads: ERROR [the user did not answer within 2m; carry on with your best judgement and say what you assumed]
    ask_user q2, deadline in 30s
    ((q2 (30)))
    timer: due in 30s
    timer: fires
    ask_user_done q2, timed_out=true
    model reads: ERROR [the user did not answer within 30s; carry on with your best judgement and say what you assumed]
    model reads: ERROR timeout_seconds must be 0 (wait until answered) or a number of seconds
    |}]
;;

let%expect_test "0 waits until answered: in the config, or for one call" =
  let run ?config ?timeout_seconds () =
    with_agent ?config [ ask ?timeout_seconds "q1"; Reply.text "ok" ]
    @@ fun agent ~fire ~settle ->
    Or_error.ok_exn (Agent.prompt agent "go");
    settle ();
    fire ();
    settle ();
    ignore
      (Agent.answer_question agent ~call_id:"q1" (Answered [ [ "Postgres" ] ])
       : unit Or_error.t);
    Agent.wait_idle agent
  in
  run ~config:{|{"ask_user_timeout_seconds": 0}|} ();
  [%expect
    {|
    ask_user q1, no deadline
    ((q1 ()))
    timer: nothing waiting
    ask_user_done q1, timed_out=false
    model reads: The user answered:
    Q: Which database?
    A: Postgres
    |}];
  (* The user turned timeouts off: the model cannot turn them on. *)
  run ~config:{|{"ask_user_timeout_seconds": 0}|} ~timeout_seconds:60 ();
  [%expect
    {|
    ask_user q1, no deadline
    ((q1 ()))
    timer: nothing waiting
    ask_user_done q1, timed_out=false
    model reads: The user answered:
    Q: Which database?
    A: Postgres
    |}];
  run ~timeout_seconds:0 ();
  [%expect
    {|
    ask_user q1, no deadline
    ((q1 ()))
    timer: nothing waiting
    ask_user_done q1, timed_out=false
    model reads: The user answered:
    Q: Which database?
    A: Postgres
    |}];
  run ~config:{|{"ask_user_timeout_seconds": 90}|} ();
  [%expect
    {|
    ask_user q1, deadline in 90s
    ((q1 (90)))
    timer: due in 90s
    timer: fires
    ask_user_done q1, timed_out=true
    model reads: ERROR [the user did not answer within 1m 30s; carry on with your best judgement and say what you assumed]
    |}]
;;

let%expect_test "an answer that comes in as the timer fires still counts" =
  with_agent [ ask "q1"; Reply.text "ok" ]
  @@ fun agent ~fire ~settle ->
  Or_error.ok_exn (Agent.prompt agent "go");
  settle ();
  fire ();
  print_s
    [%sexp
      (Agent.answer_question agent ~call_id:"q1" (Answered [ [ "SQLite" ] ])
       : unit Or_error.t)];
  Agent.wait_idle agent;
  [%expect {| |}]
;;
