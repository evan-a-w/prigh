open! Core
open! Prigh
open Tool_test_helpers
open Trace_test_helpers
module Reply = Faux_provider.Reply

(* The traces of whole sessions: [Agent] runs on virtual time with the
   models answering after [Trace_test_helpers.timed]'s delays, and its trace
   file is read back. *)

let danger =
  { Tool.spec =
      { Tool_spec.name = "danger"
      ; description = "needs approval"
      ; parameters = Tool_args.schema []
      ; parallel_safe = false
      ; destructive = Always
      ; on_host = false
      ; freeform = None
      }
  ; run = (fun _ _ -> Tool.Result.ok "done it")
  }
;;

let subagent_tools ~provider =
  Tool_subagent.create
    ~provider
    ~current_model:(fun () -> Model.default)
    ~current_thinking:(fun () -> Off)
    ~home:"/nonexistent"
    ()
  :: Tool_subagent.control_tools
;;

let trace agent =
  let spans, bad =
    Trace_file.load ~session_path:(Session.path (Agent.session agent))
  in
  if bad > 0 then printf "%d unreadable lines\n" bad;
  spans
;;

(* Runs [f] with an agent whose main model follows [main] and whose
   subagents follow [children] (by task, see [Routed_provider]); prints the
   session's trace once it is idle. *)
let with_agent
      ?(tools = fun _ -> [])
      ?(print = true)
      ?backend_host
      ?session
      ~main
      children
      f
  =
  with_sandbox
  @@ fun t ->
  let clock = Virtual_clock.create () in
  Virtual_clock.run clock
  @@ fun () ->
  Eio.Switch.run
  @@ fun sw ->
  let provider = timed clock (Routed_provider.create ~main children) in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:
        ([ work_tool clock; danger ]
         @ subagent_tools ~provider
         @ Tools.interaction
         @ tools clock)
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:t.dir
      ~trace_clock:(Virtual_clock.trace_clock clock)
      ?backend_host
      ?session:
        (Option.map session ~f:(fun path -> Or_error.ok_exn (Session.load path)))
      ()
  in
  let result = f t clock agent in
  Agent.wait_idle agent;
  if print then print_outline (trace agent);
  result
;;

(* Calls [f] (on its own fiber) whenever the agent asks for something. *)
let on_request ~sw agent f =
  Agent.subscribe agent ~f:(function
    | Loop event -> Eio.Fiber.fork ~sw (fun () -> f event)
    | _ -> ())
;;

let%expect_test "an approval's wait is its own span under the tool" =
  (with_agent
     ~main:
       [ Reply.tool_call ~id:"d1" ~name:"danger" ~arguments:"{}" ()
       ; Reply.text "done"
       ]
     []
   @@ fun _t clock agent ->
   Agent.set_mode agent ~approval:Ask;
   Eio.Switch.run
   @@ fun sw ->
   on_request ~sw agent (function
     | Tool_confirm { call_id; agent_id; _ } ->
       Virtual_clock.sleep clock 4.;
       Or_error.ok_exn (Agent.respond_confirm ?agent_id agent ~call_id Allow)
     | _ -> ());
   Or_error.ok_exn (Agent.prompt agent "go");
   Agent.wait_idle agent);
  [%expect
    {|
    prompt 5.3s
      turn 1 4.7s
        model_request deepseek/deepseek-flash 700ms  stop=tool_use
          connect 500ms
          first_token 100ms
          stream 100ms
        tool danger 4.0s  where=local result_bytes=7
          approval_wait 4.0s
      turn 2 600ms
        model_request deepseek/deepseek-flash 600ms  stop=end_turn
          connect 500ms
          first_token 100ms
          stream 0ms
    |}]
;;

let ask_user id =
  Reply.tool_call
    ~id
    ~name:"ask_user"
    ~arguments:
      {|{"questions":[{"question":"Which?","options":[{"label":"A"},{"label":"B"}]}]}|}
    ()
;;

let%expect_test "waiting for an answer to ask_user is its own span" =
  (with_agent ~main:[ ask_user "q1"; Reply.text "went with A" ] []
   @@ fun _t clock agent ->
   Eio.Switch.run
   @@ fun sw ->
   on_request ~sw agent (function
     | Ask_user { call_id; _ } ->
       Virtual_clock.sleep clock 7.;
       Or_error.ok_exn
         (Agent.answer_question agent ~call_id (Answered [ [ "A" ] ]))
     | _ -> ());
   Or_error.ok_exn (Agent.prompt agent "go");
   Agent.wait_idle agent);
  [%expect
    {|
    prompt 8.3s
      turn 1 7.7s
        model_request deepseek/deepseek-flash 700ms  stop=tool_use
          connect 500ms
          first_token 100ms
          stream 100ms
        tool ask_user 7.0s  where=local result_bytes=34
          ask_user_wait 7.0s
      turn 2 600ms
        model_request deepseek/deepseek-flash 600ms  stop=end_turn
          connect 500ms
          first_token 100ms
          stream 0ms
    |}]
;;

(* How many runs of the main agent have ended, to wait for the ones that
   deliver background work. *)
let count_runs agent =
  let ends = ref 0 in
  Agent.subscribe agent ~f:(function
    | Loop (Agent_end _) -> incr ends
    | _ -> ());
  fun n ->
    let rec wait () =
      if !ends < n || Agent.is_running agent
      then (
        Eio.Fiber.yield ();
        wait ())
    in
    wait ()
;;

(* Until the agent is idle with all its background work done and
   delivered. *)
let rec wait_quiet agent =
  let s = Agent.state agent in
  if s.running || not (List.is_empty s.subagents && List.is_empty s.jobs)
  then (
    Eio.Fiber.yield ();
    wait_quiet agent)
;;

let subagent ?(id = "c1") arguments =
  Reply.tool_call ~id ~name:"subagent" ~arguments ()
;;

let%expect_test "background subagents run side by side on their own tracks" =
  let spans =
    with_agent
      ~print:false
      ~main:
        [ Reply.tool_calls
            [ "c1", "subagent", {|{"task":"short"}|}
            ; "c2", "subagent", {|{"task":"long"}|}
            ]
        ; Reply.text "spawned"
        ; Reply.text "one back"
        ; Reply.text "both back"
        ]
      [ "short", [ work 1; Reply.text "short done" ]
      ; "long", [ work 4; Reply.text "long done" ]
      ]
    @@ fun _t _clock agent ->
    let runs = count_runs agent in
    Or_error.ok_exn (Agent.prompt agent "go");
    runs 3;
    trace agent
  in
  print_outline spans;
  print_tracks spans;
  [%expect
    {|
    prompt 1.5s
      turn 1 900ms
        model_request deepseek/deepseek-flash 900ms  stop=tool_use
          connect 500ms
          first_token 100ms
          stream 300ms
        tool subagent 0ms  where=local result_bytes=119
        tool subagent 0ms  where=local result_bytes=118
        subagent a1 2.3s  background=true forked=false continued=false
          turn 1 1.7s
            model_request deepseek/deepseek-flash 700ms  stop=tool_use
              connect 500ms
              first_token 100ms
              stream 100ms
            tool work 1.0s  where=local result_bytes=9
          turn 2 600ms
            model_request deepseek/deepseek-flash 600ms  stop=end_turn
              connect 500ms
              first_token 100ms
              stream 0ms
        subagent a2 5.3s  background=true forked=false continued=false
          turn 1 4.7s
            model_request deepseek/deepseek-flash 700ms  stop=tool_use
              connect 500ms
              first_token 100ms
              stream 100ms
            tool work 4.0s  where=local result_bytes=9
          turn 2 600ms
            model_request deepseek/deepseek-flash 600ms  stop=end_turn
              connect 500ms
              first_token 100ms
              stream 0ms
      turn 2 600ms
        model_request deepseek/deepseek-flash 600ms  stop=end_turn
          connect 500ms
          first_token 100ms
          stream 0ms
    prompt 600ms
      turn 1 600ms
        model_request deepseek/deepseek-flash 600ms  stop=end_turn
          connect 500ms
          first_token 100ms
          stream 0ms
    prompt 600ms
      turn 1 600ms
        model_request deepseek/deepseek-flash 600ms  stop=end_turn
          connect 500ms
          first_token 100ms
          stream 0ms
    track "main": 25 spans
    track "subagent a1": 12 spans
    track "subagent a2": 12 spans
    |}]
;;

let%expect_test
    "a forked subagent, and one continued after running out of turns"
  =
  let spans =
    with_agent
      ~print:false
      ~main:
        [ subagent {|{"task":"fork it","fork":true}|}
        ; Reply.text "forked"
        ; Reply.text "fork back"
        ; subagent ~id:"c2" {|{"task":"limited","max_turns":1}|}
        ; Reply.text "spawned"
        ; Reply.tool_call
            ~id:"k1"
            ~name:"subagent_continue"
            ~arguments:{|{"id":"a2","max_turns":2}|}
            ()
        ; Reply.text "continued"
        ; Reply.text "thanks"
        ]
      [ "fork it", [ Reply.text "fork done" ]
      ; "limited", [ work 2; work ~id:"w2" 3; Reply.text "limited done" ]
      ]
    @@ fun _t _clock agent ->
    let runs = count_runs agent in
    Or_error.ok_exn (Agent.prompt agent "go");
    runs 2;
    Or_error.ok_exn (Agent.prompt agent "now a limited one");
    runs 5;
    trace agent
  in
  print_outline spans;
  print_tracks spans;
  [%expect
    {|
    prompt 1.3s
      turn 1 700ms
        model_request deepseek/deepseek-flash 700ms  stop=tool_use
          connect 500ms
          first_token 100ms
          stream 100ms
        tool subagent 0ms  where=local result_bytes=129
        subagent a1 600ms  background=true forked=true continued=false
          turn 1 600ms
            model_request deepseek/deepseek-flash 600ms  stop=end_turn
              connect 500ms
              first_token 100ms
              stream 0ms
      turn 2 600ms
        model_request deepseek/deepseek-flash 600ms  stop=end_turn
          connect 500ms
          first_token 100ms
          stream 0ms
    prompt 600ms
      turn 1 600ms
        model_request deepseek/deepseek-flash 600ms  stop=end_turn
          connect 500ms
          first_token 100ms
          stream 0ms
    prompt 1.3s
      turn 1 700ms
        model_request deepseek/deepseek-flash 700ms  stop=tool_use
          connect 500ms
          first_token 100ms
          stream 100ms
        tool subagent 0ms  where=local result_bytes=121
        subagent a2 2.7s  background=true forked=false continued=false
          turn 1 2.7s
            model_request deepseek/deepseek-flash 700ms  stop=tool_use
              connect 500ms
              first_token 100ms
              stream 100ms
            tool work 2.0s  where=local result_bytes=9
      turn 2 600ms
        model_request deepseek/deepseek-flash 600ms  stop=end_turn
          connect 500ms
          first_token 100ms
          stream 0ms
    prompt 1.3s
      turn 1 700ms
        model_request deepseek/deepseek-flash 700ms  stop=tool_use
          connect 500ms
          first_token 100ms
          stream 100ms
        tool subagent_continue 0ms  where=local result_bytes=126
        subagent a2 4.3s  background=true forked=false continued=true
          turn 1 3.7s
            model_request deepseek/deepseek-flash 700ms  stop=tool_use
              connect 500ms
              first_token 100ms
              stream 100ms
            tool work 3.0s  where=local result_bytes=9
          turn 2 600ms
            model_request deepseek/deepseek-flash 600ms  stop=end_turn
              connect 500ms
              first_token 100ms
              stream 0ms
      turn 2 600ms
        model_request deepseek/deepseek-flash 600ms  stop=end_turn
          connect 500ms
          first_token 100ms
          stream 0ms
    prompt 600ms
      turn 1 600ms
        model_request deepseek/deepseek-flash 600ms  stop=end_turn
          connect 500ms
          first_token 100ms
          stream 0ms
    track "main": 48 spans
    track "subagent a1": 6 spans
    track "subagent a2": 19 spans
    |}]
;;

let%expect_test "a background job outlives the turn that started it" =
  (with_agent
     ~tools:(fun _ -> Tools.all)
     ~main:
       [ Reply.tool_call
           ~id:"b1"
           ~name:"bash"
           ~arguments:
             {|{"command":"while [ ! -e go ]; do sleep 0.01; done","background":true}|}
           ()
       ; Reply.text "started"
       ; Reply.text "it finished"
       ]
     []
   @@ fun t clock agent ->
   let runs = count_runs agent in
   Or_error.ok_exn (Agent.prompt agent "go");
   runs 1;
   Virtual_clock.sleep clock 10.;
   write t "go" "";
   runs 2);
  [%expect
    {|
    prompt 1.3s
      turn 1 700ms
        model_request deepseek/deepseek-flash 700ms  stop=tool_use
          connect 500ms
          first_token 100ms
          stream 100ms
        tool bash (while [ ! -e go ]; do sleep 0.01; done) 0ms  where=local result_bytes=166
        job j1 10.6s  outcome=exited 0
      turn 2 600ms
        model_request deepseek/deepseek-flash 600ms  stop=end_turn
          connect 500ms
          first_token 100ms
          stream 0ms
    prompt 600ms
      turn 1 600ms
        model_request deepseek/deepseek-flash 600ms  stop=end_turn
          connect 500ms
          first_token 100ms
          stream 0ms
    |}]
;;

let%expect_test "a call forwarded to a tool host times its round trip" =
  (with_agent
     ~backend_host:false
     ~tools:(fun _ -> Tools.all)
     ~main:
       [ Reply.tool_call
           ~id:"r1"
           ~name:"read"
           ~arguments:{|{"path":"notes.txt"}|}
           ()
       ; Reply.text "read it"
       ]
     []
   @@ fun t clock agent ->
   write t "notes.txt" "some notes\n";
   Eio.Switch.run
   @@ fun sw ->
   Agent.subscribe agent ~f:(function
     | Tool_exec { exec_id; name; arguments; cwd; _ } ->
       Eio.Fiber.fork ~sw (fun () ->
         (* Only the model's call is slow, not the agent's own look around. *)
         if String.equal name "read" then Virtual_clock.sleep clock 2.;
         let result =
           Host_ops.execute
             ~mcp:None
             ~env:t.env
             ~cancel:Cancellation.never
             ~on_output:ignore
             ~cwd
             ~name
             ~arguments
             ()
         in
         Or_error.ok_exn (Agent.tool_exec_result agent ~exec_id result))
     | _ -> ());
   Agent.set_hosts
     agent
     [ { id = "h"
       ; name = "h"
       ; cwd = t.dir
       ; session_id = None
       ; session_name = None
       ; home = Some t.dir
       ; exec_input = false
       ; git_location = false
       }
     ];
   Or_error.ok_exn (Agent.prompt agent "go");
   Agent.wait_idle agent);
  [%expect
    {|
    prompt 3.3s
      turn 1 2.7s
        model_request deepseek/deepseek-flash 700ms  stop=tool_use
          connect 500ms
          first_token 100ms
          stream 100ms
        tool read (notes.txt) 2.0s  where=tool_host result_bytes=11
          host_roundtrip 2.0s
      turn 2 600ms
        model_request deepseek/deepseek-flash 600ms  stop=end_turn
          connect 500ms
          first_token 100ms
          stream 0ms
    |}]
;;

let%expect_test "aborting mid-tool ends the prompt cancelled" =
  (with_agent ~main:[ work 30; Reply.text "never" ] []
   @@ fun _t clock agent ->
   Or_error.ok_exn (Agent.prompt agent "go");
   Virtual_clock.sleep clock 3.;
   print_s [%sexp (Agent.abort agent : string list)];
   Agent.wait_idle agent);
  [%expect
    {|
    ()
    prompt 3.0s [cancelled]
      turn 1 3.0s [cancelled]
        model_request deepseek/deepseek-flash 700ms  stop=tool_use
          connect 500ms
          first_token 100ms
          stream 100ms
        tool work 2.3s [cancelled]  where=local result_bytes=11
    |}]
;;

let%expect_test "the trace survives reloading the session, and goes with it" =
  let prompt text agent =
    Or_error.ok_exn (Agent.prompt agent text);
    Agent.wait_idle agent;
    Session.path (Agent.session agent)
  in
  let path =
    with_agent ~print:false ~main:[ work 1; Reply.text "done" ] []
    @@ fun _t _clock -> prompt "go"
  in
  (* Another process resumes it and appends to the same trace. *)
  let (_ : string) =
    with_agent ~print:false ~session:path ~main:[ Reply.text "again" ] []
    @@ fun _t _clock -> prompt "more"
  in
  let spans, bad = Trace_file.load ~session_path:path in
  print_outline spans;
  let prefixes =
    List.filter_map spans ~f:(fun s ->
      Option.map (String.rindex s.id '.') ~f:(fun i -> String.prefix s.id i))
    |> List.dedup_and_sort ~compare:String.compare
  in
  printf "%d bad lines, %d id prefixes\n" bad (List.length prefixes);
  let files () =
    List.map (Session.files path) ~f:(fun p ->
      String.chop_prefix_exn
        (Filename.basename p)
        ~prefix:(Filename.basename (Session.data_dir path)))
  in
  print_s [%sexp (files () : string list)];
  Session.delete_files path;
  print_s [%sexp (files () : string list)];
  [%expect
    {|
    prompt 2.3s
      turn 1 1.7s
        model_request deepseek/deepseek-flash 700ms  stop=tool_use
          connect 500ms
          first_token 100ms
          stream 100ms
        tool work 1.0s  where=local result_bytes=9
      turn 2 600ms
        model_request deepseek/deepseek-flash 600ms  stop=end_turn
          connect 500ms
          first_token 100ms
          stream 0ms
    prompt 600ms
      turn 1 600ms
        model_request deepseek/deepseek-flash 600ms  stop=end_turn
          connect 500ms
          first_token 100ms
          stream 0ms
    0 bad lines, 2 id prefixes
    (.jsonl .trace.jsonl)
    ()
    |}]
;;

let print_perfetto spans =
  match Trace_report.perfetto spans with
  | `Object fields ->
    List.iter fields ~f:(fun (key, value) ->
      match value with
      | `Array events ->
        printf "%s:\n" key;
        List.iter events ~f:(fun e -> printf "  %s\n" (Jsonaf.to_string e))
      | value -> printf "%s: %s\n" key (Jsonaf.to_string value))
  | json -> print_endline (Jsonaf.to_string json)
;;

(* One session with a bit of everything: the summary splits the main
   agent's time, and Perfetto puts subagents and jobs on their own threads. *)
let%expect_test "the summary and the Perfetto export of a session" =
  let spans =
    with_agent
      ~print:false
      ~tools:(fun _ -> Tools.all)
      ~main:
        [ Reply.tool_calls
            [ "c1", "subagent", {|{"task":"child"}|}
            ; "w1", "work", {|{"seconds":2}|}
            ; "w2", "work", {|{"seconds":3}|}
            ]
        ; Reply.tool_call ~id:"d1" ~name:"danger" ~arguments:"{}" ()
        ; ask_user "q1"
        ; Reply.tool_call
            ~id:"b1"
            ~name:"bash"
            ~arguments:
              {|{"command":"while [ ! -e go ]; do sleep 0.01; done","background":true}|}
            ()
        ; Reply.text "waiting"
        ; Reply.text "child back"
        ; Reply.text "job back"
        ]
      [ "child", [ work 1; Reply.text "child done" ] ]
    @@ fun t clock agent ->
    Agent.set_mode agent ~approval:Ask;
    let runs = count_runs agent in
    Eio.Switch.run
    @@ fun sw ->
    on_request ~sw agent (function
      | Tool_confirm { call_id; agent_id; _ } ->
        Virtual_clock.sleep clock 4.;
        Or_error.ok_exn (Agent.respond_confirm ?agent_id agent ~call_id Always)
      | Ask_user { call_id; _ } ->
        Virtual_clock.sleep clock 7.;
        Or_error.ok_exn
          (Agent.answer_question agent ~call_id (Answered [ [ "A" ] ]))
      | _ -> ());
    Or_error.ok_exn (Agent.prompt agent "go");
    runs 1;
    write t "go" "";
    wait_quiet agent;
    trace agent
  in
  print_string (Trace_report.summary ~slowest:6 spans);
  [%expect
    {|
    2 prompts, 8 turns, 8 model requests, 7 tool calls (54 spans)

    Wall time 22.4s (the main agent, prompt to idle):
      waiting on user  15.0s  67%
      model             4.4s  20%
      tools             3.0s  13%
      other              0ms   0%

    Time to first token:
      model                    requests  median    p90
      deepseek/deepseek-flash         8   600ms  600ms

    Tools (by total time, waits for approval and answers included):
      tool      calls  total   max  errors
      ask_user      1   7.0s  7.0s       0
      work          3   6.0s  3.0s       0
      danger        1   4.0s  4.0s       0
      bash          1   4.0s  4.0s       0
      subagent      1    0ms   0ms       0

    Subagents and jobs (alongside the main agent):
      background subagent runs  1   2.3s
      background jobs           1  600ms

    Slowest spans:
      7.0s  ask_user_wait  in prompt > turn 3 > tool ask_user
      7.0s  tool ask_user  in prompt > turn 3
      4.0s  approval_wait  in prompt > turn 2 > tool danger
      4.0s  tool danger  in prompt > turn 2
      4.0s  approval_wait  in prompt > turn 4 > tool bash (while [ ! -e go ]; do sleep 0.01; done)
      4.0s  tool bash (while [ ! -e go ]; do sleep 0.01; done)  in prompt > turn 4
    |}];
  print_perfetto spans;
  [%expect
    {|
    displayTimeUnit: "ms"
    traceEvents:
      {"name":"process_name","ph":"M","pid":1,"args":{"name":"prigh"}}
      {"name":"thread_name","ph":"M","pid":1,"tid":1,"args":{"name":"main"}}
      {"name":"thread_sort_index","ph":"M","pid":1,"tid":1,"args":{"sort_index":1}}
      {"name":"thread_name","ph":"M","pid":1,"tid":2,"args":{"name":"subagent a1"}}
      {"name":"thread_sort_index","ph":"M","pid":1,"tid":2,"args":{"sort_index":2}}
      {"name":"thread_name","ph":"M","pid":1,"tid":3,"args":{"name":"main (2)"}}
      {"name":"thread_sort_index","ph":"M","pid":1,"tid":3,"args":{"sort_index":3}}
      {"name":"thread_name","ph":"M","pid":1,"tid":4,"args":{"name":"job j1"}}
      {"name":"thread_sort_index","ph":"M","pid":1,"tid":4,"args":{"sort_index":4}}
      {"name":"prompt","cat":"prompt","ph":"X","ts":0,"dur":21800000,"pid":1,"tid":1,"args":{"status":"ok","model":"deepseek/deepseek-flash"}}
      {"name":"turn 1","cat":"turn","ph":"X","ts":0,"dur":4100000,"pid":1,"tid":1,"args":{"status":"ok","turn":1,"tool_calls":3}}
      {"name":"model_request deepseek/deepseek-flash","cat":"model_request","ph":"X","ts":0,"dur":1100000,"pid":1,"tid":1,"args":{"status":"ok","model":"deepseek/deepseek-flash","thinking":"on","input_tokens":20,"output_tokens":8,"cache_read_tokens":5,"cost_usd":1.413e-05,"deltas":6,"stop":"tool_use"}}
      {"name":"connect","cat":"connect","ph":"X","ts":0,"dur":500000,"pid":1,"tid":1,"args":{"status":"ok"}}
      {"name":"first_token","cat":"first_token","ph":"X","ts":500000,"dur":100000,"pid":1,"tid":1,"args":{"status":"ok"}}
      {"name":"stream","cat":"stream","ph":"X","ts":600000,"dur":500000,"pid":1,"tid":1,"args":{"status":"ok"}}
      {"name":"tool subagent","cat":"tool","ph":"X","ts":1100000,"dur":0,"pid":1,"tid":1,"args":{"status":"ok","tool":"subagent","call_id":"c1","where":"local","result_bytes":119,"is_error":false}}
      {"name":"tool work","cat":"tool","ph":"X","ts":1100000,"dur":3000000,"pid":1,"tid":1,"args":{"status":"ok","tool":"work","call_id":"w2","where":"local","result_bytes":9,"is_error":false}}
      {"name":"subagent a1","cat":"subagent","ph":"X","ts":1100000,"dur":2300000,"pid":1,"tid":2,"args":{"status":"ok","agent_id":"a1","model":"deepseek/deepseek-flash","background":true,"forked":false,"continued":false}}
      {"name":"tool work","cat":"tool","ph":"X","ts":1100000,"dur":2000000,"pid":1,"tid":3,"args":{"status":"ok","tool":"work","call_id":"w1","where":"local","result_bytes":9,"is_error":false}}
      {"name":"turn 1","cat":"turn","ph":"X","ts":1100000,"dur":1700000,"pid":1,"tid":2,"args":{"status":"ok","turn":1,"tool_calls":1}}
      {"name":"model_request deepseek/deepseek-flash","cat":"model_request","ph":"X","ts":1100000,"dur":700000,"pid":1,"tid":2,"args":{"status":"ok","model":"deepseek/deepseek-flash","thinking":"off","input_tokens":20,"output_tokens":8,"cache_read_tokens":5,"cost_usd":1.413e-05,"deltas":2,"stop":"tool_use"}}
      {"name":"connect","cat":"connect","ph":"X","ts":1100000,"dur":500000,"pid":1,"tid":2,"args":{"status":"ok"}}
      {"name":"first_token","cat":"first_token","ph":"X","ts":1600000,"dur":100000,"pid":1,"tid":2,"args":{"status":"ok"}}
      {"name":"stream","cat":"stream","ph":"X","ts":1700000,"dur":100000,"pid":1,"tid":2,"args":{"status":"ok"}}
      {"name":"tool work","cat":"tool","ph":"X","ts":1800000,"dur":1000000,"pid":1,"tid":2,"args":{"status":"ok","tool":"work","call_id":"w1","where":"local","result_bytes":9,"is_error":false}}
      {"name":"turn 2","cat":"turn","ph":"X","ts":2800000,"dur":600000,"pid":1,"tid":2,"args":{"status":"ok","turn":2,"tool_calls":0}}
      {"name":"model_request deepseek/deepseek-flash","cat":"model_request","ph":"X","ts":2800000,"dur":600000,"pid":1,"tid":2,"args":{"status":"ok","model":"deepseek/deepseek-flash","thinking":"off","input_tokens":10,"output_tokens":5,"cache_read_tokens":0,"cost_usd":9e-06,"deltas":1,"stop":"end_turn"}}
      {"name":"connect","cat":"connect","ph":"X","ts":2800000,"dur":500000,"pid":1,"tid":2,"args":{"status":"ok"}}
      {"name":"first_token","cat":"first_token","ph":"X","ts":3300000,"dur":100000,"pid":1,"tid":2,"args":{"status":"ok"}}
      {"name":"stream","cat":"stream","ph":"X","ts":3400000,"dur":0,"pid":1,"tid":2,"args":{"status":"ok"}}
      {"name":"turn 2","cat":"turn","ph":"X","ts":4100000,"dur":4700000,"pid":1,"tid":1,"args":{"status":"ok","turn":2,"tool_calls":1}}
      {"name":"model_request deepseek/deepseek-flash","cat":"model_request","ph":"X","ts":4100000,"dur":700000,"pid":1,"tid":1,"args":{"status":"ok","model":"deepseek/deepseek-flash","thinking":"on","input_tokens":20,"output_tokens":8,"cache_read_tokens":5,"cost_usd":1.413e-05,"deltas":2,"stop":"tool_use"}}
      {"name":"connect","cat":"connect","ph":"X","ts":4100000,"dur":500000,"pid":1,"tid":1,"args":{"status":"ok"}}
      {"name":"first_token","cat":"first_token","ph":"X","ts":4600000,"dur":100000,"pid":1,"tid":1,"args":{"status":"ok"}}
      {"name":"stream","cat":"stream","ph":"X","ts":4700000,"dur":100000,"pid":1,"tid":1,"args":{"status":"ok"}}
      {"name":"tool danger","cat":"tool","ph":"X","ts":4800000,"dur":4000000,"pid":1,"tid":1,"args":{"status":"ok","tool":"danger","call_id":"d1","where":"local","result_bytes":7,"is_error":false}}
      {"name":"approval_wait","cat":"approval_wait","ph":"X","ts":4800000,"dur":4000000,"pid":1,"tid":1,"args":{"status":"ok"}}
      {"name":"turn 3","cat":"turn","ph":"X","ts":8800000,"dur":7700000,"pid":1,"tid":1,"args":{"status":"ok","turn":3,"tool_calls":1}}
      {"name":"model_request deepseek/deepseek-flash","cat":"model_request","ph":"X","ts":8800000,"dur":700000,"pid":1,"tid":1,"args":{"status":"ok","model":"deepseek/deepseek-flash","thinking":"on","input_tokens":20,"output_tokens":8,"cache_read_tokens":5,"cost_usd":1.413e-05,"deltas":2,"stop":"tool_use"}}
      {"name":"connect","cat":"connect","ph":"X","ts":8800000,"dur":500000,"pid":1,"tid":1,"args":{"status":"ok"}}
      {"name":"first_token","cat":"first_token","ph":"X","ts":9300000,"dur":100000,"pid":1,"tid":1,"args":{"status":"ok"}}
      {"name":"stream","cat":"stream","ph":"X","ts":9400000,"dur":100000,"pid":1,"tid":1,"args":{"status":"ok"}}
      {"name":"tool ask_user","cat":"tool","ph":"X","ts":9500000,"dur":7000000,"pid":1,"tid":1,"args":{"status":"ok","tool":"ask_user","call_id":"q1","where":"local","result_bytes":34,"is_error":false}}
      {"name":"ask_user_wait","cat":"ask_user_wait","ph":"X","ts":9500000,"dur":7000000,"pid":1,"tid":1,"args":{"status":"ok","questions":1}}
      {"name":"turn 4","cat":"turn","ph":"X","ts":16500000,"dur":4700000,"pid":1,"tid":1,"args":{"status":"ok","turn":4,"tool_calls":1}}
      {"name":"model_request deepseek/deepseek-flash","cat":"model_request","ph":"X","ts":16500000,"dur":700000,"pid":1,"tid":1,"args":{"status":"ok","model":"deepseek/deepseek-flash","thinking":"on","input_tokens":20,"output_tokens":8,"cache_read_tokens":5,"cost_usd":1.413e-05,"deltas":2,"stop":"tool_use"}}
      {"name":"connect","cat":"connect","ph":"X","ts":16500000,"dur":500000,"pid":1,"tid":1,"args":{"status":"ok"}}
      {"name":"first_token","cat":"first_token","ph":"X","ts":17000000,"dur":100000,"pid":1,"tid":1,"args":{"status":"ok"}}
      {"name":"stream","cat":"stream","ph":"X","ts":17100000,"dur":100000,"pid":1,"tid":1,"args":{"status":"ok"}}
      {"name":"tool bash (while [ ! -e go ]; do sleep 0.01; done)","cat":"tool","ph":"X","ts":17200000,"dur":4000000,"pid":1,"tid":1,"args":{"status":"ok","tool":"bash","call_id":"b1","summary":"while [ ! -e go ]; do sleep 0.01; done","where":"local","result_bytes":166,"is_error":false}}
      {"name":"approval_wait","cat":"approval_wait","ph":"X","ts":17200000,"dur":4000000,"pid":1,"tid":1,"args":{"status":"ok"}}
      {"name":"turn 5","cat":"turn","ph":"X","ts":21200000,"dur":600000,"pid":1,"tid":1,"args":{"status":"ok","turn":5,"tool_calls":0}}
      {"name":"model_request deepseek/deepseek-flash","cat":"model_request","ph":"X","ts":21200000,"dur":600000,"pid":1,"tid":1,"args":{"status":"ok","model":"deepseek/deepseek-flash","thinking":"on","input_tokens":10,"output_tokens":5,"cache_read_tokens":0,"cost_usd":9e-06,"deltas":1,"stop":"end_turn"}}
      {"name":"job j1","cat":"job","ph":"X","ts":21200000,"dur":600000,"pid":1,"tid":4,"args":{"status":"ok","job_id":"j1","label":"while [ ! -e go ]; do sleep 0.01; done","outcome":"exited 0"}}
      {"name":"connect","cat":"connect","ph":"X","ts":21200000,"dur":500000,"pid":1,"tid":1,"args":{"status":"ok"}}
      {"name":"first_token","cat":"first_token","ph":"X","ts":21700000,"dur":100000,"pid":1,"tid":1,"args":{"status":"ok"}}
      {"name":"stream","cat":"stream","ph":"X","ts":21800000,"dur":0,"pid":1,"tid":1,"args":{"status":"ok"}}
      {"name":"prompt","cat":"prompt","ph":"X","ts":21800000,"dur":600000,"pid":1,"tid":1,"args":{"status":"ok","model":"deepseek/deepseek-flash"}}
      {"name":"turn 1","cat":"turn","ph":"X","ts":21800000,"dur":600000,"pid":1,"tid":1,"args":{"status":"ok","turn":1,"tool_calls":0}}
      {"name":"model_request deepseek/deepseek-flash","cat":"model_request","ph":"X","ts":21800000,"dur":600000,"pid":1,"tid":1,"args":{"status":"ok","model":"deepseek/deepseek-flash","thinking":"on","input_tokens":10,"output_tokens":5,"cache_read_tokens":0,"cost_usd":9e-06,"deltas":1,"stop":"end_turn"}}
      {"name":"connect","cat":"connect","ph":"X","ts":21800000,"dur":500000,"pid":1,"tid":1,"args":{"status":"ok"}}
      {"name":"first_token","cat":"first_token","ph":"X","ts":22300000,"dur":100000,"pid":1,"tid":1,"args":{"status":"ok"}}
      {"name":"stream","cat":"stream","ph":"X","ts":22400000,"dur":0,"pid":1,"tid":1,"args":{"status":"ok"}}
    |}]
;;
