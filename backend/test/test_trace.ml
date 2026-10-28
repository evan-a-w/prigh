open! Core
open! Prigh
open Tool_test_helpers
open Trace_test_helpers
module Reply = Faux_provider.Reply
module Gates = Routed_provider.Gates

let loop_config ?(retries = 0) ?(compaction = Compaction_settings.window_only) tools
  =
  { Agent_loop.Config.model = Model.default
  ; thinking = Off
  ; system = None
  ; tools
  ; max_turns = None
  ; max_tokens = None
  ; retries
  ; cache_key = None
  ; compaction
  }
;;

(* Runs [Agent_loop.run] under a [prompt] span as [Agent] does, returning
   the spans written. *)
let run_loop
      ?(level = Trace.Level.Default)
      ?retries
      ?compaction
      ?cancel
      ?(tools = fun _ -> [])
      ?(connect = 0.5)
      replies
  =
  with_sandbox
  @@ fun t ->
  let clock = Virtual_clock.create () in
  let spans = Queue.create () in
  let tracer =
    Trace.create
      ~id_prefix:"s"
      ~clock:(Virtual_clock.trace_clock clock)
      ~level
      ~write:(Queue.enqueue spans)
      ()
  in
  let provider = timed ~connect clock (Faux_provider.create replies) in
  Virtual_clock.run clock @@ fun () ->
  Trace.run tracer (fun () ->
    Trace.span ~root:true "prompt" (fun () ->
      ignore
        (Agent_loop.run
           ~env:t.env
           ~provider
           ~config:(loop_config ?retries ?compaction (tools clock))
           ~cwd:t.dir
           ?cancel
           ~sleep:(Virtual_clock.sleep clock)
           ~context:[]
           ~prompts:[ Message.user "go" ]
           ()
         : Message.t list)));
  Queue.to_list spans
;;

let%expect_test "a turn with a tool: the model's phases and the tool" =
  let spans =
    run_loop
      ~tools:(fun clock -> [ work_tool clock ])
      [ work 2; Reply.text "done" ]
  in
  print_outline spans;
  [%expect {|
    prompt 3.3s
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
    |}]
;;

let%expect_test "parallel tool calls overlap under their turn" =
  let spans =
    run_loop
      ~tools:(fun clock -> [ work_tool clock ])
      [ Reply.tool_calls
          [ "w1", "work", {|{"seconds":2}|}; "w2", "work", {|{"seconds":3}|} ]
      ; Reply.text "done"
      ]
  in
  print_outline spans;
  [%expect {|
    prompt 4.5s
      turn 1 3.9s
        model_request deepseek/deepseek-flash 900ms  stop=tool_use
          connect 500ms
          first_token 100ms
          stream 300ms
        tool work 2.0s  where=local result_bytes=9
        tool work 3.0s  where=local result_bytes=9
      turn 2 600ms
        model_request deepseek/deepseek-flash 600ms  stop=end_turn
          connect 500ms
          first_token 100ms
          stream 0ms
    |}]
;;

let%expect_test "a failed attempt, the wait and the retry are siblings" =
  let spans =
    run_loop
      ~retries:2
      [ Reply.error "HTTP 529 overloaded_error"; Reply.text "done" ]
  in
  print_outline spans;
  [%expect {|
    prompt 2.1s
      turn 1 2.1s
        model_request deepseek/deepseek-flash 500ms [error]  stop=error error=HTTP 529 overloaded_error
          connect 500ms
          first_token 0ms [error]
        retry_wait 1.0s  attempt=2 delay=1
        model_request deepseek/deepseek-flash 600ms  stop=end_turn
          connect 500ms
          first_token 100ms
          stream 0ms
    |}]
;;

let%expect_test "compaction between turns, and its summarising request" =
  let spans =
    run_loop
      ~tools:(fun _ -> [ filler ])
      ~compaction:{ Compaction_settings.window_only with mask_observations = false }
      [ Reply.tool_call ~id:"c1" ~name:"filler" ~arguments:{|{"size":40000}|} ()
      ; { (Reply.tool_call ~id:"c2" ~name:"filler" ~arguments:{|{"size":100}|} ())
          with
          usage = { input = 900_000; output = 10; cache_read = 0 }
        }
      ; Reply.text "SUMMARY"
      ; Reply.text "done"
      ]
  in
  print_outline spans;
  [%expect {|
    prompt 2.6s
      turn 1 700ms
        model_request deepseek/deepseek-flash 700ms  stop=tool_use
          connect 500ms
          first_token 100ms
          stream 100ms
        tool filler 0ms  where=local result_bytes=40000
      turn 2 700ms
        model_request deepseek/deepseek-flash 700ms  stop=tool_use
          connect 500ms
          first_token 100ms
          stream 100ms
        tool filler 0ms  where=local result_bytes=100
      compaction 600ms  tokens_before=10035 tokens_after=40
        model_request deepseek/deepseek-flash 600ms  stop=end_turn
          connect 500ms
          first_token 100ms
          stream 0ms
      turn 3 600ms
        model_request deepseek/deepseek-flash 600ms  stop=end_turn
          connect 500ms
          first_token 100ms
          stream 0ms
    |}]
;;

let%expect_test "the detailed level adds each delta's wait and handling" =
  let spans =
    run_loop
      ~level:Detailed
      [ { (Reply.text "a") with
          events = [ Thinking_delta "hmm"; Text_delta "a"; Text_delta "b" ]
        }
      ]
  in
  print_outline spans;
  [%expect {|
    prompt 800ms
      turn 1 800ms
        model_request deepseek/deepseek-flash 800ms  stop=end_turn
          connect 500ms
          first_token 100ms
          thinking 100ms
            delta_handle 0ms
            delta_wait 100ms  kind=text
          stream 100ms
            delta_handle 0ms
            delta_wait 100ms  kind=text
            delta_handle 0ms
    |}]
;;

let%expect_test "tracing off writes nothing" =
  let spans =
    run_loop
      ~level:Off
      ~tools:(fun clock -> [ work_tool clock ])
      [ work 1; Reply.text "done" ]
  in
  print_s [%sexp (List.length spans : int)];
  [%expect {| 0 |}]
;;

let%expect_test "cancelled mid-tool: every span still ends, marked cancelled" =
  let cancel = Cancellation.create () in
  let hang =
    { Tool.spec =
        { Tool_spec.name = "hang"
        ; description = "hangs until cancelled"
        ; parameters = Tool_args.schema []
        ; parallel_safe = false
        ; destructive = Never
        ; on_host = false
        ; freeform = None
        }
    ; run =
        (fun context _ ->
          Cancellation.cancel cancel;
          ignore
            (Cancellation.protect context.cancel ~f:(fun () ->
               Eio.Fiber.await_cancel ())
             : unit option);
          Tool.Result.error "[cancelled]")
    }
  in
  let spans =
    run_loop
      ~cancel
      ~tools:(fun _ -> [ hang ])
      [ Reply.tool_call ~id:"h1" ~name:"hang" ~arguments:"{}" ()
      ; Reply.text "never"
      ]
  in
  print_outline spans;
  [%expect {|
    prompt 700ms
      turn 1 700ms [cancelled]
        model_request deepseek/deepseek-flash 700ms  stop=tool_use
          connect 500ms
          first_token 100ms
          stream 100ms
        tool hang 0ms [cancelled]  where=local result_bytes=11
    |}]
;;

(* A subagent without background tasks runs synchronously, within the tool
   call that started it, on a track of its own. *)
let%expect_test "a synchronous subagent's turns nest in its tool call" =
  let spans =
    run_loop
      ~tools:(fun clock ->
        [ work_tool clock
        ; Tool_subagent.create
            ~provider:
              (timed
                 clock
                 (Faux_provider.create [ work 1; Reply.text "child report" ]))
            ~current_model:(fun () -> Model.default)
            ~current_thinking:(fun () -> Off)
            ~home:"/nonexistent"
            ()
        ])
      [ Reply.tool_call
          ~id:"c1"
          ~name:"subagent"
          ~arguments:{|{"task":"look around"}|}
          ()
      ; Reply.text "done"
      ]
  in
  print_outline spans;
  print_tracks spans;
  [%expect {|
    prompt 3.6s
      turn 1 3.0s
        model_request deepseek/deepseek-flash 700ms  stop=tool_use
          connect 500ms
          first_token 100ms
          stream 100ms
        tool subagent 2.3s  where=local result_bytes=64
          subagent c1 2.3s  background=false forked=false continued=false
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
      turn 2 600ms
        model_request deepseek/deepseek-flash 600ms  stop=end_turn
          connect 500ms
          first_token 100ms
          stream 0ms
    track "main": 12 spans
    track "subagent c1": 12 spans
    |}]
;;

let%expect_test "the trace file keeps the newest spans within its cap" =
  with_sandbox
  @@ fun t ->
  let session_path = Filename.concat t.dir "s.jsonl" in
  let exists = ref false in
  let clock = Virtual_clock.create () in
  (* Each process (here: each tracer) has its own file writer. *)
  let make_tracer () =
    let file =
      Trace_file.create
        ~max_bytes:1000
        ~session_path:(fun () -> Option.some_if !exists session_path)
        ()
    in
    Trace.create
      ~clock:(Virtual_clock.trace_clock clock)
      ~level:Default
      ~write:(Trace_file.write file)
      ()
  in
  let tracer = ref (make_tracer ()) in
  let span name =
    Trace.run !tracer (fun () -> Trace.span ~root:true name ignore)
  in
  let show () =
    let sizes =
      List.map (Trace_file.paths ~session_path) ~f:(fun p ->
        ( String.chop_prefix_exn (Filename.basename p) ~prefix:"s"
        , if Sys_unix.file_exists_exn p
          then Int64.to_string (Core_unix.stat p).st_size
          else "-" ))
    in
    let spans, bad = Trace_file.load ~session_path in
    print_s
      [%sexp
        (sizes : (string * string) list)
      , (List.map spans ~f:(fun s -> s.name) : string list)
      , ("bad", (bad : int))]
  in
  (* Before the session has a file, spans wait for it. *)
  span "a";
  span "b";
  show ();
  [%expect {| (((.trace.old.jsonl -) (.trace.jsonl -)) () (bad 0)) |}];
  Out_channel.write_all session_path ~data:"";
  exists := true;
  span "c";
  show ();
  [%expect {| (((.trace.old.jsonl -) (.trace.jsonl 402)) (a b c) (bad 0)) |}];
  List.iter (List.init 30 ~f:(sprintf "span%02d")) ~f:span;
  show ();
  [%expect {|
    (((.trace.old.jsonl 980) (.trace.jsonl 700))
     (span18 span19 span20 span21 span22 span23 span24 span25 span26 span27
      span28 span29)
     (bad 0))
    |}];
  (* A line cut short by a crash is skipped, and the next process starts
     on a line of its own. *)
  Out_channel.with_file
    ~append:true
    (List.last_exn (Trace_file.paths ~session_path))
    ~f:(fun oc -> Out_channel.output_string oc {|{"id":"cut|});
  tracer := make_tracer ();
  span "after_crash";
  show ();
  [%expect {|
    (((.trace.old.jsonl 980) (.trace.jsonl 855))
     (span18 span19 span20 span21 span22 span23 span24 span25 span26 span27
      span28 span29 after_crash)
     (bad 1))
    |}];
  (* Once the session is deleted, nothing more is written. *)
  Session.delete_files session_path;
  span "late";
  show ();
  [%expect {| (((.trace.old.jsonl -) (.trace.jsonl -)) () (bad 0)) |}]
;;
