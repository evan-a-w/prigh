open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Gates = Routed_provider.Gates

(* Spans are timed by a clock that only the test and the providers and tools
   below move, so durations are exact. *)
module Fake_clock = struct
  type t = { mutable now : float }

  let create () = { now = 0. }
  let advance t seconds = t.now <- t.now +. seconds

  let clock t =
    { Trace.Clock.mono = (fun () -> t.now)
    ; wall = (fun () -> 1_800_000_000. +. t.now)
    }
  ;;
end

(* Each request takes [connect] seconds to its first byte and [per_delta]
   per streamed delta. *)
let timed ?(connect = 0.5) ?(per_delta = 0.1) clock (provider : Provider.t) =
  { provider with
    stream =
      (fun request ~cancel ~on_event ->
        Fake_clock.advance clock connect;
        provider.stream request ~cancel ~on_event:(fun delta ->
          Fake_clock.advance clock per_delta;
          on_event delta))
  }
;;

(* Works for [seconds], a second at a time, yielding in between so parallel
   calls overlap. *)
let work_tool clock =
  { Tool.spec =
      { Tool_spec.name = "work"
      ; description = "works for SECONDS"
      ; parameters = Tool_args.schema [ "seconds", `Integer, "how long" ]
      ; parallel_safe = true
      ; destructive = Never
      ; on_host = false
      ; freeform = None
      }
  ; run =
      (fun _ args ->
        let seconds = Option.value (Tool_args.int_opt args "seconds") ~default:1 in
        for _ = 1 to seconds do
          Eio.Fiber.yield ();
          Fake_clock.advance clock 1.
        done;
        Tool.Result.ok (sprintf "worked %ds" seconds))
  }
;;

let work ?(id = "w1") seconds =
  Reply.tool_call ~id ~name:"work" ~arguments:(sprintf {|{"seconds":%d}|} seconds) ()
;;

let shown =
  [ "stop"
  ; "error"
  ; "result_bytes"
  ; "where"
  ; "attempt"
  ; "delay"
  ; "tokens_before"
  ; "tokens_after"
  ; "tokens_saved"
  ; "background"
  ; "forked"
  ; "continued"
  ; "outcome"
  ; "kind"
  ]
;;

let print_outline ?(attrs = shown) spans =
  print_string
    (Trace_report.outline ~attrs:(List.mem attrs ~equal:String.equal) spans)
;;

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
  let clock = Fake_clock.create () in
  let spans = Queue.create () in
  let tracer =
    Trace.create
      ~id_prefix:"s"
      ~clock:(Fake_clock.clock clock)
      ~level
      ~write:(Queue.enqueue spans)
      ()
  in
  let provider = timed ~connect clock (Faux_provider.create replies) in
  Trace.run tracer (fun () ->
    Trace.span ~root:true "prompt" (fun () ->
      ignore
        (Agent_loop.run
           ~env:t.env
           ~provider
           ~config:(loop_config ?retries ?compaction (tools clock))
           ~cwd:t.dir
           ?cancel
           ~sleep:(Fake_clock.advance clock)
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
