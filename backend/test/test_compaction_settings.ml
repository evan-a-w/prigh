open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

(* When conversations are compacted ([compact_at_tokens]) and which model
   summarises them ([compaction_model]): the threshold, the loop, the agent
   and the config. Masking's thresholds are in [Test_observation_mask]. *)

let model key = Option.value_exn (Model.find key)
let flash = model "deepseek/deepseek-flash"
let haiku = model "anthropic/claude-haiku-4-5"
let capped max_tokens = { Compaction_settings.window_only with max_tokens }

let%expect_test "the threshold: 80% of the window, or the cap if lower" =
  List.iter
    [ "deepseek/deepseek-flash"
    ; "openai/gpt-5.5"
    ; "anthropic/claude-haiku-4-5"
    ]
    ~f:(fun key ->
      let m = model key in
      List.iter [ None; Some 200_000; Some 50_000 ] ~f:(fun max_tokens ->
        printf
          "%s (window %d), cap %s: %d\n"
          key
          m.context_window
          (Option.value_map max_tokens ~default:"none" ~f:Int.to_string)
          (Compaction_settings.threshold (capped max_tokens) m)));
  let at ?max_tokens input_tokens =
    printf
      "%d tokens: %b\n"
      input_tokens
      (Compaction_settings.should_compact
         (capped max_tokens)
         flash
         ~input_tokens)
  in
  at 799_999;
  at 800_000;
  at 800_001;
  at ~max_tokens:200_000 200_000;
  at ~max_tokens:200_000 200_001;
  [%expect
    {|
    deepseek/deepseek-flash (window 1000000), cap none: 800000
    deepseek/deepseek-flash (window 1000000), cap 200000: 200000
    deepseek/deepseek-flash (window 1000000), cap 50000: 50000
    openai/gpt-5.5 (window 272000), cap none: 217600
    openai/gpt-5.5 (window 272000), cap 200000: 200000
    openai/gpt-5.5 (window 272000), cap 50000: 50000
    anthropic/claude-haiku-4-5 (window 200000), cap none: 160000
    anthropic/claude-haiku-4-5 (window 200000), cap 200000: 160000
    anthropic/claude-haiku-4-5 (window 200000), cap 50000: 50000
    799999 tokens: false
    800000 tokens: false
    800001 tokens: true
    200000 tokens: false
    200001 tokens: true
    |}]
;;

let%expect_test "the summariser: the compaction model when it holds the context"
  =
  let show settings session =
    printf
      "%s\n  %s summarises %s\n"
      (Sexp.to_string
         [%sexp
           { max_tokens = (settings.Compaction_settings.max_tokens : int option)
           ; model = (Option.map settings.model ~f:Model.key : string option)
           }])
      (Model.key (Compaction_settings.summary_model settings session))
      (Model.key session)
  in
  show Compaction_settings.window_only flash;
  show { (capped (Some 200_000)) with model = Some haiku } flash;
  (* Haiku's window cannot hold the 800k tokens compacted at 80%. *)
  show { (capped None) with model = Some haiku } flash;
  show { (capped None) with model = Some flash } haiku;
  [%expect
    {|
    ((max_tokens())(model()))
      deepseek/deepseek-flash summarises deepseek/deepseek-flash
    ((max_tokens(200000))(model(anthropic/claude-haiku-4-5)))
      anthropic/claude-haiku-4-5 summarises deepseek/deepseek-flash
    ((max_tokens())(model(anthropic/claude-haiku-4-5)))
      deepseek/deepseek-flash summarises deepseek/deepseek-flash
    ((max_tokens())(model(deepseek/deepseek-flash)))
      deepseek/deepseek-flash summarises anthropic/claude-haiku-4-5
    |}]
;;

let call ?(input = 100) id size =
  { (Reply.tool_call
       ~id
       ~name:"filler"
       ~arguments:(sprintf {|{"size":%d}|} size)
       ())
    with
    usage = { input; output = 10; cache_read = 0 }
  }
;;

let summary text =
  { (Reply.text text) with
    usage = { input = 100_000; output = 1_000; cache_read = 0 }
  }
;;

let is_ask (m : Message.t) =
  match m with
  | User u -> String.is_prefix u.text ~prefix:"[prigh: compaction]"
  | Assistant _ | Tool_result _ -> false
;;

(* The summary is asked for with the agent's messages and an ask at the end,
   or (when they do not fit) with a transcript of its own. *)
let print_request (r : Provider.Request.t) =
  match r.system, List.last r.messages with
  | Some system, _ when String.is_prefix system ~prefix:"You summarise" ->
    printf "summarise a transcript with %s\n" (Model.key r.model)
  | _, Some m when is_ask m -> printf "summarise with %s\n" (Model.key r.model)
  | _ -> printf "request with %s\n" (Model.key r.model)
;;

let%expect_test "a loop compacts at the cap, summarised by the compaction model"
  =
  let run compaction =
    with_sandbox
    @@ fun t ->
    let provider =
      Faux_provider.create
        ~on_request:print_request
        [ call "c1" 40_000
        ; call ~input:300_000 "c2" 100
        ; summary "SUMMARY"
        ; Reply.text "done"
        ]
    in
    ignore
      (Agent_loop.run
         ~env:t.env
         ~provider
         ~config:
           { model = flash
           ; thinking = Off
           ; system = None
           ; tools = [ filler ]
           ; max_turns = None
           ; max_tokens = None
           ; retries = 0
           ; cache_key = None
           ; compaction
           }
         ~cwd:t.dir
         ~emit:(function
           | Compacted { summary; usage; cost_usd } ->
             printf
               "compacted: %s (in=%d out=%d, $%.4f)\n"
               summary
               usage.input
               usage.output
               cost_usd
           | _ -> ())
         ~context:[]
         ~prompts:[ Message.user "the task" ]
         ()
       : Message.t list)
  in
  (* 300k tokens are under 80% of flash's 1M window. *)
  run Compaction_settings.window_only;
  [%expect
    {|
    request with deepseek/deepseek-flash
    request with deepseek/deepseek-flash
    request with deepseek/deepseek-flash
    |}];
  (* Haiku: $1 in, $5 out per million; flash would cost $0.0312. *)
  run { (capped (Some 200_000)) with model = Some haiku };
  [%expect
    {|
    request with deepseek/deepseek-flash
    request with deepseek/deepseek-flash
    summarise with anthropic/claude-haiku-4-5
    compacted: SUMMARY (in=100000 out=1000, $0.1050)
    request with deepseek/deepseek-flash
    |}]
;;

let gpt_4_1 = model "openai/gpt-4.1"

(* What the summarising request carries, and whether the agent's last
   request is where it starts. *)
let%expect_test
    "another compaction model gets the agent's messages, at its own prices"
  =
  let run ?(context = []) ~session_model summariser =
    with_sandbox
    @@ fun t ->
    let last = ref [] in
    let provider =
      Faux_provider.create
        ~on_request:(fun r ->
          print_request r;
          match List.last r.messages with
          | Some m when is_ask m ->
            printf
              "  system %s, tools [%s], thinking %s, cache key %s, max_tokens %s\n\
              \  the agent's last request and an ask: %b\n"
              (Option.value r.system ~default:"-")
              (String.concat
                 ~sep:" "
                 (List.map r.tools ~f:(fun (s : Tool_spec.t) -> s.name)))
              (Sexp.to_string [%sexp (r.thinking : Thinking.t)])
              (Option.value r.cache_key ~default:"-")
              (Option.value_map r.max_tokens ~default:"-" ~f:Int.to_string)
              ([%equal: Message.t list] (List.drop_last_exn r.messages) !last)
          | Some _ | None ->
            if not (Option.is_some r.system && List.length r.messages = 1)
            then last := r.messages)
        [ call "c1" 40_000
        ; call ~input:300_000 "c2" 100
        ; summary "SUMMARY"
        ; Reply.text "done"
        ]
    in
    ignore
      (Agent_loop.run
         ~env:t.env
         ~provider
         ~config:
           { model = session_model
           ; thinking = On (Some High)
           ; system = Some "SYSTEM"
           ; tools = [ filler ]
           ; max_turns = None
           ; max_tokens = None
           ; retries = 0
           ; cache_key = Some "session"
           ; compaction =
               { (capped (Some 200_000)) with model = Some summariser }
           }
         ~cwd:t.dir
         ~emit:(function
           | Compacted { usage; cost_usd; _ } ->
             printf
               "compacted (in=%d out=%d, $%.4f)\n"
               usage.input
               usage.output
               cost_usd
           | _ -> ())
         ~context
         ~prompts:[ Message.user "the task" ]
         ()
       : Message.t list)
  in
  (* GPT-4.1 (another provider, no thinking: $2 in, $8 out per million). *)
  run ~session_model:flash gpt_4_1;
  [%expect
    {|
    request with deepseek/deepseek-flash
    request with deepseek/deepseek-flash
    summarise with openai/gpt-4.1
      system SYSTEM, tools [filler], thinking Off, cache key session, max_tokens 8192
      the agent's last request and an ask: true
    compacted (in=100000 out=1000, $0.2080)
    request with deepseek/deepseek-flash
    |}];
  (* Haiku thinks: the agent's setting goes with the request. *)
  run ~session_model:flash haiku;
  [%expect
    {|
    request with deepseek/deepseek-flash
    request with deepseek/deepseek-flash
    summarise with anthropic/claude-haiku-4-5
      system SYSTEM, tools [filler], thinking (On(High)), cache key session, max_tokens 24576
      the agent's last request and an ask: true
    compacted (in=100000 out=1000, $0.1050)
    request with deepseek/deepseek-flash
    |}];
  (* Haiku's 200k window holds the 160k compacted for a 200k-window model,
     but not these messages with room for the reply: a transcript instead. *)
  run
    ~context:
      [ Message.user "earlier"
      ; Assistant
          { content =
              [ Tool_call { id = "c0"; name = "filler"; arguments = "{}" } ]
          ; usage = Usage.zero
          ; stop_reason = Tool_use
          ; model = Model.key flash
          }
      ; Tool_result
          { tool_call_id = "c0"
          ; tool_name = "filler"
          ; text = String.make 780_000 'x'
          ; is_error = false
          ; images = []
          }
      ]
    ~session_model:{ flash with context_window = 200_000 }
    haiku;
  [%expect
    {|
    request with deepseek/deepseek-flash
    request with deepseek/deepseek-flash
    summarise a transcript with anthropic/claude-haiku-4-5
    compacted (in=100000 out=1000, $0.1050)
    request with deepseek/deepseek-flash
    |}]
;;

let with_agent ~config replies f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  write t ".prigh/config.json" (Jsonaf.to_string (`Object config));
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create ~on_request:print_request replies)
      ~tools:[ filler ]
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:t.dir
      ()
  in
  Agent.subscribe agent ~f:(function
    | Notice n -> print_endline ("notice: " ^ n)
    | Compacted { summary } -> print_endline ("compacted: " ^ summary)
    | _ -> ());
  f agent
;;

let spends agent =
  List.iter
    (Session.active_path (Agent.session agent))
    ~f:(fun e ->
      match e.payload with
      | Spend { usage; cost_usd } ->
        printf "spend: in=%d out=%d $%.4f\n" usage.input usage.output cost_usd
      | _ -> ())
;;

let%expect_test
    "the session compacts at the cap, summarised by the compaction model"
  =
  with_agent
    ~config:
      [ "compact_at_tokens", `Number "150000"
      ; "compaction_model", `String "claude-haiku-4-5"
      ]
    [ call "c1" 40_000
    ; call ~input:300_000 "c2" 100
    ; summary "SUMMARY"
    ; Reply.text "done"
    ; call "c3" 40_000
    ; Reply.text "again"
    ; summary "SUMMARY 2"
    ]
  @@ fun agent ->
  printf
    "%s\n"
    (Sexp.to_string
       [%sexp
         (Agent.compaction_settings agent
          |> fun s ->
          s.Compaction_settings.max_tokens, Option.map s.model ~f:Model.key
          : int option * string option)]);
  Or_error.ok_exn (Agent.prompt agent "the task");
  Agent.wait_idle agent;
  Or_error.ok_exn (Agent.prompt agent "more");
  Agent.wait_idle agent;
  (* /compact too. *)
  print_s [%sexp (Agent.compact agent : string Or_error.t)];
  spends agent;
  [%expect
    {|
    ((150000)(anthropic/claude-haiku-4-5))
    request with deepseek/deepseek-flash
    request with deepseek/deepseek-flash
    summarise with anthropic/claude-haiku-4-5
    compacted: SUMMARY
    request with deepseek/deepseek-flash
    request with deepseek/deepseek-flash
    request with deepseek/deepseek-flash
    summarise with anthropic/claude-haiku-4-5
    compacted: SUMMARY 2
    (Ok "SUMMARY 2")
    spend: in=100000 out=1000 $0.1050
    spend: in=100000 out=1000 $0.1050
    |}]
;;

let%expect_test
    "set_config resolves compaction_model; an unknown one falls back"
  =
  with_agent ~config:[ "compaction_model", `String "no-such-model" ] []
  @@ fun agent ->
  let show () =
    let s : Compaction_settings.t = Agent.compaction_settings agent in
    printf
      "config %s, cap %s: %s\n"
      (Option.value (Agent.config agent).compaction_model ~default:"-")
      (Option.value_map s.max_tokens ~default:"none" ~f:Int.to_string)
      (Option.value_map s.model ~default:"the session's model" ~f:Model.key)
  in
  (* Hand-edited: the session's model summarises (Model_registry reports
     it). *)
  show ();
  let set config =
    match Agent.set_config agent config with
    | Ok () -> show ()
    | Error e -> printf "error: %s\n" (Error.to_string_hum e)
  in
  set { (Agent.config agent) with compaction_model = Some "claude-haiku-4-5" };
  set { (Agent.config agent) with compaction_model = Some "claude-haku" };
  set { (Agent.config agent) with compact_at_tokens = 0 };
  set { (Agent.config agent) with compaction_model = None };
  [%expect
    {|
    config no-such-model, cap 200000: the session's model
    config anthropic/claude-haiku-4-5, cap 200000: anthropic/claude-haiku-4-5
    error: compaction_model: unknown model "claude-haku"; did you mean: anthropic/claude-haiku-4-5 (Claude Haiku 4.5 (latest)), anthropic/claude-fable-5 (Claude Fable 5), anthropic/claude-haiku-4-5-20251001 (Claude Haiku 4.5)
    config anthropic/claude-haiku-4-5, cap none: anthropic/claude-haiku-4-5
    config -, cap none: the session's model
    |}]
;;

let%expect_test "compact_at_tokens and compaction_model in config.json" =
  let parse json =
    match Config.of_json (Jsonaf.of_string json) with
    | Ok c ->
      printf
        "%s\n"
        (Sexp.to_string
           [%sexp
             { compact_at_tokens = (c.compact_at_tokens : int)
             ; compact_at = (Config.compact_at c : int option)
             ; compaction_model = (c.compaction_model : string option)
             }])
    | Error e -> print_endline (Error.to_string_hum e)
  in
  parse {|{}|};
  parse
    {|{"compact_at_tokens": 120000, "compaction_model": "claude-haiku-4-5"}|};
  parse {|{"compact_at_tokens": 0, "compaction_model": null}|};
  parse {|{"compact_at_tokens": -1}|};
  parse {|{"compact_at_tokens": 1.5}|};
  parse {|{"compact_at_tokens": "200k"}|};
  parse {|{"compaction_model": 3}|};
  [%expect
    {|
    ((compact_at_tokens 200000)(compact_at(200000))(compaction_model()))
    ((compact_at_tokens 120000)(compact_at(120000))(compaction_model(claude-haiku-4-5)))
    ((compact_at_tokens 0)(compact_at())(compaction_model()))
    config.compact_at_tokens must be a whole number of tokens (0: only at 80% of the model's window)
    config.compact_at_tokens must be a whole number of tokens (0: only at 80% of the model's window)
    config.compact_at_tokens must be a whole number of tokens (0: only at 80% of the model's window)
    config.compaction_model must be a model's name (or null: the session's model)
    |}]
;;

let%expect_test "subagents compact with the session's settings" =
  with_sandbox
  @@ fun t ->
  let provider =
    Faux_provider.create
      ~on_request:print_request
      [ Reply.tool_call
          ~id:"s1"
          ~name:"subagent"
          ~arguments:{|{"task":"look around"}|}
          ()
      ; call "c1" 40_000
      ; call ~input:300_000 "c2" 100
      ; summary "CHILD SUMMARY"
      ; Reply.text "child done"
      ; Reply.text "main done"
      ]
  in
  let subagent =
    Tool_subagent.create
      ~provider
      ~compaction:(fun () ->
        { (capped (Some 200_000)) with model = Some haiku })
      ~current_model:(fun () -> flash)
      ~current_thinking:(fun () -> Thinking.Off)
      ~home:t.dir
      ()
  in
  ignore
    (Agent_loop.run
       ~env:t.env
       ~provider
       ~config:
         { model = flash
         ; thinking = Off
         ; system = None
         ; tools = [ filler; subagent ]
         ; max_turns = None
         ; max_tokens = None
         ; retries = 0
         ; cache_key = None
         ; compaction = Compaction_settings.window_only
         }
       ~cwd:t.dir
       ~emit:(function
         | Subagent { event = Compacted { summary; cost_usd; _ }; _ } ->
           printf "subagent compacted: %s ($%.4f)\n" summary cost_usd
         | _ -> ())
       ~context:[]
       ~prompts:[ Message.user "go" ]
       ()
     : Message.t list);
  [%expect
    {|
    request with deepseek/deepseek-flash
    request with deepseek/deepseek-flash
    request with deepseek/deepseek-flash
    summarise with anthropic/claude-haiku-4-5
    subagent compacted: CHILD SUMMARY ($0.1050)
    request with deepseek/deepseek-flash
    request with deepseek/deepseek-flash
    |}]
;;
