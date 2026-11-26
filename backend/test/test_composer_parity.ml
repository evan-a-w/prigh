open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

(* A session's features on a composer engine: models from the harness,
   thinking, /compact, context, prigh's own requests, and going back to the
   harness engine. The harnesses are scripted peers. *)

let parse line = Json.parse line |> Or_error.ok_exn
let str k j = Option.bind (Json.member k j) ~f:Json.string

let write (p : Line_transport.Peer.t) json =
  Line_transport.Peer.write_line p (Json.to_string json)
;;

let thread_frame meth params =
  `Object [ "method", `String meth; "params", params ]
;;

(* A Codex app-server: answers the handshake, the thread, [model/list]
   and turns; a turn writes a reply with usage, a compaction a
   [contextCompaction] item. *)
let codex (p : Line_transport.Peer.t) =
  let reply j result =
    write
      p
      (`Object [ "id", Option.value_exn (Json.member "id" j); "result", result ])
  in
  let turns = ref 0 in
  let turn_frames id items =
    let th = "threadId", `String "th" in
    [ thread_frame
        "turn/started"
        (`Object [ th; "turn", `Object [ "id", `String id ] ])
    ]
    @ List.map items ~f:(fun item ->
      thread_frame
        "item/completed"
        (`Object [ th; "turnId", `String id; "item", item ]))
    @ [ thread_frame
          "thread/tokenUsage/updated"
          (`Object
              [ th
              ; "turnId", `String id
              ; ( "tokenUsage"
                , `Object
                    [ ( "total"
                      , `Object
                          [ "inputTokens", `Number "9000"
                          ; "outputTokens", `Number "20"
                          ; "totalTokens", `Number "9020"
                          ] )
                    ; ( "last"
                      , `Object
                          [ "inputTokens", `Number "3000"
                          ; "outputTokens", `Number "20"
                          ; "totalTokens", `Number "3020"
                          ] )
                    ; "modelContextWindow", `Number "400000"
                    ] )
              ])
      ; thread_frame
          "turn/completed"
          (`Object
              [ th
              ; ( "turn"
                , `Object [ "id", `String id; "status", `String "completed" ] )
              ])
      ]
  in
  let rec loop () =
    match Line_transport.Peer.read_line p with
    | None -> ()
    | Some line ->
      let j = parse line in
      let params = Option.value (Json.member "params" j) ~default:`Null in
      (match str "method" j with
       | Some "initialize" ->
         reply j (`Object [ "userAgent", `String "codex/0.166.0" ])
       | Some "initialized" -> ()
       | Some ("thread/start" | "thread/resume") ->
         reply
           j
           (`Object
               [ "thread", `Object [ "id", `String "th" ]
               ; "model", `String "gpt-5"
               ])
       | Some "model/list" ->
         let model id name efforts default =
           `Object
             [ "id", `String id
             ; "model", `String id
             ; "displayName", `String name
             ; ("isDefault", if default then `True else `False)
             ; "hidden", `False
             ; ( "supportedReasoningEfforts"
               , `Array
                   (List.map efforts ~f:(fun e ->
                      `Object [ "reasoningEffort", `String e ])) )
             ; "inputModalities", `Array [ `String "text"; `String "image" ]
             ]
         in
         reply
           j
           (`Object
               [ ( "data"
                 , `Array
                     [ model
                         "gpt-5"
                         "GPT-5"
                         [ "minimal"; "low"; "medium"; "high" ]
                         true
                     ; model
                         "gpt-5-codex"
                         "GPT-5 Codex"
                         [ "low"; "medium"; "high" ]
                         false
                     ] )
               ; "nextCursor", `Null
               ])
       | Some "turn/start" ->
         incr turns;
         printf
           "codex got turn/start model=%s effort=%s\n"
           (Option.value (str "model" params) ~default:"-")
           (Option.value (str "effort" params) ~default:"-");
         let id = sprintf "tu%d" !turns in
         reply j (`Object [ "turn", `Object [ "id", `String id ] ]);
         List.iter
           (turn_frames
              id
              [ `Object
                  [ "type", `String "agentMessage"
                  ; "id", `String ("m" ^ id)
                  ; "text", `String "done"
                  ]
              ])
           ~f:(write p)
       | Some "thread/compact/start" ->
         print_endline "codex got thread/compact/start";
         reply j (`Object []);
         List.iter
           (turn_frames
              "tc"
              [ `Object
                  [ "type", `String "contextCompaction"; "id", `String "cc" ]
              ])
           ~f:(write p)
       | Some m -> printf "codex got %s\n" m
       | None -> ());
      loop ()
  in
  loop ()
;;

(* Claude Code: answers [initialize] with its models and the controls,
   prints what it is sent; a prompt gets a reply, [/compact] a compaction. *)
let claude (p : Line_transport.Peer.t) =
  let session = "s1" in
  let control_ok request_id response =
    write
      p
      (`Object
          [ "type", `String "control_response"
          ; ( "response"
            , `Object
                [ "subtype", `String "success"
                ; "request_id", `String request_id
                ; "response", response
                ] )
          ])
  in
  let model value name =
    `Object [ "value", `String value; "displayName", `String name ]
  in
  let result text cost =
    `Object
      [ "type", `String "result"
      ; "subtype", `String "success"
      ; "is_error", `False
      ; "result", `String text
      ; "session_id", `String session
      ; "total_cost_usd", `Number cost
      ; ( "usage"
        , `Object [ "input_tokens", `Number "5"; "output_tokens", `Number "2" ]
        )
      ]
  in
  let init =
    `Object
      [ "type", `String "system"
      ; "subtype", `String "init"
      ; "session_id", `String session
      ; "model", `String "claude-sonnet-4-6"
      ]
  in
  let rec loop () =
    match Line_transport.Peer.read_line p with
    | None -> ()
    | Some line ->
      let j = parse line in
      (match str "type" j with
       | Some "control_request" ->
         let request = Option.value (Json.member "request" j) ~default:`Null in
         let id = Option.value (str "request_id" j) ~default:"" in
         (match str "subtype" request with
          | Some "initialize" ->
            control_ok
              id
              (`Object
                  [ "claude_code_version", `String "2.1.300"
                  ; ( "models"
                    , `Array
                        [ model "default" "Default (Sonnet)"
                        ; model "opus" "Opus"
                        ; model "haiku" "Haiku"
                        ] )
                  ; "commands", `Array []
                  ])
          | Some sub ->
            printf
              "claude got %s %s\n"
              sub
              (Json.to_string
                 (`Object
                     (List.filter
                        (match request with
                         | `Object l -> l
                         | _ -> [])
                        ~f:(fun (k, _) -> not (String.equal k "subtype")))));
            control_ok id (`Object [])
          | None -> ())
       | Some "user" ->
         let text =
           match Json.member "message" j with
           | Some m ->
             (match Json.member "content" m with
              | Some (`String s) -> s
              | Some (`Array (first :: _)) ->
                Option.value (str "text" first) ~default:""
              | _ -> "")
           | None -> ""
         in
         let text = List.last_exn (String.split text ~on:'\n') in
         printf "claude got user %S\n" text;
         write p init;
         if String.is_prefix text ~prefix:"/compact"
         then (
           write
             p
             (`Object
                 [ "type", `String "system"
                 ; "subtype", `String "compact_boundary"
                 ; "session_id", `String session
                 ; ( "compact_metadata"
                   , `Object
                       [ "trigger", `String "manual"
                       ; "pre_tokens", `Number "50000"
                       ] )
                 ]);
           write p (result "" "0.02"))
         else (
           write
             p
             (`Object
                 [ "type", `String "assistant"
                 ; "session_id", `String session
                 ; ( "message"
                   , `Object
                       [ "id", `String "msg1"
                       ; "model", `String "claude-sonnet-4-6"
                       ; "role", `String "assistant"
                       ; ( "content"
                         , `Array
                             [ `Object
                                 [ "type", `String "text"
                                 ; "text", `String "done"
                                 ]
                             ] )
                       ; ( "usage"
                         , `Object
                             [ "input_tokens", `Number "5"
                             ; "output_tokens", `Number "2"
                             ] )
                       ] )
                 ]);
           write p (result "done" "0.01"))
       | _ -> ());
      loop ()
  in
  loop ()
;;

let print_event (e : Agent.Event.t) =
  match e with
  | Loop (Message_end (Assistant a)) ->
    printf
      "assistant (%s, %s): %S\n"
      a.model
      (Sexp.to_string [%sexp (a.stop_reason : Stop_reason.t)])
      (Message.Assistant.text a)
  | Notice n -> printf "notice: %s\n" n
  | _ -> ()
;;

let with_session ~adapter ~peer f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let composer =
    Composer_runner.create
      ~sw
      ~adapter
      ~spawn:(fun (_ : Adapter.Launch.t) ->
        Ok (Line_transport.Peer.spawn ~sw peer))
      ~interrupt_grace:(Time_float.Span.of_sec 0.5)
      ()
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [])
      ~tools:Tools.interaction
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:t.dir
      ()
  in
  let (module A : Adapter.S) = adapter in
  Agent.set_engine
    agent
    { engine = Composer
    ; adapter = Some (Adapter_id.of_string A.id |> Or_error.ok_exn)
    };
  Agent.set_runner agent (Some (Composer_runner.runner composer));
  Agent.subscribe agent ~f:print_event;
  f agent;
  Agent.set_runner agent None
;;

let prompt agent text =
  Or_error.ok_exn (Agent.prompt agent text);
  Agent.wait_idle agent
;;

let show_model agent =
  let state = Agent.state agent in
  printf
    "model %s (%s), thinking %s, context %d of %d\n"
    (Model.key state.model)
    state.model.name
    (Thinking.to_string state.thinking)
    state.context_tokens
    state.model.context_window
;;

let show_models agent =
  match Agent.external_models agent with
  | None -> print_endline "prigh's models"
  | Some models ->
    List.iter models ~f:(fun (m : Model.t) ->
      printf
        "  %s %S thinking=[%s]%s\n"
        (Model.key m)
        m.name
        (String.concat
           ~sep:" "
           (List.map (Model.thinking_options m) ~f:Thinking.to_string))
        (if m.supports_images then "" else " no-images"))
;;

let%expect_test "codex: its models, picked and thought about per turn" =
  with_session ~adapter:(module Codex_adapter) ~peer:codex
  @@ fun agent ->
  show_model agent;
  show_models agent;
  [%expect
    {|
    model codex/default (loading models…), thinking default, context 0 of 200000
      codex/default "loading models\226\128\166" thinking=[default low medium high]
    |}];
  prompt agent "hi";
  (* The harness said which models it has, and how full its context is. *)
  show_model agent;
  show_models agent;
  [%expect
    {|
    codex got turn/start model=- effort=-
    assistant (codex/gpt-5, End_turn): "done"
    model codex/gpt-5 (GPT-5), thinking default, context 3020 of 400000
      codex/gpt-5 "GPT-5" thinking=[default minimal low medium high]
      codex/gpt-5-codex "GPT-5 Codex" thinking=[default low medium high]
    |}];
  let models = Option.value_exn (Agent.external_models agent) in
  Agent.set_model
    agent
    (List.find_exn models ~f:(fun m -> String.equal m.id "gpt-5-codex"));
  Or_error.ok_exn (Agent.set_thinking agent (On (Some High)));
  print_s
    [%sexp (Agent.set_thinking agent (On (Some Minimal)) : unit Or_error.t)];
  prompt agent "again";
  show_model agent;
  [%expect
    {|
    (Error
     "GPT-5 Codex does not take thinking minimal; it takes: default, low, medium, high")
    codex got turn/start model=gpt-5-codex effort=high
    assistant (codex/gpt-5-codex, End_turn): "done"
    model codex/gpt-5-codex (GPT-5 Codex), thinking high, context 3020 of 400000
    |}]
;;

let%expect_test "codex: /compact asks Codex to compact its context" =
  with_session ~adapter:(module Codex_adapter) ~peer:codex
  @@ fun agent ->
  prompt agent "hi";
  [%expect
    {|
    codex got turn/start model=- effort=-
    assistant (codex/gpt-5, End_turn): "done"
    |}];
  print_s [%sexp (Agent.compact agent : string Or_error.t)];
  [%expect
    {|
    codex got thread/compact/start
    notice: Codex compacted its context
    (Ok "")
    |}]
;;

let%expect_test "claude: models from its handshake; switching them mid-session" =
  with_session ~adapter:(module Claude_adapter) ~peer:claude
  @@ fun agent ->
  prompt agent "hi";
  show_model agent;
  show_models agent;
  [%expect
    {|
    claude got user "hi"
    assistant (claude/claude-sonnet-4-6, End_turn): "done"
    model claude/claude-sonnet-4-6 (claude-sonnet-4-6), thinking default, context 7 of 200000
      claude/default "Default (Sonnet)" thinking=[off default low medium high max]
      claude/opus "Opus" thinking=[off default low medium high max]
      claude/haiku "Haiku" thinking=[off default low medium high max]
    |}];
  let models = Option.value_exn (Agent.external_models agent) in
  Agent.set_model
    agent
    (List.find_exn models ~f:(fun m -> String.equal m.id "opus"));
  Or_error.ok_exn (Agent.set_thinking agent (On (Some High)));
  Agent.set_mode agent ~plan:true;
  prompt agent "plan it";
  [%expect
    {|
    claude got set_model {"model":"opus"}
    claude got set_permission_mode {"mode":"plan"}
    claude got set_max_thinking_tokens {"max_thinking_tokens":32000}
    claude got user "plan it"
    assistant (claude/opus, End_turn): "done"
    |}];
  print_s
    [%sexp
      (Agent.compact agent ~instructions:"keep the plan" : string Or_error.t)];
  [%expect
    {|
    claude got user "/compact keep the plan"
    notice: Claude Code compacted its context
    assistant (claude/opus, End_turn): ""
    (Ok "")
    |}]
;;

let%expect_test "prigh's own requests use prigh's model, not the harness's" =
  with_session ~adapter:(module Codex_adapter) ~peer:codex
  @@ fun agent ->
  let key m = Model.key (Or_error.ok_exn m) in
  printf
    "session %s; approvals %s; names %s\n"
    (Model.key (Agent.state agent).model)
    (key (Agent.approval_model agent))
    (key (Agent.naming_model agent));
  [%expect
    {| session codex/default; approvals deepseek/deepseek-flash; names deepseek/deepseek-flash |}]
;;

let%expect_test "back to the harness engine: prigh's model again" =
  with_session ~adapter:(module Claude_adapter) ~peer:claude
  @@ fun agent ->
  prompt agent "hi";
  Agent.set_engine agent Engine_choice.harness;
  Agent.set_runner agent None;
  show_model agent;
  print_s
    [%message
      (Agent.engine agent : Engine_id.t)
        ~models:(Option.is_some (Agent.external_models agent) : bool)];
  [%expect
    {|
    claude got user "hi"
    assistant (claude/claude-sonnet-4-6, End_turn): "done"
    model deepseek/deepseek-flash (DeepSeek V4.1 Flash), thinking default, context 5 of 1000000
    (("Agent.engine agent" Harness) (models false))
    |}]
;;

let%expect_test "a default row's name and the id a reply came under" =
  List.iter
    [ "GPT-6.2-Sol", "gpt-6.2-sol"
    ; "Opus 5", "claude-opus-5-20260101"
    ; "Opus 5", "opus-5 (1M context)"
    ; "Sonnet 4", "claude-sonnet-4-5-20250929"
    ; "opus-4", "opus-4-1"
    ; "Haiku 4.5", "claude-haiku-4.5"
    ]
    ~f:(fun (name, id) ->
      printf "%s ~ %s: %b\n" name id (External_model.same_model name id));
  [%expect
    {|
    GPT-6.2-Sol ~ gpt-6.2-sol: true
    Opus 5 ~ claude-opus-5-20260101: true
    Opus 5 ~ opus-5 (1M context): true
    Sonnet 4 ~ claude-sonnet-4-5-20250929: false
    opus-4 ~ opus-4-1: false
    Haiku 4.5 ~ claude-haiku-4.5: true
    |}]
;;
