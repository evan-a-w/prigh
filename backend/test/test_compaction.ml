open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

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

let shape (m : Message.t) =
  match m with
  | User u when Session.is_summary_message m ->
    sprintf "S(%s)" (List.nth_exn (String.split_lines u.text) 1)
  | User u -> sprintf "U(%s)" (String.prefix u.text 20)
  | Assistant a ->
    (match Message.Assistant.tool_calls a with
     | [] -> sprintf "A(%s)" (Message.Assistant.text a)
     | calls ->
       sprintf
         "A[%s]"
         (String.concat ~sep:"," (List.map calls ~f:(fun c -> c.id))))
  | Tool_result r -> sprintf "T(%s,%d)" r.tool_call_id (String.length r.text)
;;

let shapes messages = String.concat ~sep:" " (List.map messages ~f:shape)

let print_request (r : Provider.Request.t) =
  match r.system with
  | Some system when String.is_prefix system ~prefix:"Summarise" ->
    let transcript =
      match r.messages with
      | [ User u ] -> u.text
      | _ -> "?"
    in
    printf
      "summarise: %s\n"
      (String.concat
         ~sep:" | "
         (List.filter (String.split_lines transcript) ~f:(fun l ->
            String.is_suffix l ~suffix:":"
            || String.is_prefix l ~prefix:"TOOL RESULT")))
  | _ ->
    printf
      "request%s: %s\n"
      (Option.value_map r.cache_key ~default:"" ~f:(sprintf " [%s]"))
      (shapes r.messages)
;;

let config =
  { Agent_loop.Config.model = Model.default
  ; thinking = Off
  ; system = None
  ; tools = [ filler ]
  ; max_turns = None
  ; max_tokens = None
  ; retries = 0
  ; cache_key = None
  }
;;

let run_loop ?(tools = [ filler ]) ?(context = []) replies =
  with_sandbox
  @@ fun t ->
  let provider = Faux_provider.create ~on_request:print_request replies in
  let added =
    Agent_loop.run
      ~env:t.env
      ~provider
      ~config:{ config with tools }
      ~cwd:t.dir
      ~emit:(function
        | Compacted { summary; usage; cost_usd = _ } ->
          printf "compacted: %s (summariser in=%d)\n" summary usage.input
        | Turn_end { assistant = { stop_reason = Error e; _ }; _ } ->
          printf "turn failed: %s\n" e
        | _ -> ())
      ~context
      ~prompts:[ Message.user "the task" ]
      ()
  in
  printf "added: %s\n" (shapes added)
;;

let%expect_test "which messages a compaction keeps" =
  let big = String.make 40_000 'x' in
  let a id =
    Message.Assistant
      { content = [ Tool_call { id; name = "filler"; arguments = "{}" } ]
      ; usage = Usage.zero
      ; stop_reason = Tool_use
      ; model = "m"
      }
  in
  let r id text =
    Message.Tool_result
      { tool_call_id = id
      ; tool_name = "filler"
      ; text
      ; is_error = false
      ; images = []
      }
  in
  let show ~keep messages =
    printf
      "%s\n  keep %d: %s\n"
      (shapes messages)
      keep
      (Sexp.to_string
         [%sexp
           (Compaction.plan ~keep_recent_tokens:keep messages
            : Compaction.Plan.t option)])
  in
  (* One prompt and a long run on it: the cut falls at a turn, never between
     a call and its result, and the prompt is pinned. *)
  let run =
    [ Message.user "task"
    ; a "c1"
    ; r "c1" big
    ; a "c2"
    ; r "c2" "small"
    ; a "c3"
    ; r "c3" "small"
    ]
  in
  show ~keep:8000 run;
  (* At least the last turn stays, however large. *)
  show ~keep:8000 (run @ [ a "c4"; r "c4" big ]);
  (* A later user message is kept, not pinned. *)
  show
    ~keep:8000
    [ Message.user "old"
    ; a "c1"
    ; r "c1" big
    ; Message.user "new"
    ; a "c2"
    ; r "c2" "small"
    ];
  (* Nothing but the prompt to summarise: nothing to do. *)
  show ~keep:8000 [ Message.user "task"; a "c1"; r "c1" big ];
  show ~keep:8000 [];
  (* After a compaction its pinned prompt and summary come first; the summary
     is never pinned or kept apart from what follows it. *)
  show
    ~keep:8000
    [ Message.user "task"
    ; Session.summary_message "S"
    ; a "c2"
    ; r "c2" big
    ; a "c3"
    ; r "c3" "small"
    ];
  [%expect
    {|
    U(task) A[c1] T(c1,40000) A[c2] T(c2,5) A[c3] T(c3,5)
      keep 8000: (((kept_from 3)(pinned(0))))
    U(task) A[c1] T(c1,40000) A[c2] T(c2,5) A[c3] T(c3,5) A[c4] T(c4,40000)
      keep 8000: (((kept_from 7)(pinned(0))))
    U(old) A[c1] T(c1,40000) U(new) A[c2] T(c2,5)
      keep 8000: (((kept_from 3)(pinned())))
    U(task) A[c1] T(c1,40000)
      keep 8000: ()

      keep 8000: ()
    U(task) S(S) A[c2] T(c2,40000) A[c3] T(c3,5)
      keep 8000: (((kept_from 4)(pinned(0))))
    |}]
;;

let%expect_test "a long run compacts between turns and carries on" =
  run_loop
    [ call "c1" 40_000
    ; call ~input:900_000 "c2" 100
    ; Reply.text "SUMMARY"
    ; Reply.text "done"
    ];
  [%expect
    {|
    request: U(the task)
    request: U(the task) A[c1] T(c1,40000)
    summarise: USER: | ASSISTANT: | TOOL RESULT (filler):
    compacted: SUMMARY (summariser in=10)
    request: U(the task) S(SUMMARY) A[c2] T(c2,100)
    added: U(the task) A[c1] T(c1,40000) A[c2] T(c2,100) A(done)
    |}]
;;

let%expect_test "an overflowing request is compacted and sent again" =
  run_loop
    [ call "c1" 40_000
    ; call "c2" 100
    ; Reply.context_overflow ()
    ; Reply.text "SUMMARY"
    ; Reply.text "done"
    ];
  [%expect
    {|
    request: U(the task)
    request: U(the task) A[c1] T(c1,40000)
    request: U(the task) A[c1] T(c1,40000) A[c2] T(c2,100)
    summarise: USER: | ASSISTANT: | TOOL RESULT (filler):
    compacted: SUMMARY (summariser in=10)
    request: U(the task) S(SUMMARY) A[c2] T(c2,100)
    added: U(the task) A[c1] T(c1,40000) A[c2] T(c2,100) A(done)
    |}]
;;

let%expect_test
    "a repeated overflow keeps less, then the run fails saying what to do"
  =
  run_loop
    [ call "c1" 40_000
    ; call "c2" 12_000
    ; call "c3" 12_000
    ; Reply.context_overflow ()
    ; Reply.text "S1"
    ; Reply.context_overflow ()
    ; Reply.text "S2"
    ; Reply.context_overflow ()
    ];
  [%expect
    {|
    request: U(the task)
    request: U(the task) A[c1] T(c1,40000)
    request: U(the task) A[c1] T(c1,40000) A[c2] T(c2,12000)
    request: U(the task) A[c1] T(c1,40000) A[c2] T(c2,12000) A[c3] T(c3,12000)
    summarise: USER: | ASSISTANT: | TOOL RESULT (filler):
    compacted: S1 (summariser in=10)
    request: U(the task) S(S1) A[c2] T(c2,12000) A[c3] T(c3,12000)
    summarise: USER: | USER: | Summary of the conversation so far: | ASSISTANT: | TOOL RESULT (filler):
    compacted: S2 (summariser in=10)
    request: U(the task) S(S2) A[c3] T(c3,12000)
    turn failed: HTTP 400: prompt is too long: 213462 tokens > 200000 maximum (still too long after compacting 2 times; a message too big to compact may be what does not fit: /rewind to before it, /new, or /model a model with a larger context window)
    added: U(the task) A[c1] T(c1,40000) A[c2] T(c2,12000) A[c3] T(c3,12000) A()
    |}]
;;

let%expect_test "an overflow that cannot be compacted fails with what to do" =
  run_loop [ Reply.context_overflow () ];
  [%expect
    {|
    request: U(the task)
    turn failed: HTTP 400: prompt is too long: 213462 tokens > 200000 maximum (compacting to make it fit failed: nothing to compact: conversation is short; a message too big to compact may be what does not fit: /rewind to before it, /new, or /model a model with a larger context window)
    added: U(the task) A()
    |}]
;;

let%expect_test "recognising context overflow errors" =
  List.iter
    [ "HTTP 400: prompt is too long: 213462 tokens > 200000 maximum"
    ; "HTTP 413: Request exceeds the maximum size (request_too_large)"
    ; "HTTP 400: Your input exceeds the context window of this model."
    ; "context_length_exceeded"
    ; "HTTP 400: This model's maximum context length is 131072 tokens. \
       However, you requested 140000 tokens"
    ; "HTTP 400: Input length (265330) exceeds model's maximum context length \
       (262144)."
    ; "the request exceeds the available context size, try increasing it"
    ; "HTTP 429: Rate limit reached: too many tokens per minute"
    ; "HTTP 429: The usage limit has been reached (usage limit reached)"
    ; "HTTP 500: internal error"
    ]
    ~f:(fun message ->
      printf "%-5b %s\n" (Context_overflow.matches message) message);
  [%expect
    {|
    true  HTTP 400: prompt is too long: 213462 tokens > 200000 maximum
    true  HTTP 413: Request exceeds the maximum size (request_too_large)
    true  HTTP 400: Your input exceeds the context window of this model.
    true  context_length_exceeded
    true  HTTP 400: This model's maximum context length is 131072 tokens. However, you requested 140000 tokens
    true  HTTP 400: Input length (265330) exceeds model's maximum context length (262144).
    true  the request exceeds the available context size, try increasing it
    false HTTP 429: Rate limit reached: too many tokens per minute
    false HTTP 429: The usage limit has been reached (usage limit reached)
    false HTTP 500: internal error
    |}]
;;

let%expect_test "a subagent compacts its own context, at its own cost" =
  with_sandbox
  @@ fun t ->
  let provider =
    Faux_provider.create
      ~on_request:print_request
      [ Reply.tool_call
          ~id:"s1"
          ~name:"subagent"
          ~arguments:{|{"task":"dig"}|}
          ()
      ; call "c1" 40_000
      ; call ~input:900_000 "c2" 100
      ; Reply.text "SUB SUMMARY"
      ; Reply.text "sub done"
      ; Reply.text "main done"
      ]
  in
  let subagent =
    Tool_subagent.create
      ~provider
      ~current_model:(fun () -> Model.default)
      ~current_thinking:(fun () -> Thinking.Off)
      ~home:t.dir
      ()
  in
  let added =
    Agent_loop.run
      ~env:t.env
      ~provider
      ~config:
        { config with
          tools = [ filler; subagent ]
        ; cache_key = Some "session-1"
        }
      ~cwd:t.dir
      ~emit:(function
        | Subagent { agent_id; event = Compacted { summary; usage; _ }; _ } ->
          printf
            "%s compacted: %s (summariser in=%d)\n"
            agent_id
            summary
            usage.input
        | Subagent_end { agent_id; usage; turns; result; _ } ->
          printf
            "%s ended: %d turns, in=%d: %s\n"
            agent_id
            turns
            usage.input
            (List.last_exn (String.split_lines result.text))
        | _ -> ())
      ~context:[]
      ~prompts:[ Message.user "the task" ]
      ()
  in
  printf "added: %s\n" (shapes added);
  [%expect
    {|
    request [session-1]: U(the task)
    request [session-1/s1]: U(dig)
    request [session-1/s1]: U(dig) A[c1] T(c1,40000)
    summarise: USER: | ASSISTANT: | TOOL RESULT (filler):
    s1 compacted: SUB SUMMARY (summariser in=10)
    request [session-1/s1]: U(dig) S(SUB SUMMARY) A[c2] T(c2,100)
    s1 ended: 3 turns, in=900120: [subagent: 3 turns, 900120 in / 30 out tokens, $0.2701]
    request [session-1]: U(the task) A[s1] T(s1,64)
    added: U(the task) A[s1] T(s1,64) A(main done)
    |}]
;;
