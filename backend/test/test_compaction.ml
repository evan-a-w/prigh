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

let is_ask (m : Message.t) =
  match m with
  | User u -> String.is_prefix u.text ~prefix:"[prigh: compaction]"
  | Assistant _ | Tool_result _ -> false
;;

let is_transcript_request (r : Provider.Request.t) =
  List.is_empty r.tools
  && Option.exists r.system ~f:(String.is_prefix ~prefix:"You summarise")
;;

let only_text (r : Provider.Request.t) =
  match r.messages with
  | [ User u ] -> u.text
  | _ -> "?"
;;

(* A full-fidelity summarising request shows what precedes the ask; a
   transcript one its structure up to the end of the conversation. *)
let print_request (r : Provider.Request.t) =
  let key = Option.value_map r.cache_key ~default:"" ~f:(sprintf " [%s]") in
  match List.last r.messages with
  | Some m when is_ask m ->
    printf
      "summarise%s: %s + ask\n"
      key
      (shapes (List.drop_last_exn r.messages))
  | _ when is_transcript_request r ->
    let lines =
      List.take_while
        (String.split_lines (only_text r))
        ~f:(fun l -> not (String.equal l "</conversation>"))
    in
    printf
      "summarise transcript: %s\n"
      (String.concat
         ~sep:" | "
         (List.filter lines ~f:(fun l ->
            String.is_prefix l ~prefix:"<"
            || String.is_suffix l ~suffix:":"
            || String.is_prefix l ~prefix:"TOOL RESULT")))
  | _ -> printf "request%s: %s\n" key (shapes r.messages)
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
  (* A later user message is kept, not pinned; the earlier one is pinned
     shortened. *)
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
     is never pinned or kept apart from what follows it. The prompt, the
     user's latest message, is pinned as it is. *)
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
      keep 8000: (((kept_from 3)(pinned(((index 0)(shortened false))))))
    U(task) A[c1] T(c1,40000) A[c2] T(c2,5) A[c3] T(c3,5) A[c4] T(c4,40000)
      keep 8000: (((kept_from 7)(pinned(((index 0)(shortened false))))))
    U(old) A[c1] T(c1,40000) U(new) A[c2] T(c2,5)
      keep 8000: (((kept_from 3)(pinned(((index 0)(shortened true))))))
    U(task) A[c1] T(c1,40000)
      keep 8000: ()

      keep 8000: ()
    U(task) S(S) A[c2] T(c2,40000) A[c3] T(c3,5)
      keep 8000: (((kept_from 4)(pinned(((index 0)(shortened false))))))
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
    summarise: U(the task) A[c1] T(c1,40000) + ask
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
    summarise transcript: <conversation> | USER: | ASSISTANT: | TOOL RESULT (filler):
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
    summarise transcript: <conversation> | USER: | ASSISTANT: | TOOL RESULT (filler):
    compacted: S1 (summariser in=10)
    request: U(the task) S(S1) A[c2] T(c2,12000) A[c3] T(c3,12000)
    summarise transcript: <previous-summary> | </previous-summary> | <conversation> | USER: | ASSISTANT: | TOOL RESULT (filler):
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
    summarise [session-1/s1]: U(dig) A[c1] T(c1,40000) + ask
    s1 compacted: SUB SUMMARY (summariser in=10)
    request [session-1/s1]: U(dig) S(SUB SUMMARY) A[c2] T(c2,100)
    s1 ended: 3 turns, in=900120: [subagent: 3 turns, 900120 in / 30 out tokens, $0.2701]
    request [session-1]: U(the task) A[s1] T(s1,64)
    added: U(the task) A[s1] T(s1,64) A(main done)
    |}]
;;

(* ---- the summarising request ------------------------------------------ *)

let opus = Option.value_exn (Model.find "anthropic/claude-opus-4-6")

let prompt =
  { Compaction.Prompt.system = Some "SYSTEM PROMPT"
  ; tools = [ filler.spec ]
  ; thinking = On (Some Low)
  ; cache_key = Some "session-1"
  }
;;

let assistant_calls calls =
  Message.Assistant
    { content =
        List.map calls ~f:(fun (id, name, arguments) ->
          Content.Tool_call { id; name; arguments })
    ; usage = Usage.zero
    ; stop_reason = Tool_use
    ; model = "m"
    }
;;

let result ?(text = "ok") id =
  Message.Tool_result
    { tool_call_id = id; tool_name = "t"; text; is_error = false; images = [] }
;;

let big = String.make 40_000 'x'
let abbreviate = String.substr_replace_all ~pattern:big ~with_:"<40000 x>"

let show_request (r : Provider.Request.t) =
  printf
    "model=%s system=%S tools=[%s] thinking=%s max_tokens=%s cache_key=%s\n"
    r.model.id
    (Option.value_map r.system ~default:"-" ~f:(fun s ->
       List.hd_exn (String.split_lines s)))
    (String.concat
       ~sep:","
       (List.map r.tools ~f:(fun (t : Tool_spec.t) -> t.name)))
    (Thinking.to_string r.thinking)
    (Option.value_map r.max_tokens ~default:"-" ~f:Int.to_string)
    (Option.value r.cache_key ~default:"-");
  printf "messages: %s\n" (shapes r.messages);
  match List.last r.messages with
  | Some (User u) -> printf "last message:\n%s\n" (abbreviate u.text)
  | _ -> ()
;;

let compact
      ?(model = opus)
      ?overflowed
      ?instructions
      ?(on_request = show_request)
      replies
      messages
  =
  Eio_main.run
  @@ fun _env ->
  let provider = Faux_provider.create ~on_request replies in
  match
    Compaction.compact_messages
      ~provider
      ~model
      ~prompt
      ?overflowed
      ?instructions
      messages
  with
  | Error e -> print_s [%message "compaction failed" (e : Error.t)]
  | Ok (context, outcome) ->
    printf "usage: %s\n" (Sexp.to_string [%sexp (outcome.usage : Usage.t)]);
    printf "context: %s\n" (shapes context);
    printf "summary:\n%s\n" outcome.summary
;;

let reply ?(input = 100) ?(cache_read = 0) text =
  { (Reply.text text) with usage = { input; output = 50; cache_read } }
;;

(* Work on a task, with a second instruction, that has grown past the kept
   part. *)
let work =
  [ Message.user "fix the parser"
  ; assistant_calls [ "c1", "read", {|{"path":"src/parser.ml"}|} ]
  ; result "c1"
  ; assistant_calls
      [ "c2", "edit", {|{"path":"src/parser.ml","edits":[]}|}
      ; "c3", "read", {|{"path":"src/lexer.ml"}|}
      ]
  ; result "c2"
  ; result "c3"
  ; Message.user "and keep the public API stable"
  ; assistant_calls
      [ ( "c4"
        , "apply_patch"
        , {|{"input":"*** Begin Patch\n*** Add File: src/api.mli\n+val x : int\n*** End Patch"}|}
        )
      ; "c5", "write", {|{"path":"notes.txt","content":"x"}|}
      ; "c6", "bash", {|{"command":"dune build"}|}
      ]
  ; result "c4"
  ; result "c5"
  ; result ~text:big "c6"
  ; assistant_calls [ "c7", "filler", "{}" ]
  ; result "c7"
  ]
;;

let%expect_test
    "the summariser gets the agent's own request, ending with the ask"
  =
  compact
    [ reply ~cache_read:90_000 "## Goal and constraints\nFix the parser." ]
    work;
  [%expect
    {|
    model=claude-opus-4-6 system="SYSTEM PROMPT" tools=[filler] thinking=low max_tokens=10240 cache_key=session-1
    messages: U(fix the parser) A[c1] T(c1,2) A[c2,c3] T(c2,2) T(c3,2) U(and keep the public ) A[c4,c5,c6] T(c4,2) T(c5,2) T(c6,40000) U([prigh: compaction] )
    last message:
    [prigh: compaction] The conversation up to here is about to be replaced by a summary, which is all of it that will remain (with the user's own messages, kept verbatim). Do not call any tools: reply with the summary as text.

    Use exactly these sections, as Markdown headings, and write "None" under a section with nothing to say:

    ## Goal and constraints
    What the user wants, and every requirement, constraint and preference they stated, in their words where the wording matters.
    ## Decisions
    What was decided, and why.
    ## Failed approaches and exact errors
    What was tried and did not work, with the exact error messages, failing commands and test names. Do not smooth over failures: this evidence is what keeps them from being repeated.
    ## Files
    What was learned from the files read, and what was changed in each and why. A list of the paths read and modified is appended automatically: do not repeat it.
    ## Current state
    What works, what does not, what is in progress.
    ## Next steps
    What remains to be done, in order.

    Keep exact identifiers, paths, commands, values and error messages that may be needed. Reply with the summary only, no preamble.
    usage: ((input 100)(output 50)(cache_read 90000))
    context: U(fix the parser) U(and keep the public ) S(## Goal and constraints) A[c7] T(c7,2)
    summary:
    ## Goal and constraints
    Fix the parser.

    ## Files touched (recorded from tool calls)
    Files read: src/lexer.ml
    Files modified: src/parser.ml, src/api.mli, notes.txt
    |}]
;;

let%expect_test "the user's instructions end the ask" =
  compact
    ~instructions:"focus on the lexer"
    ~on_request:(fun r ->
      match List.last r.messages with
      | Some (User u) ->
        print_endline (List.last_exn (String.split_lines u.text))
      | _ -> ())
    [ reply "S" ]
    work;
  [%expect
    {|
    The user's instructions for this summary: focus on the lexer
    usage: ((input 100)(output 50)(cache_read 0))
    context: U(fix the parser) U(and keep the public ) S(S) A[c7] T(c7,2)
    summary:
    S

    ## Files touched (recorded from tool calls)
    Files read: src/lexer.ml
    Files modified: src/parser.ml, src/api.mli, notes.txt
    |}]
;;

let%expect_test "a context that does not fit goes as a transcript" =
  (* The provider rejected the context as too long. *)
  compact ~overflowed:true [ reply "S" ] work;
  [%expect
    {|
    model=claude-opus-4-6 system="You summarise a coding agent's conversation with its user, so that the agent can carry on from the summary alone." tools=[] thinking=off max_tokens=8192 cache_key=-
    messages: U(<conversation>
    USER:)
    last message:
    <conversation>
    USER:
    fix the parser

    ASSISTANT:
    [called read {"path":"src/parser.ml"}]

    TOOL RESULT (t):
    ok

    ASSISTANT:
    [called edit {"path":"src/parser.ml","edits":[]}]
    [called read {"path":"src/lexer.ml"}]

    TOOL RESULT (t):
    ok

    TOOL RESULT (t):
    ok

    USER:
    and keep the public API stable

    ASSISTANT:
    [called apply_patch {"input":"*** Begin Patch\n*** Add File: src/api.mli\n+val x : int\n*** End Patch"}]
    [called write {"path":"notes.txt","content":"x"}]
    [called bash {"command":"dune build"}]

    TOOL RESULT (t):
    ok

    TOOL RESULT (t):
    ok

    TOOL RESULT (t):
    xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    </conversation>

    Summarise the conversation in <conversation>.

    Use exactly these sections, as Markdown headings, and write "None" under a section with nothing to say:

    ## Goal and constraints
    What the user wants, and every requirement, constraint and preference they stated, in their words where the wording matters.
    ## Decisions
    What was decided, and why.
    ## Failed approaches and exact errors
    What was tried and did not work, with the exact error messages, failing commands and test names. Do not smooth over failures: this evidence is what keeps them from being repeated.
    ## Files
    What was learned from the files read, and what was changed in each and why. A list of the paths read and modified is appended automatically: do not repeat it.
    ## Current state
    What works, what does not, what is in progress.
    ## Next steps
    What remains to be done, in order.

    Keep exact identifiers, paths, commands, values and error messages that may be needed. Reply with the summary only, no preamble.
    usage: ((input 100)(output 50)(cache_read 0))
    context: U(fix the parser) U(and keep the public ) S(S) A[c7] T(c7,2)
    summary:
    S

    ## Files touched (recorded from tool calls)
    Files read: src/lexer.ml
    Files modified: src/parser.ml, src/api.mli, notes.txt
    |}];
  (* The estimate does not fit the window. *)
  compact
    ~model:{ opus with context_window = 16_000 }
    ~on_request:print_request
    [ reply "S" ]
    work;
  [%expect
    {|
    summarise transcript: <conversation> | USER: | ASSISTANT: | TOOL RESULT (t): | ASSISTANT: | TOOL RESULT (t): | TOOL RESULT (t): | USER: | ASSISTANT: | TOOL RESULT (t): | TOOL RESULT (t): | TOOL RESULT (t):
    usage: ((input 100)(output 50)(cache_read 0))
    context: U(fix the parser) U(and keep the public ) S(S) A[c7] T(c7,2)
    summary:
    S

    ## Files touched (recorded from tool calls)
    Files read: src/lexer.ml
    Files modified: src/parser.ml, src/api.mli, notes.txt
    |}]
;;

let%expect_test
    "a full request rejected as too long falls back to the transcript"
  =
  compact
    ~on_request:print_request
    [ { (Reply.context_overflow ()) with
        usage = { input = 7; output = 0; cache_read = 0 }
      }
    ; reply "S"
    ]
    work;
  [%expect
    {|
    summarise [session-1]: U(fix the parser) A[c1] T(c1,2) A[c2,c3] T(c2,2) T(c3,2) U(and keep the public ) A[c4,c5,c6] T(c4,2) T(c5,2) T(c6,40000) + ask
    summarise transcript: <conversation> | USER: | ASSISTANT: | TOOL RESULT (t): | ASSISTANT: | TOOL RESULT (t): | TOOL RESULT (t): | USER: | ASSISTANT: | TOOL RESULT (t): | TOOL RESULT (t): | TOOL RESULT (t):
    usage: ((input 107)(output 50)(cache_read 0))
    context: U(fix the parser) U(and keep the public ) S(S) A[c7] T(c7,2)
    summary:
    S

    ## Files touched (recorded from tool calls)
    Files read: src/lexer.ml
    Files modified: src/parser.ml, src/api.mli, notes.txt
    |}];
  (* So does one answered with tool calls instead of a summary. *)
  compact
    ~on_request:print_request
    [ Reply.tool_call ~id:"x" ~name:"filler" ~arguments:"{}" (); reply "S" ]
    work;
  [%expect
    {|
    summarise [session-1]: U(fix the parser) A[c1] T(c1,2) A[c2,c3] T(c2,2) T(c3,2) U(and keep the public ) A[c4,c5,c6] T(c4,2) T(c5,2) T(c6,40000) + ask
    summarise transcript: <conversation> | USER: | ASSISTANT: | TOOL RESULT (t): | ASSISTANT: | TOOL RESULT (t): | TOOL RESULT (t): | USER: | ASSISTANT: | TOOL RESULT (t): | TOOL RESULT (t): | TOOL RESULT (t):
    usage: ((input 120)(output 58)(cache_read 5))
    context: U(fix the parser) U(and keep the public ) S(S) A[c7] T(c7,2)
    summary:
    S

    ## Files touched (recorded from tool calls)
    Files read: src/lexer.ml
    Files modified: src/parser.ml, src/api.mli, notes.txt
    |}];
  (* Other errors fail the compaction. *)
  compact
    ~on_request:print_request
    [ Reply.error "HTTP 500: internal error" ]
    work;
  [%expect
    {|
    summarise [session-1]: U(fix the parser) A[c1] T(c1,2) A[c2,c3] T(c2,2) T(c3,2) U(and keep the public ) A[c4,c5,c6] T(c4,2) T(c5,2) T(c6,40000) + ask
    ("compaction failed" (e "compaction failed: HTTP 500: internal error"))
    |}]
;;

let%expect_test "an earlier summary is updated, and its files carried on" =
  let earlier =
    Compaction_files.append
      "## Goal and constraints\nFix the parser."
      { read = [ "README.md"; "src/lexer.ml" ]; modified = [ "dune" ] }
  in
  let messages =
    [ Message.user "fix the parser"
    ; Session.summary_message earlier
    ; assistant_calls [ "c1", "edit", {|{"path":"src/lexer.ml","edits":[]}|} ]
    ; result ~text:big "c1"
    ; assistant_calls [ "c2", "filler", "{}" ]
    ; result "c2"
    ]
  in
  compact [ reply "## Goal and constraints\nFix the parser, still." ] messages;
  [%expect
    {|
    model=claude-opus-4-6 system="SYSTEM PROMPT" tools=[filler] thinking=low max_tokens=10240 cache_key=session-1
    messages: U(fix the parser) S(## Goal and constraints) A[c1] T(c1,40000) U([prigh: compaction] )
    last message:
    [prigh: compaction] The conversation up to here is about to be replaced by a summary, which is all of it that will remain (with the user's own messages, kept verbatim). Do not call any tools: reply with the summary as text.

    The conversation begins with the summary of earlier work: update it rather than starting over. Merge what happened since into its sections, keep every item that still matters, and drop only what is obsolete or resolved.

    Use exactly these sections, as Markdown headings, and write "None" under a section with nothing to say:

    ## Goal and constraints
    What the user wants, and every requirement, constraint and preference they stated, in their words where the wording matters.
    ## Decisions
    What was decided, and why.
    ## Failed approaches and exact errors
    What was tried and did not work, with the exact error messages, failing commands and test names. Do not smooth over failures: this evidence is what keeps them from being repeated.
    ## Files
    What was learned from the files read, and what was changed in each and why. A list of the paths read and modified is appended automatically: do not repeat it.
    ## Current state
    What works, what does not, what is in progress.
    ## Next steps
    What remains to be done, in order.

    Keep exact identifiers, paths, commands, values and error messages that may be needed. Reply with the summary only, no preamble.
    usage: ((input 100)(output 50)(cache_read 0))
    context: U(fix the parser) S(## Goal and constraints) A[c2] T(c2,2)
    summary:
    ## Goal and constraints
    Fix the parser, still.

    ## Files touched (recorded from tool calls)
    Files read: README.md
    Files modified: dune, src/lexer.ml
    |}];
  compact
    ~overflowed:true
    ~on_request:(fun r -> print_endline (abbreviate (only_text r)))
    [ reply "S" ]
    messages;
  [%expect
    {|
    <previous-summary>
    ## Goal and constraints
    Fix the parser.
    </previous-summary>

    <conversation>
    USER:
    fix the parser

    ASSISTANT:
    [called edit {"path":"src/lexer.ml","edits":[]}]

    TOOL RESULT (t):
    xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    </conversation>

    Summarise the conversation in <conversation>.

    <previous-summary> is the summary of the work before it: update it rather than starting over. Merge what happened since into its sections, keep every item that still matters, and drop only what is obsolete or resolved.

    Use exactly these sections, as Markdown headings, and write "None" under a section with nothing to say:

    ## Goal and constraints
    What the user wants, and every requirement, constraint and preference they stated, in their words where the wording matters.
    ## Decisions
    What was decided, and why.
    ## Failed approaches and exact errors
    What was tried and did not work, with the exact error messages, failing commands and test names. Do not smooth over failures: this evidence is what keeps them from being repeated.
    ## Files
    What was learned from the files read, and what was changed in each and why. A list of the paths read and modified is appended automatically: do not repeat it.
    ## Current state
    What works, what does not, what is in progress.
    ## Next steps
    What remains to be done, in order.

    Keep exact identifiers, paths, commands, values and error messages that may be needed. Reply with the summary only, no preamble.
    usage: ((input 100)(output 50)(cache_read 0))
    context: U(fix the parser) S(S) A[c2] T(c2,2)
    summary:
    S

    ## Files touched (recorded from tool calls)
    Files read: README.md
    Files modified: dune, src/lexer.ml
    |}]
;;

let%expect_test "files from summaries and tool calls" =
  let show files = print_s [%sexp (files : Compaction_files.t)] in
  let files = Compaction_files.add_calls Compaction_files.empty work in
  show files;
  let summary = Compaction_files.append "TEXT" files in
  print_endline summary;
  let text, parsed = Compaction_files.of_summary summary in
  printf "%S\n" text;
  show parsed;
  (* No files, no section; a summary without one parses as itself. *)
  print_endline (Compaction_files.append "TEXT" Compaction_files.empty);
  let text, parsed = Compaction_files.of_summary "TEXT" in
  printf "%S\n" text;
  show parsed;
  [%expect
    {|
    ((read (src/lexer.ml)) (modified (src/parser.ml src/api.mli notes.txt)))
    TEXT

    ## Files touched (recorded from tool calls)
    Files read: src/lexer.ml
    Files modified: src/parser.ml, src/api.mli, notes.txt
    "TEXT"
    ((read (src/lexer.ml)) (modified (src/parser.ml src/api.mli notes.txt)))
    TEXT
    "TEXT"
    ((read ()) (modified ()))
    |}]
;;

(* ---- the user's messages are carried over ---------------------------- *)

(* The user's earlier messages are pinned shortened, the newest first while
   they fit in about 6k tokens (m00 to m08 do not), the latest as it is; what
   the harness wrote in their place (a reminder, a goal continuation, a
   report of background work) is not, but a message they sent while it
   worked is. *)
let%expect_test "the user's own messages are pinned, shortened, within a budget"
  =
  let user i = Message.user (sprintf "m%02d %s" i (String.make 3000 'z')) in
  let injected source text =
    Message.user ~origin:{ Injected.empty with source = Some source } text
  in
  let messages =
    List.concat_map (List.range 0 20) ~f:(fun i ->
      [ user i
      ; assistant_calls [ sprintf "a%d" i, "filler", "{}" ]
      ; result (sprintf "a%d" i)
      ])
    @ [ injected
          (Reminder { name = "check"; repeat = None; max = None })
          "reminder text"
      ; injected (Goal_continuation { continuation = 1; max = 5 }) "carry on"
      ; Message.user "[job j1 exited 0] make\nok"
      ; Message.user
          ~origin:
            { Injected.empty with
              notes = [ Injected.Note.steer ~from_user:true ~follow_up:false ]
            }
          "[The user sends you this while you work: take it into account and \
           carry on.]\n\n\
           steered"
      ; Message.user ("latest " ^ String.make 30_000 'L')
      ; assistant_calls [ "b1", "filler", "{}" ]
      ; result ~text:big "b1"
      ; assistant_calls [ "b2", "filler", "{}" ]
      ; result "b2"
      ]
  in
  let plan =
    Option.value_exn
      (Compaction.plan
         ~keep_recent_tokens:Compaction.default_keep_recent_tokens
         messages)
  in
  print_s [%sexp (plan : Compaction.Plan.t)];
  (* Shortening again changes nothing: a later compaction pins them again. *)
  let shortened = Pinned_messages.shorten (List.hd_exn messages) in
  printf
    "shortened again unchanged: %b\n"
    (Message.equal shortened (Pinned_messages.shorten shortened));
  let provider = Faux_provider.create [ reply "S" ] in
  let context, _ =
    Or_error.ok_exn
      (Eio_main.run (fun _env ->
         Compaction.compact_messages ~provider ~model:opus ~prompt messages))
  in
  List.iter context ~f:(fun m ->
    match m with
    | User u ->
      printf
        "%s (%d) ...%S\n"
        (shape m)
        (String.length u.text)
        (String.suffix u.text 30)
    | _ -> print_endline (shape m));
  [%expect
    {|
    ((kept_from 67)
     (pinned
      (((index 27) (shortened true)) ((index 30) (shortened true))
       ((index 33) (shortened true)) ((index 36) (shortened true))
       ((index 39) (shortened true)) ((index 42) (shortened true))
       ((index 45) (shortened true)) ((index 48) (shortened true))
       ((index 51) (shortened true)) ((index 54) (shortened true))
       ((index 57) (shortened true)) ((index 63) (shortened true))
       ((index 64) (shortened false)))))
    shortened again unchanged: true
    U(m09 zzzzzzzzzzzzzzzz) (2030) ..."\n[... 1004 characters omitted]"
    U(m10 zzzzzzzzzzzzzzzz) (2030) ..."\n[... 1004 characters omitted]"
    U(m11 zzzzzzzzzzzzzzzz) (2030) ..."\n[... 1004 characters omitted]"
    U(m12 zzzzzzzzzzzzzzzz) (2030) ..."\n[... 1004 characters omitted]"
    U(m13 zzzzzzzzzzzzzzzz) (2030) ..."\n[... 1004 characters omitted]"
    U(m14 zzzzzzzzzzzzzzzz) (2030) ..."\n[... 1004 characters omitted]"
    U(m15 zzzzzzzzzzzzzzzz) (2030) ..."\n[... 1004 characters omitted]"
    U(m16 zzzzzzzzzzzzzzzz) (2030) ..."\n[... 1004 characters omitted]"
    U(m17 zzzzzzzzzzzzzzzz) (2030) ..."\n[... 1004 characters omitted]"
    U(m18 zzzzzzzzzzzzzzzz) (2030) ..."\n[... 1004 characters omitted]"
    U(m19 zzzzzzzzzzzzzzzz) (2030) ..."\n[... 1004 characters omitted]"
    U([The user sends you ) (85) ..."ccount and carry on.]\n\nsteered"
    U(latest LLLLLLLLLLLLL) (30007) ..."LLLLLLLLLLLLLLLLLLLLLLLLLLLLLL"
    S(S) (37) ..." of the conversation so far:\nS"
    A[b2]
    T(b2,2)
    |}]
;;

let%expect_test "pinned messages in a session survive a reload, old files load" =
  with_sandbox
  @@ fun t ->
  let session = Session.create ~dir:t.dir ~cwd:t.dir () in
  List.iter
    (List.hd_exn work
     :: Message.user ("long " ^ String.make 5000 'y')
     :: List.tl_exn work)
    ~f:(fun m -> ignore (Session.append_message session m : Session.Entry.t));
  let provider = Faux_provider.create [ reply "S" ] in
  let show session =
    List.iter (Session.messages session) ~f:(fun m ->
      match m with
      | User u -> printf "%s (%d)\n" (shape m) (String.length u.text)
      | _ -> printf "%s\n" (shape m))
  in
  (match
     Compaction.compact ~env:t.env ~provider ~model:opus ~prompt session
   with
   | Ok _ -> ()
   | Error e -> print_s [%sexp (e : Error.t)]);
  show session;
  print_endline
    (Tool_test_helpers.mask
       t
       (List.last_exn (In_channel.read_lines (Session.path session))));
  [%expect
    {|
    U(fix the parser) (14)
    U(long yyyyyyyyyyyyyyy) (2030)
    U(and keep the public ) (30)
    S(S) (161)
    A[c7]
    T(c7,2)
    ["Entry",{"id":"<id>","parent":"<id>","at":<at>,"payload":["Compaction",{"summary":"S\n\n## Files touched (recorded from tool calls)\nFiles read: src/lexer.ml\nFiles modified: src/parser.ml, src/api.mli, notes.txt","kept_from":"<id>","pinned":[{"id":"<id>","shortened":true},{"id":"<id>","shortened":true},{"id":"<id>"}]}]}]
    |}];
  show (Or_error.ok_exn (Session.load (Session.path session)));
  [%expect
    {|
    U(fix the parser) (14)
    U(long yyyyyyyyyyyyyyy) (2030)
    U(and keep the public ) (30)
    S(S) (161)
    A[c7]
    T(c7,2)
    |}];
  (* Before there could be several, [pinned] was one id. *)
  let path = Filename.concat t.dir "old.jsonl" in
  Out_channel.write_lines
    path
    [ {|["Header",{"id":"s1","cwd":"/a","created_at":"2026-01-01 00:00:00Z"}]|}
    ; {|["Entry",{"id":"e1","parent":null,"payload":["Message",["User",{"text":"the task"}]]}]|}
    ; {|["Entry",{"id":"e2","parent":"e1","payload":["Message",["User",{"text":"more"}]]}]|}
    ; {|["Entry",{"id":"e3","parent":"e2","payload":["Message",["User",{"text":"kept"}]]}]|}
    ; {|["Entry",{"id":"e4","parent":"e3","payload":["Compaction",{"summary":"S","kept_from":"e3","pinned":"e1"}]}]|}
    ; {|["Entry",{"id":"e5","parent":"e4","payload":["Compaction",{"summary":"S2","kept_from":"e3"}]}]|}
    ];
  let old = Or_error.ok_exn (Session.load path) in
  List.iter (Session.active_path old) ~f:(fun e ->
    match e.payload with
    | Compaction { pinned; _ } -> print_s [%sexp (pinned : Pinned_messages.t)]
    | _ -> ());
  Session.rewind old ~to_:"e4" |> Or_error.ok_exn;
  show old;
  [%expect
    {|
    (((id e1) (shortened false)))
    ()
    U(the task) (8)
    S(S) (37)
    U(kept) (4)
    |}]
;;
