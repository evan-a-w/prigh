open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

let exhausted =
  Reply.text
    ~stop_reason:
      (Error "HTTP 429: The usage limit has been reached (usage limit reached)")
    ""
;;

let with_agent ?(config = []) ?(cwd = fun dir -> dir) replies f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  write t ".prigh/config.json" (Jsonaf.to_string (`Object config));
  let provider =
    Faux_provider.create
      ~on_request:(fun (r : Provider.Request.t) ->
        let last =
          match List.last r.messages with
          | Some (User u) -> "user: " ^ u.text
          | Some (Assistant _) -> "assistant"
          | Some (Tool_result _) -> "tool result"
          | None -> "nothing"
        in
        printf "request to %s, last %s\n" (Model.key r.model) last)
      replies
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:[]
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:(cwd t.dir)
      ()
  in
  Agent.subscribe agent ~f:(function
    | Notice n -> print_endline ("notice: " ^ n)
    | Loop (Message_end (Assistant a)) ->
      printf
        "assistant (%s): %s %s\n"
        a.model
        (Message.Assistant.text a)
        (Sexp.to_string [%sexp (a.stop_reason : Stop_reason.t)])
    | _ -> ());
  f t agent
;;

let chain =
  [ ( "fallback_models"
    , `Array
        [ `String "openai-codex/gpt-6-sol"
        ; `String "anthropic/claude-opus-5-5"
        ; `String "deepseek/deepseek-flash"
        ] )
  ]
;;

let%expect_test "classifying provider errors" =
  List.iter
    [ "HTTP 429: The usage limit has been reached (usage limit reached)"
    ; "HTTP 429: Rate limited, slow down"
    ; "HTTP 402: Insufficient Balance"
    ; "HTTP 429: You exceeded your current quota, please check your plan"
    ; "HTTP 400: Your credit balance is too low to access the Anthropic API"
    ; "not logged in to anthropic: use /login anthropic"
    ; "HTTP 503: overloaded"
    ]
    ~f:(fun message ->
      printf
        "unavailable=%b retryable=%b  %s\n"
        (Usage_limit.unavailable message)
        (Agent_loop.is_retryable_error message)
        message);
  [%expect
    {|
    unavailable=true retryable=false  HTTP 429: The usage limit has been reached (usage limit reached)
    unavailable=false retryable=true  HTTP 429: Rate limited, slow down
    unavailable=true retryable=false  HTTP 402: Insufficient Balance
    unavailable=true retryable=false  HTTP 429: You exceeded your current quota, please check your plan
    unavailable=true retryable=false  HTTP 400: Your credit balance is too low to access the Anthropic API
    unavailable=true retryable=false  not logged in to anthropic: use /login anthropic
    unavailable=false retryable=true  HTTP 503: overloaded
    |}];
  List.iter
    [ ( false
      , {|{"error":{"type":"usage_limit_reached","message":"The usage limit has been reached"}}|}
      )
    ; false, {|{"error":{"code":"insufficient_quota","message":"Quota gone"}}|}
    ; false, {|{"error":{"type":"rate_limit_error","message":"Slow down"}}|}
    ; ( true
      , {|{"type":"error","error":{"type":"rate_limit_error","message":"This request would exceed your account's rate limit."}}|}
      )
    ]
    ~f:(fun (limit_rejected, body) ->
      print_endline
        (Sse_request.error_message_of_body ~limit_rejected ~status:429 body));
  [%expect
    {|
    HTTP 429: The usage limit has been reached (usage limit reached)
    HTTP 429: Quota gone (usage limit reached)
    HTTP 429: Slow down
    HTTP 429: This request would exceed your account's rate limit. (usage limit reached)
    |}]
;;

let%expect_test "a run hands over down the chain and carries on" =
  with_agent ~config:chain [ exhausted; exhausted; Reply.text "done it" ]
  @@ fun _t agent ->
  print_endline (Model.key (Agent.state agent).model);
  [%expect {| openai-codex/gpt-6-sol |}];
  Or_error.ok_exn (Agent.prompt agent "fix the bug");
  Agent.wait_idle agent;
  [%expect
    {|
    request to openai-codex/gpt-6-sol, last user: fix the bug
    assistant (gpt-6-sol):  (Error"HTTP 429: The usage limit has been reached (usage limit reached)")
    notice: openai-codex/gpt-6-sol: HTTP 429: The usage limit has been reached (usage limit reached); handing over to anthropic/claude-opus-5-5
    request to anthropic/claude-opus-5-5, last user: [prigh: openai-codex/gpt-6-sol cannot continue (HTTP 429: The usage limit has been reached (usage limit reached)), so anthropic/claude-opus-5-5 takes over this conversation from here. Carry on with the task where it left off.]
    assistant (claude-opus-5-5):  (Error"HTTP 429: The usage limit has been reached (usage limit reached)")
    notice: anthropic/claude-opus-5-5: HTTP 429: The usage limit has been reached (usage limit reached); handing over to deepseek/deepseek-flash
    request to deepseek/deepseek-flash, last user: [prigh: anthropic/claude-opus-5-5 cannot continue (HTTP 429: The usage limit has been reached (usage limit reached)), so deepseek/deepseek-flash takes over this conversation from here. Carry on with the task where it left off.]
    assistant (deepseek-flash): done it End_turn
    |}];
  (* The session stays on the model that took over. *)
  print_endline (Model.key (Agent.state agent).model);
  [%expect {| deepseek/deepseek-flash |}]
;;

let%expect_test "a hand-over never shows the agent idle in between" =
  with_agent ~config:chain [ exhausted; Reply.text "done" ]
  @@ fun _t agent ->
  Agent.subscribe agent ~f:(function
    | State_changed s ->
      printf "state: running=%b model=%s\n" s.running (Model.key s.model)
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  [%expect
    {|
    state: running=true model=openai-codex/gpt-6-sol
    request to openai-codex/gpt-6-sol, last user: go
    assistant (gpt-6-sol):  (Error"HTTP 429: The usage limit has been reached (usage limit reached)")
    notice: openai-codex/gpt-6-sol: HTTP 429: The usage limit has been reached (usage limit reached); handing over to anthropic/claude-opus-5-5
    state: running=true model=anthropic/claude-opus-5-5
    state: running=true model=anthropic/claude-opus-5-5
    request to anthropic/claude-opus-5-5, last user: [prigh: openai-codex/gpt-6-sol cannot continue (HTTP 429: The usage limit has been reached (usage limit reached)), so anthropic/claude-opus-5-5 takes over this conversation from here. Carry on with the task where it left off.]
    assistant (claude-opus-5-5): done End_turn
    state: running=false model=anthropic/claude-opus-5-5
    |}]
;;

let%expect_test "the end of the chain stops; other errors never hand over" =
  with_agent
    ~config:chain
    [ exhausted
    ; exhausted
    ; exhausted
    ; Reply.text ~stop_reason:(Error "HTTP 400: bad request") ""
    ]
  @@ fun _t agent ->
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  [%expect
    {|
    request to openai-codex/gpt-6-sol, last user: go
    assistant (gpt-6-sol):  (Error"HTTP 429: The usage limit has been reached (usage limit reached)")
    notice: openai-codex/gpt-6-sol: HTTP 429: The usage limit has been reached (usage limit reached); handing over to anthropic/claude-opus-5-5
    request to anthropic/claude-opus-5-5, last user: [prigh: openai-codex/gpt-6-sol cannot continue (HTTP 429: The usage limit has been reached (usage limit reached)), so anthropic/claude-opus-5-5 takes over this conversation from here. Carry on with the task where it left off.]
    assistant (claude-opus-5-5):  (Error"HTTP 429: The usage limit has been reached (usage limit reached)")
    notice: anthropic/claude-opus-5-5: HTTP 429: The usage limit has been reached (usage limit reached); handing over to deepseek/deepseek-flash
    request to deepseek/deepseek-flash, last user: [prigh: anthropic/claude-opus-5-5 cannot continue (HTTP 429: The usage limit has been reached (usage limit reached)), so deepseek/deepseek-flash takes over this conversation from here. Carry on with the task where it left off.]
    assistant (deepseek-flash):  (Error"HTTP 429: The usage limit has been reached (usage limit reached)")
    notice: deepseek/deepseek-flash cannot continue and no model in fallback_models is left to hand over to; /model picks another
    |}];
  (* A new prompt starts a new chain from the current model; an ordinary
     error just stops. *)
  Or_error.ok_exn (Agent.prompt agent "again");
  Agent.wait_idle agent;
  [%expect
    {|
    request to deepseek/deepseek-flash, last user: again
    assistant (deepseek-flash):  (Error"HTTP 400: bad request")
    |}]
;;

let%expect_test "a model outside the chain hands over to the chain's first" =
  with_agent
    ~config:(("default_model", `String "deepseek/deepseek-v4-pro") :: chain)
    [ exhausted; Reply.text "ok" ]
  @@ fun _t agent ->
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  [%expect
    {|
    request to deepseek/deepseek-v4-pro, last user: go
    assistant (deepseek-v4-pro):  (Error"HTTP 429: The usage limit has been reached (usage limit reached)")
    notice: deepseek/deepseek-v4-pro: HTTP 429: The usage limit has been reached (usage limit reached); handing over to openai-codex/gpt-6-sol
    request to openai-codex/gpt-6-sol, last user: [prigh: deepseek/deepseek-v4-pro cannot continue (HTTP 429: The usage limit has been reached (usage limit reached)), so openai-codex/gpt-6-sol takes over this conversation from here. Carry on with the task where it left off.]
    assistant (gpt-6-sol): ok End_turn
    |}]
;;

let%expect_test "no chain: the error stands" =
  with_agent [ exhausted ]
  @@ fun _t agent ->
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  [%expect
    {|
    request to deepseek/deepseek-flash, last user: go
    assistant (deepseek-flash):  (Error"HTTP 429: The usage limit has been reached (usage limit reached)")
    |}]
;;

let%expect_test "set_config: fallback models resolved to keys, errors name them"
  =
  with_agent []
  @@ fun _t agent ->
  let set models =
    let config = { (Agent.config agent) with fallback_models = models } in
    match Agent.set_config agent config with
    | Ok () ->
      print_s [%sexp ((Agent.config agent).fallback_models : string list)]
    | Error e -> print_endline (Error.to_string_hum e)
  in
  set
    [ "GPT-6 Sol"
    ; "claude-opus-5-5"
    ; "deepseek-flash"
    ; "deepseek/deepseek-flash"
    ];
  set [ "gpt-6-soll" ];
  [%expect
    {|
    (openai-codex/gpt-6-sol anthropic/claude-opus-5-5 deepseek/deepseek-flash)
    fallback_models: unknown model "gpt-6-soll"; did you mean: openai-codex/gpt-6-sol (GPT-6 Sol), openai/gpt-5.6-sol (GPT-5.6 Sol), openai-codex/gpt-5.6-sol (GPT-5.6 Sol)
    |}]
;;

let%expect_test "default_cwd: where new sessions start" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  Core_unix.mkdir_p (Filename.concat t.dir "work");
  let create () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [])
      ~tools:[]
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:"/"
      ()
  in
  let show () = print_endline (mask t (Agent.state (create ())).cwd) in
  write t ".prigh/config.json" {|{"default_cwd": "$DIR/work"}|};
  write t ".prigh/config.json" (sprintf {|{"default_cwd": "%s/work"}|} t.dir);
  show ();
  write t ".prigh/config.json" {|{"default_cwd": "/no/such/dir"}|};
  show ();
  [%expect
    {|
    $DIR/work
    /
    |}];
  (* An explicit -cwd wins; set_config refuses a directory that is not
     there. *)
  write t ".prigh/config.json" (sprintf {|{"default_cwd": "%s/work"}|} t.dir);
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [])
      ~tools:[]
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~use_default_cwd:false
      ~cwd:"/"
      ()
  in
  print_endline (Agent.state agent).cwd;
  let set dir =
    match
      Agent.set_config
        agent
        { (Agent.config agent) with default_cwd = Some dir }
    with
    | Ok () -> print_endline "ok"
    | Error e ->
      print_endline
        (String.substr_replace_all
           (Error.to_string_hum e)
           ~pattern:(Core_unix.gethostname ())
           ~with_:"HOST")
  in
  set "/no/such/dir";
  set "work";
  set "~";
  [%expect
    {|
    /
    default_cwd: /no/such/dir is not a directory on the backend (HOST)
    default_cwd: work must be absolute (or start with ~/)
    ok
    |}]
;;
