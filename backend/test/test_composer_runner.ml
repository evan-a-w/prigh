open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

(* A Codex app-server replaying a recorded transcript: it waits for each
   request we are expected to send (by method; the ids are ours), answers
   it with the recorded answer, and writes the recorded notifications and
   requests. Requests the recording made that we do not ([config/read],
   [thread/name/set]) are skipped with their answers. *)
let codex_replay
      ?(on_wait = fun (_ : string) -> ())
      name
      (p : Line_transport.Peer.t)
  =
  let lines = In_channel.read_lines [%string "fixtures/codex/%{name}.ndjson"] in
  let ours =
    [ "initialize"
    ; "thread/start"
    ; "thread/resume"
    ; "turn/start"
    ; "turn/interrupt"
    ; "turn/steer"
    ]
  in
  let ids = Int.Table.create () in
  let skipped = Int.Table.create () in
  let member k j = Json.member k j in
  let int_of j = Option.bind j ~f:Json.int in
  let rec read_until f =
    match Line_transport.Peer.read_line p with
    | None -> None
    | Some line ->
      let j = Json.parse line |> Or_error.ok_exn in
      if f j then Some j else read_until f
  in
  List.iter lines ~f:(fun line ->
    let record = Json.parse line |> Or_error.ok_exn in
    let kind =
      Option.value
        (Option.bind (member "type" record) ~f:Json.string)
        ~default:""
    in
    let label =
      Option.value
        (Option.bind (member "label" record) ~f:Json.string)
        ~default:""
    in
    let frame = Option.value (member "frame" record) ~default:`Null in
    let id = int_of (member "id" frame) in
    let meth = member "method" frame in
    match kind with
    | "expect_outbound" ->
      (match meth, id with
       | Some _, Some id when List.mem ours label ~equal:String.equal ->
         on_wait label;
         (match
            read_until (fun j ->
              match member "method" j with
              | Some (`String m) -> String.equal m label
              | _ -> false)
          with
          | Some j ->
            Option.iter
              (int_of (member "id" j))
              ~f:(fun ours -> Hashtbl.set ids ~key:id ~data:ours)
          | None -> ())
       | Some _, Some id -> Hashtbl.set skipped ~key:id ~data:()
       | Some _, None -> ()
       | None, _ ->
         (* our answer to its request *)
         ignore
           (read_until (fun j -> Option.is_none (member "method" j))
            : Json.t option))
    | "emit_inbound" ->
      (match meth, id with
       | None, Some id when Hashtbl.mem skipped id -> ()
       | None, Some id ->
         let ours = Option.value (Hashtbl.find ids id) ~default:id in
         let frame =
           match frame with
           | `Object fields ->
             `Object
               (List.map fields ~f:(fun (k, v) ->
                  if String.equal k "id"
                  then k, `Number (Int.to_string ours)
                  else k, v))
           | j -> j
         in
         Line_transport.Peer.write_line p (Json.to_string frame)
       | _ -> Line_transport.Peer.write_line p (Json.to_string frame))
    | _ -> ())
;;

(* Claude Code replaying a recording of its output for one prompt, after
   answering [initialize]; its permission requests wait for our answer. *)
let claude_replay ?(exit_after = None) name (p : Line_transport.Peer.t) =
  let lines = In_channel.read_lines [%string "fixtures/claude/%{name}.jsonl"] in
  let read () =
    match Line_transport.Peer.read_line p with
    | None -> None
    | Some line -> Some (Json.parse line |> Or_error.ok_exn)
  in
  let request_id j = Option.bind (Json.member "request_id" j) ~f:Json.string in
  (match read () with
   | Some init ->
     Line_transport.Peer.write_line
       p
       (sprintf
          {|{"type":"control_response","response":{"subtype":"success","request_id":"%s","response":{"claude_code_version":"2.1.268","models":[],"commands":[]}}}|}
          (Option.value (request_id init) ~default:""))
   | None -> ());
  (* the prompt *)
  ignore (read () : Json.t option);
  List.iteri lines ~f:(fun i line ->
    (match exit_after with
     | Some n when i = n -> Line_transport.Peer.exit p ~code:1
     | _ -> ());
    Line_transport.Peer.write_line p line;
    match Json.parse line with
    | Ok j when Option.is_some (Json.member "request" j) ->
      (match Json.member "type" j with
       | Some (`String "control_request") ->
         (match read () with
          | Some answer -> printf "claude got: %s\n" (Json.to_string answer)
          | None -> ())
       | _ -> ())
    | _ -> ())
;;

let print_event t (e : Agent.Event.t) =
  match e with
  | Loop (Message_end (Assistant a)) ->
    printf
      "assistant (%s, %s): %S\n"
      a.model
      (Sexp.to_string [%sexp (a.stop_reason : Stop_reason.t)])
      (Message.Assistant.text a)
  | Loop (Message_end (User u)) -> printf "user: %S\n" u.text
  | Loop (Tool_start c) ->
    printf "tool_start %s %s\n" c.name (mask t c.arguments)
  | Loop (Tool_end { call; result }) ->
    printf
      "tool_end %s -> %s%S\n"
      call.name
      (if result.is_error then "ERROR " else "")
      (String.prefix result.text 120)
  | Notice n -> printf "notice: %s\n" n
  | _ -> ()
;;

let with_composer ?(approval : Approval_mode.t option) ~adapter ~peer f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let launches = ref 0 in
  let composer =
    Composer_runner.create
      ~sw
      ~adapter
      ~spawn:(fun (launch : Adapter.Launch.t) ->
        incr launches;
        printf
          "spawn #%d: %s\n"
          !launches
          (String.concat ~sep:" " (List.take launch.argv 2));
        Ok (Line_transport.Peer.spawn ~sw (peer !launches)))
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
      ~runner:(Composer_runner.runner composer)
      ~cwd:t.dir
      ()
  in
  (let (module A : Adapter.S) = adapter in
   Agent.set_engine
     agent
     { engine = Composer
     ; adapter = Some (Adapter_id.of_string A.id |> Or_error.ok_exn)
     });
  Option.iter approval ~f:(fun approval -> Agent.set_mode agent ~approval);
  Agent.subscribe agent ~f:(print_event t);
  f t agent composer;
  Composer_runner.close composer
;;

let prompt agent text =
  Or_error.ok_exn (Agent.prompt agent text);
  Agent.wait_idle agent
;;

let%expect_test "codex: a text turn, recorded like a harness turn" =
  with_composer
    ~adapter:(module Codex_adapter)
    ~peer:(fun _ -> codex_replay "simple")
  @@ fun _ agent composer ->
  prompt agent "say it";
  print_s
    [%message
      (Composer_runner.thread_id composer : string option)
        ~messages:((Agent.state agent).message_count : int)];
  [%expect
    {|
    user: "[Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]\n\nsay it"
    spawn #1: codex app-server
    assistant (codex/gpt-6-luna, End_turn): "fixture simple ok"
    (("Composer_runner.thread_id composer"
      (01a0d5eb-f0c0-7481-a417-5e8871805cbc))
     (messages 2))
    |}]
;;

let%expect_test
    "codex: a command's approval goes through the session's approvals"
  =
  with_composer
    ~approval:Ask
    ~adapter:(module Codex_adapter)
    ~peer:(fun _ -> codex_replay "tool_call_read_only_on_request")
  @@ fun _ agent _ ->
  Agent.subscribe agent ~f:(function
    | Loop (Tool_confirm { call_id; name; summary; _ }) ->
      printf "confirm %s %s: %s\n" call_id name summary;
      Or_error.ok_exn (Agent.respond_confirm agent ~call_id Allow)
    | _ -> ());
  prompt agent "run it";
  [%expect
    {|
    user: "[Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]\n\nrun it"
    spawn #1: codex app-server
    assistant (codex/gpt-6-sol, Tool_use): "I\226\128\153ll write the exact text to the file with a local shell command."
    tool_start bash {"command":"printf '%s' 'codex app-server approval fixture' > .codex-probe-write-action.txt"}
    confirm exec-02c3fc6e-7a54-4c15-afda-bc59a03b138a bash: printf '%s' 'codex app-server approval fixture' > .codex-probe-write-action.txt
    tool_end bash -> ""
    assistant (codex/gpt-6-sol, End_turn): "Created or overwrote `.codex-probe-write-action.txt` with exactly `codex app-server approval fixture`."
    |}]
;;

let%expect_test "codex: its questions go to the user" =
  with_composer
    ~adapter:(module Codex_adapter)
    ~peer:(fun _ -> codex_replay "plan_questions")
  @@ fun _ agent _ ->
  Agent.subscribe agent ~f:(function
    | Loop (Ask_user { call_id; questions; _ }) ->
      List.iter questions ~f:(fun q -> printf "ask_user: %s\n" q.question);
      Or_error.ok_exn
        (Agent.answer_question
           agent
           ~call_id
           (Answered [ [ "Strict schemas (Recommended)" ] ]))
    | _ -> ());
  prompt agent "ask me";
  [%expect
    {|
    user: "[Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]\n\nask me"
    spawn #1: codex app-server
    ask_user: Should this fixture prefer strict schemas or UI flexibility?
    assistant (codex/gpt-6-luna, End_turn): "plan questions fixture complete"
    |}]
;;

let%expect_test "codex: aborting the run interrupts the turn" =
  with_composer
    ~adapter:(module Codex_adapter)
    ~peer:(fun _ p ->
      codex_replay "turn_interrupt" p ~on_wait:(function
        | "turn/interrupt" -> print_endline "peer: waiting for turn/interrupt"
        | _ -> ()))
  @@ fun _ agent _ ->
  Agent.subscribe agent ~f:(function
    | State_changed { running = true; _ } -> ()
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "start");
  (* Once the turn started (the peer waits for the interrupt), abort. *)
  Eio_unix.sleep 0.05;
  print_s [%sexp (Agent.abort agent : string list)];
  Agent.wait_idle agent;
  [%expect
    {|
    user: "[Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]\n\nstart"
    spawn #1: codex app-server
    peer: waiting for turn/interrupt
    ()
    |}]
;;

let%expect_test
    "codex: an interrupt Codex ignores ends the turn after the grace period"
  =
  with_composer
    ~adapter:(module Codex_adapter)
    ~peer:(fun _ p ->
      codex_replay "turn_interrupt" p ~on_wait:(function
        | "turn/interrupt" ->
          (* never answers *)
          Eio.Fiber.await_cancel ()
        | _ -> ()))
  @@ fun _ agent _ ->
  Or_error.ok_exn (Agent.prompt agent "start");
  Eio_unix.sleep 0.05;
  ignore (Agent.abort agent : string list);
  Agent.wait_idle agent;
  print_s [%message "ended" ~running:((Agent.state agent).running : bool)];
  [%expect
    {|
    user: "[Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]\n\nstart"
    spawn #1: codex app-server
    (ended (running false))
    |}]
;;

let usage_limit_frames =
  [ {|{"method":"turn/started","params":{"threadId":"th","turn":{"id":"tu","items":[],"status":"inProgress"}}}|}
  ; {|{"method":"error","params":{"threadId":"th","turnId":"tu","willRetry":false,"error":{"message":"You've hit your usage limit.","codexErrorInfo":"usageLimitExceeded"}}}|}
  ; {|{"method":"turn/completed","params":{"threadId":"th","turn":{"id":"tu","items":[],"status":"failed","error":{"message":"You've hit your usage limit.","codexErrorInfo":"usageLimitExceeded"}}}}|}
  ]
;;

(* A Codex that answers the handshake and the thread, then plays [frames]
   for the turn, or exits with [crash] partway. *)
let codex_turn ?crash frames (p : Line_transport.Peer.t) =
  let next () =
    Json.parse (Option.value_exn (Line_transport.Peer.read_line p))
    |> Or_error.ok_exn
  in
  let reply j result =
    Line_transport.Peer.write_line
      p
      (Json.to_string
         (`Object
             [ "id", Option.value_exn (Json.member "id" j); "result", result ]))
  in
  let init = next () in
  reply init (`Object [ "userAgent", `String "codex/0.166.0" ]);
  let rec thread () =
    let j = next () in
    match Json.member "method" j with
    | Some (`String (("thread/start" | "thread/resume") as m)) ->
      printf "codex got %s\n" m;
      reply
        j
        (`Object
            [ "thread", `Object [ "id", `String "th" ]
            ; "model", `String "gpt-5"
            ])
    | _ -> thread ()
  in
  thread ();
  let turn = next () in
  reply
    turn
    (`Object
        [ "turn", `Object [ "id", `String "tu"; "status", `String "inProgress" ]
        ]);
  List.iteri frames ~f:(fun i frame ->
    (match crash with
     | Some n when i = n ->
       Line_transport.Peer.write_stderr
         p
         "thread 'main' panicked at codex-rs/core\n";
       Line_transport.Peer.exit p ~code:101
     | _ -> ());
    Line_transport.Peer.write_line p frame)
;;

let%expect_test
    "codex: a usage limit fails the run the way the agent recognises"
  =
  with_composer
    ~adapter:(module Codex_adapter)
    ~peer:(fun _ -> codex_turn usage_limit_frames)
  @@ fun _ agent _ ->
  prompt agent "go";
  [%expect
    {|
    user: "[Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]\n\ngo"
    spawn #1: codex app-server
    codex got thread/start
    assistant (codex/gpt-5, (Error"You've hit your usage limit. (usage limit reached)")): ""
    notice: Codex: the usage limit is reached and the provider did not say when it resets, so the run will not resume by itself; send a message to try again, or /model picks another model
    |}]
;;

let%expect_test
    "codex: a crash mid-turn ends the turn, and the next one restarts it"
  =
  with_composer
    ~adapter:(module Codex_adapter)
    ~peer:(fun n ->
      if n = 1
      then
        codex_turn
          ~crash:1
          [ {|{"method":"turn/started","params":{"threadId":"th","turn":{"id":"tu","items":[],"status":"inProgress"}}}|}
          ; {|{"method":"item/started","params":{"threadId":"th","turnId":"tu","item":{"type":"commandExecution","id":"c1","command":"make","status":"inProgress"}}}|}
          ]
      else
        codex_turn
          [ {|{"method":"turn/started","params":{"threadId":"th","turn":{"id":"tu2","items":[],"status":"inProgress"}}}|}
          ; {|{"method":"item/completed","params":{"threadId":"th","turnId":"tu2","item":{"type":"agentMessage","id":"m1","text":"back again"}}}|}
          ; {|{"method":"turn/completed","params":{"threadId":"th","turn":{"id":"tu2","items":[],"status":"completed"}}}|}
          ])
  @@ fun _ agent _ ->
  prompt agent "first";
  prompt agent "second";
  [%expect
    {|
    user: "[Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]\n\nfirst"
    spawn #1: codex app-server
    codex got thread/start
    assistant (codex/gpt-5, (Error"Codex exited (code 101)\nthread 'main' panicked at codex-rs/core")): ""
    user: "second"
    spawn #2: codex app-server
    codex got thread/resume
    assistant (codex/gpt-5, End_turn): "back again"
    |}]
;;

let%expect_test "claude: a denied write" =
  with_composer
    ~approval:Ask
    ~adapter:(module Claude_adapter)
    ~peer:(fun _ -> claude_replay "denied_write")
  @@ fun _ agent _ ->
  Agent.subscribe agent ~f:(function
    | Loop (Tool_confirm { call_id; name; summary; _ }) ->
      printf "confirm %s %s: %s\n" call_id name summary;
      Or_error.ok_exn
        (Agent.respond_confirm agent ~call_id (Deny (Some "not that file")))
    | _ -> ());
  prompt agent "write it";
  [%expect
    {|
    user: "[Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]\n\nwrite it"
    spawn #1: claude --output-format
    assistant (claude/claude-sonnet-4-6, Tool_use): ""
    tool_start write {"path":"/tmp/claude-replay-tool_call_denied_write/.claude-denied-write.txt","content":"approval fixture"}
    confirm toolu_016StH6rgvnkDxU2MWt46gYE write: /tmp/claude-replay-tool_call_denied_write/.claude-denied-write.txt
    claude got: {"type":"control_response","response":{"subtype":"success","request_id":"cli-req-1","response":{"behavior":"deny","message":"not that file"}}}
    tool_end write -> ERROR "[denied by user] Their reason: not that file"
    assistant (claude/claude-sonnet-4-6, End_turn): "write permission denied"
    |}]
;;

let%expect_test "claude: a simple turn, with its cost recorded as spend" =
  with_composer
    ~adapter:(module Claude_adapter)
    ~peer:(fun _ -> claude_replay "simple")
  @@ fun _ agent composer ->
  prompt agent "hi";
  let state = Agent.state agent in
  print_s
    [%message
      (Composer_runner.thread_id composer : string option)
        ~cost_usd:(Float.round_decimal state.cost_usd ~decimal_digits:4 : float)];
  [%expect
    {|
    user: "[Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]\n\nhi"
    spawn #1: claude --output-format
    assistant (claude/claude-sonnet-4-6, End_turn): "fixture simple ok"
    (("Composer_runner.thread_id composer"
      (77171d01-fb4f-4dff-a961-d9dd334be93d))
     (cost_usd 0.0117))
    |}]
;;

let%expect_test "claude: the process exits mid-turn" =
  with_composer
    ~adapter:(module Claude_adapter)
    ~peer:(fun _ -> claude_replay ~exit_after:(Some 2) "simple")
  @@ fun _ agent _ ->
  prompt agent "hi";
  [%expect
    {|
    user: "[Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]\n\nhi"
    spawn #1: claude --output-format
    assistant (claude/claude-sonnet-4-6, (Error"Claude Code exited (code 1)")): "fixture simple ok"
    |}]
;;

let%expect_test "spawn failing: the run says why" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let composer =
    Composer_runner.create
      ~sw
      ~adapter:(module Codex_adapter)
      ~spawn:(fun _ ->
        Or_error.error_string
          "Codex is not installed on laptop: install it (npm i -g \
           @openai/codex), then try again")
      ()
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [])
      ~tools:[]
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~runner:(Composer_runner.runner composer)
      ~cwd:t.dir
      ()
  in
  Agent.subscribe agent ~f:(print_event t);
  prompt agent "hi";
  [%expect
    {|
    user: "[Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]\n\nhi"
    assistant (codex/default, (Error"Codex could not start: Codex is not installed on laptop: install it (npm i -g @openai/codex), then try again")): ""
    |}]
;;
