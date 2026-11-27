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
      ?(on_request = fun (_ : Json.t) -> ())
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
            on_request j;
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

let with_composer
      ?(approval : Approval_mode.t option)
      ?(on_launch = ignore)
      ?start
      ?transport_notice
      ~adapter
      ~peer
      f
  =
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
        on_launch launch;
        let p = Line_transport.Peer.spawn ~sw (peer !launches) in
        match transport_notice with
        | None -> Ok p
        | Some notice ->
          Ok
            (Line_transport.create
               ~read_line:(fun () -> Line_transport.read_line p)
               ~write_line:(Line_transport.write_line p)
               ~stop:(fun () -> Line_transport.stop p)
               ~ended:(fun () -> Line_transport.ended p)
               ~is_ended:(fun () -> Line_transport.is_ended p)
               ~notice
               ()))
      ?start
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

(* What the frontends' activity is made of: the CLI starting, waited for,
   streaming, asking and running tools, and never the model's waits. *)
let%expect_test "codex: a turn's activity, from starting to done" =
  with_composer
    ~approval:Ask
    ~adapter:(module Codex_adapter)
    ~peer:(fun _ -> codex_replay "tool_call_read_only_on_request")
  @@ fun _ agent _ ->
  let last = ref "" in
  let show s =
    if not (String.equal s !last) then print_endline ("  > " ^ s);
    last := s
  in
  Agent.subscribe agent ~f:(function
    | Loop (Agent_waiting { agent; starting }) ->
      show (sprintf "agent_waiting %s starting=%b" agent starting)
    | Loop Agent_start -> show "agent_start"
    | Loop (Agent_end _) -> show "agent_end"
    | Loop Turn_start -> show "turn_start"
    | Loop (Turn_end _) -> show "turn_end"
    | Loop (Message_start (Assistant _)) -> show "message_start assistant"
    | Loop (Message_update { delta = Thinking_delta _; _ }) -> show "thinking"
    | Loop (Message_update { delta = Text_delta _; _ }) -> show "text"
    | Loop (Tool_confirm { call_id; _ }) ->
      show "tool_confirm";
      Or_error.ok_exn (Agent.respond_confirm agent ~call_id Allow)
    | Loop (Tool_confirm_done _) -> show "tool_confirm_done"
    | _ -> ());
  prompt agent "run it";
  [%expect
    {|
      > agent_start
    user: "[Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]\n\nrun it"
      > agent_waiting Codex starting=true
    spawn #1: codex app-server
      > agent_waiting Codex starting=false
      > turn_start
      > agent_waiting Codex starting=false
      > message_start assistant
      > thinking
      > text
    assistant (codex/gpt-6-sol, Tool_use): "I\226\128\153ll write the exact text to the file with a local shell command."
    tool_start bash {"command":"printf '%s' 'codex app-server approval fixture' > .codex-probe-write-action.txt"}
      > tool_confirm
      > tool_confirm_done
    tool_end bash -> ""
      > turn_end
      > agent_waiting Codex starting=false
      > turn_start
      > agent_waiting Codex starting=false
      > message_start assistant
      > text
    assistant (codex/gpt-6-sol, End_turn): "Created or overwrote `.codex-probe-write-action.txt` with exactly `codex app-server approval fixture`."
      > turn_end
      > agent_end
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
let codex_turn
      ?crash
      ?(on_request = fun (_ : Json.t) -> ())
      frames
      (p : Line_transport.Peer.t)
  =
  let next () =
    let j =
      Json.parse (Option.value_exn (Line_transport.Peer.read_line p))
      |> Or_error.ok_exn
    in
    on_request j;
    j
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
  let rec turn () =
    let j = next () in
    match Json.member "method" j with
    | Some (`String "turn/start") -> j
    | _ -> turn ()
  in
  reply
    (turn ())
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

(* The sandbox and approval policy of Codex's thread and turn requests. *)
let print_policy j =
  match Json.member "method" j, Json.member "params" j with
  | Some (`String m), Some params
    when List.mem
           [ "thread/start"; "thread/resume"; "turn/start" ]
           m
           ~equal:String.equal ->
    let field k = Option.map (Json.member k params) ~f:Json.to_string in
    printf
      "%s: approvalPolicy %s, sandbox %s\n"
      m
      (Option.value (field "approvalPolicy") ~default:"-")
      (Option.value
         (Option.first_some (field "sandbox") (field "sandboxPolicy"))
         ~default:"-")
  | _ -> ()
;;

let no_sandbox ~cwd =
  { (Adapter.Start.default ~cwd) with
    no_sandbox = Some "laptop (no user namespaces)"
  }
;;

let%expect_test
    "codex where its sandbox cannot run: none, said once, prigh still asks"
  =
  with_composer
    ~approval:Ask
    ~start:no_sandbox
    ~adapter:(module Codex_adapter)
    ~peer:(fun _ ->
      codex_replay "tool_call_read_only_on_request" ~on_request:print_policy)
  @@ fun _ agent _ ->
  Agent.subscribe agent ~f:(function
    | Loop (Tool_confirm { call_id; name; _ }) ->
      printf "confirm %s\n" name;
      Or_error.ok_exn (Agent.respond_confirm agent ~call_id Allow)
    | _ -> ());
  prompt agent "run it";
  [%expect
    {|
    user: "[Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]\n\nrun it"
    spawn #1: codex app-server
    thread/start: approvalPolicy "untrusted", sandbox "danger-full-access"
    notice: Codex's sandbox cannot run on laptop (no user namespaces): it runs without it, and prigh still asks before its commands and edits as the session's mode says
    turn/start: approvalPolicy "untrusted", sandbox {"type":"dangerFullAccess"}
    assistant (codex/gpt-6-sol, Tool_use): "I\226\128\153ll write the exact text to the file with a local shell command."
    tool_start bash {"command":"printf '%s' 'codex app-server approval fixture' > .codex-probe-write-action.txt"}
    confirm bash
    tool_end bash -> ""
    assistant (codex/gpt-6-sol, End_turn): "Created or overwrote `.codex-probe-write-action.txt` with exactly `codex app-server approval fixture`."
    |}]
;;

let done_frames turn =
  [ sprintf
      {|{"method":"turn/started","params":{"threadId":"th","turn":{"id":"%s","items":[],"status":"inProgress"}}}|}
      turn
  ; sprintf
      {|{"method":"turn/completed","params":{"threadId":"th","turn":{"id":"%s","items":[],"status":"completed"}}}|}
      turn
  ]
;;

let%expect_test
    "codex without its sandbox: said in the first turn it matters, once"
  =
  with_composer
    ~approval:Auto
    ~start:no_sandbox
    ~adapter:(module Codex_adapter)
    ~peer:(fun n ->
      codex_turn ~on_request:print_policy (done_frames (sprintf "tu%d" n)))
  @@ fun _ agent _ ->
  prompt agent "in Auto";
  Agent.set_mode agent ~approval:Auto_edit;
  prompt agent "in Auto-edit";
  prompt agent "again";
  [%expect
    {|
    user: "[Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]\n\nin Auto"
    spawn #1: codex app-server
    thread/start: approvalPolicy "untrusted", sandbox "danger-full-access"
    codex got thread/start
    turn/start: approvalPolicy "untrusted", sandbox {"type":"dangerFullAccess"}
    user: "in Auto-edit"
    spawn #2: codex app-server
    thread/resume: approvalPolicy "untrusted", sandbox "danger-full-access"
    codex got thread/resume
    notice: Codex's sandbox cannot run on laptop (no user namespaces): it runs without it, and prigh still asks before its commands and edits as the session's mode says
    turn/start: approvalPolicy "untrusted", sandbox {"type":"dangerFullAccess"}
    user: "again"
    spawn #3: codex app-server
    thread/resume: approvalPolicy "untrusted", sandbox "danger-full-access"
    codex got thread/resume
    turn/start: approvalPolicy "untrusted", sandbox {"type":"dangerFullAccess"}
    |}]
;;

let%expect_test "codex where its sandbox runs: the mode's" =
  with_composer
    ~approval:Auto_edit
    ~adapter:(module Codex_adapter)
    ~peer:(fun _ -> codex_turn ~on_request:print_policy (done_frames "tu"))
  @@ fun _ agent _ ->
  prompt agent "go";
  [%expect
    {|
    user: "[Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]\n\ngo"
    spawn #1: codex app-server
    thread/start: approvalPolicy "untrusted", sandbox "workspace-write"
    codex got thread/start
    turn/start: approvalPolicy "untrusted", sandbox {"type":"workspaceWrite"}
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
    assistant (codex/default, (Error"Codex is not installed on laptop: install it (npm i -g @openai/codex), then try again")): ""
    |}]
;;

(* Codex taking prigh's login ([refuse]: one too old for it), asking for a
   fresh token in the turn, and reporting usage. *)
let codex_on_login ~refuse (p : Line_transport.Peer.t) =
  let field k j = Option.bind (Json.member k j) ~f:Json.string in
  let rec until meth =
    match Line_transport.Peer.read_line p with
    | None -> None
    | Some line ->
      let j = Json.parse line |> Or_error.ok_exn in
      if Option.equal String.equal (field "method" j) (Some meth)
      then Some j
      else until meth
  in
  let write s = Line_transport.Peer.write_line p s in
  let reply j result =
    write
      (Json.to_string
         (`Object
             [ "id", Option.value_exn (Json.member "id" j)
             ; "result", Json.parse result |> Or_error.ok_exn
             ]))
  in
  let tokens j =
    let params = Option.value_exn (Json.member "params" j) in
    sprintf
      "%s %s %s"
      (Option.value (field "accessToken" params) ~default:"-")
      (Option.value (field "chatgptAccountId" params) ~default:"-")
      (match Json.member "chatgptPlanType" params with
       | Some (`String p) -> p
       | _ -> "null")
  in
  Option.iter (until "initialize") ~f:(fun j ->
    reply j {|{"userAgent":"prigh/0.120.0 (Linux; test)"}|});
  let rec turns () =
    match Line_transport.Peer.read_line p with
    | None -> ()
    | Some line ->
      let j = Json.parse line |> Or_error.ok_exn in
      (match field "method" j with
       | Some "account/login/start" ->
         printf "codex login: %s\n" (tokens j);
         if refuse
         then
           write
             (sprintf
                {|{"id":%s,"error":{"code":-32600,"message":"unknown variant `chatgptAuthTokens`"}}|}
                (Json.to_string (Option.value_exn (Json.member "id" j))))
         else reply j {|{"type":"chatgptAuthTokens"}|}
       | Some "thread/start" ->
         reply j {|{"thread":{"id":"t1"},"model":"gpt-5"}|}
       | Some "turn/start" ->
         reply j {|{"turn":{"id":"u1"}}|};
         write
           {|{"method":"turn/started","params":{"threadId":"t1","turn":{"id":"u1"}}}|};
         write
           {|{"method":"account/chatgptAuthTokens/refresh","id":0,"params":{"reason":"unauthorized","previousAccountId":"acct-1"}}|};
         (match Line_transport.Peer.read_line p with
          | Some answer -> printf "codex refreshed: %s\n" answer
          | None -> ());
         write
           {|{"method":"item/completed","params":{"item":{"type":"agentMessage","id":"m1","text":"ok"},"threadId":"t1","turnId":"u1"}}|};
         write
           {|{"method":"thread/tokenUsage/updated","params":{"threadId":"t1","turnId":"u1","tokenUsage":{"total":{"totalTokens":12,"inputTokens":10,"cachedInputTokens":0,"outputTokens":2,"reasoningOutputTokens":0},"last":{"totalTokens":12,"inputTokens":10,"cachedInputTokens":0,"outputTokens":2,"reasoningOutputTokens":0},"modelContextWindow":1000}}}|};
         write
           {|{"method":"turn/completed","params":{"threadId":"t1","turn":{"id":"u1","status":"completed"}}}|}
       | _ -> ());
      turns ()
  in
  turns ()
;;

(* prigh's login [work]: its tokens [tok-N], the first about to expire. *)
let prigh_login ?(fail = false) () =
  let n = ref 0 in
  Cli_credential.create
    ~provider:Openai_codex
    ~account:(Own (Account_id.of_string "work" |> Or_error.ok_exn))
    ~label:"work"
    ~fetch:(fun ?refused ?valid_for_ms:_ () ->
      incr n;
      printf
        "prigh fetches a token%s\n"
        (Option.value_map refused ~default:"" ~f:(sprintf ", refused %s"));
      if fail
      then Or_error.error_string "refreshing failed: /login openai-codex"
      else
        Ok
          { Provider_auth.Resolved.token = sprintf "tok-%d" !n
          ; method_ = Oauth
          ; account_id = Some "acct-1"
          ; source = "oauth"
          ; account = None
          ; expires_ms =
              Some (Credential.now_ms () + if !n = 1 then 60_000 else 3_600_000)
          })
    ~record:(fun usage ~cost_usd ->
      printf
        "recorded on work: %s, $%.2f\n"
        (Sexp.to_string [%sexp (usage : Usage.t)])
        cost_usd)
    ()
;;

let with_login ?fail () ~cwd =
  { (Adapter.Start.default ~cwd) with login = Some (prigh_login ?fail ()) }
;;

let%expect_test "codex on prigh's login: over stdin, refreshed, recorded" =
  with_composer
    ~adapter:(module Codex_adapter)
    ~start:(with_login ())
    ~peer:(fun _ -> codex_on_login ~refuse:false)
  @@ fun _ agent _ ->
  prompt agent "hi";
  [%expect
    {|
    user: "[Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]\n\nhi"
    spawn #1: codex app-server
    prigh fetches a token
    codex login: tok-1 acct-1 null
    prigh fetches a token
    codex login: tok-2 acct-1 null
    prigh fetches a token, refused tok-2
    codex refreshed: {"id":0,"result":{"accessToken":"tok-3","chatgptAccountId":"acct-1","chatgptPlanType":null}}
    recorded on work: ((input 10)(output 2)(cache_read 0)), $0.00
    assistant (codex/gpt-5, End_turn): "ok"
    |}]
;;

let%expect_test "codex without the mode, or no token: its own login, saying so" =
  with_composer
    ~adapter:(module Codex_adapter)
    ~start:(with_login ())
    ~peer:(fun _ -> codex_on_login ~refuse:true)
  @@ fun _ agent _ ->
  prompt agent "hi";
  [%expect
    {|
    user: "[Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]\n\nhi"
    spawn #1: codex app-server
    prigh fetches a token
    codex login: tok-1 acct-1 null
    notice: Codex 0.120.0 did not take prigh's ChatGPT login "work" (unknown variant `chatgptAuthTokens`): this session runs on Codex's own login on its tool host (codex login there). Update Codex there (npm install -g @openai/codex) to run on prigh's, or choose Codex's own login in Settings → Engines.
    codex refreshed: {"id":0,"error":{"code":-32603,"message":"prigh handed this Codex no login: it runs on its own"}}
    assistant (codex/gpt-5, End_turn): "ok"
    |}];
  with_composer
    ~adapter:(module Codex_adapter)
    ~start:(with_login ~fail:true ())
    ~peer:(fun _ -> codex_on_login ~refuse:false)
  @@ fun _ agent _ ->
  prompt agent "hi";
  [%expect
    {|
    user: "[Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]\n\nhi"
    spawn #1: codex app-server
    prigh fetches a token
    notice: prigh's ChatGPT login "work" has no token for Codex (refreshing failed: /login openai-codex): this session runs on Codex's own login on its tool host (codex login there).
    codex refreshed: {"id":0,"error":{"code":-32603,"message":"prigh handed this Codex no login: it runs on its own"}}
    assistant (codex/gpt-5, End_turn): "ok"
    |}]
;;

(* Claude Code on prigh's login: it answers [initialize] saying which
   [apiKeySource] it uses, then each prompt with "ok", or a 401 when
   [refuse]. *)
let claude_on_login
      ?(api_key_source = "none")
      ~refuse
      ()
      (p : Line_transport.Peer.t)
  =
  let read () =
    Option.map (Line_transport.Peer.read_line p) ~f:(fun line ->
      Json.parse line |> Or_error.ok_exn)
  in
  let write = Line_transport.Peer.write_line p in
  let request_id j = Option.bind (Json.member "request_id" j) ~f:Json.string in
  (match read () with
   | Some init ->
     write
       (sprintf
          {|{"type":"control_response","response":{"subtype":"success","request_id":"%s","response":{"claude_code_version":"2.1.330","models":[],"commands":[],"account":{"tokenSource":"CLAUDE_CODE_OAUTH_TOKEN","apiKeySource":"%s"}}}}|}
          (Option.value (request_id init) ~default:"")
          api_key_source)
   | None -> ());
  let rec turns () =
    match read () with
    | None -> ()
    | Some j ->
      (match Option.bind (Json.member "type" j) ~f:Json.string with
       | Some "user" ->
         write
           {|{"type":"system","subtype":"init","apiKeySource":"none","model":"claude-sonnet-4-6","session_id":"s1"}|};
         if refuse
         then
           write
             {|{"type":"result","subtype":"success","is_error":true,"api_error_status":401,"result":"OAuth token has expired","session_id":"s1","usage":{"input_tokens":0,"output_tokens":0}}|}
         else (
           write
             {|{"type":"assistant","message":{"model":"claude-sonnet-4-6","id":"m1","type":"message","role":"assistant","content":[{"type":"text","text":"ok"}],"usage":{"input_tokens":3,"output_tokens":1}},"session_id":"s1","uuid":"u1"}|};
           write
             {|{"type":"result","subtype":"success","is_error":false,"result":"ok","session_id":"s1","total_cost_usd":0.01,"usage":{"input_tokens":3,"output_tokens":1}}|})
       | _ -> ());
      turns ()
  in
  turns ()
;;

(* prigh's Claude login [max]: [tok-N], expiring in [expires_min.(N-1)]
   minutes. *)
let claude_login ?(fail = false) ~expires_min () =
  let n = ref 0 in
  Cli_credential.create
    ~provider:Anthropic
    ~account:(Own (Account_id.of_string "max" |> Or_error.ok_exn))
    ~label:"max"
    ~fetch:(fun ?refused ?valid_for_ms:_ () ->
      incr n;
      printf
        "prigh fetches a token%s\n"
        (Option.value_map refused ~default:"" ~f:(sprintf ", refused %s"));
      if fail
      then Or_error.error_string "refreshing failed: /login anthropic"
      else
        Ok
          { Provider_auth.Resolved.token = sprintf "tok-%d" !n
          ; method_ = Oauth
          ; account_id = None
          ; source = "oauth"
          ; account = None
          ; expires_ms =
              Some
                (Credential.now_ms ()
                 + (60_000 * List.nth_exn expires_min (!n - 1)))
          })
    ~record:(fun usage ~cost_usd ->
      printf
        "recorded on max: %s, $%.2f\n"
        (Sexp.to_string [%sexp (usage : Usage.t)])
        cost_usd)
    ()
;;

let print_launch (launch : Adapter.Launch.t) =
  let auth =
    List.filter launch.env_set ~f:(fun (k, _) ->
      String.is_prefix k ~prefix:"ANTHROPIC"
      || String.equal k "CLAUDE_CODE_OAUTH_TOKEN")
  in
  printf
    "  env %s,%s unset %s%s\n"
    (Sexp.to_string [%sexp (auth : (string * string) list)])
    (Option.value_map launch.secret ~default:"" ~f:(fun s ->
       sprintf " %s on %s (else %s)," s.value s.fd_env s.env))
    (String.concat ~sep:" " launch.env_unset)
    (Option.value_map
       (List.find launch.argv ~f:(String.is_prefix ~prefix:"--resume="))
       ~default:""
       ~f:(fun r -> ", " ^ r))
;;

let%expect_test "claude on prigh's login: on a descriptor, renewed, refused" =
  let credential = claude_login ~expires_min:[ 30; 120; 120 ] () in
  with_composer
    ~adapter:(module Claude_adapter)
    ~on_launch:print_launch
    ~start:(fun ~cwd ->
      { (Adapter.Start.default ~cwd) with login = Some credential })
    ~peer:(fun n -> claude_on_login ~refuse:(n = 2 || n = 3) ())
  @@ fun _ agent _ ->
  prompt agent "hi";
  [%expect
    {|
    user: "[Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]\n\nhi"
    prigh fetches a token
    spawn #1: claude --output-format
      env (), tok-1 on CLAUDE_CODE_OAUTH_TOKEN_FILE_DESCRIPTOR (else CLAUDE_CODE_OAUTH_TOKEN), unset CLAUDECODE ANTHROPIC_API_KEY ANTHROPIC_AUTH_TOKEN CLAUDE_CODE_OAUTH_TOKEN
    recorded on max: ((input 3)(output 1)(cache_read 0)), $0.01
    assistant (claude/claude-sonnet-4-6, End_turn): "ok"
    |}];
  (* The token expires within the hour: a new process, on the thread. *)
  prompt agent "again";
  [%expect
    {|
    user: "again"
    prigh fetches a token
    spawn #2: claude --output-format
      env (), tok-2 on CLAUDE_CODE_OAUTH_TOKEN_FILE_DESCRIPTOR (else CLAUDE_CODE_OAUTH_TOKEN), unset CLAUDECODE ANTHROPIC_API_KEY ANTHROPIC_AUTH_TOKEN CLAUDE_CODE_OAUTH_TOKEN, --resume=s1
    assistant (claude/claude-sonnet-4-6, (Error"OAuth token has expired: Anthropic refused prigh's login \"max\": prigh refreshes it and restarts Claude Code on your next message (/login anthropic if it is refused again)")): ""
    |}];
  (* Refused: a newer token; refused again: Claude Code's own login. *)
  prompt agent "and again";
  [%expect
    {|
    user: "and again"
    prigh fetches a token, refused tok-2
    spawn #3: claude --output-format
      env (), tok-3 on CLAUDE_CODE_OAUTH_TOKEN_FILE_DESCRIPTOR (else CLAUDE_CODE_OAUTH_TOKEN), unset CLAUDECODE ANTHROPIC_API_KEY ANTHROPIC_AUTH_TOKEN CLAUDE_CODE_OAUTH_TOKEN, --resume=s1
    assistant (claude/claude-sonnet-4-6, (Error"OAuth token has expired: Anthropic refused prigh's login \"max\": prigh refreshes it and restarts Claude Code on your next message (/login anthropic if it is refused again)")): ""
    |}];
  prompt agent "once more";
  [%expect
    {|
    user: "once more"
    notice: Claude Code was refused prigh's Anthropic login "max" again, after a refresh: this session runs on Claude Code's own login on its tool host from now on (/login anthropic logs in again, for new sessions).
    spawn #4: claude --output-format
      env (), unset CLAUDECODE, --resume=s1
    assistant (claude/claude-sonnet-4-6, End_turn): "ok"
    |}]
;;

let%expect_test "claude on an old tool host: the token in its env, said once" =
  with_composer
    ~adapter:(module Claude_adapter)
    ~transport_notice:"laptop's prigh is too old (the token is in its env)"
    ~start:(fun ~cwd ->
      { (Adapter.Start.default ~cwd) with
        login = Some (claude_login ~expires_min:[ 30; 120 ] ())
      })
    ~peer:(fun _ -> claude_on_login ~refuse:false ())
  @@ fun _ agent _ ->
  prompt agent "hi";
  prompt agent "again";
  [%expect
    {|
    user: "[Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]\n\nhi"
    prigh fetches a token
    spawn #1: claude --output-format
    notice: laptop's prigh is too old (the token is in its env)
    recorded on max: ((input 3)(output 1)(cache_read 0)), $0.01
    assistant (claude/claude-sonnet-4-6, End_turn): "ok"
    user: "again"
    prigh fetches a token
    spawn #2: claude --output-format
    recorded on max: ((input 3)(output 1)(cache_read 0)), $0.00
    assistant (claude/claude-sonnet-4-6, End_turn): "ok"
    |}]
;;

let%expect_test "claude: no token, or an apiKeyHelper: its own login, saying so"
  =
  with_composer
    ~adapter:(module Claude_adapter)
    ~on_launch:print_launch
    ~start:(fun ~cwd ->
      { (Adapter.Start.default ~cwd) with
        login = Some (claude_login ~fail:true ~expires_min:[] ())
      })
    ~peer:(fun _ -> claude_on_login ~refuse:false ())
  @@ fun _ agent _ ->
  prompt agent "hi";
  [%expect
    {|
    user: "[Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]\n\nhi"
    prigh fetches a token
    spawn #1: claude --output-format
      env (), unset CLAUDECODE
    notice: prigh's Anthropic login "max" has no token for Claude Code (refreshing failed: /login anthropic): this session runs on Claude Code's own login on its tool host (claude /login there).
    assistant (claude/claude-sonnet-4-6, End_turn): "ok"
    |}];
  with_composer
    ~adapter:(module Claude_adapter)
    ~on_launch:print_launch
    ~start:(fun ~cwd ->
      { (Adapter.Start.default ~cwd) with
        login = Some (claude_login ~expires_min:[ 120 ] ())
      })
    ~peer:(fun _ ->
      claude_on_login ~api_key_source:"apiKeyHelper" ~refuse:false ())
  @@ fun _ agent _ ->
  prompt agent "hi";
  [%expect
    {|
    user: "[Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]\n\nhi"
    prigh fetches a token
    spawn #1: claude --output-format
      env (), tok-1 on CLAUDE_CODE_OAUTH_TOKEN_FILE_DESCRIPTOR (else CLAUDE_CODE_OAUTH_TOKEN), unset CLAUDECODE ANTHROPIC_API_KEY ANTHROPIC_AUTH_TOKEN CLAUDE_CODE_OAUTH_TOKEN
    notice: Claude Code uses its apiKeyHelper instead of prigh's Anthropic login "max", which it prefers to a token handed to it: remove apiKeyHelper from its settings (~/.claude/settings.json on its tool host), or choose Claude Code's own login in Settings → Engines.
    assistant (claude/claude-sonnet-4-6, End_turn): "ok"
    |}]
;;

let%expect_test "codex: steer wakes immediately without polling delay" =
  let in_turn, mark_in_turn = Eio.Promise.create () in
  let steer_recorded, mark_steer_recorded = Eio.Promise.create () in
  let steered_at = ref None in
  with_composer
    ~adapter:(module Codex_adapter)
    ~peer:(fun _ p ->
      let read () =
        Option.map (Line_transport.Peer.read_line p) ~f:(fun l ->
          Json.parse l |> Or_error.ok_exn)
      in
      let write j = Line_transport.Peer.write_line p (Json.to_string j) in
      let reply j result =
        write
          (`Object
              [ "id", Option.value_exn (Json.member "id" j); "result", result ])
      in
      let rec loop () =
        match read () with
        | None -> ()
        | Some j ->
          (match Option.bind (Json.member "method" j) ~f:Json.string with
           | Some "initialize" ->
             reply j (`Object [ "userAgent", `String "codex/0.166.0" ])
           | Some "thread/start" ->
             reply
               j
               (`Object
                   [ "thread", `Object [ "id", `String "th-1" ]
                   ; "model", `String "gpt-5"
                   ])
           | Some "turn/start" ->
             reply
               j
               (`Object
                   [ ( "turn"
                     , `Object
                         [ "id", `String "tu-1"
                         ; "status", `String "inProgress"
                         ] )
                   ]);
             write
               (`Object
                   [ "method", `String "turn/started"
                   ; ( "params"
                     , `Object
                         [ "threadId", `String "th-1"
                         ; "turn", `Object [ "id", `String "tu-1" ]
                         ] )
                   ]);
             Eio.Promise.resolve mark_in_turn ()
           | Some "turn/steer" ->
             steered_at := Some (Time_float.now ());
             reply j (`Object [ "turnId", `String "tu-1" ]);
             Eio.Promise.await steer_recorded;
             write
               (`Object
                   [ "method", `String "item/completed"
                   ; ( "params"
                     , `Object
                         [ "threadId", `String "th-1"
                         ; ( "item"
                           , `Object
                               [ "type", `String "agentMessage"
                               ; "id", `String "m1"
                               ; "text", `String "Steered!"
                               ] )
                         ] )
                   ]);
             write
               (`Object
                   [ "method", `String "turn/completed"
                   ; ( "params"
                     , `Object
                         [ "threadId", `String "th-1"
                         ; ( "turn"
                           , `Object
                               [ "id", `String "tu-1"
                               ; "status", `String "completed"
                               ] )
                         ] )
                   ])
           | Some _ | None -> ());
          loop ()
      in
      loop ())
  @@ fun _ agent _ ->
  Agent.subscribe agent ~f:(function
    | Loop (Message_end (User u)) when String.equal u.text "pivot now" ->
      ignore (Eio.Promise.try_resolve mark_steer_recorded () : bool)
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "start");
  Eio.Promise.await in_turn;
  let sent_at = Time_float.now () in
  Or_error.ok_exn (Agent.steer agent "pivot now");
  Agent.wait_idle agent;
  let fast =
    match !steered_at with
    | Some at ->
      Float.(Time_float.Span.to_ms (Time_float.diff at sent_at) < 150.)
    | None -> false
  in
  printf "steered_fast=%b\n" fast;
  [%expect
    {|
    user: "[Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]\n\nstart"
    spawn #1: codex app-server
    user: "pivot now"
    assistant (codex/gpt-5, End_turn): "Steered!"
    steered_fast=true
    |}]
;;

let%expect_test
    "codex: thread/name/updated sets native session description, preserving \
     manual name"
  =
  let send_between, trigger_between = Eio.Promise.create () in
  let between_sent, mark_between_sent = Eio.Promise.create () in
  let send_after_manual, trigger_after_manual = Eio.Promise.create () in
  let after_manual_sent, mark_after_manual_sent = Eio.Promise.create () in
  with_composer
    ~adapter:(module Codex_adapter)
    ~peer:(fun _ p ->
      let read () =
        Option.map (Line_transport.Peer.read_line p) ~f:(fun l ->
          Json.parse l |> Or_error.ok_exn)
      in
      let write j = Line_transport.Peer.write_line p (Json.to_string j) in
      let reply j result =
        write
          (`Object
              [ "id", Option.value_exn (Json.member "id" j); "result", result ])
      in
      let notify_title title =
        write
          (`Object
              [ "method", `String "thread/name/updated"
              ; ( "params"
                , `Object
                    [ "threadId", `String "th-1"; "threadName", `String title ]
                )
              ])
      in
      let rec loop () =
        match read () with
        | None -> ()
        | Some j ->
          (match Option.bind (Json.member "method" j) ~f:Json.string with
           | Some "initialize" ->
             reply j (`Object [ "userAgent", `String "codex/0.166.0" ])
           | Some "thread/start" ->
             reply
               j
               (`Object
                   [ "thread", `Object [ "id", `String "th-1" ]
                   ; "model", `String "gpt-5"
                   ])
           | Some "turn/start" ->
             reply
               j
               (`Object
                   [ ( "turn"
                     , `Object
                         [ "id", `String "tu-1"
                         ; "status", `String "inProgress"
                         ] )
                   ]);
             write
               (`Object
                   [ "method", `String "turn/started"
                   ; ( "params"
                     , `Object
                         [ "threadId", `String "th-1"
                         ; "turn", `Object [ "id", `String "tu-1" ]
                         ] )
                   ]);
             notify_title "Initial Codex title";
             write
               (`Object
                   [ "method", `String "item/completed"
                   ; ( "params"
                     , `Object
                         [ "threadId", `String "th-1"
                         ; ( "item"
                           , `Object
                               [ "type", `String "agentMessage"
                               ; "id", `String "m1"
                               ; "text", `String "Done."
                               ] )
                         ] )
                   ]);
             write
               (`Object
                   [ "method", `String "turn/completed"
                   ; ( "params"
                     , `Object
                         [ "threadId", `String "th-1"
                         ; ( "turn"
                           , `Object
                               [ "id", `String "tu-1"
                               ; "status", `String "completed"
                               ] )
                         ] )
                   ]);
             Eio.Promise.await send_between;
             notify_title "Refined Codex title";
             Eio.Promise.resolve mark_between_sent ();
             Eio.Promise.await send_after_manual;
             notify_title "Should not overwrite manual";
             Eio.Promise.resolve mark_after_manual_sent ()
           | Some _ | None -> ());
          loop ()
      in
      loop ())
  @@ fun _ agent _ ->
  let title_updated = Eio.Condition.create () in
  Agent.subscribe agent ~f:(function
    | State_changed _ -> Eio.Condition.broadcast title_updated
    | _ -> ());
  prompt agent "fix bug";
  let show label =
    let s = Agent.session agent in
    printf
      "%s: name=%s desc=%s native=%b wanted=%b\n"
      label
      (Option.value (Session.name s) ~default:"-")
      (Option.value (Session.description s) ~default:"-")
      (Session_description.has_native s)
      (Session_description.wanted s)
  in
  show "in-turn";
  Eio.Promise.resolve trigger_between ();
  Eio.Promise.await between_sent;
  while
    not
      (Option.equal
         String.equal
         (Session.description (Agent.session agent))
         (Some "Refined Codex title"))
  do
    Eio.Condition.await_no_mutex title_updated
  done;
  show "between-turns";
  ignore
    (Session.set_name (Agent.session agent) ~name:"My manual name"
     : Session.Entry.t);
  Eio.Promise.resolve trigger_after_manual ();
  Eio.Promise.await after_manual_sent;
  Eio.Fiber.yield ();
  Eio.Fiber.yield ();
  show "after-manual";
  [%expect
    {|
    user: "[Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]\n\nfix bug"
    spawn #1: codex app-server
    assistant (codex/gpt-5, End_turn): "Done."
    in-turn: name=- desc=Initial Codex title native=true wanted=false
    between-turns: name=- desc=Refined Codex title native=true wanted=false
    after-manual: name=My manual name desc=Refined Codex title native=true wanted=false
    |}]
;;

let%expect_test
    "claude: injected prigh MCP tools use real tool spec and are never \
     approved twice"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let fake_worktree =
    { Tool.spec = (Tool_worktree.create ()).spec
    ; run =
        (fun _ args -> Tool_result.ok ("ran worktree " ^ Json.to_string args))
    }
  in
  let confirms = ref 0 in
  let peer (p : Line_transport.Peer.t) =
    let read () =
      Option.map (Line_transport.Peer.read_line p) ~f:(fun l ->
        Json.parse l |> Or_error.ok_exn)
    in
    let req_id j = Option.bind (Json.member "request_id" j) ~f:Json.string in
    (match read () with
     | Some init ->
       Line_transport.Peer.write_line
         p
         (sprintf
            {|{"type":"control_response","response":{"subtype":"success","request_id":"%s","response":{"claude_code_version":"2.1.268","models":[],"commands":[]}}}|}
            (Option.value (req_id init) ~default:""))
     | None -> ());
    ignore (read () : Json.t option);
    Line_transport.Peer.write_line
      p
      {|{"type":"system","subtype":"init","session_id":"s1","model":"claude-sonnet-4-6"}|};
    let send_and_expect label line =
      Line_transport.Peer.write_line p line;
      match read () with
      | Some reply -> printf "%s -> %s\n" label (Json.to_string reply)
      | None -> printf "%s -> EOF\n" label
    in
    (* 1. worktree(action=list): read-only, must NOT ask in Ask mode *)
    send_and_expect
      "can_use_tool list"
      {|{"type":"control_request","request_id":"perm-1","request":{"subtype":"can_use_tool","tool_name":"mcp__prigh__worktree","tool_use_id":"tu-1","input":{"action":"list"}}}|};
    send_and_expect
      "mcp list"
      {|{"type":"control_request","request_id":"mcp-1","request":{"subtype":"mcp_message","server_name":"prigh","message":{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"worktree","arguments":{"action":"list"}}}}}|};
    (* 2. worktree(action=remove): destructive, must ask ONCE (not twice) *)
    send_and_expect
      "can_use_tool remove"
      {|{"type":"control_request","request_id":"perm-2","request":{"subtype":"can_use_tool","tool_name":"mcp__prigh__worktree","tool_use_id":"tu-2","input":{"action":"remove","path":"/wt/a"}}}|};
    send_and_expect
      "mcp remove"
      {|{"type":"control_request","request_id":"mcp-2","request":{"subtype":"mcp_message","server_name":"prigh","message":{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"worktree","arguments":{"action":"remove","path":"/wt/a"}}}}}|};
    Line_transport.Peer.write_line
      p
      {|{"type":"result","subtype":"success","is_error":false,"result":"done","session_id":"s1","usage":{"input_tokens":1,"output_tokens":1}}|}
  in
  let composer =
    Composer_runner.create
      ~sw
      ~adapter:(module Claude_adapter)
      ~spawn:(fun _ -> Ok (Line_transport.Peer.spawn ~sw peer))
      ~interrupt_grace:(Time_float.Span.of_sec 0.5)
      ()
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [])
      ~tools:(fake_worktree :: Tools.interaction)
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~runner:(Composer_runner.runner composer)
      ~cwd:t.dir
      ()
  in
  Agent.set_engine agent { engine = Composer; adapter = Some Claude };
  Agent.set_mode agent ~approval:Ask;
  Agent.subscribe agent ~f:(function
    | Loop (Tool_confirm { call_id; agent_id; name; summary; _ }) ->
      incr confirms;
      printf "confirm #%d %s %s: %s\n" !confirms call_id name summary;
      Or_error.ok_exn (Agent.respond_confirm ?agent_id agent ~call_id Allow)
    | _ -> ());
  prompt agent "tidy worktrees";
  Composer_runner.close composer;
  printf "total confirms = %d\n" !confirms;
  [%expect
    {|
    can_use_tool list -> {"type":"control_response","response":{"subtype":"success","request_id":"perm-1","response":{"behavior":"allow","updatedInput":{"action":"list"}}}}
    mcp list -> {"type":"control_response","response":{"subtype":"success","request_id":"mcp-1","response":{"mcp_response":{"jsonrpc":"2.0","id":1,"result":{"content":[{"type":"text","text":"ran worktree {\"action\":\"list\"}"}],"isError":false}}}}}
    confirm #1 tu-2 worktree: remove /wt/a
    can_use_tool remove -> {"type":"control_response","response":{"subtype":"success","request_id":"perm-2","response":{"behavior":"allow","updatedInput":{"action":"remove","path":"/wt/a"}}}}
    mcp remove -> {"type":"control_response","response":{"subtype":"success","request_id":"mcp-2","response":{"mcp_response":{"jsonrpc":"2.0","id":2,"result":{"content":[{"type":"text","text":"ran worktree {\"action\":\"remove\",\"path\":\"/wt/a\"}"}],"isError":false}}}}}
    total confirms = 1
    |}]
;;
