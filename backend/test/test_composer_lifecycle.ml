open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

(* The composer's process lifecycle: turns the harness runs of its own,
   ends that come late, stops, and what a turn leaves open when it ends.
   Every wait is on a condition the peer or the test signals. *)

let notice_ (e : Adapter_event.t) =
  match e with
  | Notice text -> printf "notice: %s\n" text
  | Turn_end { stop; _ } ->
    printf
      "turn_end %s\n"
      (Sexp.to_string [%sexp (stop : Adapter_event.Stop.t)])
  | e -> printf "%s\n" (Sexp.to_string [%sexp (e : Adapter_event.t)])
;;

let with_driver ?interrupt_grace ?request_timeout f =
  Eio_main.run
  @@ fun _ ->
  Eio.Switch.run
  @@ fun sw ->
  let transport =
    Line_transport.Peer.spawn ~sw (fun _ -> Eio.Fiber.await_cancel ())
  in
  let driver =
    Adapter.Driver.create
      ~sw
      ~name:"Codex"
      ?interrupt_grace
      ?request_timeout
      transport
  in
  Adapter.Driver.run driver ~handle_line:ignore ~on_exit:ignore;
  f driver;
  Adapter.Driver.stop driver
;;

let text s : Adapter_event.t = Text_done { item = "m"; text = s }
let ended : Adapter_event.t = Turn_end { turn_id = None; stop = Done }

let%expect_test "a turn the harness runs of its own never reaches ours" =
  with_driver
  @@ fun d ->
  Adapter.Driver.on_orphan_request d (fun e ->
    printf "refused: %s\n" (Sexp.to_string [%sexp (e : Adapter_event.t)]));
  (* Our turn. *)
  Adapter.Driver.begin_turn d;
  Adapter.Driver.push d (text "ours");
  Adapter.Driver.push d ended;
  notice_ (Adapter.Driver.next_event d);
  notice_ (Adapter.Driver.next_event d);
  (* Then one of its own, which asks something and says something. *)
  Adapter.Driver.push d (Turn_started { turn_id = "theirs" });
  Adapter.Driver.push d (User_question { request_id = "q1"; questions = [] });
  Adapter.Driver.push d (text "a background task finished");
  Adapter.Driver.push
    d
    (Usage
       { usage = Usage.zero
       ; cost_usd = None
       ; context_tokens = None
       ; context_window = None
       });
  (* Our next turn waits for it to end; its end is a notice, not ours. *)
  Eio.Fiber.both
    (fun () ->
       Adapter.Driver.await_idle d;
       print_endline "idle";
       Adapter.Driver.begin_turn d;
       Adapter.Driver.push d (text "second");
       Adapter.Driver.push d ended)
    (fun () ->
       print_endline "its turn ends";
       Adapter.Driver.push d ended);
  for _ = 1 to 5 do
    notice_ (Adapter.Driver.next_event d)
  done;
  [%expect
    {|
    (Text_done(item m)(text ours))
    turn_end Done
    refused: (User_question(request_id q1)(questions()))
    its turn ends
    idle
    (Usage((usage((input 0)(output 0)(cache_read 0)))(cost_usd())(context_tokens())(context_window())))
    notice: Codex is finishing a turn of its own: your message waits for it
    notice: Codex ran a turn of its own between your messages: a background task finished
    (Text_done(item m)(text second))
    turn_end Done
    |}]
;;

let%expect_test "an end that comes after the turn was ended for it is dropped" =
  with_driver ~interrupt_grace:(Time_float.Span.of_sec 0.)
  @@ fun d ->
  Adapter.Driver.begin_turn d;
  Adapter.Driver.watch_interrupt d;
  notice_ (Adapter.Driver.next_event d);
  print_s
    [%message (Adapter.Driver.alive d : bool) (Adapter.Driver.in_turn d : bool)];
  (* The harness's own end, late: no turn's. *)
  Adapter.Driver.push d (Turn_end { turn_id = Some "late"; stop = Interrupted });
  Adapter.Driver.begin_turn d;
  Adapter.Driver.push d (text "next");
  notice_ (Adapter.Driver.next_event d);
  [%expect
    {|
    turn_end Interrupted
    (("Adapter.Driver.alive d" false) ("Adapter.Driver.in_turn d" false))
    (Text_done(item m)(text next))
    |}]
;;

let%expect_test "stopping the process ends its turn at once" =
  with_driver
  @@ fun d ->
  Adapter.Driver.begin_turn d;
  Adapter.Driver.stop d;
  notice_ (Adapter.Driver.next_event d);
  Adapter.Driver.write d "ignored: it is stopping";
  print_s [%message (Adapter.Driver.alive d : bool)];
  [%expect
    {|
    turn_end (Failed(kind(Crashed(stderr_tail"")))(message"Codex was stopped before its turn ended"))
    ("Adapter.Driver.alive d" false)
    |}]
;;

let%expect_test "a request with no answer fails, and is withdrawn" =
  with_driver ~request_timeout:(Time_float.Span.of_sec 0.001)
  @@ fun d ->
  print_s
    [%sexp
      (Adapter.Driver.request d "{}" ~matches:(fun (_ : int) -> true)
       : int Or_error.t)];
  (* A later answer goes nowhere. *)
  Adapter.Driver.answer d 1;
  [%expect {| (Error "no answer within 1ms") |}]
;;

(* A Codex peer: answers [initialize] (unless [hang_init]), the thread, and
   the turn, then plays [frames]; [signal] says how far it got. *)
let codex ?(hang_init = false) ~frames ~signal (p : Line_transport.Peer.t) =
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
  signal "initialize";
  if hang_init then Eio.Fiber.await_cancel ();
  reply init (`Object [ "userAgent", `String "codex/0.166.0" ]);
  let rec thread () =
    let j = next () in
    match Json.member "method" j with
    | Some (`String ("thread/start" | "thread/resume")) ->
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
  List.iter frames ~f:(Line_transport.Peer.write_line p);
  signal "turn";
  (* Then whatever comes, unanswered. *)
  let rec drain () =
    match Line_transport.Peer.read_line p with
    | Some _ -> drain ()
    | None -> ()
  in
  drain ()
;;

let with_codex
      ?request_timeout
      ?(wrap = Fn.id)
      ?(adapter = (module Codex_adapter : Adapter.S))
      ?(tools = Tools.interaction)
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
      ~spawn:(fun (_ : Adapter.Launch.t) ->
        incr launches;
        printf "spawn #%d\n" !launches;
        Ok (wrap (Line_transport.Peer.spawn ~sw (peer !launches))))
      ?request_timeout
      ()
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [])
      ~tools
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~runner:(Composer_runner.runner composer)
      ~cwd:t.dir
      ()
  in
  let (module A) = adapter in
  Agent.set_engine
    agent
    { engine = Composer
    ; adapter = Some (Or_error.ok_exn (Adapter_id.of_string A.id))
    };
  Agent.subscribe agent ~f:(function
    | Loop (Message_end (Assistant a)) ->
      printf
        "assistant: %s %S\n"
        (Sexp.to_string [%sexp (a.stop_reason : Stop_reason.t)])
        (Message.Assistant.text a)
    | Loop (Tool_confirm { name; _ }) -> printf "confirm %s\n" name
    | Loop (Tool_confirm_done _) -> print_endline "confirm done"
    | Notice n -> printf "notice: %s\n" n
    | _ -> ());
  f agent composer;
  Composer_runner.close composer
;;

(* [signal] resolves the promise named by what the peer reached. *)
let signals () =
  let table = String.Table.create () in
  let get name =
    Hashtbl.find_or_add table name ~default:(fun () -> Eio.Promise.create ())
  in
  ( (fun name -> fst (get name))
  , fun name ->
      let _, r = get name in
      ignore (Eio.Promise.try_resolve r () : bool) )
;;

let%expect_test "Esc while the harness starts ends the turn" =
  let reached, signal = signals () in
  with_codex ~peer:(fun n -> codex ~hang_init:(n = 1) ~frames:[] ~signal)
  @@ fun agent _ ->
  Or_error.ok_exn (Agent.prompt agent "hi");
  Eio.Promise.await (reached "initialize");
  ignore (Agent.abort agent : string list);
  Agent.wait_idle agent;
  print_s [%message "" ~running:((Agent.state agent).running : bool)];
  [%expect
    {|
    spawn #1
    (running false)
    |}]
;;

let%expect_test "a harness that does not answer its start fails the turn" =
  let reached, signal = signals () in
  ignore reached;
  with_codex ~request_timeout:(Time_float.Span.of_sec 0.001) ~peer:(fun _ ->
    codex ~hang_init:true ~frames:[] ~signal)
  @@ fun agent _ ->
  Or_error.ok_exn (Agent.prompt agent "hi");
  Agent.wait_idle agent;
  [%expect
    {|
    spawn #1
    assistant: (Error"Codex could not start: (\"Codex did not start\" \"no answer within 1ms\")") ""
    |}]
;;

let approval =
  {|{"method":"item/commandExecution/requestApproval","id":0,"params":{"threadId":"th","turnId":"tu","itemId":"c1","command":"rm -rf build","cwd":"/w"}}|}
;;

let%expect_test "stopping the harness mid-turn ends it and closes its dialog" =
  let reached, signal = signals () in
  with_codex ~peer:(fun _ ->
    codex
      ~hang_init:false
      ~signal
      ~frames:
        [ {|{"method":"turn/started","params":{"threadId":"th","turn":{"id":"tu","items":[],"status":"inProgress"}}}|}
        ; approval
        ])
  @@ fun agent composer ->
  Agent.set_mode agent ~approval:Ask;
  let confirming, confirm = Eio.Promise.create () in
  Agent.subscribe agent ~f:(function
    | Loop (Tool_confirm _) ->
      ignore (Eio.Promise.try_resolve confirm () : bool)
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "clean");
  Eio.Promise.await (reached "turn");
  Eio.Promise.await confirming;
  (* As when its host goes away. *)
  Composer_runner.close composer;
  Agent.wait_idle agent;
  print_s
    [%message
      ""
        ~pending:(List.length (Agent.pending_confirms agent) : int)
        ~running:((Agent.state agent).running : bool)];
  [%expect
    {|
    spawn #1
    confirm bash
    assistant: (Error"Codex was stopped before its turn ended") ""
    confirm done
    ((pending 0) (running false))
    |}]
;;

(* Claude Code announcing one command and asking to run another (a hook
   rewrote it): the user is asked about what would run. *)
let claude_rewritten (p : Line_transport.Peer.t) =
  let read () =
    Json.parse (Option.value_exn (Line_transport.Peer.read_line p))
    |> Or_error.ok_exn
  in
  let init = read () in
  Line_transport.Peer.write_line
    p
    (sprintf
       {|{"type":"control_response","response":{"subtype":"success","request_id":"%s","response":{"models":[],"commands":[]}}}|}
       (Option.value_exn
          (Option.bind (Json.member "request_id" init) ~f:Json.string)));
  ignore (read () : Json.t);
  List.iter
    ~f:(Line_transport.Peer.write_line p)
    [ {|{"type":"assistant","message":{"id":"m1","role":"assistant","content":[{"type":"tool_use","id":"tu1","name":"Bash","input":{"command":"ls"}}]},"session_id":"s","parent_tool_use_id":null}|}
    ; {|{"type":"control_request","request_id":"r1","request":{"subtype":"can_use_tool","tool_name":"Bash","input":{"command":"rm -rf build"},"tool_use_id":"tu1"}}|}
    ];
  printf "claude got: %s\n" (Json.to_string (read ()));
  Line_transport.Peer.write_line
    p
    {|{"type":"result","subtype":"success","is_error":false,"result":"","session_id":"s","total_cost_usd":0}|}
;;

let%expect_test "Claude's approval judges the command it asks to run" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let composer =
    Composer_runner.create
      ~sw
      ~adapter:(module Claude_adapter)
      ~spawn:(fun (_ : Adapter.Launch.t) ->
        Ok (Line_transport.Peer.spawn ~sw claude_rewritten))
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
  Agent.set_engine agent { engine = Composer; adapter = Some Claude };
  Agent.set_mode agent ~approval:Ask;
  Agent.subscribe agent ~f:(function
    | Loop (Tool_confirm { call_id; name; summary; _ }) ->
      printf "confirm %s: %s\n" name summary;
      Or_error.ok_exn
        (Agent.respond_confirm agent ~call_id (Deny (Some "not that")))
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "list");
  Agent.wait_idle agent;
  Composer_runner.close composer;
  [%expect
    {|
    confirm bash: rm -rf build
    claude got: {"type":"control_response","response":{"subtype":"success","request_id":"r1","response":{"behavior":"deny","message":"not that"}}}
    |}]
;;

let%expect_test "claude: an MCP call it gives up on is stopped and not answered"
  =
  Eio_main.run
  @@ fun _ ->
  Eio.Switch.run
  @@ fun sw ->
  let running, run = Eio.Promise.create () in
  let abandoned, abandon = Eio.Promise.create () in
  let finished, finish = Eio.Promise.create () in
  let injected =
    Injected_tools.create ~tools:[ Tool_render_ui.tool ] ~execute:(fun _ _ ->
      Eio.Promise.resolve run ();
      Fun.protect
        ~finally:(fun () -> Eio.Promise.resolve abandon ())
        (fun () -> Eio.Fiber.await_cancel ()))
  in
  let read p =
    Option.map (Line_transport.Peer.read_line p) ~f:(fun line ->
      Json.parse line |> Or_error.ok_exn)
  in
  let transport =
    Line_transport.Peer.spawn ~sw (fun p ->
      (match read p with
       | Some init ->
         Line_transport.Peer.write_line
           p
           (sprintf
              {|{"type":"control_response","response":{"subtype":"success","request_id":"%s","response":{"models":[],"commands":[]}}}|}
              (Option.value_exn
                 (Option.bind (Json.member "request_id" init) ~f:Json.string)))
       | None -> ());
      Line_transport.Peer.write_line
        p
        {|{"type":"control_request","request_id":"r-m","request":{"subtype":"mcp_message","server_name":"prigh","message":{"jsonrpc":"2.0","id":7,"method":"tools/call","params":{"name":"render_ui","arguments":{"components":[]}}}}}|};
      Eio.Promise.await running;
      print_endline "the tool runs";
      Line_transport.Peer.write_line
        p
        {|{"type":"control_cancel_request","request_id":"r-m"}|};
      Eio.Promise.await abandoned;
      print_endline "the tool is stopped";
      let rec rest () =
        match Line_transport.Peer.read_line p with
        | Some line ->
          printf "claude got: %s\n" line;
          rest ()
        | None -> print_endline "claude's input ends"
      in
      rest ();
      Eio.Promise.resolve finish ())
  in
  let adapter =
    Claude_adapter.start
      ~sw
      ~injected
      transport
      (Adapter.Start.default ~cwd:"/work")
    |> Or_error.ok_exn
  in
  Eio.Promise.await abandoned;
  Claude_adapter.stop adapter;
  Eio.Promise.await finished;
  [%expect
    {|
    the tool runs
    the tool is stopped
    claude's input ends
    |}]
;;

let done_frames =
  [ {|{"method":"turn/started","params":{"threadId":"th","turn":{"id":"tu","items":[],"status":"inProgress"}}}|}
  ; {|{"method":"item/completed","params":{"threadId":"th","turnId":"tu","item":{"type":"agentMessage","id":"m1","text":"hello"}}}|}
  ; {|{"method":"turn/completed","params":{"threadId":"th","turn":{"id":"tu","items":[],"status":"completed"}}}|}
  ]
;;

let%expect_test "a turn that raises stops its process: the next starts another" =
  let _, signal = signals () in
  (* The first process's transport fails to send the first prompt. *)
  let wrapped = ref 0 in
  let wrap transport =
    incr wrapped;
    let first = !wrapped = 1 in
    Line_transport.create
      ~read_line:(fun () -> Line_transport.read_line transport)
      ~write_line:(fun line ->
        if first && String.is_substring line ~substring:"boom"
        then failwith "the line could not be sent"
        else Line_transport.write_line transport line)
      ~stop:(fun () -> Line_transport.stop transport)
      ~ended:(fun () -> Line_transport.ended transport)
      ~is_ended:(fun () -> Line_transport.is_ended transport)
      ()
  in
  with_codex ~wrap ~peer:(fun _ ->
    codex ~hang_init:false ~frames:done_frames ~signal)
  @@ fun agent _ ->
  Or_error.ok_exn (Agent.prompt agent "boom");
  Agent.wait_idle agent;
  Or_error.ok_exn (Agent.prompt agent "fine");
  Agent.wait_idle agent;
  [%expect
    {|
    spawn #1
    notice: run failed: (Failure "the line could not be sent")
    spawn #2
    assistant: End_turn "hello"
    |}]
;;

let%expect_test "a new session in the agent stops the old one's process" =
  let reached, signal = signals () in
  let peer_done, peer_ended = Eio.Promise.create () in
  with_codex ~peer:(fun _ p ->
    codex ~hang_init:false ~frames:done_frames ~signal p;
    print_endline "its input ends";
    Eio.Promise.resolve peer_ended ())
  @@ fun agent _ ->
  Or_error.ok_exn (Agent.prompt agent "hi");
  Agent.wait_idle agent;
  Eio.Promise.await (reached "turn");
  Agent.new_session agent;
  Eio.Promise.await peer_done;
  print_s [%message "" ~engine:(Agent.engine agent : Engine_id.t)];
  [%expect
    {|
    spawn #1
    assistant: End_turn "hello"
    its input ends
    (engine Harness)
    |}]
;;

let%expect_test "the session's host leaving stops its process" =
  let reached, signal = signals () in
  let peer_done, peer_ended = Eio.Promise.create () in
  with_codex ~peer:(fun _ p ->
    codex ~hang_init:false ~frames:done_frames ~signal p;
    print_endline "its input ends";
    Eio.Promise.resolve peer_ended ())
  @@ fun agent _ ->
  let laptop =
    { Agent.Host.id = "host-laptop"
    ; name = "laptop"
    ; cwd = "/"
    ; session_id = None
    ; session_name = None
    ; home = None
    ; exec_input = true
    ; git_location = false
    ; computer = None
    }
  in
  Agent.set_hosts agent [ laptop ];
  ignore
    (Or_error.ok_exn (Agent.set_active_host agent "host-laptop" ~cwd:None)
     : Dir_change.Outcome.t);
  Or_error.ok_exn (Agent.prompt agent "hi");
  Agent.wait_idle agent;
  Eio.Promise.await (reached "turn");
  Agent.set_hosts agent [];
  Eio.Promise.await peer_done;
  [%expect
    {|
    notice: tools now run on laptop in /
    spawn #1
    assistant: End_turn "hello"
    notice: waiting for tool host "laptop" to reconnect; /host picks another
    its input ends
    |}]
;;

let%expect_test
    "claude: one of prigh's tools approved as its turn ends does not run"
  =
  let ran = ref false in
  let memory =
    { Tool.spec = Tool_memory.spec
    ; run =
        (fun _ _ ->
          ran := true;
          Tool_result.ok "saved")
    }
  in
  with_codex
    ~adapter:(module Claude_adapter)
    ~tools:[ memory ]
    ~peer:(fun _ p ->
      let read () =
        Option.map (Line_transport.Peer.read_line p) ~f:(fun line ->
          Json.parse line |> Or_error.ok_exn)
      in
      let str path j =
        List.fold path ~init:(Some j) ~f:(fun j key ->
          Option.bind j ~f:(Json.member key))
        |> Option.bind ~f:Json.string
        |> Option.value ~default:""
      in
      let init = Option.value_exn (read ()) in
      Line_transport.Peer.write_line
        p
        (sprintf
           {|{"type":"control_response","response":{"subtype":"success","request_id":"%s","response":{"models":[],"commands":[]}}}|}
           (str [ "request_id" ] init));
      ignore (read () : Json.t option);
      Line_transport.Peer.write_line
        p
        {|{"type":"control_request","request_id":"r-m","request":{"subtype":"mcp_message","server_name":"prigh","message":{"jsonrpc":"2.0","id":7,"method":"tools/call","params":{"name":"memory","arguments":{"action":"save","text":"x"}}}}}|};
      let rec loop () =
        match read () with
        | None -> ()
        | Some j ->
          (match str [ "type" ] j, str [ "request"; "subtype" ] j with
           | "control_request", "interrupt" ->
             Line_transport.Peer.write_line
               p
               (sprintf
                  {|{"type":"control_response","response":{"subtype":"success","request_id":"%s"}}|}
                  (str [ "request_id" ] j));
             Line_transport.Peer.write_line
               p
               {|{"type":"result","subtype":"error_during_execution","is_error":true,"session_id":"s","total_cost_usd":0}|}
           | _ -> printf "claude got: %s\n" (Json.to_string j));
          loop ()
      in
      loop ())
  @@ fun agent _ ->
  Agent.set_mode agent ~approval:Ask;
  Agent.subscribe agent ~f:(function
    | Loop (Tool_confirm { call_id; _ }) ->
      (* Allowed, and the turn ended, before the call carries on. *)
      Or_error.ok_exn (Agent.respond_confirm agent ~call_id Allow);
      ignore (Agent.abort agent : string list)
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "remember x");
  Agent.wait_idle agent;
  print_s [%message (!ran : bool)];
  [%expect
    {|
    spawn #1
    confirm memory
    confirm done
    claude got: {"type":"control_response","response":{"subtype":"success","request_id":"r-m","response":{"mcp_response":{"jsonrpc":"2.0","id":7,"result":{"content":[{"type":"text","text":"[cancelled]: its turn ended"}],"isError":true}}}}}
    (!ran false)
    |}]
;;
