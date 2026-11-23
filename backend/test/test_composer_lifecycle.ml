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

let with_codex ?request_timeout ~peer f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let launches = ref 0 in
  let composer =
    Composer_runner.create
      ~sw
      ~adapter:(module Codex_adapter)
      ~spawn:(fun (_ : Adapter.Launch.t) ->
        incr launches;
        printf "spawn #%d\n" !launches;
        Ok (Line_transport.Peer.spawn ~sw (peer !launches)))
      ?request_timeout
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
  Agent.set_engine agent { engine = Composer; adapter = Some Codex };
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
