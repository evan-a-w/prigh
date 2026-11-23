open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

(* Engines: what a backend deploys ([-engines], [-adapters]), the session
   entries recording a session's engine and its external agent's threads,
   and what [hello], [state] and [new_session] say of them. *)

let flags ?engines ?adapters ?(env = []) () =
  let getenv var = List.Assoc.find env var ~equal:String.equal in
  match Engines.of_flags ~engines ~adapters ~getenv with
  | Ok t ->
    print_s
      [%sexp
        (t : Engines.t)
      , (Engines.check_build t : unit Or_error.t)
      , (Engines.default_choice t : Engine_choice.t)]
  | Error e -> print_endline (Error.to_string_hum e)
;;

let%expect_test "-engines and -adapters" =
  flags ();
  [%expect
    {|
    (((engines (Harness)) (adapters ()) (acp_agents ())) (Ok ())
     ((engine Harness) (adapter ())))
    |}];
  flags ~engines:"harness" ();
  [%expect
    {|
    (((engines (Harness)) (adapters ()) (acp_agents ())) (Ok ())
     ((engine Harness) (adapter ())))
    |}];
  (* The composer is not built yet: startup fails with what to run. *)
  flags ~engines:"composer, harness" ();
  [%expect
    {|
    (((engines (Composer Harness)) (adapters (Codex Claude)) (acp_agents ()))
     (Ok ()) ((engine Composer) (adapter (Codex))))
    |}];
  flags ~engines:"harness,composer" ~adapters:"claude,acp:gemini,prigh" ();
  [%expect
    {|
    (((engines (Harness Composer)) (adapters (Claude (Acp gemini) Prigh))
      (acp_agents ()))
     (Ok ()) ((engine Harness) (adapter ())))
    |}];
  flags ~env:[ "PRIGH_ENGINES", "composer"; "PRIGH_ADAPTERS", "codex" ] ();
  [%expect
    {|
    (((engines (Composer)) (adapters (Codex)) (acp_agents ())) (Ok ())
     ((engine Composer) (adapter (Codex))))
    |}];
  (* Flags outrank the environment. *)
  flags ~engines:"harness" ~env:[ "PRIGH_ENGINES", "composer" ] ();
  [%expect
    {|
    (((engines (Harness)) (adapters ()) (acp_agents ())) (Ok ())
     ((engine Harness) (adapter ())))
    |}];
  flags ~engines:"harnes" ();
  [%expect {| "unknown engine \"harnes\": expected harness or composer" |}];
  flags ~engines:"composer" ~adapters:"cursor" ();
  [%expect
    {| "unknown adapter \"cursor\": expected codex, claude, prigh or acp:<name>" |}];
  flags ~adapters:"codex" ();
  [%expect
    {| adapters (codex) are for the composer engine: add -engines composer (or harness,composer) |}];
  flags ~engines:"harness" ~adapters:"codex" ();
  [%expect
    {| adapters (codex) are for the composer engine: add composer to the engines |}];
  flags ~engines:"," ();
  [%expect {| no engines: give harness, composer or both |}]
;;

let composer_deployed =
  Or_error.ok_exn
    (Engines.create
       ~engines:[ Harness; Composer ]
       ~adapters:[ Codex; Claude ]
       ())
;;

let%expect_test "choosing a new session's engine" =
  let choose ?(t = composer_deployed) ?engine ?adapter () =
    print_s
      [%sexp (Engines.choose t ~engine ~adapter : Engine_choice.t Or_error.t)]
  in
  choose ();
  [%expect {| (Ok ((engine Harness) (adapter ()))) |}];
  choose ~engine:"composer" ();
  [%expect {| (Ok ((engine Composer) (adapter (Codex)))) |}];
  choose ~adapter:"claude" ();
  [%expect {| (Ok ((engine Composer) (adapter (Claude)))) |}];
  choose ~engine:"harness" ~adapter:"claude" ();
  [%expect
    {|
    (Error
     "the harness engine takes no adapter (got claude): use engine composer")
    |}];
  choose ~adapter:"acp:gemini" ();
  [%expect
    {|
    (Error
     "this backend's composer does not drive gemini (ACP) (it drives Codex, Claude Code)")
    |}];
  choose ~t:Engines.default ~engine:"composer" ();
  [%expect
    {|
    (Error
     "this backend does not run the composer engine (it runs harness): start one with prigh serve -engines composer")
    |}]
;;

let%expect_test "a session the backend cannot run" =
  let unavailable t choice =
    print_s
      [%sexp
        (Engines.unavailable ~buildable:(Fn.const true) t choice
         : string option)]
  in
  unavailable Engines.default Engine_choice.harness;
  [%expect {| () |}];
  unavailable Engines.default { engine = Composer; adapter = Some Codex };
  [%expect
    {| ("this session runs on the composer (Codex) engine, which this backend does not run (it runs harness): open it on a backend started with prigh serve -engines composer; it can still be read here") |}];
  unavailable
    composer_deployed
    { engine = Composer; adapter = Some (Acp "gemini") };
  [%expect
    {| ("this session drives gemini (ACP), which this backend's composer does not (it drives Codex, Claude Code): add it with -adapters codex,claude,acp:gemini") |}]
;;

let with_dir f = f (Filename_unix.temp_dir "prigh-engines" "")

let show ?(ids = []) session =
  let mask s =
    List.fold ids ~init:s ~f:(fun s id ->
      String.substr_replace_all s ~pattern:id ~with_:"<id>")
  in
  print_endline
  @@ mask
  @@ Sexp.to_string_hum
       [%sexp
         { engine = (Session.engine session : Engine_choice.t Or_error.t)
         ; threads = (Session.threads session : Session.Thread.t list)
         ; turns =
             (Session.external_turns session : Session.External_turn.t list)
         }]
;;

let%expect_test "session entries: engine, threads, external turns, handoffs" =
  with_dir
  @@ fun dir ->
  let t = Session.create ~dir ~cwd:"/proj" () in
  show t;
  [%expect
    {| ((engine (Ok ((engine Harness) (adapter ())))) (threads ()) (turns ())) |}];
  let (_ : Session.Entry.t) =
    Session.set_engine t { engine = Composer; adapter = Some Codex }
  in
  let (_ : Session.Entry.t) =
    Session.record_thread
      t
      { adapter = "codex"
      ; host = "backend"
      ; thread_id = "thr_1"
      ; cwd = "/proj"
      }
  in
  let m = Session.append_message t (Message.user "hi") in
  let (_ : Session.Entry.t) =
    Session.record_external_turn
      t
      { thread_id = "thr_1"; turn_id = "turn_1"; through = m.id }
  in
  let (_ : Session.Entry.t) =
    Session.set_engine t { engine = Composer; adapter = Some Claude }
  in
  let (_ : Session.Entry.t) =
    Session.record_handoff
      t
      ~from_adapter:(Some "codex")
      ~to_adapter:"claude"
      ~text:"<handoff>…</handoff>"
  in
  let (_ : Session.Entry.t) =
    Session.record_thread
      t
      { adapter = "claude"
      ; host = "laptop"
      ; thread_id = "sess-2"
      ; cwd = "/proj"
      }
  in
  let loaded = Or_error.ok_exn (Session.load (Session.path t)) in
  show ~ids:[ m.id ] loaded;
  [%expect
    {|
    ((engine (Ok ((engine Composer) (adapter (Claude)))))
     (threads
      (((adapter claude) (host laptop) (thread_id sess-2) (cwd /proj))
       ((adapter codex) (host backend) (thread_id thr_1) (cwd /proj))))
     (turns (((thread_id thr_1) (turn_id turn_1) (through <id>)))))
    |}];
  (* The entries as written, and the summary's engine. *)
  In_channel.read_lines (Session.path t)
  |> List.filter ~f:(fun line ->
    List.exists [ "Engine"; "Thread"; "External_turn"; "Handoff" ] ~f:(fun k ->
      String.is_substring line ~substring:(sprintf {|"%s"|} k)))
  |> List.iter ~f:(fun line ->
    let json = Json.of_string line in
    print_endline
      (String.substr_replace_all ~pattern:m.id ~with_:"<id>"
       @@ Json.to_string
            (match json with
             | `Array [ _; `Object fields ] ->
               `Object
                 (List.filter fields ~f:(fun (k, _) ->
                    not
                      (List.mem [ "id"; "parent"; "at" ] k ~equal:String.equal)))
             | json -> json)));
  [%expect
    {|
    {"payload":["Engine",{"engine":"composer","adapter":"codex"}]}
    {"payload":["Thread",{"adapter":"codex","host":"backend","thread_id":"thr_1","cwd":"/proj"}]}
    {"payload":["External_turn",{"thread_id":"thr_1","turn_id":"turn_1","through":"<id>"}]}
    {"payload":["Engine",{"engine":"composer","adapter":"claude"}]}
    {"payload":["Handoff",{"from_adapter":"codex","to_adapter":"claude","text":"<handoff>…</handoff>"}]}
    {"payload":["Thread",{"adapter":"claude","host":"laptop","thread_id":"sess-2","cwd":"/proj"}]}
    |}];
  let summary =
    Option.value_exn
      (Session.summary_with
         ~live:(fun _ -> None)
         ~load:Session.load
         (Session.path t))
  in
  print_s
    [%sexp (summary.engine : string option), (summary.adapter : string option)];
  [%expect {| ((composer) (claude)) |}]
;;

let%expect_test "a session naming an engine this prigh does not know" =
  with_dir
  @@ fun dir ->
  let t = Session.create ~dir ~cwd:"/proj" () in
  let (_ : Session.Entry.t) =
    Session.set_engine t { engine = Composer; adapter = Some Codex }
  in
  let (_ : Session.Entry.t) = Session.append_message t (Message.user "hi") in
  let path = Session.path t in
  Out_channel.write_all
    path
    ~data:
      (String.substr_replace_all
         (In_channel.read_all path)
         ~pattern:{|"composer"|}
         ~with_:{|"turbo"|});
  show (Or_error.ok_exn (Session.load path));
  [%expect
    {|
    ((engine (Error "unknown engine \"turbo\": expected harness or composer"))
     (threads ()) (turns ()))
    |}]
;;

(* A server deploying [engines] with one client, as test_rpc's. *)
let with_server ?(engines = composer_deployed) f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let sessions_dir = Filename.concat t.dir "sessions" in
  let provider = Faux_provider.create [] in
  let new_agent ?session ~cwd () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:Tools.all
      ~sessions_dir
      ~home:t.dir
      ?session
      ~backend_host:true
      ~cwd
      ()
  in
  let server =
    Rpc_server.create
      ~env:t.env
      ~sw
      ~login:
        (Login_manager.create
           ~env:t.env
           ~sw
           ~getenv:(fun _ -> None)
           ~store:(Auth_store.create ~path:(Filename.concat t.dir "auth.json"))
           ())
      ~sessions_dir
      ~cwd:t.dir
      ~engines
      ~new_agent
      ()
  in
  let client = Rpc_server.connect server ~send:(fun _ -> ()) in
  let call ?(params = "{}") meth =
    Rpc_server.handle
      server
      client
      (Json.of_string
         (sprintf {|{"id": "r", "method": "%s", "params": %s}|} meth params))
  in
  f t ~server ~client ~call ~new_agent ~sessions_dir
;;

let field json path =
  List.fold path ~init:(Some json) ~f:(fun json key ->
    match json with
    | Some (`Object fields) -> List.Assoc.find fields key ~equal:String.equal
    | _ -> None)
;;

let print_fields json paths =
  List.iter paths ~f:(fun path ->
    printf
      "%s: %s\n"
      (String.concat ~sep:"." path)
      (Option.value_map (field json path) ~default:"-" ~f:Json.to_string))
;;

let engine_paths prefix =
  List.map
    [ [ "engine" ]; [ "adapter" ]; [ "capabilities" ]; [ "unavailable" ] ]
    ~f:(fun p -> prefix @ p)
;;

let%expect_test "hello and state: nothing new from a harness-only backend" =
  with_server ~engines:Engines.default
  @@ fun _ ~server:_ ~client:_ ~call ~new_agent:_ ~sessions_dir:_ ->
  let hello = call "hello" in
  print_fields
    hello
    ([ [ "result"; "features" ]
     ; [ "result"; "engines" ]
     ; [ "result"; "adapters" ]
     ]
     @ engine_paths [ "result"; "state" ]);
  [%expect
    {|
    result.features: ["pasted_text"]
    result.engines: -
    result.adapters: -
    result.state.engine: -
    result.state.adapter: -
    result.state.capabilities: -
    result.state.unavailable: -
    |}];
  print_fields
    (call ~params:{|{"engine": "composer"}|} "new_session")
    [ [ "error" ] ];
  [%expect
    {| error: "this backend does not run the composer engine (it runs harness): start one with prigh serve -engines composer" |}]
;;

let%expect_test "hello, new_session and state with the composer deployed" =
  with_server
  @@ fun _ ~server:_ ~client:_ ~call ~new_agent:_ ~sessions_dir:_ ->
  let hello = call "hello" in
  print_fields
    hello
    ([ [ "result"; "features" ]
     ; [ "result"; "engines" ]
     ; [ "result"; "adapters" ]
     ]
     @ engine_paths [ "result"; "state" ]);
  [%expect
    {|
    result.features: ["pasted_text","engines"]
    result.engines: ["harness","composer"]
    result.adapters: ["codex","claude"]
    result.state.engine: -
    result.state.adapter: -
    result.state.capabilities: -
    result.state.unavailable: -
    |}];
  let session = call ~params:{|{"adapter": "claude"}|} "new_session" in
  print_fields session (engine_paths [ "result"; "state" ]);
  [%expect
    {|
    result.state.engine: "composer"
    result.state.adapter: "claude"
    result.state.capabilities: {"fork":"by_handoff","rewind":"unsupported","context_edit":false,"compact":"native","steer":"between_turns","plan_mode":true,"btw":false,"subagents":"observe","model_fallback":false,"images":true,"ask_user":true}
    result.state.unavailable: -
    |}];
  print_fields (call "get_state") (engine_paths [ "result" ]);
  [%expect
    {|
    result.engine: "composer"
    result.adapter: "claude"
    result.capabilities: {"fork":"by_handoff","rewind":"unsupported","context_edit":false,"compact":"native","steer":"between_turns","plan_mode":true,"btw":false,"subagents":"observe","model_fallback":false,"images":true,"ask_user":true}
    result.unavailable: -
    |}];
  List.iter
    [ {|{"adapter": "acp:gemini"}|}; {|{"engine": "warp"}|}; {|{"engine": 3}|} ]
    ~f:(fun params -> print_fields (call ~params "new_session") [ [ "error" ] ]);
  [%expect
    {|
    error: "this backend's composer does not drive gemini (ACP) (it drives Codex, Claude Code)"
    error: "unknown engine \"warp\": expected harness or composer"
    error: "param \"engine\" must be a string"
    |}]
;;

let%expect_test
    "a composer session on a backend without the composer can be read, not run"
  =
  with_server ~engines:Engines.default
  @@ fun _ ~server:_ ~client:_ ~call ~new_agent:_ ~sessions_dir ->
  ignore (call "hello" : Json.t);
  let session = Session.create ~dir:sessions_dir ~cwd:"/proj" () in
  let (_ : Session.Entry.t) =
    Session.set_engine session { engine = Composer; adapter = Some Codex }
  in
  let (_ : Session.Entry.t) =
    Session.append_message session (Message.user "from codex's day")
  in
  let state =
    call
      ~params:(sprintf {|{"path": %S}|} (Session.id session))
      "switch_session"
  in
  print_fields state (engine_paths [ "result"; "state" ] @ [ [ "error" ] ]);
  [%expect
    {|
    result.state.engine: "composer"
    result.state.adapter: "codex"
    result.state.capabilities: {"fork":"unsupported","rewind":"unsupported","context_edit":false,"compact":"unsupported","steer":"between_turns","plan_mode":false,"btw":false,"subagents":"observe","model_fallback":false,"images":false,"ask_user":false}
    result.state.unavailable: "this session runs on the composer (Codex) engine, which this backend does not run (it runs harness): open it on a backend started with prigh serve -engines composer; it can still be read here"
    error: -
    |}];
  print_fields (call ~params:{|{"text": "hi"}|} "prompt") [ [ "error" ] ];
  [%expect
    {| error: "this session runs on the composer (Codex) engine, which this backend does not run (it runs harness): open it on a backend started with prigh serve -engines composer; it can still be read here" |}];
  (* Nothing else runs the harness in its place either, nor edits its
     history behind its harness's back. *)
  List.iter
    [ "set_goal", {|{"text": "tests pass"}|}
    ; "compact", "{}"
    ; "rewind", {|{"index": 0}|}
    ; "btw", {|{"text": "aside"}|}
    ]
    ~f:(fun (meth, params) -> print_fields (call ~params meth) [ [ "error" ] ]);
  [%expect
    {|
    error: "this session runs on the composer (Codex) engine, which this backend does not run (it runs harness): open it on a backend started with prigh serve -engines composer; it can still be read here"
    error: "Codex sessions can't be compacted: start a new session (/new) for a fresh context"
    error: "Codex sessions can't rewind: start a new session (/new) to begin again"
    error: "Codex sessions have no side questions (/btw): ask in the session instead"
    |}];
  print_fields (call "get_state") [ [ "result"; "capabilities" ] ];
  [%expect
    {| result.capabilities: {"fork":"unsupported","rewind":"unsupported","context_edit":false,"compact":"unsupported","steer":"between_turns","plan_mode":false,"btw":false,"subagents":"observe","model_fallback":false,"images":false,"ask_user":false} |}];
  (match field (call "get_messages") [ "result" ] with
   | Some (`Array messages) -> printf "%d message(s)\n" (List.length messages)
   | json -> print_s [%sexp (Option.map json ~f:Json.to_string : string option)]);
  [%expect {| 1 message(s) |}]
;;
