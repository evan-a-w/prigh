open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

let member path j =
  List.fold path ~init:(Some j) ~f:(fun j key ->
    Option.bind j ~f:(Json.member key))
;;

let string_at path j = Option.bind (member path j) ~f:Json.string

let write (p : Line_transport.Peer.t) j =
  Line_transport.Peer.write_line p (Json.to_string j)
;;

let read (p : Line_transport.Peer.t) =
  Option.map (Line_transport.Peer.read_line p) ~f:(fun line ->
    Json.parse line |> Or_error.ok_exn)
;;

let fake_codex (p : Line_transport.Peer.t) =
  let reply j result =
    write
      p
      (`Object [ "id", Option.value_exn (Json.member "id" j); "result", result ])
  in
  let rec loop () =
    match read p with
    | None -> ()
    | Some j ->
      (match string_at [ "method" ] j with
       | Some "initialize" ->
         reply j (`Object [ "userAgent", `String "codex/0.166.0" ])
       | Some "model/list" ->
         reply
           j
           (`Object
               [ ( "data"
                 , `Array
                     [ `Object
                         [ "id", `String "gpt-6.2-sol"
                         ; "displayName", `String "GPT-6.2-Sol"
                         ; "isDefault", `True
                         ]
                     ; `Object
                         [ "id", `String "gpt-6.2-mini"
                         ; "displayName", `String "GPT-6.2-Mini"
                         ; "isDefault", `False
                         ]
                     ] )
               ])
       | Some "account/rateLimits/read" -> reply j (`Object [])
       | Some "thread/start" ->
         reply
           j
           (`Object
               [ "thread", `Object [ "id", `String "thr-codex" ]
               ; "model", `String "gpt-6.2-sol"
               ])
       | Some _ | None -> ());
      loop ()
  in
  loop ()
;;

let fake_agy (p : Line_transport.Peer.t) =
  let reply j result =
    write
      p
      (`Object
          [ "jsonrpc", `String "2.0"
          ; "id", Option.value_exn (Json.member "id" j)
          ; "result", result
          ])
  in
  let rec loop () =
    match read p with
    | None -> ()
    | Some j ->
      (match string_at [ "method" ] j with
       | Some "initialize" ->
         reply
           j
           (`Object
               [ "protocolVersion", `Number "1"
               ; "agentInfo", `Object [ "name", `String "antigravity" ]
               ])
       | Some "session/new" ->
         reply
           j
           (`Object
               [ "sessionId", `String "ses-agy"
               ; ( "models"
                 , `Object
                     [ "currentModelId", `String "sol-6.2"
                     ; ( "availableModels"
                       , `Array
                           [ `Object
                               [ "modelId", `String "sol-6.2"
                               ; "name", `String "Sol 6.2"
                               ]
                           ; `Object
                               [ "modelId", `String "flash-3.8"
                               ; "name", `String "Flash 3.8"
                               ]
                           ] )
                     ] )
               ])
       | Some "session/set_model" | Some "session/close" -> reply j (`Object [])
       | Some _ | None -> ());
      loop ()
  in
  loop ()
;;

let with_three_modes ?config ?(connect_laptop = false) f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  Option.iter config ~f:(fun c -> Or_error.ok_exn (Config.save ~home:t.dir c));
  let sessions_dir = Filename.concat t.dir "sessions" in
  let laptop_dir = Filename.concat t.dir "laptop-proj" in
  Core_unix.mkdir_p laptop_dir;
  let engines =
    Or_error.ok_exn
      (Engines.create
         ~engines:[ Harness; Composer ]
         ~adapters:[ Backend (Acp "antigravity"); Codex; Acp "antigravity" ]
         ())
  in
  let new_agent ?session ~cwd () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [])
      ~tools:Tools.interaction
      ~sessions_dir
      ~home:t.dir
      ?session
      ~backend_host:true
      ~cwd
      ()
  in
  let spawn_external _agent ~host (launch : Adapter.Launch.t) =
    let is_codex = List.exists launch.argv ~f:(String.equal "codex") in
    let peer =
      Line_transport.Peer.spawn ~sw (if is_codex then fake_codex else fake_agy)
    in
    let base_bridge =
      Injected_tools.Bridge.create
        ~prigh:"prigh"
        ~socket:"/tmp/test.sock"
        ~token:"tok"
    in
    let mcp : Line_transport.Mcp.t =
      { bridge = base_bridge; serve = ignore; route = None }
    in
    ignore (host : string);
    Ok
      (Line_transport.create
         ~read_line:(fun () -> Line_transport.read_line peer)
         ~write_line:(fun line -> Line_transport.write_line peer line)
         ~stop:(fun () -> Line_transport.stop peer)
         ~ended:(fun () -> Line_transport.ended peer)
         ~is_ended:(fun () -> Line_transport.is_ended peer)
         ~mcp
         ())
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
      ~home:t.dir
      ~engines
      ~spawn_external
      ~new_agent
      ()
  in
  if connect_laptop
  then (
    let host_client = Rpc_server.connect server ~send:ignore in
    ignore
      (Rpc_server.handle
         server
         host_client
         (Json.of_string
            (sprintf
               {|{"id":"h","method":"hello","params":{"name":"laptop","tools":true,"host_id":"host-laptop","cwd":%S,"home":%S}}|}
               laptop_dir
               t.dir))
       : Json.t));
  let note_changed = ref None in
  let next_change () =
    let p, r = Eio.Promise.create () in
    note_changed := Some r;
    p
  in
  let client =
    Rpc_server.connect server ~send:(fun ev ->
      match string_at [ "event" ] ev with
      | Some "models_changed" ->
        Option.iter !note_changed ~f:(fun r ->
          note_changed := None;
          Eio.Promise.resolve r ())
      | _ -> ())
  in
  let call ?(params = "{}") meth =
    Rpc_server.handle
      server
      client
      (Json.of_string
         (sprintf {|{"id":"r","method":%S,"params":%s}|} meth params))
  in
  let call_ok ?params meth =
    let resp = call ?params meth in
    match Json.member "ok" resp with
    | Some `True -> Option.value_exn (Json.member "result" resp)
    | _ -> failwithf "RPC %s failed: %s" meth (Json.to_string resp) ()
  in
  let show_state label =
    let st = call_ok "get_state" in
    printf
      "%s: engine=%s adapter=%s model=%s host=%s\n"
      label
      (Option.value (string_at [ "engine" ] st) ~default:"harness")
      (Option.value (string_at [ "adapter" ] st) ~default:"-")
      (Option.value (string_at [ "model"; "key" ] st) ~default:"?")
      (Option.value (string_at [ "active_host" ] st) ~default:"?")
  in
  let model_summary () =
    match call_ok "list_models" with
    | `Array items ->
      let keys = List.filter_map items ~f:(fun m -> string_at [ "key" ] m) in
      let has_native =
        List.exists keys ~f:(String.is_prefix ~prefix:"anthropic/")
      in
      let bridge = List.filter keys ~f:(String.is_prefix ~prefix:"backend:") in
      let host =
        List.filter keys ~f:(fun k ->
          String.is_prefix k ~prefix:"codex/"
          || String.is_prefix k ~prefix:"acp:")
      in
      printf
        "native=%b bridge=[%s] host=[%s]\n"
        has_native
        (String.concat ~sep:", " bridge)
        (String.concat ~sep:", " host)
    | _ -> print_endline "not an array"
  in
  f ~call ~call_ok ~show_state ~model_summary ~next_change;
  Rpc_server.shutdown server
;;

let%expect_test "list_models returns Native, Bridge, and Host across all modes" =
  with_three_modes
  @@ fun ~call:_ ~call_ok ~show_state ~model_summary ~next_change ->
  ignore (call_ok "hello" : Json.t);
  let bridge_loaded = next_change () in
  ignore (call_ok "new_session" : Json.t);
  (* On Native: returns native + bridge (probes bridge) + host placeholders. *)
  show_state "on native";
  model_summary ();
  Eio.Promise.await bridge_loaded;
  model_summary ();
  (* Switch to Bridge: still returns native + bridge + host. *)
  ignore
    (call_ok
       ~params:{|{"model": "backend:acp:antigravity/sol-6.2"}|}
       "set_model"
     : Json.t);
  show_state "on bridge";
  model_summary ();
  (* Switch to Host (codex): probes codex and still returns native + bridge + host. *)
  ignore
    (call_ok ~params:{|{"model": "codex/gpt-6.2-sol"}|} "set_model" : Json.t);
  let codex_loaded = next_change () in
  ignore (call_ok "list_models" : Json.t);
  Eio.Promise.await codex_loaded;
  show_state "on host";
  model_summary ();
  [%expect
    {|
    on native: engine=harness adapter=- model=deepseek/deepseek-flash host=backend
    native=true bridge=[backend:acp:antigravity/default] host=[codex/default, acp:antigravity/default]
    native=true bridge=[backend:acp:antigravity/sol-6.2, backend:acp:antigravity/flash-3.8] host=[codex/default, acp:antigravity/default]
    on bridge: engine=composer adapter=backend:acp:antigravity model=backend:acp:antigravity/sol-6.2 host=backend
    native=true bridge=[backend:acp:antigravity/sol-6.2, backend:acp:antigravity/flash-3.8] host=[codex/default, acp:antigravity/default]
    on host: engine=composer adapter=codex model=codex/gpt-6.2-sol host=backend
    native=true bridge=[backend:acp:antigravity/sol-6.2, backend:acp:antigravity/flash-3.8] host=[codex/gpt-6.2-sol, codex/gpt-6.2-mini, acp:antigravity/default]
    |}]
;;

let%expect_test "set_model transparently switches Native <-> Bridge <-> Host" =
  with_three_modes
  @@ fun ~call:_ ~call_ok ~show_state ~model_summary:_ ~next_change ->
  ignore (call_ok "hello" : Json.t);
  ignore (call_ok "new_session" : Json.t);
  show_state "start";
  (* Native -> Bridge *)
  ignore
    (call_ok
       ~params:{|{"model": "backend:acp:antigravity/flash-3.8"}|}
       "set_model"
     : Json.t);
  show_state "native -> bridge";
  (* Bridge -> Host (codex) *)
  ignore
    (call_ok ~params:{|{"model": "codex/gpt-6.2-mini"}|} "set_model" : Json.t);
  show_state "bridge -> host(codex)";
  (* Host (codex) -> Host (acp:antigravity) *)
  ignore
    (call_ok ~params:{|{"model": "acp:antigravity/sol-6.2"}|} "set_model"
     : Json.t);
  show_state "host(codex) -> host(agy)";
  (* Host -> Native *)
  ignore
    (call_ok ~params:{|{"model": "anthropic/claude-sonnet-4-6"}|} "set_model"
     : Json.t);
  show_state "host -> native";
  (* Native -> Host directly *)
  ignore
    (call_ok ~params:{|{"model": "codex/gpt-6.2-sol"}|} "set_model" : Json.t);
  let codex_loaded = next_change () in
  ignore (call_ok "list_models" : Json.t);
  Eio.Promise.await codex_loaded;
  show_state "native -> host(codex)";
  (* Unprefixed model on current Host adapter resolves against current adapter *)
  ignore (call_ok ~params:{|{"model": "gpt-6.2-mini"}|} "set_model" : Json.t);
  show_state "unprefixed on host(codex)";
  [%expect
    {|
    start: engine=harness adapter=- model=deepseek/deepseek-flash host=backend
    native -> bridge: engine=composer adapter=backend:acp:antigravity model=backend:acp:antigravity/flash-3.8 host=backend
    bridge -> host(codex): engine=composer adapter=codex model=codex/gpt-6.2-mini host=backend
    host(codex) -> host(agy): engine=composer adapter=acp:antigravity model=acp:antigravity/sol-6.2 host=backend
    host -> native: engine=harness adapter=- model=anthropic/claude-sonnet-4-6 host=backend
    native -> host(codex): engine=composer adapter=codex model=codex/gpt-6.2-sol host=backend
    unprefixed on host(codex): engine=composer adapter=codex model=codex/gpt-6.2-mini host=backend
    |}]
;;

let%expect_test
    "default_model works for Native, Bridge, and Host (with default_host)"
  =
  (* 1. Native default_model *)
  (with_three_modes
     ~config:
       { Config.default with
         default_model = Some "anthropic/claude-sonnet-4-6"
       }
   @@ fun ~call:_ ~call_ok ~show_state ~model_summary:_ ~next_change:_ ->
   ignore (call_ok "hello" : Json.t);
   show_state "native hello";
   ignore (call_ok "new_session" : Json.t);
   show_state "native new_session");
  (* 2. Bridge default_model *)
  (with_three_modes
     ~config:
       { Config.default with
         default_model = Some "backend:acp:antigravity/flash-3.8"
       }
   @@ fun ~call:_ ~call_ok ~show_state ~model_summary:_ ~next_change:_ ->
   ignore (call_ok "hello" : Json.t);
   show_state "bridge hello";
   ignore (call_ok "new_session" : Json.t);
   show_state "bridge new_session");
  (* 3. Host default_model with default_host *)
  (with_three_modes
     ~connect_laptop:true
     ~config:
       { Config.default with
         default_model = Some "codex/gpt-6.2-mini"
       ; default_host = Some "host-laptop"
       }
   @@ fun ~call:_ ~call_ok ~show_state ~model_summary:_ ~next_change:_ ->
   ignore (call_ok "hello" : Json.t);
   show_state "host hello";
   ignore (call_ok "new_session" : Json.t);
   show_state "host new_session");
  [%expect
    {|
    native hello: engine=harness adapter=- model=anthropic/claude-sonnet-4-6 host=backend
    native new_session: engine=harness adapter=- model=anthropic/claude-sonnet-4-6 host=backend
    bridge hello: engine=composer adapter=backend:acp:antigravity model=backend:acp:antigravity/flash-3.8 host=backend
    bridge new_session: engine=composer adapter=backend:acp:antigravity model=backend:acp:antigravity/flash-3.8 host=backend
    host hello: engine=composer adapter=codex model=codex/gpt-6.2-mini host=backend
    host new_session: engine=composer adapter=codex model=codex/gpt-6.2-mini host=host-laptop
    |}]
;;

let%expect_test
    "Model_registry.unknown_start_model ignores valid external specs"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let store = Auth_store.create ~path:(Filename.concat t.dir "auth.json") in
  let check model =
    Or_error.ok_exn
      (Config.save
         ~home:t.dir
         { Config.default with default_model = Some model });
    let reg =
      Model_registry.create
        ~home:t.dir
        ~env:t.env
        ~sw
        ~store
        ~getenv:(fun _ -> None)
        ()
    in
    let problems = Model_registry.problems reg in
    printf "%s -> %d problem(s)\n" model (List.length problems)
  in
  check "anthropic/claude-sonnet-4-6";
  check "backend:acp:antigravity/sol-6.2";
  check "acp:antigravity/sol-6.2";
  check "codex/gpt-6.2-sol";
  check "claude/opus";
  check "nonexistent/model-xyz";
  [%expect
    {|
    anthropic/claude-sonnet-4-6 -> 0 problem(s)
    backend:acp:antigravity/sol-6.2 -> 0 problem(s)
    acp:antigravity/sol-6.2 -> 0 problem(s)
    codex/gpt-6.2-sol -> 0 problem(s)
    claude/opus -> 0 problem(s)
    nonexistent/model-xyz -> 1 problem(s)
    |}]
;;
