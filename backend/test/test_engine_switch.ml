open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

(* Moving a composer session between adapters ([set_adapter]), tool hosts
   and forks: the harness taking over is handed what it has not seen of the
   session, and each turn is recorded so the next handoff starts there. The
   harnesses are scripted peers. *)

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

(* What a harness was sent, with the handoff (if any) shown in full. *)
let show_prompt mask ~name text = printf "%s got:\n%s\n" name (mask text)

(* A Codex app-server that answers every turn with "codex reply N"; it fails
   to resume a thread when [refuse_resume] is set. *)
let fake_codex ~mask ~refuse_resume (p : Line_transport.Peer.t) =
  let reply j result =
    write
      p
      (`Object [ "id", Option.value_exn (Json.member "id" j); "result", result ])
  in
  let current = ref "codex-thread" in
  let thread j id =
    current := id;
    reply
      j
      (`Object
          [ "thread", `Object [ "id", `String id ]; "model", `String "gpt-5" ])
  in
  let turns = ref 0 in
  let rec loop () =
    match read p with
    | None -> ()
    | Some j ->
      (match string_at [ "method" ] j with
       | Some "initialize" ->
         reply j (`Object [ "userAgent", `String "codex/0.166.0" ])
       | Some "thread/start" ->
         print_endline "codex: thread/start";
         thread j "codex-thread"
       | Some "thread/fork" ->
         printf
           "codex: thread/fork %s at %s\n"
           (Option.value_exn (string_at [ "params"; "threadId" ] j))
           (Option.value
              (string_at [ "params"; "lastTurnId" ] j)
              ~default:"its end");
         thread j "codex-fork"
       | Some "thread/resume" ->
         let id = Option.value_exn (string_at [ "params"; "threadId" ] j) in
         if !refuse_resume
         then (
           printf "codex: thread/resume %s refused\n" id;
           write
             p
             (`Object
                 [ "id", Option.value_exn (Json.member "id" j)
                 ; ( "error"
                   , `Object
                       [ "code", `Number "-32600"
                       ; "message", `String ("no rollout found for " ^ id)
                       ] )
                 ]))
         else (
           printf "codex: thread/resume %s\n" id;
           thread j id)
       | Some "turn/start" ->
         incr turns;
         let turn = sprintf "tu-%d" !turns in
         (match member [ "params"; "input" ] j with
          | Some (`Array (first :: _)) ->
            show_prompt
              mask
              ~name:"codex"
              (Option.value (string_at [ "text" ] first) ~default:"")
          | _ -> ());
         reply
           j
           (`Object
               [ ( "turn"
                 , `Object
                     [ "id", `String turn; "status", `String "inProgress" ] )
               ]);
         let notify meth params =
           write
             p
             (`Object [ "method", `String meth; "params", `Object params ])
         in
         let turn_json status =
           ( "turn"
           , `Object
               [ "id", `String turn
               ; "items", `Array []
               ; "status", `String status
               ] )
         in
         notify
           "turn/started"
           [ "threadId", `String !current; turn_json "inProgress" ];
         notify
           "item/completed"
           [ "threadId", `String !current
           ; "turnId", `String turn
           ; ( "item"
             , `Object
                 [ "type", `String "agentMessage"
                 ; "id", `String ("m" ^ turn)
                 ; "text", `String (sprintf "codex reply %d" !turns)
                 ] )
           ];
         notify
           "turn/completed"
           [ "threadId", `String !current; turn_json "completed" ]
       | Some _ | None -> ());
      loop ()
  in
  loop ()
;;

(* Claude Code answering every prompt with "claude reply N", in session
   [claude-session]. *)
let fake_claude ~mask (p : Line_transport.Peer.t) =
  let turns = ref 0 in
  let rec loop () =
    match read p with
    | None -> ()
    | Some j ->
      (match string_at [ "type" ] j with
       | Some "control_request" ->
         write
           p
           (`Object
               [ "type", `String "control_response"
               ; ( "response"
                 , `Object
                     [ "subtype", `String "success"
                     ; ( "request_id"
                       , `String
                           (Option.value
                              (string_at [ "request_id" ] j)
                              ~default:"") )
                     ; "response", `Object [ "models", `Array [] ]
                     ] )
               ])
       | Some "user" ->
         incr turns;
         (match member [ "message"; "content" ] j with
          | Some (`Array (first :: _)) ->
            show_prompt
              mask
              ~name:"claude"
              (Option.value (string_at [ "text" ] first) ~default:"")
          | _ -> ());
         let text = sprintf "claude reply %d" !turns in
         List.iter
           [ {|{"type":"system","subtype":"init","session_id":"claude-session","model":"claude-sonnet-4-6","cwd":"/x","tools":[],"mcp_servers":[],"permissionMode":"default","apiKeySource":"none","slash_commands":[]}|}
           ; sprintf
               {|{"type":"assistant","message":{"model":"claude-sonnet-4-6","id":"msg_%d","type":"message","role":"assistant","content":[{"type":"text","text":"%s"}],"stop_reason":null,"usage":{"input_tokens":3,"output_tokens":2}},"parent_tool_use_id":null,"session_id":"claude-session","uuid":"u%d"}|}
               !turns
               text
               !turns
           ; sprintf
               {|{"type":"result","subtype":"success","is_error":false,"num_turns":1,"result":"%s","stop_reason":"end_turn","session_id":"claude-session","total_cost_usd":0,"usage":{"input_tokens":3,"output_tokens":2}}|}
               text
           ]
           ~f:(Line_transport.Peer.write_line p)
       | Some _ | None -> ());
      loop ()
  in
  loop ()
;;

(* What the session recorded of its external agents. *)
let show_entries session =
  let session = Or_error.ok_exn (Session.load (Session.path session)) in
  List.iter (Session.active_path session) ~f:(fun (e : Session.Entry.t) ->
    match e.payload with
    | Engine { engine; adapter } ->
      printf
        "engine %s%s\n"
        engine
        (Option.value_map adapter ~default:"" ~f:(fun a -> " " ^ a))
    | Thread { adapter; thread_id; _ } ->
      printf "thread %s %s\n" adapter thread_id
    | External_turn { thread_id; turn_id; _ } ->
      printf "external turn %s %s\n" thread_id turn_id
    | Handoff { from_adapter; to_adapter; _ } ->
      printf
        "handoff %s -> %s\n"
        (Option.value from_adapter ~default:"-")
        to_adapter
    | Message (User u) ->
      printf "user %S\n" (List.last_exn (String.split_lines u.text))
    | Message (Assistant a) ->
      printf "assistant %S\n" (Message.Assistant.text a)
    | _ -> ())
;;

let composer =
  Or_error.ok_exn
    (Engines.create
       ~engines:[ Harness; Composer ]
       ~adapters:[ Codex; Claude ]
       ())
;;

let with_server f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let sessions_dir = Filename.concat t.dir "sessions" in
  let provider = Faux_provider.create [] in
  let latest = ref None in
  let new_agent ?session ~cwd () =
    let agent =
      Agent.create
        ~env:t.env
        ~sw
        ~provider
        ~tools:Tools.interaction
        ~sessions_dir
        ~home:t.dir
        ?session
        ~backend_host:true
        ~cwd
        ()
    in
    latest := Some agent;
    agent
  in
  let mask s =
    mask t s
    |> String.substr_replace_all
         ~pattern:
           (Option.value_map !latest ~default:"<no agent>" ~f:Agent.active_host)
         ~with_:"<host>"
  in
  let refuse_resume = ref false in
  let spawn_external _agent ~host:_ (launch : Adapter.Launch.t) =
    printf "spawn %s\n" (String.concat ~sep:" " launch.argv |> mask);
    Ok
      (Line_transport.Peer.spawn
         ~sw
         (match launch.argv with
          | "codex" :: _ -> fake_codex ~mask ~refuse_resume
          | _ -> fake_claude ~mask))
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
      ~engines:composer
      ~spawn_external
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
  let agent () = Option.value_exn !latest in
  let prompt text =
    (match
       call ~params:(Json.to_string (`Object [ "text", `String text ])) "prompt"
     with
     | `Object fields when List.Assoc.mem fields "error" ~equal:String.equal ->
       print_endline (Json.to_string (`Object fields))
     | _ -> ());
    Agent.wait_idle (agent ())
  in
  (* The session as a backend that starts again would have it. *)
  let reload () =
    let session =
      Or_error.ok_exn (Session.load (Session.path (Agent.session (agent ()))))
    in
    new_agent ~session ~cwd:t.dir ()
  in
  f ~call ~prompt ~agent ~refuse_resume ~reload;
  Rpc_server.shutdown server
;;

let print_result json =
  match json with
  | `Object fields ->
    (match List.Assoc.find fields "error" ~equal:String.equal with
     | Some e -> printf "error: %s\n" (Json.to_string e)
     | None ->
       let state =
         Option.bind
           (List.Assoc.find fields "result" ~equal:String.equal)
           ~f:(fun r ->
             match Json.member "state" r with
             | Some s -> Some s
             | None -> Some r)
       in
       printf
         "ok: engine %s, adapter %s\n"
         (Option.value
            (Option.bind state ~f:(string_at [ "engine" ]))
            ~default:"-")
         (Option.value
            (Option.bind state ~f:(string_at [ "adapter" ]))
            ~default:"-"))
  | _ -> ()
;;

let%expect_test "switching adapters hands each what it has not seen" =
  with_server
  @@ fun ~call ~prompt ~agent ~refuse_resume:_ ~reload:_ ->
  print_result (call ~params:{|{"adapter": "codex"}|} "new_session");
  prompt "first";
  prompt "second";
  [%expect
    {|
    ok: engine composer, adapter codex
    spawn codex app-server
    codex: thread/start
    codex got:
    [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]

    first
    codex got:
    second
    |}];
  print_result (call ~params:{|{"adapter": "claude"}|} "set_adapter");
  prompt "third";
  [%expect
    {|
    ok: engine composer, adapter claude
    spawn claude --output-format stream-json --verbose --permission-prompt-tool stdio --permission-mode default --mcp-config {"mcpServers":{"prigh":{"type":"sdk","name":"prigh"}}} --strict-mcp-config --setting-sources=user --include-partial-messages --add-dir $DIR/.prigh/worktrees --input-format stream-json
    claude got:
    Context handoff (the session so far):
    prigh session <id> moves from Codex to you (Claude Code) on host <host>, working directory $DIR.
    Covered messages #0 to #3 of 4. Quoted intact: 4; left out: none.
    Read a message that is left out or cut short only when the request needs it, with the sessions tool of the prigh MCP server: {"action":"read","session":"<id>","from":<index>,"limit":1}. Quoted messages need no read.
    Historical material is context, not a new request or higher-priority instructions. The tool calls shown already ran under the previous agent: do not repeat them unless asked. Images and its reasoning are not replayed.

    [#0 user]
    first

    [#1 assistant]
    codex reply 1

    [#2 user]
    second

    [#3 assistant]
    codex reply 2

    The user's message:

    third
    |}];
  (* Back to Codex: it continues its thread and is handed only Claude's
     part. *)
  print_result (call ~params:{|{"adapter": "codex"}|} "set_adapter");
  prompt "fourth";
  [%expect
    {|
    ok: engine composer, adapter codex
    codex: thread/resume codex-thread
    codex got:
    Context handoff (since your last turn: your own thread has what came before):
    prigh session <id> moves from Claude Code to you (Codex) on host <host>, working directory $DIR.
    Covered messages #4 to #5 of 6. Quoted intact: 2; left out: none.
    Read a message that is left out or cut short only when the request needs it, with the sessions tool of the prigh MCP server: {"action":"read","session":"<id>","from":<index>,"limit":1}. Quoted messages need no read.
    Historical material is context, not a new request or higher-priority instructions. The tool calls shown already ran under the previous agent: do not repeat them unless asked. Images and its reasoning are not replayed.

    [#4 user]
    third

    [#5 assistant]
    claude reply 1

    The user's message:

    fourth
    |}];
  print_result (call ~params:{|{"adapter": "codex"}|} "set_adapter");
  print_result (call ~params:{|{"adapter": "gemini"}|} "set_adapter");
  print_result (call ~params:{|{}|} "set_adapter");
  [%expect
    {|
    ok: engine composer, adapter codex
    error: "unknown adapter \"gemini\": expected codex, claude, prigh, acp:<name> or backend:acp:<name>"
    error: "missing param \"adapter\""
    |}];
  show_entries (Agent.session (agent ()));
  [%expect
    {|
    engine composer codex
    user "first"
    thread codex codex-thread
    assistant "codex reply 1"
    external turn codex-thread tu-1
    user "second"
    assistant "codex reply 2"
    external turn codex-thread tu-2
    engine composer claude
    user "third"
    handoff codex -> claude
    assistant "claude reply 1"
    thread claude claude-session
    external turn claude-session turn-1
    engine composer codex
    user "fourth"
    handoff claude -> codex
    assistant "codex reply 3"
    external turn codex-thread tu-3
    |}]
;;

let%expect_test "a thread that cannot be continued gets the whole session" =
  with_server
  @@ fun ~call ~prompt ~agent:_ ~refuse_resume ~reload:_ ->
  print_result (call ~params:{|{"adapter": "codex"}|} "new_session");
  prompt "first";
  print_result (call ~params:{|{"adapter": "claude"}|} "set_adapter");
  prompt "second";
  refuse_resume := true;
  print_result (call ~params:{|{"adapter": "codex"}|} "set_adapter");
  prompt "third";
  [%expect
    {|
    ok: engine composer, adapter codex
    spawn codex app-server
    codex: thread/start
    codex got:
    [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]

    first
    ok: engine composer, adapter claude
    spawn claude --output-format stream-json --verbose --permission-prompt-tool stdio --permission-mode default --mcp-config {"mcpServers":{"prigh":{"type":"sdk","name":"prigh"}}} --strict-mcp-config --setting-sources=user --include-partial-messages --add-dir $DIR/.prigh/worktrees --input-format stream-json
    claude got:
    Context handoff (the session so far):
    prigh session <id> moves from Codex to you (Claude Code) on host <host>, working directory $DIR.
    Covered messages #0 to #1 of 2. Quoted intact: 2; left out: none.
    Read a message that is left out or cut short only when the request needs it, with the sessions tool of the prigh MCP server: {"action":"read","session":"<id>","from":<index>,"limit":1}. Quoted messages need no read.
    Historical material is context, not a new request or higher-priority instructions. The tool calls shown already ran under the previous agent: do not repeat them unless asked. Images and its reasoning are not replayed.

    [#0 user]
    first

    [#1 assistant]
    codex reply 1

    The user's message:

    second
    ok: engine composer, adapter codex
    codex: thread/resume codex-thread refused
    codex: thread/start
    codex got:
    Context handoff (the session so far):
    prigh session <id> moves from Claude Code to you (Codex) on host <host>, working directory $DIR.
    Covered messages #0 to #3 of 4. Quoted intact: 4; left out: none.
    Read a message that is left out or cut short only when the request needs it, with the sessions tool of the prigh MCP server: {"action":"read","session":"<id>","from":<index>,"limit":1}. Quoted messages need no read.
    Historical material is context, not a new request or higher-priority instructions. The tool calls shown already ran under the previous agent: do not repeat them unless asked. Images and its reasoning are not replayed.

    [#0 user]
    first

    [#1 assistant]
    codex reply 1

    [#2 user]
    second

    [#3 assistant]
    claude reply 1

    The user's message:

    third
    |}]
;;

let%expect_test
    "a fork of a Codex session forks its thread, with nothing to hand over"
  =
  with_server
  @@ fun ~call ~prompt ~agent ~refuse_resume:_ ~reload:_ ->
  print_result (call ~params:{|{"adapter": "codex"}|} "new_session");
  prompt "first";
  print_result (call "clone");
  prompt "in the fork";
  [%expect
    {|
    ok: engine composer, adapter codex
    spawn codex app-server
    codex: thread/start
    codex got:
    [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]

    first
    ok: engine composer, adapter codex
    codex: thread/fork codex-thread at tu-1
    codex got:
    in the fork
    |}];
  show_entries (Agent.session (agent ()));
  [%expect
    {|
    engine composer codex
    user "first"
    assistant "codex reply 1"
    thread codex codex-thread
    external turn codex-thread tu-1
    user "in the fork"
    external turn codex-fork tu-1
    thread codex codex-fork
    assistant "codex reply 2"
    external turn codex-fork tu-2
    |}]
;;

let%expect_test "a harness session moves to an adapter" =
  with_server
  @@ fun ~call ~prompt:_ ~agent:_ ~refuse_resume:_ ~reload:_ ->
  print_result (call "new_session");
  print_result (call ~params:{|{"adapter": "claude"}|} "set_adapter");
  [%expect
    {|
    ok: engine -, adapter -
    ok: engine composer, adapter claude
    |}]
;;

let%expect_test "moving to another tool host: a new process and thread there" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let host = ref "laptop" in
  let session = ref None in
  let composer =
    Composer_runner.create
      ~sw
      ~adapter:(module Codex_adapter)
      ~spawn:(fun (launch : Adapter.Launch.t) ->
        printf "spawn on %s: %s\n" !host (String.concat ~sep:" " launch.argv);
        Ok
          (Line_transport.Peer.spawn
             ~sw
             (fake_codex ~mask:(mask t) ~refuse_resume:(ref false))))
      ~host:(fun () -> !host)
      ~on_thread:(fun thread_id ->
        ignore
          (Session.record_thread
             (Option.value_exn !session)
             { adapter = "codex"
             ; host = !host
             ; thread_id
             ; cwd = t.dir
             ; fork_from = None
             }
           : Session.Entry.t))
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
  session := Some (Agent.session agent);
  Agent.set_engine agent { engine = Composer; adapter = Some Codex };
  let prompt text =
    Or_error.ok_exn (Agent.prompt agent text);
    Agent.wait_idle agent
  in
  prompt "here";
  host := "desktop";
  prompt "there";
  [%expect
    {|
    spawn on laptop: codex app-server
    codex: thread/start
    codex got:
    [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]

    here
    spawn on desktop: codex app-server
    codex: thread/start
    codex got:
    Context handoff (the session so far):
    prigh session <id> moves to you (Codex) on host desktop, working directory $DIR.
    It ran on host laptop until now: files, processes and anything not committed may differ here.
    Covered messages #0 to #1 of 2. Quoted intact: 2; left out: none.
    Read a message that is left out or cut short only when the request needs it, with the sessions tool of the prigh MCP server: {"action":"read","session":"<id>","from":<index>,"limit":1}. Quoted messages need no read.
    Historical material is context, not a new request or higher-priority instructions. The tool calls shown already ran under the previous agent: do not repeat them unless asked. Images and its reasoning are not replayed.

    [#0 user]
    here

    [#1 assistant]
    codex reply 1

    The user's message:

    there
    |}];
  Composer_runner.close composer
;;

let%expect_test "back to the harness engine and out again; models follow" =
  with_server
  @@ fun ~call ~prompt ~agent ~refuse_resume:_ ~reload:_ ->
  let models () =
    match call "list_models" with
    | `Object fields ->
      (match List.Assoc.find fields "result" ~equal:String.equal with
       | Some (`Array models) ->
         let keys =
           List.filter_map models ~f:(fun m ->
             match Json.member "provider" m, Json.member "id" m with
             | Some (`String p), Some (`String id) -> Some (p ^ "/" ^ id)
             | _ -> None)
         in
         printf
           "models: %s%s\n"
           (String.concat ~sep:" " (List.take keys 3))
           (if List.length keys > 3 then " ..." else "")
       | _ -> print_endline "models: ?")
    | _ -> ()
  in
  let model () = print_endline (Model.key (Agent.state (agent ())).model) in
  print_result (call ~params:{|{"adapter": "codex"}|} "new_session");
  prompt "first";
  model ();
  models ();
  print_result (call ~params:{|{"model": "nope"}|} "set_model");
  print_result (call ~params:{|{"model": "default@work"}|} "set_model");
  print_result
    (call ~params:{|{"model": "default", "account": "work"}|} "set_model");
  [%expect
    {|
    ok: engine composer, adapter codex
    spawn codex app-server
    codex: thread/start
    codex got:
    [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]

    first
    codex/gpt-5
    models: codex/default claude/default anthropic/claude-fable-5 ...
    error: "Codex has no model \"nope\": pick one of default (/model lists them)"
    error: "Codex uses its own login, not prigh's: pick default without @LOGIN"
    error: "Codex uses its own login, not prigh's: pick the model without @LOGIN"
    |}];
  print_result (call ~params:{|{"adapter": "harness"}|} "set_adapter");
  model ();
  models ();
  [%expect
    {|
    ok: engine -, adapter -
    deepseek/deepseek-flash
    models: anthropic/claude-fable-5 anthropic/claude-fable-5-1 anthropic/claude-fable-5-2 ...
    |}];
  print_result (call ~params:{|{"adapter": "codex"}|} "set_adapter");
  model ();
  [%expect
    {|
    ok: engine composer, adapter codex
    codex/gpt-5
    |}]
;;

let%expect_test "back from an agent CLI, the session has its login again" =
  with_server
  @@ fun ~call ~prompt:_ ~agent ~refuse_resume:_ ~reload ->
  let show agent =
    let state = Agent.state agent in
    printf
      "%s%s\n"
      (Model.key state.model)
      (Option.value_map state.account ~default:"" ~f:(fun a ->
         "@" ^ Account_ref.to_string a))
  in
  print_result (call "new_session");
  Agent.set_model
    ~account:(Or_error.ok_exn (Account_ref.of_string "work"))
    (agent ())
    (Agent.state (agent ())).model;
  show (agent ());
  print_result (call ~params:{|{"adapter": "codex"}|} "set_adapter");
  show (agent ());
  print_result (call ~params:{|{"adapter": "harness"}|} "set_adapter");
  show (agent ());
  (* A session is written once it has a message. *)
  ignore
    (Session.append_message (Agent.session (agent ())) (Message.user "hi")
     : Session.Entry.t);
  show (reload ());
  [%expect
    {|
    ok: engine -, adapter -
    deepseek/deepseek-flash@work
    ok: engine composer, adapter codex
    codex/default
    ok: engine -, adapter -
    deepseek/deepseek-flash@work
    deepseek/deepseek-flash@work
    |}]
;;

let%expect_test
    "backend:acp:antigravity appears in /model and switches seamlessly"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let sessions_dir = Filename.concat t.dir "sessions" in
  let engines =
    Or_error.ok_exn
      (Engines.create
         ~engines:[ Harness; Composer ]
         ~adapters:[ Backend (Acp "antigravity") ]
         ())
  in
  let provider = Faux_provider.create [] in
  let latest = ref None in
  let new_agent ?session ~cwd () =
    let agent =
      Agent.create
        ~env:t.env
        ~sw
        ~provider
        ~tools:Tools.interaction
        ~sessions_dir
        ~home:t.dir
        ?session
        ~backend_host:true
        ~cwd
        ()
    in
    latest := Some agent;
    agent
  in
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
             (Json.parse
                {|{"protocolVersion":1,"agentCapabilities":{"loadSession":true,"sessionCapabilities":{"close":{}},"_meta":{"antigravity":{"systemPrompt":{"append":true}}}},"authMethods":[]}|}
              |> Or_error.ok_exn)
         | Some "session/new" ->
           reply
             j
             (Json.parse
                {|{"sessionId":"s-agy","models":{"currentModelId":"gemini-2.5-pro","availableModels":[{"modelId":"gemini-2.5-pro","name":"Gemini 2.5 Pro"},{"modelId":"claude-sonnet-4-5","name":"Claude Sonnet 4.5"}]}}|}
              |> Or_error.ok_exn)
         | Some "session/close" -> reply j (`Object [])
         | Some _ | None -> ());
        loop ()
    in
    loop ()
  in
  let spawned_hosts = ref [] in
  let spawn_external _agent ~host (_launch : Adapter.Launch.t) =
    spawned_hosts := !spawned_hosts @ [ host ];
    Ok (Line_transport.Peer.spawn ~sw fake_agy)
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
      ~spawn_external
      ~new_agent
      ()
  in
  let models_loaded, set_models_loaded = Eio.Promise.create () in
  let client =
    Rpc_server.connect server ~send:(fun ev ->
      match string_at [ "event" ] ev with
      | Some "models_changed" when not (Eio.Promise.is_resolved models_loaded)
        -> Eio.Promise.resolve set_models_loaded ()
      | _ -> ())
  in
  let call ?(params = "{}") meth =
    Rpc_server.handle
      server
      client
      (Json.of_string
         (sprintf {|{"id": "r", "method": "%s", "params": %s}|} meth params))
  in
  let show_state () =
    let st = Agent.state (Option.value_exn !latest) in
    printf
      "engine=%s adapter=%s model=%s\n"
      (Engine_id.to_string st.engine.engine)
      (Option.value_map st.engine.adapter ~default:"-" ~f:Adapter_id.to_string)
      (Model.key st.model)
  in
  let has_backend_models () =
    match call "list_models" with
    | `Object fields ->
      (match List.Assoc.find fields "result" ~equal:String.equal with
       | Some (`Array models) ->
         let backend =
           List.filter_map models ~f:(fun m ->
             match Json.member "provider" m, Json.member "id" m with
             | Some (`String p), Some (`String id)
               when String.is_prefix p ~prefix:"backend:" -> Some (p ^ "/" ^ id)
             | _ -> None)
         in
         let harness =
           List.count models ~f:(fun m ->
             match Json.member "provider" m with
             | Some (`String p) -> not (String.is_prefix p ~prefix:"backend:")
             | _ -> false)
         in
         printf
           "list_models: harness=%b backend=[%s]\n"
           (harness > 0)
           (String.concat ~sep:", " backend)
       | _ -> ())
    | _ -> ()
  in
  print_result (call "new_session");
  show_state ();
  (* First list_models triggers the background catalog fetch on backend; wait
     deterministically for models_changed so the real model list is loaded. *)
  ignore (call "list_models" : Json.t);
  Eio.Promise.await models_loaded;
  printf "spawned hosts=[%s]\n" (String.concat ~sep:", " !spawned_hosts);
  has_backend_models ();
  (* Switch directly from harness to backend:acp:antigravity via set_model! *)
  print_result
    (call
       ~params:{|{"model": "backend:acp:antigravity/gemini-2.5-pro"}|}
       "set_model");
  show_state ();
  (* On backend:acp:antigravity, list_models still offers harness models too. *)
  has_backend_models ();
  (* Switch directly back to harness via set_model! *)
  print_result
    (call ~params:{|{"model": "deepseek/deepseek-flash"}|} "set_model");
  show_state ();
  Rpc_server.shutdown server;
  [%expect
    {|
    ok: engine -, adapter -
    engine=harness adapter=- model=deepseek/deepseek-flash
    spawned hosts=[backend]
    list_models: harness=true backend=[backend:acp:antigravity/gemini-2.5-pro, backend:acp:antigravity/claude-sonnet-4-5]
    ok: engine -, adapter -
    engine=composer adapter=backend:acp:antigravity model=backend:acp:antigravity/gemini-2.5-pro
    list_models: harness=true backend=[backend:acp:antigravity/gemini-2.5-pro, backend:acp:antigravity/claude-sonnet-4-5]
    ok: engine -, adapter -
    engine=harness adapter=- model=deepseek/deepseek-flash
    |}]
;;

let%expect_test
    "backend:acp:antigravity multi-turn integration: remote tool hosts, no \
     duplicate handoff, hashed MCP tools & output"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let mac_dir = Filename.concat t.dir "mac-proj" in
  let linux_dir = Filename.concat t.dir "linux-proj" in
  Core_unix.mkdir_p mac_dir;
  Core_unix.mkdir_p linux_dir;
  Tool_test_helpers.write t "mac-proj/mac-only.txt" "hello from mac\n";
  Tool_test_helpers.write t "linux-proj/linux-only.txt" "hello from linux\n";
  let sessions_dir = Filename.concat t.dir "sessions" in
  let engines =
    Or_error.ok_exn
      (Engines.create
         ~engines:[ Harness; Composer ]
         ~adapters:[ Backend (Acp "antigravity") ]
         ())
  in
  let provider =
    Faux_provider.create [ Faux_provider.Reply.text "from harness" ]
  in
  let latest = ref None in
  let new_agent ?session ~cwd () =
    let agent =
      Agent.create
        ~env:t.env
        ~sw
        ~provider
        ~tools:(Tools.all @ Tools.interaction)
        ~sessions_dir
        ~home:t.dir
        ?session
        ~backend_host:true
        ~cwd
        ()
    in
    Agent.set_mode agent ~approval:Auto;
    latest := Some agent;
    agent
  in
  let spawns = ref 0 in
  let routes : Injected_tools.t String.Table.t = String.Table.create () in
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
    let session_routes : string String.Table.t = String.Table.create () in
    let turn = ref 0 in
    let rec loop () =
      match read p with
      | None -> ()
      | Some j ->
        (match string_at [ "method" ] j with
         | Some "initialize" ->
           reply
             j
             (Json.parse
                {|{"protocolVersion":1,"agentCapabilities":{"loadSession":true,"sessionCapabilities":{"close":{}},"_meta":{"antigravity":{"systemPrompt":{"append":true}}}},"authMethods":[]}|}
              |> Or_error.ok_exn)
         | Some (("session/new" | "session/load") as meth) ->
           (* Capture this session's MCP route token from mcpServers[0].env. *)
           let route_id =
             Option.bind (Json.member "params" j) ~f:(fun params ->
               match Json.member "mcpServers" params with
               | Some (`Array (srv :: _)) ->
                 (match Json.member "env" srv with
                  | Some (`Array env) ->
                    List.find_map env ~f:(fun e ->
                      match string_at [ "name" ] e, string_at [ "value" ] e with
                      | Some "PRIGH_MCP_TOKEN", Some v ->
                        Option.map (String.lsplit2 v ~on:':') ~f:snd
                      | _ -> None)
                  | _ -> None)
               | _ -> None)
           in
           let sid =
             if String.equal meth "session/load"
             then
               string_at [ "params"; "sessionId" ] j
               |> Option.value ~default:"s-agy-mt"
             else "s-agy-mt"
           in
           Option.iter route_id ~f:(fun r ->
             Hashtbl.set session_routes ~key:sid ~data:r);
           reply
             j
             (Json.parse
                {|{"sessionId":"s-agy-mt","models":{"currentModelId":"gemini-2.5-pro","availableModels":[{"modelId":"gemini-2.5-pro","name":"Gemini 2.5 Pro"}]}}|}
              |> Or_error.ok_exn)
         | Some "session/prompt" ->
           incr turn;
           let n = !turn in
           let sid =
             string_at [ "params"; "sessionId" ] j |> Option.value ~default:""
           in
           let prompt_text =
             match Json.member "params" j with
             | Some params ->
               (match Json.member "prompt" params with
                | Some (`Array (first :: _)) ->
                  string_at [ "text" ] first |> Option.value ~default:""
                | _ -> "")
             | None -> ""
           in
           let has_handoff =
             String.is_substring prompt_text ~substring:"Context handoff"
           in
           let handoff_kind =
             if
               String.is_substring
                 prompt_text
                 ~substring:"Context handoff (the session so far):"
             then "Full"
             else if
               String.is_substring
                 prompt_text
                 ~substring:"Context handoff (since your last turn"
             then "Since"
             else "none"
           in
           let has_host_note =
             String.is_substring prompt_text ~substring:"now runs on linux"
             || String.is_substring prompt_text ~substring:"Tool host"
             || String.is_substring prompt_text ~substring:"host linux"
             || String.is_substring prompt_text ~substring:"linux-proj"
           in
           let has_harness_msg =
             String.is_substring prompt_text ~substring:"from harness"
           in
           let has_turn1_msg =
             String.is_substring prompt_text ~substring:"Please ls the cwd"
           in
           printf
             "agy turn %d: handoff=%s (any=%b host_note=%b harness=%b turn1=%b)\n"
             n
             handoff_kind
             has_handoff
             has_host_note
             has_harness_msg
             has_turn1_msg;
           if n = 1 || n = 3
           then (
             let call_id = sprintf "tc-%d" n in
             write
               p
               (`Object
                   [ "jsonrpc", `String "2.0"
                   ; "method", `String "session/update"
                   ; ( "params"
                     , `Object
                         [ "sessionId", `String sid
                         ; ( "update"
                           , `Object
                               [ "sessionUpdate", `String "tool_call"
                               ; "toolCallId", `String call_id
                               ; "kind", `String "other"
                               ; "title", `String "mcp_prigh_79ce654a_ls"
                               ; "status", `String "pending"
                               ; "rawInput", `Object []
                               ] )
                         ] )
                   ]);
             let route = Hashtbl.find_exn session_routes sid in
             let injected = Hashtbl.find_exn routes route in
             let mcp_reply =
               Injected_tools.handle_line
                 injected
                 {|{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"ls","arguments":{}}}|}
               |> Option.value_exn
             in
             let mcp_text =
               match Json.parse mcp_reply with
               | Ok r ->
                 (match Json.member "result" r with
                  | Some res ->
                    (match Json.member "content" res with
                     | Some (`Array (blk :: _)) ->
                       string_at [ "text" ] blk |> Option.value ~default:""
                     | _ -> "")
                  | None -> "")
               | Error _ -> ""
             in
             printf "agy turn %d mcp ls -> %S\n" n (String.strip mcp_text);
             (* Antigravity omits tool output on completed MCP tool_call_update. *)
             write
               p
               (`Object
                   [ "jsonrpc", `String "2.0"
                   ; "method", `String "session/update"
                   ; ( "params"
                     , `Object
                         [ "sessionId", `String sid
                         ; ( "update"
                           , `Object
                               [ "sessionUpdate", `String "tool_call_update"
                               ; "toolCallId", `String call_id
                               ; "status", `String "completed"
                               ] )
                         ] )
                   ]));
           write
             p
             (`Object
                 [ "jsonrpc", `String "2.0"
                 ; "method", `String "session/update"
                 ; ( "params"
                   , `Object
                       [ "sessionId", `String sid
                       ; ( "update"
                         , `Object
                             [ "sessionUpdate", `String "agent_message_chunk"
                             ; ( "content"
                               , `Object
                                   [ "type", `String "text"
                                   ; "text", `String (sprintf "agy-reply-%d" n)
                                   ] )
                             ] )
                       ] )
                 ]);
           reply j (`Object [ "stopReason", `String "end_turn" ])
         | Some "session/close" -> reply j (`Object [])
         | Some _ | None -> ());
        loop ()
    in
    loop ()
  in
  let spawn_external _agent ~host (_launch : Adapter.Launch.t) =
    incr spawns;
    printf "spawn #%d on %s\n" !spawns host;
    let inner = Line_transport.Peer.spawn ~sw fake_agy in
    let base_bridge =
      Injected_tools.Bridge.create
        ~prigh:"prigh"
        ~socket:"/tmp/test.sock"
        ~token:"tok"
    in
    let mcp : Line_transport.Mcp.t =
      { bridge = base_bridge
      ; serve = ignore
      ; route =
          Some
            (fun route injected ->
              Hashtbl.set routes ~key:route ~data:injected;
              ( Injected_tools.Bridge.with_route base_bridge ~route
              , fun () -> Hashtbl.remove routes route ))
      }
    in
    Ok
      (Line_transport.create
         ~read_line:(fun () -> Line_transport.read_line inner)
         ~write_line:(fun line -> Line_transport.write_line inner line)
         ~stop:(fun () -> Line_transport.stop inner)
         ~ended:(fun () -> Line_transport.ended inner)
         ~is_ended:(fun () -> Line_transport.is_ended inner)
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
      ~engines
      ~spawn_external
      ~new_agent
      ()
  in
  let connect_tool_host ~id ~name ~cwd =
    let stream = Eio.Stream.create Int.max_value in
    let client = Rpc_server.connect server ~send:(Eio.Stream.add stream) in
    ignore
      (Rpc_server.handle
         server
         client
         (Json.of_string
            (sprintf
               {|{"id":"h","method":"hello","params":{"name":%S,"tools":true,"host_id":%S,"cwd":%S,"home":%S}}|}
               name
               id
               cwd
               t.dir))
       : Json.t);
    Eio.Fiber.fork_daemon ~sw (fun () ->
      let rec go () =
        let ev = Eio.Stream.take stream in
        if
          Option.equal
            String.equal
            (string_at [ "event" ] ev)
            (Some "tool_exec")
        then (
          let tool_name = string_at [ "name" ] ev |> Option.value ~default:"" in
          let exec_cwd = string_at [ "cwd" ] ev |> Option.value ~default:cwd in
          let arguments =
            Option.value (Json.member "arguments" ev) ~default:(`Object [])
          in
          let result : Tool.Result.t =
            if String.equal tool_name Host_ops.instructions_op
            then Tool.Result.ok "[]"
            else if String.is_prefix tool_name ~prefix:"$"
            then
              Host_ops.execute
                ~mcp:None
                ~env:t.env
                ~cancel:Cancellation.never
                ~on_output:ignore
                ~cwd:exec_cwd
                ~name:tool_name
                ~arguments
                ()
            else (
              match Tools.find tool_name with
              | Some tool ->
                let ctx =
                  Tool.Context.create
                    ~env:t.env
                    ~cwd:exec_cwd
                    ~home:(fun () -> Some t.dir)
                    ()
                in
                tool.run ctx arguments
              | None -> Tool.Result.error ("unknown tool: " ^ tool_name))
          in
          ignore
            (Rpc_server.handle
               server
               client
               (`Object
                   [ "id", `Number "0"
                   ; "method", `String "tool_exec_result"
                   ; ( "params"
                     , `Object
                         [ ( "exec_id"
                           , `String
                               (string_at [ "exec_id" ] ev
                                |> Option.value ~default:"") )
                         ; "text", `String result.text
                         ; ( "is_error"
                           , if result.is_error then `True else `False )
                         ] )
                   ])
             : Json.t));
        go ()
      in
      go ())
  in
  connect_tool_host ~id:"host-mac" ~name:"mac" ~cwd:mac_dir;
  connect_tool_host ~id:"host-linux" ~name:"linux" ~cwd:linux_dir;
  let models_loaded, set_models_loaded = Eio.Promise.create () in
  let client =
    Rpc_server.connect server ~send:(fun ev ->
      match string_at [ "event" ] ev with
      | Some "models_changed" when not (Eio.Promise.is_resolved models_loaded)
        -> Eio.Promise.resolve set_models_loaded ()
      | _ -> ())
  in
  let call ?(params = "{}") meth =
    let resp =
      Rpc_server.handle
        server
        client
        (Json.of_string
           (sprintf {|{"id": "r", "method": "%s", "params": %s}|} meth params))
    in
    (match Json.member "ok" resp with
     | Some `True -> ()
     | _ -> failwithf "RPC %s failed: %s" meth (Json.to_string resp) ());
    resp
  in
  let send_prompt text =
    ignore (call ~params:(sprintf {|{"text": %S}|} text) "prompt" : Json.t);
    Agent.wait_idle (Option.value_exn !latest)
  in
  ignore (call "new_session" : Json.t);
  ignore (call "list_models" : Json.t);
  Eio.Promise.await models_loaded;
  ignore
    (call
       ~params:(sprintf {|{"host": "host-mac", "cwd": %S}|} mac_dir)
       "set_active_host"
     : Json.t);
  ignore
    (call
       ~params:{|{"model": "backend:acp:antigravity/gemini-2.5-pro"}|}
       "set_model"
     : Json.t);
  (* Turn 1 on host-mac: no handoff, ls runs on host-mac, output preserved. *)
  send_prompt "Please ls the cwd";
  (* Turn 2 on same host-mac: NO handoff! *)
  send_prompt "Follow up on mac";
  (* Turn 3 after switching active host to host-linux: compact Since note only. *)
  ignore
    (call
       ~params:(sprintf {|{"host": "host-linux", "cwd": %S}|} linux_dir)
       "set_active_host"
     : Json.t);
  send_prompt "Now ls linux";
  (* Turn 4 on same host-linux: NO handoff! *)
  send_prompt "Follow up on linux";
  (* Switch to harness for 1 turn, then back to backend:acp:antigravity. *)
  ignore
    (call ~params:{|{"model": "deepseek/deepseek-flash"}|} "set_model" : Json.t);
  send_prompt "Harness turn";
  ignore
    (call
       ~params:{|{"model": "backend:acp:antigravity/gemini-2.5-pro"}|}
       "set_model"
     : Json.t);
  send_prompt "Back to agy";
  (* Inspect all recorded tool results in the session. *)
  List.iter
    (Agent.messages (Option.value_exn !latest))
    ~f:(function
      | Tool_result r ->
        printf
          "recorded tool_result: name=%s err=%b text=%S\n"
          r.tool_name
          r.is_error
          (String.strip r.text)
      | _ -> ());
  printf "total spawns=%d\n" !spawns;
  Rpc_server.shutdown server;
  [%expect
    {|
    spawn #1 on backend
    agy turn 1: handoff=none (any=false host_note=false harness=false turn1=true)
    agy turn 1 mcp ls -> "mac-only.txt"
    agy turn 2: handoff=none (any=false host_note=false harness=false turn1=false)
    agy turn 3: handoff=none (any=false host_note=true harness=false turn1=false)
    agy turn 3 mcp ls -> "linux-only.txt"
    agy turn 4: handoff=none (any=false host_note=false harness=false turn1=false)
    agy turn 5: handoff=Since (any=true host_note=true harness=true turn1=false)
    recorded tool_result: name=ls err=false text="mac-only.txt"
    recorded tool_result: name=ls err=false text="linux-only.txt"
    total spawns=1
    |}]
;;
