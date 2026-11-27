open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

(* Rewinding and forking composer sessions: the files go back to their
   checkpoint before the turns dropped, and the agent's own conversation is
   rolled back (Codex: thread/revert) or resumed at a message (Claude Code:
   --resume-session-at); forks fork the agent's thread. The harnesses are
   scripted peers that edit the files of a real git repository. *)

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

(* What each turn does to the files, in order, across processes. *)
let edits : (unit -> unit) Queue.t = Queue.create ()
let run_edit () = Option.iter (Queue.dequeue edits) ~f:(fun f -> f ())

(* A Codex app-server whose turns run the next of [edits] and answer
   "codex reply N" (N counting across its processes). *)
let fake_codex ~turns (p : Line_transport.Peer.t) =
  let reply j result =
    write
      p
      (`Object [ "id", Option.value_exn (Json.member "id" j); "result", result ])
  in
  let thread = ref "codex-thread" in
  let thread_reply j =
    reply
      j
      (`Object
          [ "thread", `Object [ "id", `String !thread ]
          ; "model", `String "gpt-5"
          ])
  in
  let rec loop () =
    match read p with
    | None -> ()
    | Some j ->
      (match string_at [ "method" ] j with
       | Some "initialize" ->
         reply j (`Object [ "userAgent", `String "codex/0.166.0" ])
       | Some "thread/start" ->
         print_endline "codex: thread/start";
         thread_reply j
       | Some "thread/resume" ->
         printf
           "codex: thread/resume %s\n"
           (Option.value_exn (string_at [ "params"; "threadId" ] j));
         thread_reply j
       | Some "thread/fork" ->
         printf
           "codex: thread/fork %s at %s\n"
           (Option.value_exn (string_at [ "params"; "threadId" ] j))
           (Option.value
              (string_at [ "params"; "lastTurnId" ] j)
              ~default:"its end");
         thread := "codex-fork";
         thread_reply j
       | Some "thread/revert" ->
         printf
           "codex: thread/revert %s before %s\n"
           (Option.value_exn (string_at [ "params"; "threadId" ] j))
           (Option.value_exn (string_at [ "params"; "beforeTurnId" ] j));
         reply j (`Object [])
       | Some "turn/start" ->
         incr turns;
         let turn = sprintf "tu-%d" !turns in
         (match member [ "params"; "input" ] j with
          | Some (`Array (first :: _)) ->
            printf
              "codex got: %S\n"
              (List.last_exn
                 (String.split_lines
                    (Option.value (string_at [ "text" ] first) ~default:"")))
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
           [ "threadId", `String !thread; turn_json "inProgress" ];
         run_edit ();
         notify
           "item/completed"
           [ "threadId", `String !thread
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
           [ "threadId", `String !thread; turn_json "completed" ]
       | Some _ | None -> ());
      loop ()
  in
  loop ()
;;

(* Claude Code answering "claude reply N", its messages' uuids u1, u2, ... *)
let fake_claude ~turns (p : Line_transport.Peer.t) =
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
         run_edit ();
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

let composer =
  Or_error.ok_exn
    (Engines.create
       ~engines:[ Harness; Composer ]
       ~adapters:[ Codex; Claude ]
       ())
;;

let git_env =
  "export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1 GIT_AUTHOR_NAME=t \
   GIT_AUTHOR_EMAIL=t@t GIT_COMMITTER_NAME=t GIT_COMMITTER_EMAIL=t@t"
;;

let sh dir script =
  match
    Sys_unix.command
      (sprintf
         "cd %s && %s && (%s) > /dev/null 2>&1"
         (Filename.quote dir)
         git_env
         script)
  with
  | 0 -> ()
  | n -> raise_s [%message "setup failed" (script : string) (n : int)]
;;

module Server = struct
  type t =
    { call : ?params:string -> string -> Json.t
    ; prompt : string -> unit
    ; agent : unit -> Agent.t
    ; dir : string (* the session's working directory *)
    ; mask : string -> string
    }
end

let with_server ?(git = true) f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  Queue.clear edits;
  let dir = Filename.concat t.dir "work" in
  Core_unix.mkdir_p dir;
  if git
  then
    sh
      dir
      "git init -q && echo one > a.txt && echo bee > b.txt && git add -A && \
       git commit -qm init";
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
  let mask = mask t in
  let turns = ref 0 in
  let spawn_external _agent ~host:_ (launch : Adapter.Launch.t) =
    printf "spawn %s\n" (String.concat ~sep:" " launch.argv |> mask);
    Ok
      (Line_transport.Peer.spawn
         ~sw
         (match launch.argv with
          | "codex" :: _ -> fake_codex ~turns
          | _ -> fake_claude ~turns))
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
      ~cwd:dir
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
  f { Server.call; prompt; agent; dir; mask };
  Rpc_server.shutdown server
;;

let print_result (s : Server.t) json =
  match json with
  | `Object fields ->
    (match List.Assoc.find fields "error" ~equal:String.equal with
     | Some e ->
       printf
         "error: %s\n"
         (s.mask
            (match e with
             | `String m -> m
             | e ->
               Option.value
                 (Option.bind (Json.member "message" e) ~f:Json.string)
                 ~default:(Json.to_string e)))
     | None -> print_endline "ok")
  | _ -> ()
;;

let files (s : Server.t) =
  let r =
    Process.run_collect
      ~env:(Agent.env (s.agent ()))
      ~cwd:s.dir
      ~prog:"sh"
      ~args:
        [ "-c"
        ; "find . -path ./.git -prune -o -type f -print | sort | while read f; \
           do printf '%s: %s\\n' \"$f\" \"$(cat \"$f\")\"; done"
        ]
      ()
  in
  print_string r.stdout
;;

let entry_of (s : Server.t) ~user =
  let session = Agent.session (s.agent ()) in
  List.find_map_exn
    (Session.active_path session)
    ~f:(fun (e : Session.Entry.t) ->
      match e.payload with
      | Message (User u) when String.is_suffix u.text ~suffix:user -> Some e.id
      | _ -> None)
;;

let rewind (s : Server.t) ~to_user =
  print_result
    s
    (s.call
       ~params:
         (Json.to_string (`Object [ "to", `String (entry_of s ~user:to_user) ]))
       "rewind")
;;

let show_turns (s : Server.t) =
  List.iter
    (Session.external_turns (Agent.session (s.agent ())))
    ~f:(fun (run : Session.External_turn.t) ->
      printf
        "external turn %s %s%s, files %s\n"
        run.thread_id
        run.turn_id
        (Option.value_map run.mark ~default:"" ~f:(fun m -> " at " ^ m))
        (match run.before, run.after with
         | Some _, Some _ -> "checkpointed"
         | _ -> "not checkpointed"))
;;

let in_dir (s : Server.t) file f = f (Filename.concat s.dir file)

let%expect_test "codex: rewinding puts the files back and reverts its thread" =
  with_server
  @@ fun s ->
  Queue.enqueue edits (fun () ->
    in_dir s "a.txt" (Out_channel.write_all ~data:"two\n");
    in_dir s "new.txt" (Out_channel.write_all ~data:"made by turn 1\n"));
  Queue.enqueue edits (fun () ->
    in_dir s "b.txt" Core_unix.unlink;
    in_dir s "a.txt" (Out_channel.write_all ~data:"three\n");
    Core_unix.mkdir_p (Filename.concat s.dir "sub");
    in_dir s "sub/deep.txt" (Out_channel.write_all ~data:"made by turn 2\n"));
  print_result s (s.call ~params:{|{"adapter": "codex"}|} "new_session");
  s.prompt "first";
  s.prompt "second";
  show_turns s;
  files s;
  [%expect
    {|
    ok
    spawn codex app-server
    codex: thread/start
    codex got: "first"
    codex got: "second"
    external turn codex-thread tu-1, files checkpointed
    external turn codex-thread tu-2, files checkpointed
    ./a.txt: three
    ./new.txt: made by turn 1
    ./sub/deep.txt: made by turn 2
    |}];
  (* Back to the second prompt: its turn's changes are undone. *)
  rewind s ~to_user:"second";
  files s;
  [%expect
    {|
    ok
    ./a.txt: two
    ./b.txt: bee
    ./new.txt: made by turn 1
    |}];
  (* The next turn restarts Codex, which drops the turn from its thread. *)
  s.prompt "again";
  show_turns s;
  [%expect
    {|
    codex: thread/resume codex-thread
    codex: thread/revert codex-thread before tu-2
    codex got: "again"
    external turn codex-thread tu-1, files checkpointed
    external turn codex-thread tu-3, files checkpointed
    |}];
  (* All the way back: the files as they were before Codex ran. *)
  rewind s ~to_user:"first";
  files s;
  s.prompt "from the start";
  [%expect
    {|
    ok
    ./a.txt: one
    ./b.txt: bee
    codex: thread/resume codex-thread
    codex: thread/revert codex-thread before tu-1
    codex got: "from the start"
    |}];
  (* The repository's own state was never touched. *)
  let r =
    Process.run_collect
      ~env:(Agent.env (s.agent ()))
      ~cwd:s.dir
      ~prog:"git"
      ~args:[ "status"; "--short" ]
      ()
  in
  print_string r.stdout;
  [%expect {| |}]
;;

let%expect_test "a rewind is refused when files changed since the agent's turn" =
  with_server
  @@ fun s ->
  Queue.enqueue edits (fun () ->
    in_dir s "a.txt" (Out_channel.write_all ~data:"two\n"));
  print_result s (s.call ~params:{|{"adapter": "codex"}|} "new_session");
  s.prompt "first";
  (* The user edits a file after Codex's turn. *)
  in_dir s "b.txt" (Out_channel.write_all ~data:"mine\n");
  rewind s ~to_user:"first";
  files s;
  [%expect
    {|
    ok
    spawn codex app-server
    codex: thread/start
    codex got: "first"
    error: could not put the files back as they were before Codex's turn (nothing was changed): files changed since the agent's last turn: b.txt
    ./a.txt: two
    ./b.txt: mine
    |}];
  (* The session was not rewound either. *)
  s.prompt "next";
  [%expect {| codex got: "next" |}]
;;

let%expect_test "files changed between turns: refused" =
  with_server
  @@ fun s ->
  Queue.enqueue edits (fun () ->
    in_dir s "a.txt" (Out_channel.write_all ~data:"two\n"));
  Queue.enqueue edits ignore;
  print_result s (s.call ~params:{|{"adapter": "codex"}|} "new_session");
  s.prompt "first";
  in_dir s "b.txt" (Out_channel.write_all ~data:"mine\n");
  s.prompt "second";
  rewind s ~to_user:"first";
  files s;
  [%expect
    {|
    ok
    spawn codex app-server
    codex: thread/start
    codex got: "first"
    codex got: "second"
    error: files changed between Codex's turns being rewound, outside them: b.txt. Rewinding would undo those changes too: commit or stash them first, or fork from that message instead (/fork)
    ./a.txt: two
    ./b.txt: mine
    |}]
;;

let%expect_test "outside a git repository: no rewind, and why" =
  with_server ~git:false
  @@ fun s ->
  print_result s (s.call ~params:{|{"adapter": "codex"}|} "new_session");
  s.prompt "first";
  show_turns s;
  rewind s ~to_user:"first";
  [%expect
    {|
    ok
    spawn codex app-server
    codex: thread/start
    codex got: "first"
    external turn codex-thread tu-1, files not checkpointed
    error: Codex sessions can't rewind here: $DIR/work is not in a git repository, so prigh can't put its files back: fork from that message instead (/fork)
    |}];
  let state = s.call "get_state" in
  let note =
    List.find_map
      [ [ "result"; "state" ]; [ "result" ] ]
      ~f:(fun path -> member (path @ [ "capabilities"; "rewind_note" ]) state)
  in
  print_endline
    (Option.value_map note ~default:"no note" ~f:(fun n ->
       s.mask (Json.to_string n)));
  [%expect
    {| "$DIR/work is not in a git repository, so prigh can't put its files back: fork from that message instead (/fork)" |}]
;;

let%expect_test "codex: a fork forks its thread at the turn" =
  with_server
  @@ fun s ->
  print_result s (s.call ~params:{|{"adapter": "codex"}|} "new_session");
  s.prompt "first";
  s.prompt "second";
  print_result
    s
    (s.call
       ~params:
         (Json.to_string
            (`Object
                [ ( "at"
                  , `String
                      (let session = Agent.session (s.agent ()) in
                       (* the reply to "first" *)
                       List.find_map_exn
                         (Session.active_path session)
                         ~f:(fun (e : Session.Entry.t) ->
                           match e.payload with
                           | Message (Assistant a)
                             when String.equal
                                    (Message.Assistant.text a)
                                    "codex reply 1" -> Some e.id
                           | _ -> None)) )
                ]))
       "fork");
  s.prompt "in the fork";
  show_turns s;
  [%expect
    {|
    ok
    spawn codex app-server
    codex: thread/start
    codex got: "first"
    codex got: "second"
    ok
    codex: thread/fork codex-thread at tu-1
    codex got: "in the fork"
    external turn codex-thread tu-1, files not checkpointed
    external turn codex-fork tu-1, files not checkpointed
    external turn codex-fork tu-3, files checkpointed
    |}]
;;

let%expect_test "claude: rewinding resumes it at its message; forks fork it" =
  with_server
  @@ fun s ->
  Queue.enqueue edits (fun () ->
    in_dir s "a.txt" (Out_channel.write_all ~data:"two\n"));
  Queue.enqueue edits (fun () ->
    in_dir s "a.txt" (Out_channel.write_all ~data:"three\n"));
  print_result s (s.call ~params:{|{"adapter": "claude"}|} "new_session");
  s.prompt "first";
  s.prompt "second";
  show_turns s;
  rewind s ~to_user:"second";
  files s;
  s.prompt "again";
  [%expect
    {|
    ok
    spawn claude --output-format stream-json --verbose --permission-prompt-tool stdio --permission-mode default --mcp-config {"mcpServers":{"prigh":{"type":"sdk","name":"prigh"}}} --strict-mcp-config --setting-sources=user --include-partial-messages --input-format stream-json
    external turn claude-session turn-1 at u1, files checkpointed
    external turn claude-session turn-2 at u2, files checkpointed
    ok
    ./a.txt: two
    ./b.txt: bee
    spawn claude --output-format stream-json --verbose --permission-prompt-tool stdio --permission-mode default --resume=claude-session --resume-session-at=u1 --mcp-config {"mcpServers":{"prigh":{"type":"sdk","name":"prigh"}}} --strict-mcp-config --setting-sources=user --include-partial-messages --input-format stream-json
    |}];
  print_result s (s.call "clone");
  s.prompt "in the fork";
  [%expect
    {|
    ok
    spawn claude --output-format stream-json --verbose --permission-prompt-tool stdio --permission-mode default --resume=claude-session --fork-session --resume-session-at=u3 --mcp-config {"mcpServers":{"prigh":{"type":"sdk","name":"prigh"}}} --strict-mcp-config --setting-sources=user --include-partial-messages --input-format stream-json
    |}]
;;

let%expect_test "deleting a session drops its checkpoints" =
  with_server
  @@ fun s ->
  print_result s (s.call ~params:{|{"adapter": "codex"}|} "new_session");
  s.prompt "first";
  let path = Session.path (Agent.session (s.agent ())) in
  let refs () =
    let r =
      Process.run_collect
        ~env:(Agent.env (s.agent ()))
        ~cwd:s.dir
        ~prog:"git"
        ~args:[ "for-each-ref"; "--format=%(refname)"; Checkpoint.ref_prefix ]
        ()
    in
    List.length (String.split_lines r.stdout)
  in
  printf "checkpoint refs: %d\n" (refs ());
  print_result s (s.call "new_session");
  print_result
    s
    (s.call
       ~params:(Json.to_string (`Object [ "path", `String path ]))
       "delete_session");
  (* They go in the background: wait for them to. *)
  let rec wait n =
    if refs () > 0 && n > 0
    then (
      Eio_unix.sleep 0.05;
      wait (n - 1))
  in
  wait 400;
  printf "checkpoint refs: %d\n" (refs ());
  [%expect
    {|
    ok
    spawn codex app-server
    codex: thread/start
    codex got: "first"
    checkpoint refs: 2
    ok
    ok
    checkpoint refs: 0
    |}]
;;
