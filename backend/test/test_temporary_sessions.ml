open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

(* Temporary sessions: never written or listed, reachable only with their
   key, ended (with everything they made) when left or after the grace
   period without a client, convertible with [keep_session], and cleaned up
   by the next backend when one stopped while they were open. *)

let sh = Test_worktree.sh
let init_repo = Test_worktree.init_repo
let call_tool ~id ~name arguments = Reply.tool_call ~id ~name ~arguments ()

let bash ?(extra = []) ~id command =
  call_tool
    ~id
    ~name:"bash"
    (Jsonaf.to_string (`Object ([ "command", `String command ] @ extra)))
;;

(* The grace periods' clock: sleepers wait until [advance]. *)
module Clock = struct
  type t = { mutable sleepers : (float * unit Eio.Promise.u) list }

  let create () = { sleepers = [] }

  let sleep t seconds =
    let p, r = Eio.Promise.create () in
    t.sleepers <- (seconds, r) :: t.sleepers;
    Eio.Promise.await p
  ;;

  let advance t =
    let due = t.sleepers in
    t.sleepers <- [];
    List.iter due ~f:(fun (_, r) -> Eio.Promise.resolve r ());
    for _ = 1 to 50 do
      Eio.Fiber.yield ()
    done
  ;;
end

module Client = struct
  type t =
    { server : Rpc_server.t
    ; client : Rpc_server.Client.t
    ; sent : Jsonaf.t Queue.t
    }

  let request (c : t) meth params =
    Rpc_server.handle
      c.server
      c.client
      (`Object
          [ "id", `String "r"; "method", `String meth; "params", `Object params ])
  ;;

  let result json =
    match Jsonaf.member "ok" json, Jsonaf.member "result" json with
    | Some `True, Some result -> Ok result
    | _ ->
      (match Jsonaf.member "error" json with
       | Some (`String e) -> Error e
       | _ -> Error (Jsonaf.to_string json))
  ;;

  let connect ?(hello = []) server =
    let sent = Queue.create () in
    let client = Rpc_server.connect server ~send:(Queue.enqueue sent) in
    let c = { server; client; sent } in
    let reply = request c "hello" hello in
    c, result reply
  ;;

  let agent (c : t) = Rpc_server.agent_of_client c.server c.client

  (* Waits for the [temporary_ended] event, which the cleanup sends once
     done (it runs in its own fiber), and prints it. *)
  let await_ended t (c : t) =
    let ended () =
      Queue.find c.sent ~f:(fun json ->
        match Jsonaf.member "event" json with
        | Some (`String "temporary_ended") -> true
        | _ -> false)
    in
    let rec loop n =
      match ended () with
      | Some json -> print_endline (mask t (Jsonaf.to_string json))
      | None when n = 0 -> print_endline "(no temporary_ended event)"
      | None ->
        Eio.Fiber.yield ();
        loop (n - 1)
    in
    loop 100_000
  ;;
end

let field name json =
  match Jsonaf.member name json with
  | Some (`String s) -> s
  | Some json -> Jsonaf.to_string json
  | None -> "-"
;;

let state_field name json =
  match Jsonaf.member "state" json with
  | Some state -> field name state
  | None -> "-"
;;

(* What the sessions directory holds, the ledger included. *)
let print_sessions_dir t ~sessions_dir =
  let rec walk dir =
    match Sys_unix.is_directory dir with
    | `Yes ->
      List.concat_map
        (List.sort (Sys_unix.ls_dir dir) ~compare:String.compare)
        ~f:(fun name -> walk (Filename.concat dir name))
    | `No | `Unknown -> [ dir ]
  in
  match walk sessions_dir with
  | [] -> print_endline "sessions dir: empty"
  | files ->
    List.iter files ~f:(fun file ->
      printf "sessions dir: %s\n" (mask t file))
;;

let print_listed t (c : Client.t) =
  match Client.result (Client.request c "list_sessions" []) with
  | Error e -> print_endline e
  | Ok (`Array []) -> print_endline "listed: none"
  | Ok (`Array sessions) ->
    List.iter sessions ~f:(fun s ->
      printf "listed: %s (%s)\n" (mask t (field "id" s)) (field "first_prompt" s))
  | Ok json -> print_endline (Jsonaf.to_string json)
;;

let exists t path =
  printf
    "%s: %s\n"
    (mask t path)
    (if Sys_unix.file_exists_exn path then "exists" else "gone")
;;

let with_server ?(main = []) ?(children = []) f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let sessions_dir = Filename.concat t.dir "sessions" in
  let repo = Filename.concat t.dir "repo" in
  let trees = Filename.concat t.dir "trees" in
  init_repo t;
  Config.save ~home:t.dir { Config.default with worktrees_dir = trees }
  |> Or_error.ok_exn;
  let provider = Routed_provider.create ~main children in
  let new_agent ?session ~cwd () =
    let agent_ref = ref None in
    let worktrees_dir () = trees in
    let agent =
      Agent.create
        ~env:t.env
        ~sw
        ~provider
        ~tools:
          (Tools.all
           @ [ Tool_subagent.create
                 ~provider
                 ~worktrees_dir
                 ~current_model:(fun () -> Model.default)
                 ~current_thinking:(fun () -> Off)
                 ~home:"/nonexistent"
                 ()
             ; Tool_worktree.create
                 ~worktrees_dir
                 ~session:(fun () -> Option.map !agent_ref ~f:Agent.session)
                 ()
             ])
        ~sessions_dir
        ~home:t.dir
        ?session
        ~cwd
        ()
    in
    agent_ref := Some agent;
    agent
  in
  let clock = Clock.create () in
  let make_server () =
    Rpc_server.create
      ~env:t.env
      ~sw
      ~backend_host:true
      ~sleep:(Clock.sleep clock)
      ~login:
        (Login_manager.create
           ~env:t.env
           ~sw
           ~getenv:(fun _ -> None)
           ~store:(Auth_store.create ~path:(Filename.concat t.dir "auth.json"))
           ())
      ~sessions_dir
      ~cwd:repo
      ~new_agent
      ()
  in
  f t ~sessions_dir ~repo ~trees ~clock ~make_server
;;

(* Runs a prompt to the end, its background subagents included. *)
let prompt (c : Client.t) text =
  ignore (Client.request c "prompt" [ "text", `String text ] : Jsonaf.t);
  let agent = Client.agent c in
  Agent.wait_idle agent;
  while Agent.has_running_subagents agent || Agent.is_running agent do
    Eio.Fiber.yield ();
    Agent.wait_idle agent
  done
;;

let new_temporary (c : Client.t) =
  let state =
    Client.request c "new_session" [ "temporary", `True ]
    |> Client.result
    |> Result.ok_or_failwith
  in
  let id = state_field "session_id" state in
  let key = field "temporary_key" state in
  printf
    "new temporary session: temporary=%s key=%s\n"
    (state_field "temporary" state)
    (if String.length key = 32 then "<32 hex>" else key);
  id, key
;;

let%expect_test "unlisted, unreachable without its key, nothing written" =
  with_server ~main:[ Reply.text "hello"; Reply.text "hi again" ]
  @@ fun t ~sessions_dir ~repo:_ ~trees:_ ~clock:_ ~make_server ->
  let server = make_server () in
  let a, _ = Client.connect server in
  let id, key = new_temporary a in
  prompt a "a secret";
  print_listed t a;
  print_sessions_dir t ~sessions_dir;
  [%expect
    {|
    new temporary session: temporary=true key=<32 hex>
    listed: none
    sessions dir: $DIR/sessions/.temporary/<id>.json
    |}];
  (* the ledger holds no transcript *)
  let ledger =
    In_channel.read_all
      (Filename.concat sessions_dir (sprintf ".temporary/%s.json" id))
  in
  printf "ledger mentions the prompt: %b\n" (String.is_substring ledger ~substring:"secret");
  [%expect {| ledger mentions the prompt: false |}];
  let b, _ = Client.connect server in
  print_listed t b;
  (match Client.result (Client.request b "switch_session" [ "path", `String id ]) with
   | Ok _ -> print_endline "switched (wrong)"
   | Error e -> print_endline (mask t e));
  (match Client.connect server ~hello:[ "session", `String id ] with
   | _, Ok _ -> print_endline "hello without the key: attached (wrong)"
   | _, Error e -> print_endline (mask t e));
  (match
     Client.connect
       server
       ~hello:[ "session", `String id; "temporary_key", `String "nope" ]
   with
   | _, Ok _ -> print_endline "hello with a wrong key: attached (wrong)"
   | _, Error e -> print_endline (mask t e));
  [%expect
    {|
    listed: none
    no session "<id>"
    no session "<id>"
    temporary session "<id>" is gone: it was deleted when it was left
    |}];
  (match
     Client.connect
       server
       ~hello:[ "session", `String id; "temporary_key", `String key ]
   with
   | _, Ok _ -> print_endline "hello with the key: attached"
   | _, Error e -> print_endline (mask t e));
  [%expect {| hello with the key: attached |}]
;;

let%expect_test "a client that goes comes back within the grace period, \
                 not after" =
  with_server ~main:[ Reply.text "hello" ]
  @@ fun t ~sessions_dir ~repo:_ ~trees:_ ~clock ~make_server ->
  let server = make_server () in
  let a, _ = Client.connect server in
  let id, key = new_temporary a in
  prompt a "remember me";
  Rpc_server.disconnect server a.client;
  let hello = [ "session", `String id; "temporary_key", `String key ] in
  let b, reply = Client.connect server ~hello in
  (match reply with
   | Ok _ ->
     printf
       "reattached: %d messages\n"
       (List.length (Agent.messages (Client.agent b)))
   | Error e -> print_endline (mask t e));
  [%expect {|
    new temporary session: temporary=true key=<32 hex>
    reattached: 2 messages
    |}];
  (* the first grace period's end does nothing: it came back *)
  Clock.advance clock;
  print_sessions_dir t ~sessions_dir;
  [%expect {| sessions dir: $DIR/sessions/.temporary/<id>.json |}];
  Rpc_server.disconnect server b.client;
  Clock.advance clock;
  print_sessions_dir t ~sessions_dir;
  (match Client.connect server ~hello with
   | _, Ok _ -> print_endline "reattached (wrong)"
   | _, Error e -> print_endline (mask t e));
  [%expect
    {|
    sessions dir: empty
    temporary session "<id>" is gone: it was deleted when it was left
    |}]
;;

let long_output = "seq 1 5000"

let%expect_test "leaving deletes everything it made, but dirty worktrees" =
  with_server
    ~main:
      [ call_tool
          ~id:"w1"
          ~name:"worktree"
          {|{"action":"create","branch":"feat/clean"}|}
      ; call_tool
          ~id:"w2"
          ~name:"worktree"
          {|{"action":"create","branch":"feat/dirty"}|}
      ; bash ~id:"b1" long_output
      ; bash ~id:"b2" "sleep 1000" ~extra:[ "background", `True ]
      ; call_tool ~id:"s1" ~name:"subagent" {|{"task":"Long work"}|}
      ; Reply.text "working"
      ]
    ~children:[ "Long work", [ bash ~id:"n1" long_output; Reply.text "done" ] ]
  @@ fun t ~sessions_dir ~repo ~trees ~clock:_ ~make_server ->
  let server = make_server () in
  let a, _ = Client.connect server in
  let (_ : string), (_ : string) = new_temporary a in
  let agent = Client.agent a in
  ignore (Client.request a "prompt" [ "text", `String "go" ] : Jsonaf.t);
  (* not [wait_idle]: that waits for the job too *)
  while Agent.is_running agent || Agent.has_running_subagents agent do
    Eio.Fiber.yield ()
  done;
  let uploads = Filename.concat repo File_ops.uploads_dir in
  Core_unix.mkdir_p uploads;
  let upload = Filename.concat uploads "notes.txt" in
  Out_channel.write_all upload ~data:"private";
  Agent.note_upload agent ~path:upload;
  sh t "echo x > ../trees/repo/feat-dirty/new.txt";
  let outputs =
    List.concat_map (Agent.leftovers agent) ~f:(fun (l : Leftover.t) ->
      match l.kind with
      | Output -> [ l.path ]
      | Upload | Worktree -> [])
  in
  let leftovers =
    List.map (Agent.leftovers agent) ~f:(fun (l : Leftover.t) ->
      let path =
        if List.mem outputs l.path ~equal:String.equal then "<output>" else mask t l.path
      in
      sprintf "%s %s" (Sexp.to_string (Leftover.Kind.sexp_of_t l.kind)) path)
    |> List.sort ~compare:String.compare
  in
  List.iter leftovers ~f:print_endline;
  printf
    "running jobs: %d, subagents: %d\n"
    (Agent.running_background agent)
    (List.length (Agent.subagents agent));
  [%expect
    {|
    new temporary session: temporary=true key=<32 hex>
    Output <output>
    Output <output>
    Upload $DIR/repo/.prigh/uploads/notes.txt
    Worktree $DIR/trees/repo/feat-clean
    Worktree $DIR/trees/repo/feat-dirty
    running jobs: 1, subagents: 1
    |}];
  (* leaving: a new session *)
  ignore (Client.request a "new_session" [] : Jsonaf.t);
  Client.await_ended t a;
  [%expect
    {| {"type":"event","event":"temporary_ended","session":"<id>","kept":[{"path":"$DIR/trees/repo/feat-dirty","kind":"worktree","why":"$DIR/trees/repo/feat-dirty has uncommitted changes (1 file): commit or stash them first, or force the removal"}]} |}];
  printf "running jobs: %d\n" (Agent.running_background agent);
  List.iter outputs ~f:(fun path ->
    printf "output: %s\n" (if Sys_unix.file_exists_exn path then "exists" else "gone"));
  exists t upload;
  exists t (Filename.concat trees "repo/feat-clean");
  exists t (Filename.concat trees "repo/feat-dirty/new.txt");
  print_sessions_dir t ~sessions_dir;
  print_listed t a;
  [%expect
    {|
    running jobs: 0
    output: gone
    output: gone
    $DIR/repo/.prigh/uploads/notes.txt: gone
    $DIR/trees/repo/feat-clean: gone
    $DIR/trees/repo/feat-dirty/new.txt: exists
    sessions dir: empty
    listed: none
    |}]
;;

let%expect_test "leaving stops a running turn" =
  with_server ~main:[ bash ~id:"b1" "sleep 1000"; Reply.text "slept" ]
  @@ fun t ~sessions_dir ~repo:_ ~trees:_ ~clock:_ ~make_server ->
  let server = make_server () in
  let a, _ = Client.connect server in
  let (_ : string), (_ : string) = new_temporary a in
  let agent = Client.agent a in
  ignore (Client.request a "prompt" [ "text", `String "sleep" ] : Jsonaf.t);
  while List.is_empty (Agent.jobs agent) && not (Agent.is_running agent) do
    Eio.Fiber.yield ()
  done;
  printf "running: %b\n" (Agent.is_running agent);
  ignore (Client.request a "new_session" [] : Jsonaf.t);
  Client.await_ended t a;
  printf "running: %b\n" (Agent.is_running agent);
  print_sessions_dir t ~sessions_dir;
  [%expect
    {|
    new temporary session: temporary=true key=<32 hex>
    running: true
    {"type":"event","event":"temporary_ended","session":"<id>","kept":[]}
    running: false
    sessions dir: empty
    |}]
;;

let%expect_test "keep_session makes it a normal session" =
  with_server
    ~main:
      [ call_tool ~id:"s1" ~name:"subagent" {|{"task":"Side work"}|}
      ; Reply.text "started"
      ; Reply.text "noted"
      ]
    ~children:[ "Side work", [ Reply.text "done" ] ]
  @@ fun t ~sessions_dir ~repo:_ ~trees:_ ~clock ~make_server ->
  let server = make_server () in
  let a, _ = Client.connect server in
  let id, _ = new_temporary a in
  prompt a "worth keeping";
  (match Client.result (Client.request a "keep_session" []) with
   | Ok state -> printf "kept: temporary=%s\n" (state_field "temporary" state)
   | Error e -> print_endline e);
  print_listed t a;
  print_sessions_dir t ~sessions_dir;
  [%expect
    {|
    new temporary session: temporary=true key=<32 hex>
    kept: temporary=-
    listed: <id> (worth keeping)
    sessions dir: $DIR/sessions/<stamp>_<id>/subagents.jsonl
    sessions dir: $DIR/sessions/<stamp>_<id>.jsonl
    |}];
  (match Client.result (Client.request a "keep_session" []) with
   | Ok _ -> print_endline "kept twice (wrong)"
   | Error e -> print_endline e);
  [%expect {| this session is not temporary: it is saved already |}];
  (* leaving it, and the grace period, end nothing now *)
  ignore (Client.request a "new_session" [] : Jsonaf.t);
  Rpc_server.disconnect server a.client;
  Clock.advance clock;
  let b, _ = Client.connect server ~hello:[ "session", `String id ] in
  printf
    "reopened: %d messages, %d subagents\n"
    (List.length (Agent.messages (Client.agent b)))
    (List.length (Agent.subagents (Client.agent b)));
  [%expect {| reopened: 6 messages, 1 subagents |}]
;;

let%expect_test "a backend that stopped mid-session: the next one cleans up" =
  with_server
    ~main:
      [ call_tool
          ~id:"w1"
          ~name:"worktree"
          {|{"action":"create","branch":"feat/clean"}|}
      ; bash ~id:"b1" long_output
      ; Reply.text "made things"
      ]
  @@ fun t ~sessions_dir ~repo ~trees ~clock:_ ~make_server ->
  let server = make_server () in
  let a, _ = Client.connect server in
  let (_ : string), (_ : string) = new_temporary a in
  prompt a "make things";
  let agent = Client.agent a in
  let upload = Filename.concat repo ".prigh/uploads/pic.png" in
  Core_unix.mkdir_p (Filename.dirname upload);
  Out_channel.write_all upload ~data:"png";
  Agent.note_upload agent ~path:upload;
  let output =
    List.find_map_exn (Agent.leftovers agent) ~f:(fun (l : Leftover.t) ->
      Option.some_if (Leftover.Kind.equal l.kind Output) l.path)
  in
  (* the backend dies: nothing ends the session *)
  Rpc_server.shutdown server;
  print_sessions_dir t ~sessions_dir;
  [%expect
    {|
    new temporary session: temporary=true key=<32 hex>
    sessions dir: $DIR/sessions/.temporary/<id>.json
    |}];
  let next = make_server () in
  Rpc_server.clean_leftovers next;
  print_sessions_dir t ~sessions_dir;
  exists t upload;
  printf "output: %s\n" (if Sys_unix.file_exists_exn output then "exists" else "gone");
  exists t (Filename.concat trees "repo/feat-clean");
  let b, _ = Client.connect next in
  print_listed t b;
  [%expect
    {|
    sessions dir: empty
    $DIR/repo/.prigh/uploads/pic.png: gone
    output: gone
    $DIR/trees/repo/feat-clean: gone
    listed: none
    |}]
;;
