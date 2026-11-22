open! Core
open! Prigh
open Tool_test_helpers
open! Test_temporary_sessions

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
        if List.mem outputs l.path ~equal:String.equal
        then "<output>"
        else mask t l.path
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
    {| {"type":"event","event":"temporary_ended","session":"<id>","messages":14,"kept":[{"path":"$DIR/trees/repo/feat-dirty","kind":"worktree","why":"$DIR/trees/repo/feat-dirty has uncommitted changes (1 file): commit or stash them first, or force the removal"}]} |}];
  printf "running jobs: %d\n" (Agent.running_background agent);
  List.iter outputs ~f:(fun path ->
    printf
      "output: %s\n"
      (if Sys_unix.file_exists_exn path then "exists" else "gone"));
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
  printf
    "output: %s\n"
    (if Sys_unix.file_exists_exn output then "exists" else "gone");
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
