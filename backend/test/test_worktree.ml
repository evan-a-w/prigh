open! Core
open! Prigh
open Tool_test_helpers

let sh ?(dir = "repo") (t : Tool_test_helpers.t) script =
  let command =
    sprintf
      "mkdir -p %s && cd %s && export GIT_CONFIG_GLOBAL=/dev/null \
       GIT_CONFIG_NOSYSTEM=1 GIT_AUTHOR_NAME=t GIT_AUTHOR_EMAIL=t@t \
       GIT_COMMITTER_NAME=t GIT_COMMITTER_EMAIL=t@t && (%s) > /dev/null 2>&1"
      (Filename.quote (Filename.concat t.dir dir))
      (Filename.quote (Filename.concat t.dir dir))
      script
  in
  match Sys_unix.command command with
  | 0 -> ()
  | n -> raise_s [%message "setup failed" (script : string) (n : int)]
;;

let init_repo ?(dir = "repo") t =
  sh
    ~dir
    t
    "git init -q -b main . && echo one > a.txt && git add -A && git commit -q \
     -m init"
;;

let sha_re = Re.compile (Re.Perl.re "[0-9a-f]{40}")
let show t s = mask t (Re.replace_string sha_re ~by:"<sha>" s)

let print_worktree t (w : Worktree.t) =
  printf
    "%s branch=%s%s%s%s%s%s changes=%s%s\n"
    (show t w.path)
    (Option.value w.branch ~default:"(detached)")
    (if w.main then " main" else "")
    (if w.managed then " managed" else "")
    (if w.locked then " locked" else "")
    (if w.prunable then " prunable" else "")
    (if Option.is_none w.head then " no-head" else "")
    (Option.value_map w.changes ~default:"?" ~f:Int.to_string)
    (match w.upstream with
     | None -> ""
     | Some u -> sprintf " upstream=%s +%d -%d" u w.ahead w.behind)
;;

let print_listing t = function
  | Error e -> printf "error: %s\n" (show t (Error.to_string_hum e))
  | Ok (l : Worktree.Listing.t) ->
    printf "root=%s project_dir=%s\n" (show t l.root) (show t l.project_dir);
    List.iter l.worktrees ~f:(print_worktree t)
;;

let print_created t = function
  | Error e -> printf "error: %s\n" (show t (Error.to_string_hum e))
  | Ok (c : Worktree.Created.t) ->
    printf "created (new_branch=%b): " c.new_branch;
    print_worktree t c.worktree
;;

let print_removed t = function
  | Error e -> printf "error: %s\n" (show t (Error.to_string_hum e))
  | Ok w ->
    printf "removed: ";
    print_worktree t w
;;

let with_agent f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  Core_unix.mkdir_p (Filename.concat t.dir "repo");
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [])
      ~tools:Tools.all
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:(Filename.concat t.dir "repo")
      ()
  in
  Or_error.ok_exn
    (Agent.set_config
       agent
       { (Agent.config agent) with
         worktrees_dir = Filename.concat t.dir "trees"
       });
  f t agent
;;

let%expect_test "slug" =
  List.iter
    [ "main"; "feat/worktrees"; "fix//a b"; "user/x_y.z"; ".hidden"; "--"; "é" ]
    ~f:(fun b -> printf "%-15s %s\n" b (Worktree.slug b));
  [%expect
    {|
    main            main
    feat/worktrees  feat-worktrees
    fix//a b        fix-a-b
    user/x_y.z      user-x_y.z
    .hidden         hidden
    --              worktree
    é              worktree
    |}]
;;

let%expect_test "list and create: new, existing, remote and based branches" =
  with_agent
  @@ fun t agent ->
  print_listing t (Agent.worktrees agent);
  [%expect
    {| error: not a git repository: $DIR/repo (git init starts one, or /cd into one) |}];
  init_repo t;
  print_listing t (Agent.worktrees agent);
  [%expect
    {|
    root=$DIR/repo project_dir=$DIR/trees/repo
    $DIR/repo branch=main main changes=0
    |}];
  (* A new branch from HEAD. *)
  print_created t (Agent.create_worktree agent ~branch:"feat/x" ~base:None);
  [%expect
    {| created (new_branch=true): $DIR/trees/repo/feat-x branch=feat/x managed changes=0 |}];
  (* A slug already taken. *)
  print_created t (Agent.create_worktree agent ~branch:"feat-x" ~base:None);
  [%expect
    {| created (new_branch=true): $DIR/trees/repo/feat-x-2 branch=feat-x managed changes=0 |}];
  (* An existing branch, checked out as it is; with a base it is refused. *)
  sh t "git branch old && git tag v1";
  print_created t (Agent.create_worktree agent ~branch:"old" ~base:(Some "v1"));
  print_created t (Agent.create_worktree agent ~branch:"old" ~base:None);
  [%expect
    {|
    error: branch old already exists: leave out the base to check it out, or pick a new name
    created (new_branch=false): $DIR/trees/repo/old branch=old managed changes=0
    |}];
  (* From a base; unknown bases, bad names and checked-out branches. *)
  sh t "echo two >> a.txt && git commit -q -am two";
  print_created
    t
    (Agent.create_worktree agent ~branch:"from-tag" ~base:(Some "v1"));
  print_created t (Agent.create_worktree agent ~branch:"y" ~base:(Some "nope"));
  print_created t (Agent.create_worktree agent ~branch:"bad..name" ~base:None);
  print_created t (Agent.create_worktree agent ~branch:"feat/x" ~base:None);
  print_created t (Agent.create_worktree agent ~branch:"main" ~base:None);
  [%expect
    {|
    created (new_branch=true): $DIR/trees/repo/from-tag branch=from-tag managed changes=0
    error: unknown revision "nope": name a branch, tag or commit (e.g. main)
    error: not a valid branch name: "bad..name" (e.g. feat/my-change)
    error: branch feat/x is already checked out at $DIR/trees/repo/feat-x: open that worktree instead
    error: branch main is already checked out at $DIR/repo: open that worktree instead
    |}];
  (* A branch only a remote has is tracked; ahead and behind are counted. *)
  sh
    t
    "git remote add origin /nonexistent && git update-ref \
     refs/remotes/origin/remote-only HEAD~1 && git update-ref \
     refs/remotes/origin/main HEAD~1";
  print_created t (Agent.create_worktree agent ~branch:"remote-only" ~base:None);
  sh t "git branch -u origin/main main";
  sh ~dir:"trees/repo/feat-x" t "echo three > b.txt && echo x >> a.txt";
  print_listing t (Agent.worktrees agent);
  [%expect
    {|
    created (new_branch=true): $DIR/trees/repo/remote-only branch=remote-only managed changes=0 upstream=origin/remote-only +0 -0
    root=$DIR/repo project_dir=$DIR/trees/repo
    $DIR/repo branch=main main changes=0 upstream=origin/main +1 -0
    $DIR/trees/repo/feat-x branch=feat/x managed changes=2
    $DIR/trees/repo/feat-x-2 branch=feat-x managed changes=0
    $DIR/trees/repo/from-tag branch=from-tag managed changes=0
    $DIR/trees/repo/old branch=old managed changes=0
    $DIR/trees/repo/remote-only branch=remote-only managed changes=0 upstream=origin/remote-only +0 -0
    |}];
  (* Listing from inside a worktree gives the same repository. *)
  ignore
    (Or_error.ok_exn
       (Agent.set_cwd agent ~path:(Filename.concat t.dir "trees/repo/old"))
     : Dir_change.Outcome.t);
  print_listing t (Agent.worktrees agent);
  [%expect
    {|
    root=$DIR/repo project_dir=$DIR/trees/repo
    $DIR/repo branch=main main changes=0 upstream=origin/main +1 -0
    $DIR/trees/repo/feat-x branch=feat/x managed changes=2
    $DIR/trees/repo/feat-x-2 branch=feat-x managed changes=0
    $DIR/trees/repo/from-tag branch=from-tag managed changes=0
    $DIR/trees/repo/old branch=old managed changes=0
    $DIR/trees/repo/remote-only branch=remote-only managed changes=0 upstream=origin/remote-only +0 -0
    |}]
;;

let%expect_test "two repositories with the same name get their own directories" =
  with_agent
  @@ fun t agent ->
  init_repo t;
  init_repo ~dir:"other/repo" t;
  print_created t (Agent.create_worktree agent ~branch:"a" ~base:None);
  ignore
    (Or_error.ok_exn
       (Agent.set_cwd agent ~path:(Filename.concat t.dir "other/repo"))
     : Dir_change.Outcome.t);
  print_created t (Agent.create_worktree agent ~branch:"a" ~base:None);
  print_listing t (Agent.worktrees agent);
  (* A directory prigh did not make is left alone. *)
  init_repo ~dir:"third/repo" t;
  Core_unix.mkdir_p (Filename.concat t.dir "trees/repo-3");
  ignore
    (Or_error.ok_exn
       (Agent.set_cwd agent ~path:(Filename.concat t.dir "third/repo"))
     : Dir_change.Outcome.t);
  print_listing t (Agent.worktrees agent);
  print_endline
    (show
       t
       (In_channel.read_all (Filename.concat t.dir "trees/repo-2/.prigh-repo")));
  [%expect
    {|
    created (new_branch=true): $DIR/trees/repo/a branch=a managed changes=0
    created (new_branch=true): $DIR/trees/repo-2/a branch=a managed changes=0
    root=$DIR/other/repo project_dir=$DIR/trees/repo-2
    $DIR/other/repo branch=main main changes=0
    $DIR/trees/repo-2/a branch=a managed changes=0
    root=$DIR/third/repo project_dir=$DIR/trees/repo-4
    $DIR/third/repo branch=main main changes=0
    $DIR/other/repo/.git
    |}]
;;

let%expect_test "remove: dirty, locked, main, unknown, gone" =
  with_agent
  @@ fun t agent ->
  init_repo t;
  let create branch =
    ignore
      (Or_error.ok_exn (Agent.create_worktree agent ~branch ~base:None)
       : Worktree.Created.t)
  in
  List.iter [ "clean"; "dirty"; "locked"; "gone" ] ~f:create;
  sh ~dir:"trees/repo/dirty" t "echo x > new.txt";
  sh t "git worktree lock ../trees/repo/locked";
  sh t "rm -rf ../trees/repo/gone";
  print_listing t (Agent.worktrees agent);
  [%expect
    {|
    root=$DIR/repo project_dir=$DIR/trees/repo
    $DIR/repo branch=main main changes=0
    $DIR/trees/repo/clean branch=clean managed changes=0
    $DIR/trees/repo/dirty branch=dirty managed changes=1
    $DIR/trees/repo/gone branch=gone managed prunable changes=?
    $DIR/trees/repo/locked branch=locked managed locked changes=0
    |}];
  let remove ?(force = false) path =
    print_removed t (Agent.remove_worktree agent ~path ~force)
  in
  remove (Filename.concat t.dir "trees/repo/dirty");
  remove (Filename.concat t.dir "trees/repo/locked");
  remove ".";
  remove "../nowhere";
  [%expect
    {|
    error: $DIR/trees/repo/dirty has uncommitted changes (1 file): commit or stash them first, or force the removal
    error: $DIR/trees/repo/locked is locked: git worktree unlock $DIR/trees/repo/locked, or force the removal
    error: $DIR/repo is the repository's main worktree: it cannot be removed
    error: not a worktree of $DIR/repo: ../nowhere (/worktree lists them)
    |}];
  (* Nor one somebody works in: [keep]'s directories, as the host sees
     them. *)
  Core_unix.mkdir_p (Filename.concat t.dir "trees/repo/clean/sub");
  let keep dir =
    print_removed
      t
      (Worktree.remove
         ~keep:[ dir ]
         ~env:t.env
         ~cancel:Cancellation.never
         ~cwd:(Filename.concat t.dir "repo")
         ~dir:(Filename.concat t.dir "trees")
         ~path:"../trees/repo/clean"
         ~force:true
         ())
  in
  keep "../trees/repo/clean/sub/../sub";
  keep (Filename.concat t.dir "trees/repo/clean");
  [%expect
    {|
    error: $DIR/trees/repo/clean is in use: $DIR/trees/repo/clean/sub, inside it, is where the agent or the session works (work elsewhere first; the session moves with /cd)
    error: $DIR/trees/repo/clean is in use: $DIR/trees/repo/clean, inside it, is where the agent or the session works (work elsewhere first; the session moves with /cd)
    |}];
  Core_unix.rmdir (Filename.concat t.dir "trees/repo/clean/sub");
  remove "../trees/repo/clean";
  remove ~force:true (Filename.concat t.dir "trees/repo/dirty");
  remove ~force:true (Filename.concat t.dir "trees/repo/locked");
  remove (Filename.concat t.dir "trees/repo/gone");
  print_listing t (Agent.worktrees agent);
  [%expect
    {|
    removed: $DIR/trees/repo/clean branch=clean managed changes=0
    removed: $DIR/trees/repo/dirty branch=dirty managed changes=1
    removed: $DIR/trees/repo/locked branch=locked managed locked changes=0
    removed: $DIR/trees/repo/gone branch=gone managed prunable changes=?
    root=$DIR/repo project_dir=$DIR/trees/repo
    $DIR/repo branch=main main changes=0
    |}];
  (* The branches stay. *)
  sh t "git branch --format='%(refname:short)' > ../branches";
  print_string (In_channel.read_all (Filename.concat t.dir "branches"));
  [%expect
    {|
    clean
    dirty
    gone
    locked
    main
    |}]
;;

let%expect_test "over RPC: sessions in each worktree, the current one, removal" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let sessions_dir = Filename.concat t.dir "sessions" in
  let repo = Filename.concat t.dir "repo" in
  init_repo t;
  Config.save
    ~home:t.dir
    { Config.default with worktrees_dir = Filename.concat t.dir "trees" }
  |> Or_error.ok_exn;
  let new_agent ?session ~cwd () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [])
      ~tools:Tools.all
      ~sessions_dir
      ~home:t.dir
      ?session
      ~cwd
      ()
  in
  let server =
    Rpc_server.create
      ~env:t.env
      ~sw
      ~backend_host:true
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
  let connect () = Rpc_server.connect server ~send:ignore in
  let call client ?(params = "{}") meth =
    let request =
      Jsonaf.of_string
        (sprintf {|{"id": "r1", "method": "%s", "params": %s}|} meth params)
    in
    let reply = Rpc_server.handle server client request in
    print_endline (show t (Jsonaf.to_string reply));
    reply
  in
  let call_ client ?params meth =
    ignore (call client ?params meth : Jsonaf.t)
  in
  let a = connect () in
  let b = connect () in
  call_ a ~params:{|{"branch": "feat/a"}|} "create_worktree";
  call_ a ~params:{|{"branch": "b", "base": "main"}|} "create_worktree";
  call_ a ~params:{|{"base": "main"}|} "create_worktree";
  let wt_a = Filename.concat t.dir "trees/repo/feat-a" in
  call_ b ~params:(sprintf {|{"path": "%s/sub"}|} wt_a) "set_cwd";
  Core_unix.mkdir_p (Filename.concat wt_a "sub");
  call_ b ~params:(sprintf {|{"path": "%s/sub"}|} wt_a) "set_cwd";
  call_ b ~params:{|{"name": "in a"}|} "set_session_name";
  call_ a ~params:{|{"name": "in main"}|} "set_session_name";
  call_ a "list_worktrees";
  call_ b "list_worktrees";
  (* b works in feat-a, so it may not remove it; a may (it is clean). *)
  call_ b ~params:(sprintf {|{"path": "%s"}|} wt_a) "remove_worktree";
  call_
    a
    ~params:{|{"path": "../trees/repo/b", "force": "yes"}|}
    "remove_worktree";
  call_ a ~params:{|{"path": "../trees/repo/b"}|} "remove_worktree";
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{"worktree":{"path":"$DIR/trees/repo/feat-a","branch":"feat/a","head":"<sha>","main":false,"managed":true,"locked":false,"prunable":false,"changes":0,"upstream":null,"ahead":0,"behind":0,"current":false,"sessions":[]},"new_branch":true,"root":"$DIR/repo","cwd":"$DIR/trees/repo/feat-a"}}
    {"type":"response","id":"r1","ok":true,"result":{"worktree":{"path":"$DIR/trees/repo/b","branch":"b","head":"<sha>","main":false,"managed":true,"locked":false,"prunable":false,"changes":0,"upstream":null,"ahead":0,"behind":0,"current":false,"sessions":[]},"new_branch":true,"root":"$DIR/repo","cwd":"$DIR/trees/repo/b"}}
    {"type":"response","id":"r1","ok":false,"error":"missing param \"branch\""}
    {"type":"response","id":"r1","ok":false,"error":"<host>: not a directory: $DIR/trees/repo/feat-a/sub"}
    {"type":"response","id":"r1","ok":true,"result":{"cwd":"$DIR/trees/repo/feat-a/sub","created":false}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{"root":"$DIR/repo","project_dir":"$DIR/trees/repo","worktrees":[{"path":"$DIR/repo","branch":"main","head":"<sha>","main":true,"managed":false,"locked":false,"prunable":false,"changes":0,"upstream":null,"ahead":0,"behind":0,"current":true,"sessions":[{"id":"<id>","path":"$DIR/sessions/<stamp>_<id>.jsonl","name":"in main","live":true,"running":false}]},{"path":"$DIR/trees/repo/b","branch":"b","head":"<sha>","main":false,"managed":true,"locked":false,"prunable":false,"changes":0,"upstream":null,"ahead":0,"behind":0,"current":false,"sessions":[]},{"path":"$DIR/trees/repo/feat-a","branch":"feat/a","head":"<sha>","main":false,"managed":true,"locked":false,"prunable":false,"changes":0,"upstream":null,"ahead":0,"behind":0,"current":false,"sessions":[{"id":"<id>","path":"$DIR/sessions/<stamp>_<id>.jsonl","name":"in a","live":true,"running":false}]}]}}
    {"type":"response","id":"r1","ok":true,"result":{"root":"$DIR/repo","project_dir":"$DIR/trees/repo","worktrees":[{"path":"$DIR/repo","branch":"main","head":"<sha>","main":true,"managed":false,"locked":false,"prunable":false,"changes":0,"upstream":null,"ahead":0,"behind":0,"current":false,"sessions":[{"id":"<id>","path":"$DIR/sessions/<stamp>_<id>.jsonl","name":"in main","live":true,"running":false}]},{"path":"$DIR/trees/repo/b","branch":"b","head":"<sha>","main":false,"managed":true,"locked":false,"prunable":false,"changes":0,"upstream":null,"ahead":0,"behind":0,"current":false,"sessions":[]},{"path":"$DIR/trees/repo/feat-a","branch":"feat/a","head":"<sha>","main":false,"managed":true,"locked":false,"prunable":false,"changes":0,"upstream":null,"ahead":0,"behind":0,"current":true,"sessions":[{"id":"<id>","path":"$DIR/sessions/<stamp>_<id>.jsonl","name":"in a","live":true,"running":false}]}]}}
    {"type":"response","id":"r1","ok":false,"error":"this session works in $DIR/trees/repo/feat-a: /cd out of it (e.g. to the main worktree) first"}
    {"type":"response","id":"r1","ok":false,"error":"param \"force\" must be a boolean"}
    {"type":"response","id":"r1","ok":true,"result":{"worktree":{"path":"$DIR/trees/repo/b","branch":"b","head":"<sha>","main":false,"managed":true,"locked":false,"prunable":false,"changes":0,"upstream":null,"ahead":0,"behind":0,"current":false,"sessions":[]}}}
    |}]
;;

let%expect_test "a tool host runs the operations; ~ is its home" =
  with_sandbox
  @@ fun t ->
  init_repo t;
  let exec name arguments =
    let result =
      Host_ops.execute
        ~mcp:None
        ~env:t.env
        ~cancel:Cancellation.never
        ~on_output:ignore
        ~cwd:(Filename.concat t.dir "repo")
        ~name
        ~arguments
        ()
    in
    printf
      "%s%s\n"
      (if result.is_error then "error: " else "")
      (show t result.text)
  in
  let home = Sys.getenv "HOME" in
  Core_unix.putenv ~key:"HOME" ~data:(Filename.concat t.dir "home");
  Exn.protect
    ~finally:(fun () ->
      Option.iter home ~f:(fun data -> Core_unix.putenv ~key:"HOME" ~data))
    ~f:(fun () ->
      exec
        Worktree.create_op
        (Worktree.create_args ~dir:"~/wt" ~branch:"x" ~base:None ());
      exec Worktree.list_op (Worktree.list_args ~dir:"~/wt");
      exec
        Worktree.remove_op
        (Worktree.remove_args
           ~dir:"~/wt"
           ~path:"../home/wt/repo/x"
           ~force:false
           ());
      exec Worktree.remove_op (`Object []));
  [%expect
    {|
    {"worktree":{"path":"$DIR/home/wt/repo/x","branch":"x","head":"<sha>","main":false,"managed":true,"locked":false,"prunable":false,"changes":0,"upstream":null,"ahead":0,"behind":0},"new_branch":true,"root":"$DIR/repo","cwd":"$DIR/home/wt/repo/x"}
    {"root":"$DIR/repo","project_dir":"$DIR/home/wt/repo","worktrees":[{"path":"$DIR/repo","branch":"main","head":"<sha>","main":true,"managed":false,"locked":false,"prunable":false,"changes":0,"upstream":null,"ahead":0,"behind":0},{"path":"$DIR/home/wt/repo/x","branch":"x","head":"<sha>","main":false,"managed":true,"locked":false,"prunable":false,"changes":0,"upstream":null,"ahead":0,"behind":0}]}
    {"path":"$DIR/home/wt/repo/x","branch":"x","head":"<sha>","main":false,"managed":true,"locked":false,"prunable":false,"changes":0,"upstream":null,"ahead":0,"behind":0}
    error: missing path
    |}]
;;
