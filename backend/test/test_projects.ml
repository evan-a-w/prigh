open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

(* Sessions as projects: where a cwd is in git ([Git_location]), recorded in
   the session and listed by [list_sessions]; subagents in worktrees of
   their own. *)

let sh = Test_worktree.sh
let init_repo = Test_worktree.init_repo
let show = Test_worktree.show

let print_location t cwd =
  match Git_location.find ~cwd:(Filename.concat t.dir cwd) with
  | None -> printf "%-14s -> none\n" cwd
  | Some l ->
    printf
      "%-14s -> root=%s worktree=%s branch=%s%s\n"
      cwd
      (show t l.root)
      (show t l.worktree)
      (match l.branch with
       | Some b
         when String.length b = 8 && String.for_all b ~f:Char.is_hex_digit ->
         "<sha8>"
       | Some b -> b
       | None -> "-")
      (if l.main then " main" else "")
;;

let%expect_test
    "Git_location: main checkout, worktrees, subdirectories, detached, outside"
  =
  with_sandbox
  @@ fun t ->
  init_repo t;
  sh
    t
    "mkdir -p src/deep && git worktree add -q -b feat/x ../wt-x && git \
     worktree add -q --detach ../wt-d && mkdir -p ../wt-x/sub";
  Core_unix.mkdir_p (Filename.concat t.dir "plain/dir");
  List.iter
    [ "repo"
    ; "repo/src/deep"
    ; "wt-x"
    ; "wt-x/sub"
    ; "wt-d"
    ; "plain/dir"
    ; "missing"
    ]
    ~f:(print_location t);
  [%expect
    {|
    repo           -> root=$DIR/repo worktree=$DIR/repo branch=main main
    repo/src/deep  -> root=$DIR/repo worktree=$DIR/repo branch=main main
    wt-x           -> root=$DIR/repo worktree=$DIR/wt-x branch=feat/x
    wt-x/sub       -> root=$DIR/repo worktree=$DIR/wt-x branch=feat/x
    wt-d           -> root=$DIR/repo worktree=$DIR/wt-d branch=<sha8>
    plain/dir      -> none
    missing        -> none
    |}];
  (* On a tool host: [$git_location] answers JSON. *)
  List.iter [ "wt-x/sub"; "plain" ] ~f:(fun cwd ->
    let result =
      Host_ops.execute
        ~mcp:None
        ~env:t.env
        ~cancel:Cancellation.never
        ~on_output:ignore
        ~cwd:(Filename.concat t.dir cwd)
        ~name:Git_location.op
        ~arguments:(`Object [])
        ()
    in
    print_endline (show t result.text);
    print_s
      [%sexp
        (Or_error.map (Git_location.of_result result) ~f:Option.is_some
         : bool Or_error.t)]);
  [%expect
    {|
    {"root":"$DIR/repo","worktree":"$DIR/wt-x","branch":"feat/x","main":false}
    (Ok true)
    null
    (Ok false)
    |}]
;;

let%expect_test "a session records its repository as its cwd changes" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  init_repo t;
  sh t "git worktree add -q -b feat/y ../wt-y";
  Core_unix.mkdir_p (Filename.concat t.dir "plain");
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [ Reply.text "hi" ])
      ~tools:[]
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:(Filename.concat t.dir "repo")
      ()
  in
  let print () =
    let session = Agent.session agent in
    printf
      "%s: %s\n"
      (show t (Session.cwd session))
      (match Session.repo session with
       | None -> "not recorded"
       | Some None -> "not in a repository"
       | Some (Some l) -> sprintf "%s in %s" (show t l.root) (show t l.worktree))
  in
  print ();
  (* A run records it; so does each change of directory. *)
  Or_error.ok_exn (Agent.prompt agent "hello");
  Agent.wait_idle agent;
  print ();
  List.iter [ "../wt-y"; "../plain"; "../repo" ] ~f:(fun path ->
    ignore (Or_error.ok_exn (Agent.set_cwd agent ~path) : Dir_change.Outcome.t);
    print ());
  (* Only changes are written: the same place again adds nothing. *)
  let repos () =
    List.count
      (Session.entries (Agent.session agent))
      ~f:(fun e ->
        match e.payload with
        | Repo _ -> true
        | _ -> false)
  in
  let before = repos () in
  ignore
    (Or_error.ok_exn (Agent.set_cwd agent ~path:".") : Dir_change.Outcome.t);
  printf "new entries: %d\n" (repos () - before);
  (* The summary carries it, from the file too. *)
  let summary =
    List.hd_exn (Session.list ~dir:(Filename.concat t.dir "sessions"))
  in
  print_endline
    (show
       t
       (Sexp.to_string [%sexp (summary.repo : Git_location.t option option)]));
  [%expect
    {|
    $DIR/repo: not recorded
    $DIR/repo: $DIR/repo in $DIR/repo
    $DIR/wt-y: $DIR/repo in $DIR/wt-y
    $DIR/plain: not in a repository
    $DIR/repo: $DIR/repo in $DIR/repo
    new entries: 0
    ((((root $DIR/repo)(worktree $DIR/repo)(branch(main))(main true))))
    |}]
;;

let%expect_test "agent worktrees: recorded, removed, not forked" =
  with_sandbox
  @@ fun t ->
  let session = Session.create ~dir:(Filename.concat t.dir "s") ~cwd:"/r" () in
  ignore (Session.set_name session ~name:"n" : Session.Entry.t);
  let w path =
    { Agent_worktree.agent = "1"
    ; task = "t"
    ; path
    ; branch = Filename.basename path
    ; root = "/r"
    }
  in
  ignore (Session.add_agent_worktree session (w "/t/a") : Session.Entry.t);
  ignore (Session.add_agent_worktree session (w "/t/b") : Session.Entry.t);
  ignore (Session.worktree_removed session ~path:"/t/a" : Session.Entry.t);
  let paths s =
    List.map (Session.agent_worktrees s) ~f:(fun (w : Agent_worktree.t) ->
      w.path)
  in
  let loaded = Or_error.ok_exn (Session.load (Session.path session)) in
  let forked =
    Or_error.ok_exn (Session.fork session ~dir:(Filename.concat t.dir "s"))
  in
  print_s
    [%sexp
      { live = (paths session : string list)
      ; loaded = (paths loaded : string list)
      ; forked = (paths forked : string list)
      }];
  [%expect {| ((live (/t/b)) (loaded (/t/b)) (forked ())) |}]
;;

let subagent ~provider ~trees =
  Tool_subagent.create
    ~provider
    ~worktrees_dir:(fun () -> trees)
    ~current_model:(fun () -> Model.default)
    ~current_thinking:(fun () -> Off)
    ~home:"/nonexistent"
    ()
;;

let%expect_test "a subagent in a worktree of its own" =
  with_sandbox
  @@ fun t ->
  init_repo t;
  Core_unix.mkdir_p (Filename.concat t.dir "repo/src");
  sh t "echo x > src/f && git add -A && git commit -q -m src";
  let trees = Filename.concat t.dir "trees" in
  let provider =
    Faux_provider.create
      [ Reply.tool_call
          ~id:"b1"
          ~name:"bash"
          ~arguments:{|{"command":"pwd; git branch --show-current"}|}
          ()
      ; Reply.text "fixed it"
      ; Reply.text "again"
      ; Reply.text "named"
      ]
  in
  let subagent = subagent ~provider ~trees in
  let events = Queue.create () in
  let run ?(cwd = "repo/src") args =
    let context =
      Tool.Context.create
        ~env:t.env
        ~call_id:"b1"
        ~cwd:(Filename.concat t.dir cwd)
        ~tools:(Tools.all @ [ subagent ])
        ~emit:(Queue.enqueue events)
        ()
    in
    let result = Tool.execute subagent context (Jsonaf.of_string args) in
    printf
      "%s%s\n"
      (if result.is_error then "error: " else "")
      (show t result.text)
  in
  (* "auto": a branch named after the task, from HEAD, in the same
     subdirectory; the report says where the work is. *)
  run {|{"task":"Fix the parser bug","worktree":"auto"}|};
  [%expect
    {|
    fixed it
    [subagent: 2 turns, 30 in / 13 out tokens, $0.0000]
    [worktree: the work is on branch agent/fix-the-parser-bug, checked out in $DIR/trees/repo/agent-fix-the-parser-bug (a worktree of $DIR/repo); review it there, then merge the branch or remove the worktree (/worktree rm $DIR/trees/repo/agent-fix-the-parser-bug)]
    |}];
  (* It ran there. *)
  Queue.iter events ~f:(function
    | Subagent_start { worktree; _ } ->
      print_endline
        (show
           t
           (Sexp.to_string_hum [%sexp (worktree : Agent_worktree.t option)]))
    | Subagent { event = Tool_end { result; _ }; _ } ->
      print_endline (show t result.text)
    | _ -> ());
  [%expect
    {|
    (((agent b1) (task "Fix the parser bug")
      (path
       $DIR/trees/repo/agent-fix-the-parser-bug)
      (branch agent/fix-the-parser-bug)
      (root $DIR/repo)))
    $DIR/trees/repo/agent-fix-the-parser-bug/src
    agent/fix-the-parser-bug
    |}];
  (* The same task again: a fresh branch, not the first one's. *)
  run {|{"task":"Fix the parser bug","worktree":true}|};
  (* A named branch; refusals come before anything runs. *)
  run ~cwd:"repo" {|{"task":"x","worktree":"feat/named"}|};
  run ~cwd:"repo" {|{"task":"x","worktree":"bad..name"}|};
  Core_unix.mkdir_p (Filename.concat t.dir "plain");
  run ~cwd:"plain" {|{"task":"x","worktree":"auto"}|};
  run ~cwd:"repo" {|{"task":"x","worktree":3}|};
  [%expect
    {|
    again
    [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    [worktree: the work is on branch agent/fix-the-parser-bug-2, checked out in $DIR/trees/repo/agent-fix-the-parser-bug-2 (a worktree of $DIR/repo); review it there, then merge the branch or remove the worktree (/worktree rm $DIR/trees/repo/agent-fix-the-parser-bug-2)]
    named
    [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    [worktree: the work is on branch feat/named, checked out in $DIR/trees/repo/feat-named (a worktree of $DIR/repo); review it there, then merge the branch or remove the worktree (/worktree rm $DIR/trees/repo/feat-named)]
    error: could not make the subagent's worktree: not a valid branch name: "bad..name" (e.g. feat/my-change)
    error: could not make the subagent's worktree: not a git repository: $DIR/plain (git init starts one, or /cd into one)
    error: invalid arguments: worktree must be a branch name or "auto" (or true)
    |}]
;;

(* [list_sessions]' [repo] and [agent_worktrees] fields, as a client sees
   them. *)
let print_sessions t json =
  match json with
  | `Object fields ->
    (match List.Assoc.find fields ~equal:String.equal "result" with
     | Some (`Array sessions) ->
       List.iter sessions ~f:(fun s ->
         let field name =
           Option.value_map
             (Jsonaf.member name s)
             ~default:"absent"
             ~f:(fun j -> show t (Jsonaf.to_string j))
         in
         printf
           "%s cwd=%s\n  repo=%s\n  agent_worktrees=%s\n"
           (field "name")
           (field "cwd")
           (field "repo")
           (field "agent_worktrees"))
     | _ -> print_endline (show t (Jsonaf.to_string json)))
  | _ -> print_endline (show t (Jsonaf.to_string json))
;;

let%expect_test "over RPC: sessions by project, subagents' worktrees, removal" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let sessions_dir = Filename.concat t.dir "sessions" in
  let repo = Filename.concat t.dir "repo" in
  let trees = Filename.concat t.dir "trees" in
  init_repo t;
  init_repo ~dir:"other" t;
  Core_unix.mkdir_p (Filename.concat t.dir "plain");
  Config.save ~home:t.dir { Config.default with worktrees_dir = trees }
  |> Or_error.ok_exn;
  let provider =
    Routed_provider.create
      ~main:
        [ Reply.tool_call
            ~id:"s1"
            ~name:"subagent"
            ~arguments:{|{"task":"Tidy up","worktree":"auto"}|}
            ()
        ; Reply.text "started"
        ; Reply.text "thanks"
        ]
      [ "Tidy up", [ Reply.text "tidied" ] ]
  in
  let new_agent ?session ~cwd () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:(Tools.all @ [ subagent ~provider ~trees ])
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
  let call client ?(params = "{}") meth =
    Rpc_server.handle
      server
      client
      (Jsonaf.of_string
         (sprintf {|{"id": "r1", "method": "%s", "params": %s}|} meth params))
  in
  let call_ client ?params meth =
    let reply = call client ?params meth in
    match Jsonaf.member "ok" reply with
    | Some `True -> ()
    | _ -> print_endline (show t (Jsonaf.to_string reply))
  in
  let session ~name ~cwd =
    let c = Rpc_server.connect server ~send:ignore in
    call_ c ~params:(sprintf {|{"path": %S}|} cwd) "set_cwd";
    call_ c ~params:(sprintf {|{"name": %S}|} name) "set_session_name";
    c
  in
  (* Sessions in the main checkout, in a worktree, in another repository and
     in a plain folder. *)
  let main = session ~name:"main" ~cwd:repo in
  call_
    main
    ~params:(sprintf {|{"branch": "feat/w", "cwd": %S}|} repo)
    "create_worktree";
  let wt = Filename.concat trees "repo/feat-w" in
  let _in_wt = session ~name:"in worktree" ~cwd:wt in
  let _other = session ~name:"other" ~cwd:(Filename.concat t.dir "other") in
  let _plain = session ~name:"plain" ~cwd:(Filename.concat t.dir "plain") in
  (* A session from before repositories were recorded: looked up from the
     backend's files. *)
  let old = Session.create ~dir:sessions_dir ~cwd:repo () in
  ignore (Session.set_name old ~name:"old" : Session.Entry.t);
  (* A subagent asked for a worktree. *)
  call_ main ~params:{|{"text": "go"}|} "prompt";
  let agent = Rpc_server.agent_of_client server main in
  Agent.wait_idle agent;
  while Agent.has_running_subagents agent || Agent.is_running agent do
    Eio.Fiber.yield ();
    Agent.wait_idle agent
  done;
  print_sessions t (call main "list_sessions");
  [%expect
    {|
    "main" cwd="$DIR/repo"
      repo={"root":"$DIR/repo","worktree":"$DIR/repo","branch":"main","main":true}
      agent_worktrees=[{"agent":"a1","task":"Tidy up","path":"$DIR/trees/repo/agent-tidy-up","branch":"agent/tidy-up","root":"$DIR/repo"}]
    "old" cwd="$DIR/repo"
      repo={"root":"$DIR/repo","worktree":"$DIR/repo","branch":"main","main":true}
      agent_worktrees=[]
    "plain" cwd="$DIR/plain"
      repo=null
      agent_worktrees=[]
    "other" cwd="$DIR/other"
      repo={"root":"$DIR/other","worktree":"$DIR/other","branch":"main","main":true}
      agent_worktrees=[]
    "in worktree" cwd="$DIR/trees/repo/feat-w"
      repo={"root":"$DIR/repo","worktree":"$DIR/trees/repo/feat-w","branch":"feat/w","main":false}
      agent_worktrees=[]
    |}];
  (* Removing it from any session (here one in another repository, naming
     the repository) forgets it. *)
  let agent_wt = Filename.concat trees "repo/agent-tidy-up" in
  let other = session ~name:"remover" ~cwd:(Filename.concat t.dir "other") in
  call_
    other
    ~params:(sprintf {|{"path": %S, "cwd": %S}|} agent_wt repo)
    "remove_worktree";
  print_sessions t (call main "list_sessions");
  [%expect
    {|
    "main" cwd="$DIR/repo"
      repo={"root":"$DIR/repo","worktree":"$DIR/repo","branch":"main","main":true}
      agent_worktrees=[]
    "remover" cwd="$DIR/other"
      repo={"root":"$DIR/other","worktree":"$DIR/other","branch":"main","main":true}
      agent_worktrees=[]
    "old" cwd="$DIR/repo"
      repo={"root":"$DIR/repo","worktree":"$DIR/repo","branch":"main","main":true}
      agent_worktrees=[]
    "plain" cwd="$DIR/plain"
      repo=null
      agent_worktrees=[]
    "other" cwd="$DIR/other"
      repo={"root":"$DIR/other","worktree":"$DIR/other","branch":"main","main":true}
      agent_worktrees=[]
    "in worktree" cwd="$DIR/trees/repo/feat-w"
      repo={"root":"$DIR/repo","worktree":"$DIR/trees/repo/feat-w","branch":"feat/w","main":false}
      agent_worktrees=[]
    |}];
  (* One removed outside prigh: removing it only forgets it. *)
  let gone = Filename.concat trees "repo/gone" in
  ignore
    (Session.add_agent_worktree
       (Agent.session agent)
       { agent = "9"; task = "t"; path = gone; branch = "gone"; root = repo }
     : Session.Entry.t);
  print_endline
    (show
       t
       (Jsonaf.to_string
          (call
             other
             ~params:(sprintf {|{"path": %S, "cwd": %S}|} gone repo)
             "remove_worktree")));
  print_s
    [%sexp (List.length (Session.agent_worktrees (Agent.session agent)) : int)];
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{"worktree":{"path":"$DIR/trees/repo/gone","branch":"gone","head":null,"main":false,"managed":false,"locked":false,"prunable":true,"changes":null,"upstream":null,"ahead":0,"behind":0,"current":false,"sessions":[]}}}
    0
    |}]
;;
