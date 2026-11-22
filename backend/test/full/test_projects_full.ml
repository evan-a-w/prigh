open! Core
open! Prigh
open Tool_test_helpers
open! Test_projects

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
  let other_session =
    session ~name:"other" ~cwd:(Filename.concat t.dir "other")
  in
  (* Made from a session in another repository, naming the repository: that
     session records it. *)
  call_
    other_session
    ~params:(sprintf {|{"branch": "feat/for-other", "cwd": %S}|} repo)
    "create_worktree";
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
    "in worktree" cwd="$DIR/trees/repo/feat-w"
      repo={"root":"$DIR/repo","worktree":"$DIR/trees/repo/feat-w","branch":"feat/w","main":false}
      worktrees=[{"path":"$DIR/trees/repo/feat-w","branch":"feat/w","root":"$DIR/repo","source":"entered"}]
    "main" cwd="$DIR/repo"
      repo={"root":"$DIR/repo","worktree":"$DIR/repo","branch":"main","main":true}
      worktrees=[{"path":"$DIR/trees/repo/feat-w","branch":"feat/w","root":"$DIR/repo","source":"created"},{"path":"$DIR/trees/repo/agent-tidy-up","branch":"agent/tidy-up","root":"$DIR/repo","source":"agent","agent":"a1","task":"Tidy up"}]
    "old" cwd="$DIR/repo"
      repo={"root":"$DIR/repo","worktree":"$DIR/repo","branch":"main","main":true}
      worktrees=[]
    "other" cwd="$DIR/other"
      repo={"root":"$DIR/other","worktree":"$DIR/other","branch":"main","main":true}
      worktrees=[{"path":"$DIR/trees/repo/feat-for-other","branch":"feat/for-other","root":"$DIR/repo","source":"created"}]
    "plain" cwd="$DIR/plain"
      repo=null
      worktrees=[]
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
    "in worktree" cwd="$DIR/trees/repo/feat-w"
      repo={"root":"$DIR/repo","worktree":"$DIR/trees/repo/feat-w","branch":"feat/w","main":false}
      worktrees=[{"path":"$DIR/trees/repo/feat-w","branch":"feat/w","root":"$DIR/repo","source":"entered"}]
    "main" cwd="$DIR/repo"
      repo={"root":"$DIR/repo","worktree":"$DIR/repo","branch":"main","main":true}
      worktrees=[{"path":"$DIR/trees/repo/feat-w","branch":"feat/w","root":"$DIR/repo","source":"created"}]
    "old" cwd="$DIR/repo"
      repo={"root":"$DIR/repo","worktree":"$DIR/repo","branch":"main","main":true}
      worktrees=[]
    "other" cwd="$DIR/other"
      repo={"root":"$DIR/other","worktree":"$DIR/other","branch":"main","main":true}
      worktrees=[{"path":"$DIR/trees/repo/feat-for-other","branch":"feat/for-other","root":"$DIR/repo","source":"created"}]
    "plain" cwd="$DIR/plain"
      repo=null
      worktrees=[]
    "remover" cwd="$DIR/other"
      repo={"root":"$DIR/other","worktree":"$DIR/other","branch":"main","main":true}
      worktrees=[]
    |}];
  (* One removed outside prigh: removing it only forgets it. *)
  let gone = Filename.concat trees "repo/gone" in
  ignore
    (Session.add_agent_worktree
       (Agent.session agent)
       { agent = "9"
       ; task = "t"
       ; path = gone
       ; branch = "gone"
       ; root = repo
       ; host = None
       }
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
    [%sexp
      (List.map
         (Session.worktrees (Agent.session agent))
         ~f:(fun (w : Session_worktree.t) -> Filename.basename w.path)
       : string list)];
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{"worktree":{"path":"$DIR/trees/repo/gone","branch":"gone","head":null,"main":false,"managed":false,"locked":false,"prunable":true,"changes":null,"upstream":null,"ahead":0,"behind":0,"current":false,"sessions":[]}}}
    (feat-w)
    |}]
;;
