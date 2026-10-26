open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

(* Worktrees the model makes: with the [worktree] tool (the main agent's
   recorded as made in the session, a subagent's as that subagent's), and
   with [git worktree] in bash (the safety net). *)

let sh = Test_worktree.sh
let init_repo = Test_worktree.init_repo
let show = Test_worktree.show
let call_tool ~id ~name arguments = Reply.tool_call ~id ~name ~arguments ()
let worktree ~id arguments = call_tool ~id ~name:"worktree" arguments

let bash ~id command =
  call_tool
    ~id
    ~name:"bash"
    (Jsonaf.to_string (`Object [ "command", `String command ]))
;;

(* The sessions' [worktrees], as [list_sessions] gives them. *)
let print_sessions t json =
  match Jsonaf.member "result" json with
  | Some (`Array sessions) ->
    List.iter sessions ~f:(fun s ->
      match Jsonaf.member "worktrees" s with
      | Some (`Array worktrees) ->
        List.iter worktrees ~f:(fun w ->
          print_endline (show t (Jsonaf.to_string w)))
      | _ -> print_endline (show t (Jsonaf.to_string s)))
  | _ -> print_endline (show t (Jsonaf.to_string json))
;;

let with_server script f =
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
  let main, children = script ~trees in
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
  let client = Rpc_server.connect server ~send:ignore in
  let call ?(params = "{}") meth =
    Rpc_server.handle
      server
      client
      (Jsonaf.of_string
         (sprintf {|{"id": "r1", "method": "%s", "params": %s}|} meth params))
  in
  ignore (call ~params:(sprintf {|{"path": %S}|} repo) "set_cwd" : Jsonaf.t);
  let agent = Rpc_server.agent_of_client server client in
  let printed = ref 0 in
  let subagents_printed = String.Hash_set.create () in
  (* Runs a prompt to the end (background subagents included) and prints the
     tool results it got, then the session's worktrees. *)
  let prompt text =
    ignore
      (call
         ~params:(Jsonaf.to_string (`Object [ "text", `String text ]))
         "prompt"
       : Jsonaf.t);
    Agent.wait_idle agent;
    while Agent.has_running_subagents agent || Agent.is_running agent do
      Eio.Fiber.yield ();
      Agent.wait_idle agent
    done;
    let print_results ?(prefix = "") messages =
      List.iter messages ~f:(function
        | Message.Tool_result r ->
          printf
            "%s%s%s: %s\n"
            prefix
            r.tool_name
            (if r.is_error then " (error)" else "")
            (show t r.text)
        | User _ | Assistant _ -> ())
    in
    let messages = Agent.messages agent in
    print_results (List.drop messages !printed);
    printed := List.length messages;
    List.iter (Agent.subagents agent) ~f:(fun (s : Subagent_log.Summary.t) ->
      if not (Hash_set.mem subagents_printed s.id)
      then (
        Hash_set.add subagents_printed s.id;
        Option.iter (Agent.subagent agent s.id) ~f:(fun (_, transcript) ->
          print_results
            ~prefix:(sprintf "[%s] " s.id)
            (List.map transcript ~f:(fun (m : Timed_message.t) -> m.message)))));
    print_endline "-- worktrees:";
    print_sessions t (call "list_sessions")
  in
  f t ~prompt
;;

let%expect_test "the worktree tool: made, listed, removed; a subagent's" =
  with_server (fun ~trees ->
    let feat_b = Filename.concat trees "repo/feat-b" in
    ( [ worktree ~id:"w1" {|{"action":"create","branch":"feat/a"}|}
      ; worktree ~id:"w2" {|{"action":"list"}|}
      ; worktree ~id:"w3" {|{"action":"create","branch":"feat/a"}|}
      ; Reply.text "made one"
      ; call_tool ~id:"s1" ~name:"subagent" {|{"task":"Nested work"}|}
      ; Reply.text "started"
      ; Reply.text "thanks"
      ; worktree ~id:"r1" {|{"action":"remove","path":"."}|}
      ; worktree ~id:"r2" {|{"action":"remove","path":"../trees/repo/feat-a"}|}
      ; worktree
          ~id:"r3"
          {|{"action":"remove","path":"../trees/repo/feat-a","force":true}|}
      ; call_tool
          ~id:"s2"
          ~name:"subagent"
          (sprintf {|{"task":"Remove mine","cwd":%S}|} feat_b)
      ; Reply.text "started"
      ; Reply.text "ok"
      ]
    , [ ( "Nested work"
        , [ worktree
              ~id:"n1"
              {|{"action":"create","branch":"feat/b","base":"main"}|}
          ; Reply.text "made mine"
          ] )
      ; ( "Remove mine"
        , [ worktree ~id:"n2" {|{"action":"list"}|}
          ; worktree ~id:"n3" (sprintf {|{"action":"remove","path":%S}|} feat_b)
          ; Reply.text "it is in use"
          ] )
      ] ))
  @@ fun t ~prompt ->
  prompt "make one";
  [%expect
    {|
    worktree: made worktree $DIR/trees/repo/feat-a, a checkout of $DIR/repo on the new branch feat/a
    Work there: cd '$DIR/trees/repo/feat-a' in bash commands, paths under it in file tools, or pass it as a subagent's cwd. Remove it with action=remove when it is done with (its branch stays).
    worktree: worktrees of $DIR/repo (new ones go in $DIR/trees/repo):
    - $DIR/repo  main  clean  [main checkout]  [you are here]
    - $DIR/trees/repo/feat-a  feat/a  clean  [made in this session]
    worktree (error): could not make the worktree: branch feat/a is already checked out at $DIR/trees/repo/feat-a: open that worktree instead
    (worktree action=list shows the worktrees and their branches)
    -- worktrees:
    {"path":"$DIR/trees/repo/feat-a","branch":"feat/a","root":"$DIR/repo","source":"created"}
    |}];
  prompt "and a subagent";
  [%expect
    {|
    subagent: started agent a1 (Nested work); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    [a1] worktree: made worktree $DIR/trees/repo/feat-b, a checkout of $DIR/repo on the new branch feat/b (from main)
    Work there: cd '$DIR/trees/repo/feat-b' in bash commands, paths under it in file tools, or pass it as a subagent's cwd. Remove it with action=remove when it is done with (its branch stays).
    -- worktrees:
    {"path":"$DIR/trees/repo/feat-a","branch":"feat/a","root":"$DIR/repo","source":"created"}
    {"path":"$DIR/trees/repo/feat-b","branch":"feat/b","root":"$DIR/repo","source":"agent","agent":"a1","task":"Nested work"}
    |}];
  sh t "echo x > ../trees/repo/feat-a/new.txt";
  prompt "tidy up";
  [%expect
    {|
    worktree (error): could not remove the worktree: $DIR/repo is the repository's main worktree: it cannot be removed
    (worktree action=list shows the worktrees)
    worktree (error): could not remove the worktree: $DIR/trees/repo/feat-a has uncommitted changes (1 file): commit or stash them first, or force the removal
    (worktree action=list shows the worktrees)
    worktree: removed worktree $DIR/trees/repo/feat-a (the branch feat/a stays)
    subagent: started agent a2 (Remove mine); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    [a2] worktree: worktrees of $DIR/repo (new ones go in $DIR/trees/repo):
    - $DIR/repo  main  clean  [main checkout]
    - $DIR/trees/repo/feat-b  feat/b  clean  [you are here]  [made for subagent a1]
    [a2] worktree (error): could not remove the worktree: $DIR/trees/repo/feat-b is in use: $DIR/trees/repo/feat-b, inside it, is where the agent or the session works (work elsewhere first; the session moves with /cd)
    (worktree action=list shows the worktrees)
    -- worktrees:
    {"path":"$DIR/trees/repo/feat-b","branch":"feat/b","root":"$DIR/repo","source":"agent","agent":"a1","task":"Nested work"}
    |}]
;;

let%expect_test "git worktree in bash: recorded, with a note when outside" =
  with_server (fun ~trees ->
    ( [ bash ~id:"b1" "git worktree add -q -b feat/c ../outside"
      ; bash
          ~id:"b2"
          (sprintf
             "git worktree add -q -b feat/d %s"
             (Filename.concat trees "repo/feat-d"))
      ; bash ~id:"b3" "echo unrelated"
      ; Reply.text "made two"
      ; bash ~id:"b4" "git worktree remove ../outside"
      ; call_tool ~id:"s1" ~name:"subagent" {|{"task":"By hand"}|}
      ; Reply.text "started"
      ; Reply.text "ok"
      ]
    , [ ( "By hand"
        , [ bash ~id:"n1" "git worktree add -q -b feat/e ../by-hand"
          ; Reply.text "done"
          ] )
      ] ))
  @@ fun t ~prompt ->
  (* The project directory exists once prigh made a worktree there. *)
  ignore
    (Worktree.create
       ~env:t.env
       ~cancel:Cancellation.never
       ~cwd:(Filename.concat t.dir "repo")
       ~dir:(Filename.concat t.dir "trees")
       ~branch:"feat/first"
       ~base:None
       ()
     : Worktree.Created.t Or_error.t);
  prompt "make two by hand";
  [%expect
    {|
    bash:
    [prigh: git worktree add made $DIR/outside outside the standard place ($DIR/trees/repo). It is recorded on the session, but next time use the worktree tool (or the subagent tool's worktree option) rather than git worktree add.]
    bash:
    bash: unrelated

    -- worktrees:
    {"path":"$DIR/outside","branch":"feat/c","root":"$DIR/repo","source":"created"}
    {"path":"$DIR/trees/repo/feat-d","branch":"feat/d","root":"$DIR/repo","source":"created"}
    |}];
  prompt "remove one, and a subagent";
  [%expect
    {|
    bash:
    subagent: started agent a1 (By hand); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    [a1] bash:
    [prigh: git worktree add made $DIR/by-hand outside the standard place ($DIR/trees/repo). It is recorded on the session, but next time use the worktree tool (or the subagent tool's worktree option) rather than git worktree add.]
    -- worktrees:
    {"path":"$DIR/trees/repo/feat-d","branch":"feat/d","root":"$DIR/repo","source":"created"}
    {"path":"$DIR/by-hand","branch":"feat/e","root":"$DIR/repo","source":"agent","agent":"a1","task":"By hand"}
    |}]
;;
