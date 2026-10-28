open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

(* The [pull_request] tool on {!Fake_forge.set_up_repos}'s repository and
   {!Fake_forge.github}: git runs for real (pushes and fetches go to a local
   bare repository), the API is the fake's. *)

let sign_in store ~token =
  Or_error.ok_exn
    (Auth_store.modify_entry
       store
       (Forge_credential.key ~host:"github.com")
       ~f:(fun _ ->
         Ok
           (Some
              (Forge_credential.to_json
                 { kind = Github
                 ; method_ = Token
                 ; token
                 ; refresh = None
                 ; expires_ms = None
                 ; client_id = None
                 ; user = Some "octocat"
                 }))))
  |> (ignore : Jsonaf.t option -> unit)
;;

let forges ~fake ~home store =
  Forges.create
    ~http:(Fake_forge.http fake)
    ~sleep:(fun _ -> ())
    ~getenv:(fun _ -> None)
    ~home
    ~store
    ()
;;

let with_repo f =
  with_sandbox
  @@ fun t ->
  let repo = Fake_forge.set_up_repos t in
  let fake = Fake_forge.create Fake_forge.github in
  let store = Auth_store.create ~path:(Filename.concat t.dir "auth.json") in
  let tool =
    Tool_pull_request.create
      ~forges:(forges ~fake ~home:t.dir store)
      ~worktrees_dir:(fun () -> Filename.concat t.dir "worktrees")
      ()
  in
  let run ?(cwd = repo) args =
    let context =
      Tool.Context.create
        ~env:t.env
        ~cwd
        ~emit:(function
          | Worktrees_changed { added; removed } ->
            print_endline
              (mask_sexp
                 t
                 [%sexp
                   "worktrees_changed"
                   , (added : Session_worktree.t list)
                   , (removed : string list)])
          | _ -> ())
        ()
    in
    let result = Tool.execute tool context (Jsonaf.of_string args) in
    printf
      "%s%s\n"
      (if result.is_error then "ERROR: " else "")
      (mask t result.text)
  in
  f t ~repo ~fake ~store ~tool ~run
;;

let%expect_test "not signed in: says where to sign in, asks nothing of the host" =
  with_repo
  @@ fun _ ~repo:_ ~fake ~store:_ ~tool:_ ~run ->
  run {|{"action":"list"}|};
  Fake_forge.print_requests fake;
  [%expect {| ERROR: pull_request list: not signed in to github.com: ask the user to sign in under Settings → Git hosting in prigh-web, or with /login github |}]
;;

let%expect_test "list, view, checks and diff" =
  with_repo
  @@ fun _ ~repo:_ ~fake ~store ~tool:_ ~run ->
  sign_in store ~token:"ghp_mine";
  run {|{"action":"list"}|};
  run {|{"action":"list","state":"merged"}|};
  run {|{"action":"view","number":7}|};
  run {|{"action":"checks","number":7}|};
  run {|{"action":"diff","number":7}|};
  run {|{"action":"view","number":99}|};
  run {|{"action":"view"}|};
  run {|{"action":"merge","number":7}|};
  Fake_forge.print_requests fake;
  [%expect {|
    open pull requests in o/r on github.com:
    #7 From a contributor (open; alice, fork:feat/x -> main) https://github.com/o/r/pull/7
    #5 Old one (open, draft; alice, feat/x -> main) https://github.com/o/r/pull/5
    ERROR: pull_request list: unknown state "merged": open, closed or all
    #7 From a contributor (open) in o/r on github.com
    https://github.com/o/r/pull/7
    alice wants to merge fork:feat/x into main
    created 2026-10-20T10:00:00Z, updated 2026-10-21T10:00:00Z
    checks: success
    - build: success https://ci/1

    Does x.
    #7 From a contributor (head abc123)
    checks: success
    - build: success https://ci/1
    diff of #7 From a contributor (fork:feat/x -> main):
    diff --git a/b b/b
    new file mode 100644
    --- /dev/null
    +++ b/b
    @@ -0,0 +1 @@
    +b

    ERROR: pull_request view: github.com: pull request #99 of o/r not found (HTTP 404): action=list lists the open ones; the closest: #7 From a contributor (open), #5 Old one (open, draft)
    ERROR: invalid arguments: missing "number": the pull request's number (action=list shows them)
    ERROR: invalid arguments: unknown action "merge": use list, view, checks, diff, create or checkout
    GET https://api.github.com/repos/o/r/pulls?state=open&per_page=100&sort=updated&direction=desc
      Authorization: Bearer ghp_mine
    GET https://api.github.com/repos/o/r/pulls/7
      Authorization: Bearer ghp_mine
    GET https://api.github.com/repos/o/r/commits/abc123/check-runs?per_page=100
      Authorization: Bearer ghp_mine
    GET https://api.github.com/repos/o/r/commits/abc123/status
      Authorization: Bearer ghp_mine
    GET https://api.github.com/repos/o/r/pulls/7
      Authorization: Bearer ghp_mine
    GET https://api.github.com/repos/o/r/commits/abc123/check-runs?per_page=100
      Authorization: Bearer ghp_mine
    GET https://api.github.com/repos/o/r/commits/abc123/status
      Authorization: Bearer ghp_mine
    GET https://api.github.com/repos/o/r/pulls/7
      Authorization: Bearer ghp_mine
    GET https://api.github.com/repos/o/r/pulls/7
      Authorization: Bearer ghp_mine
      Accept: application/vnd.github.diff
    GET https://api.github.com/repos/o/r/commits/abc123/check-runs?per_page=100
      Authorization: Bearer ghp_mine
    GET https://api.github.com/repos/o/r/commits/abc123/status
      Authorization: Bearer ghp_mine
    GET https://api.github.com/repos/o/r/pulls/99
      Authorization: Bearer ghp_mine
    GET https://api.github.com/repos/o/r/pulls?state=all&per_page=100&sort=updated&direction=desc
      Authorization: Bearer ghp_mine
    |}]
;;

let%expect_test "create: pushes the branch first, or refuses an unpushed one" =
  with_repo
  @@ fun t ~repo ~fake ~store ~tool:_ ~run ->
  sign_in store ~token:"ghp_mine";
  let reply ?draft ~number title =
    Fake_forge.create_reply
    := ( 201
       , Fake_forge.pr_json
           ?draft
           ~head:"feat/add-things"
           ~host:"github.com"
           ~number
           ~title
           () )
  in
  reply ~draft:true ~number:8 "Add things";
  run {|{"action":"create","title":"Add things","push":false}|};
  run
    {|{"action":"create","title":"Add things","body":"Adds c and d.","draft":true}|};
  print_endline
    (Fake_forge.git_out
       (Filename.concat t.dir "bare.git")
       "log --format=%s feat/add-things");
  (* Pushed now: without pushing is fine until there is a new commit. *)
  reply ~number:9 "Again";
  run {|{"action":"create","title":"Again","push":false,"base":"main"}|};
  Fake_forge.sh repo "echo e > e && git add e && git commit -qm 'Add e'";
  run {|{"action":"create","title":"Again","push":false}|};
  (* Without a title: the branch's name, and the commits' subjects. *)
  reply ~number:10 "Add things";
  run {|{"action":"create","title":" "}|};
  Fake_forge.print_requests fake;
  [%expect {|
    ERROR: pull_request create: origin does not have feat/add-things with all its commits: push it first (the push option, or git push -u origin feat/add-things)
    pushed feat/add-things to origin, then opened #8 Add things (open, draft) in o/r on github.com
    https://github.com/o/r/pull/8
    alice wants to merge feat/add-things into main
    It is a draft.
    Add d
    Add c
    Initial
    opened #9 Again (open) in o/r on github.com
    https://github.com/o/r/pull/9
    alice wants to merge feat/add-things into main
    ERROR: pull_request create: origin does not have feat/add-things with all its commits: push it first (the push option, or git push -u origin feat/add-things)
    pushed feat/add-things to origin, then opened #10 Add things (open) in o/r on github.com
    https://github.com/o/r/pull/10
    alice wants to merge feat/add-things into main
    GET https://api.github.com/repos/o/r
      Authorization: Bearer ghp_mine
    GET https://api.github.com/repos/o/r
      Authorization: Bearer ghp_mine
    POST https://api.github.com/repos/o/r/pulls
      Authorization: Bearer ghp_mine
      {"title":"Add things","body":"Adds c and d.","head":"feat/add-things","base":"main","draft":true}
    POST https://api.github.com/repos/o/r/pulls
      Authorization: Bearer ghp_mine
      {"title":"Again","body":"","head":"feat/add-things","base":"main","draft":false}
    GET https://api.github.com/repos/o/r
      Authorization: Bearer ghp_mine
    GET https://api.github.com/repos/o/r
      Authorization: Bearer ghp_mine
    GET https://api.github.com/repos/o/r
      Authorization: Bearer ghp_mine
    POST https://api.github.com/repos/o/r/pulls
      Authorization: Bearer ghp_mine
      {"title":"Add things","body":"- Add c\n- Add d\n- Add e","head":"feat/add-things","base":"main","draft":false}
    |}]
;;

let%expect_test "checkout: into a worktree the session records; cwd picks the \
                 repository"
  =
  with_repo
  @@ fun t ~repo ~fake ~store ~tool:_ ~run ->
  sign_in store ~token:"ghp_mine";
  run {|{"action":"checkout","number":7}|};
  let worktree = Filename.concat t.dir "worktrees/r/pr-7" in
  print_endline (Fake_forge.git_out worktree "log --format=%s -1");
  run {|{"action":"checkout","number":7}|};
  run {|{"action":"checkout","number":3}|};
  (* From another directory, naming the worktree. *)
  run ~cwd:t.dir (sprintf {|{"action":"list","cwd":%S}|} worktree);
  run ~cwd:t.dir {|{"action":"list"}|};
  Fake_forge.sh repo "git remote remove origin && git remote add upstream /srv/x.git";
  run {|{"action":"list"}|};
  Fake_forge.sh repo "git checkout -q --detach";
  run {|{"action":"create","title":"Detached"}|};
  Fake_forge.print_requests fake;
  [%expect {|
    (worktrees_changed
     (((path $DIR/worktrees/r/pr-7) (branch (pr-7)) (root $DIR/r)
       (source (Pull_request (number 7)))))
     ())
    checked #7 out in $DIR/worktrees/r/pr-7, on the branch pr-7 (a new worktree)
    Work there: cd '$DIR/worktrees/r/pr-7' in bash commands, paths under it in file tools, or pass it as a subagent's cwd. Remove it with the worktree tool when it is done with.
    From a contributor
    (worktrees_changed
     (((path $DIR/worktrees/r/pr-7) (branch (pr-7)) (root $DIR/r)
       (source (Pull_request (number 7)))))
     ())
    checked #7 out in $DIR/worktrees/r/pr-7, on the branch pr-7 (its worktree already, fast-forwarded)
    Work there: cd '$DIR/worktrees/r/pr-7' in bash commands, paths under it in file tools, or pass it as a subagent's cwd. Remove it with the worktree tool when it is done with.
    ERROR: pull_request checkout: git fetch origin refs/pull/3/head (is #3 a pull request of origin's repository? action=list lists them): fatal: couldn't find remote ref refs/pull/3/head; the closest: #5 Old one (open, draft), #7 From a contributor (open)
    open pull requests in o/r on github.com:
    #7 From a contributor (open; alice, fork:feat/x -> main) https://github.com/o/r/pull/7
    #5 Old one (open, draft; alice, feat/x -> main) https://github.com/o/r/pull/5
    ERROR: pull_request list: not a git repository: $DIR (cwd names one)
    ERROR: pull_request list: no GitHub or Forgejo remote in $DIR/r (git remote -v lists upstream /srv/x.git): add one with git remote add origin URL
    ERROR: pull_request create: no GitHub or Forgejo remote in $DIR/r (git remote -v lists upstream /srv/x.git): add one with git remote add origin URL
    GET https://api.github.com/repos/o/r/pulls?state=all&per_page=100&sort=updated&direction=desc
      Authorization: Bearer ghp_mine
    GET https://api.github.com/repos/o/r/pulls?state=open&per_page=100&sort=updated&direction=desc
      Authorization: Bearer ghp_mine
    |}]
;;

let%expect_test "namespaces: each tool uses only its own world's sign-in" =
  with_sandbox
  @@ fun t ->
  let repo = Fake_forge.set_up_repos t in
  let fake = Fake_forge.create Fake_forge.github in
  let tool name =
    let world =
      Namespace.world
        { name; token = name ^ "-token" }
        ~home:t.dir
        ~legacy_auth_file:(Filename.concat t.dir "auth.json")
    in
    world, Tool_pull_request.create ~forges:(forges ~fake ~home:world.home world.store) ()
  in
  let alice_world, alice = tool "alice" in
  let _, bob = tool "bob" in
  sign_in alice_world.store ~token:"ghp_alice";
  let run who tool =
    let result =
      Tool.execute
        tool
        (Tool.Context.create ~env:t.env ~cwd:repo ())
        (Jsonaf.of_string {|{"action":"list"}|})
    in
    printf
      "%s: %s%s\n"
      who
      (if result.is_error then "ERROR: " else "")
      (List.hd_exn (String.split_lines (mask t result.text)))
  in
  run "alice" alice;
  run "bob" bob;
  Fake_forge.print_requests fake;
  [%expect {|
    alice: open pull requests in o/r on github.com:
    bob: ERROR: pull_request list: not signed in to github.com: ask the user to sign in under Settings → Git hosting in prigh-web, or with /login github
    GET https://api.github.com/repos/o/r/pulls?state=open&per_page=100&sort=updated&direction=desc
      Authorization: Bearer ghp_alice
    |}]
;;

(* An agent driven by [Faux_provider], printing confirmations and results. *)
let with_agent ~answer replies ~sub_replies f =
  with_repo
  @@ fun t ~repo ~fake ~store ~tool ~run:_ ->
  sign_in store ~token:"ghp_mine";
  Eio.Switch.run
  @@ fun sw ->
  let subagent =
    Tool_subagent.create
      ~provider:(Faux_provider.create sub_replies)
      ~current_model:(fun () -> Model.default)
      ~current_thinking:(fun () -> Off)
      ~home:t.dir
      ()
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create replies)
      ~tools:(Tools.all @ [ subagent; tool ])
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:repo
      ()
  in
  Agent.subscribe agent ~f:(function
    | Loop (Tool_confirm { call_id; agent_id; name; summary; rules }) ->
      printf
        "confirm %s%s %s: %s\n"
        (Option.value_map agent_id ~default:"" ~f:(fun a -> a ^ "/"))
        call_id
        name
        (mask t summary);
      Or_error.ok_exn
        (Agent.respond_confirm
           ?agent_id
           agent
           ~call_id
           (answer { Agent.Pending_confirm.call_id; agent_id; name; summary; rules }))
    | Loop (Tool_end { call; result }) ->
      printf
        "%s %s -> %s%s\n"
        call.name
        (mask t call.arguments)
        (if result.is_error then "ERROR " else "")
        (mask t (String.strip result.text))
    | Loop (Subagent { event = Tool_end { call; result }; agent_id; _ }) ->
      printf
        "  [%s] %s -> %s%s\n"
        agent_id
        call.name
        (if result.is_error then "ERROR " else "")
        (mask t (String.strip result.text))
    | _ -> ());
  f t agent ~fake
;;

let%expect_test "an agent lists without asking; create asks in ask mode" =
  with_agent
    ~answer:(fun c ->
      if String.is_substring c.summary ~substring:"Denied"
      then Deny (Some "not yet")
      else Allow)
    ~sub_replies:
      [ Reply.tool_call
          ~id:"s1"
          ~name:"pull_request"
          ~arguments:{|{"action":"checks","number":7}|}
          ()
      ; Reply.tool_call
          ~id:"s2"
          ~name:"pull_request"
          ~arguments:{|{"action":"create","title":"Denied too"}|}
          ()
      ; Reply.text "#7 passes"
      ]
    [ Reply.tool_call
        ~id:"c1"
        ~name:"pull_request"
        ~arguments:{|{"action":"list"}|}
        ()
    ; Reply.tool_call
        ~id:"c2"
        ~name:"pull_request"
        ~arguments:{|{"action":"create","title":"Denied","push":false}|}
        ()
    ; Reply.tool_call
        ~id:"c3"
        ~name:"subagent"
        ~arguments:{|{"task":"look at #7"}|}
        ()
    ; Reply.text "started"
    ; Reply.text "noted"
    ]
  @@ fun _ agent ~fake ->
  Agent.set_mode agent ~approval:Ask;
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  Fake_forge.print_requests fake;
  [%expect {|
    pull_request {"action":"list"} -> open pull requests in o/r on github.com:
    #7 From a contributor (open; alice, fork:feat/x -> main) https://github.com/o/r/pull/7
    #5 Old one (open, draft; alice, feat/x -> main) https://github.com/o/r/pull/5
    confirm c2 pull_request: create Denied
    pull_request {"action":"create","title":"Denied","push":false} -> ERROR [denied by user] Their reason: not yet
    subagent {"task":"look at #7"} -> started agent a1 (look at #7); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
      [a1] pull_request -> #7 From a contributor (head abc123)
    checks: success
    - build: success https://ci/1
    confirm a1/s2 pull_request: create Denied too
      [a1] pull_request -> ERROR [denied by user] Their reason: not yet
    GET https://api.github.com/repos/o/r/pulls?state=open&per_page=100&sort=updated&direction=desc
      Authorization: Bearer ghp_mine
    GET https://api.github.com/repos/o/r/pulls/7
      Authorization: Bearer ghp_mine
    GET https://api.github.com/repos/o/r/commits/abc123/check-runs?per_page=100
      Authorization: Bearer ghp_mine
    GET https://api.github.com/repos/o/r/commits/abc123/status
      Authorization: Bearer ghp_mine
    |}]
;;

let%expect_test "auto mode creates without asking" =
  with_agent
    ~answer:(fun _ -> Allow)
    ~sub_replies:[]
    [ Reply.tool_call
        ~id:"c1"
        ~name:"pull_request"
        ~arguments:{|{"action":"create","title":"Add things"}|}
        ()
    ; Reply.text "done"
    ]
  @@ fun _ agent ~fake:_ ->
  Fake_forge.create_reply
  := ( 201
     , Fake_forge.pr_json
         ~head:"feat/add-things"
         ~host:"github.com"
         ~number:8
         ~title:"Add things"
         () );
  Agent.set_mode agent ~approval:Auto;
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  [%expect {|
    pull_request {"action":"create","title":"Add things"} -> pushed feat/add-things to origin, then opened #8 Add things (open) in o/r on github.com
    https://github.com/o/r/pull/8
    alice wants to merge feat/add-things into main
    |}]
;;
