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
