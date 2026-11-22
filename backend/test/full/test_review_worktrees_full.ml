open! Core
open! Prigh
open Tool_test_helpers
open! Test_review_worktrees

let%expect_test
    "git_diff of a worktree: committed, uncommitted and untracked changes \
     against its base"
  =
  with_agent []
  @@ fun t agent ->
  let feat = worktree t ~branch:"feat/x" ~base:None in
  let rel = String.chop_prefix_exn feat ~prefix:(t.dir ^ "/") in
  (* A commit on the branch, an uncommitted change and an untracked file. *)
  sh
    t
    ~dir:rel
    "printf 'one\\nTWO\\n' > a.txt && git commit -q -am 'feat: two' && echo \
     three >> a.txt && echo new > b.txt";
  print_diff ~full:true t (Agent.git_diff agent ~dir:feat ~default_base:true);
  [%expect
    {|
    root=$DIR/wts/repo/feat-x branch=feat/x main=false base=main against=<sha> files=2 +3 -1
    branches: feat/x main
    diff --git a/a.txt b/a.txt
    index <ids> 100644
    --- a/a.txt
    +++ b/a.txt
    @@ -1,2 +1,3 @@
     one
    -two
    +TWO
    +three
    diff --git a/b.txt b/b.txt
    new file mode 100644
    index <ids>
    --- /dev/null
    +++ b/b.txt
    @@ -0,0 +1 @@
    +new
    |}];
  (* Without the default base (the model's /review of the uncommitted
     changes), or with HEAD as the base: what is not committed. *)
  print_diff t (Agent.git_diff agent ~dir:feat);
  print_diff t (Agent.git_diff agent ~dir:feat ~base:"HEAD" ~default_base:true);
  [%expect
    {|
    root=$DIR/wts/repo/feat-x branch=feat/x main=false base=HEAD against=HEAD files=2 +2 -0
    root=$DIR/wts/repo/feat-x branch=feat/x main=false base=HEAD against=HEAD files=2 +2 -0
    |}];
  (* The main checkout keeps comparing with HEAD; a base is chosen. *)
  print_diff t (Agent.git_diff agent ~default_base:true);
  print_diff t (Agent.git_diff agent ~base:"feat/x" ~default_base:true);
  [%expect
    {|
    root=$DIR/repo branch=main main=true base=HEAD against=HEAD files=0 +0 -0
    root=$DIR/repo branch=main main=true base=feat/x against=<sha> files=0 +0 -0
    |}];
  (* A relative directory is the session's; a subdirectory reviews its
     worktree. *)
  print_diff
    t
    (Agent.git_diff agent ~dir:("../" ^ rel ^ "/") ~default_base:true);
  [%expect
    {| root=$DIR/wts/repo/feat-x branch=feat/x main=false base=main against=<sha> files=2 +3 -1 |}];
  (* The recorded base: a branch made from feat/x compares with it. *)
  let sub = worktree t ~branch:"feat/y" ~base:(Some "feat/x") in
  let sub_rel = String.chop_prefix_exn sub ~prefix:(t.dir ^ "/") in
  sh t ~dir:sub_rel "echo y > y.txt && git add y.txt && git commit -q -m y";
  print_diff t (Agent.git_diff agent ~dir:sub ~default_base:true);
  [%expect
    {| root=$DIR/wts/repo/feat-y branch=feat/y main=false base=feat/x against=<sha> files=1 +1 -0 |}];
  (* Made by hand, nothing recorded: the remote's default branch, else main. *)
  sh
    t
    "git worktree add -q -b manual ../manual && cd ../manual && echo m > m.txt \
     && git add m.txt && git commit -q -m m";
  let manual = Filename.concat t.dir "manual" in
  print_diff t (Agent.git_diff agent ~dir:manual ~default_base:true);
  sh
    t
    "git update-ref refs/remotes/origin/trunk HEAD && git symbolic-ref \
     refs/remotes/origin/HEAD refs/remotes/origin/trunk";
  print_diff ~full:true t (Agent.git_diff agent ~dir:manual ~default_base:true);
  [%expect
    {|
    root=$DIR/manual branch=manual main=false base=main against=<sha> files=1 +1 -0
    root=$DIR/manual branch=manual main=false base=origin/trunk against=<sha> files=1 +1 -0
    branches: feat/x feat/y main manual origin/trunk
    diff --git a/m.txt b/m.txt
    new file mode 100644
    index <ids>
    --- /dev/null
    +++ b/m.txt
    @@ -0,0 +1 @@
    +m
    |}];
  (* A deleted recorded base falls back too; unknown bases and directories
     say what to do. *)
  sh t "git config branch.feat/y.prighBase gone";
  print_diff t (Agent.git_diff agent ~dir:sub ~default_base:true);
  print_diff t (Agent.git_diff agent ~dir:sub ~base:"nope" ~default_base:true);
  print_diff t (Agent.git_diff agent ~dir:t.dir ~default_base:true);
  [%expect
    {|
    root=$DIR/wts/repo/feat-y branch=feat/y main=false base=origin/trunk against=<sha> files=2 +2 -1
    error: unknown revision "nope": name a branch, tag or commit (closest: main, manual, feat/x)
    error: not a git repository: $DIR (name a worktree of the repository: /worktree lists them)
    |}]
;;
