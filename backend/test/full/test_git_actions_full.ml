open! Core
open! Prigh
open Tool_test_helpers
open! Test_git_actions

let%expect_test "status, stage, unstage, commit, amend and push in a worktree" =
  with_agent
  @@ fun t agent ->
  call t agent "git_status" [];
  [%expect
    {| error: not a git repository: $DIR (name a worktree of the repository: /worktree lists them) |}];
  setup t;
  call t agent "git_status" [ dir ];
  [%expect
    {| $DIR/repo branch=main upstream=origin/main ahead=0 behind=0 head=true |}];
  write t "repo/a.txt" "one\ntwo\n";
  write t "repo/dir with space/b.txt" "new\n";
  Core_unix.unlink (Filename.concat t.dir "repo/c.txt");
  call t agent "git_status" [ dir ];
  [%expect
    {|
    $DIR/repo branch=main upstream=origin/main ahead=0 behind=0 head=true
      .M a.txt
      .D c.txt
      .? dir with space/b.txt
    |}];
  (* Paths stay inside the worktree; pathspec magic is taken literally. *)
  call t agent "git_stage" [ dir; paths [ "../origin.git/HEAD" ] ];
  call t agent "git_stage" [ dir; paths [ "/etc/passwd" ] ];
  call t agent "git_stage" [ dir; paths [ "x/../../y" ] ];
  call t agent "git_stage" [ dir; paths [ ":(glob)*" ] ];
  call t agent "git_stage" [ dir; paths [] ];
  [%expect
    {|
    error: ../origin.git/HEAD is outside the worktree: name files inside it
    error: /etc/passwd is absolute: name files relative to the worktree's top directory
    error: x/../../y is outside the worktree: name files inside it
    error: git add: fatal: pathspec ':(glob)*' did not match any files
    error: no files given: tick the files to stage
    |}];
  call
    t
    agent
    "git_stage"
    [ dir; paths [ "a.txt"; "c.txt"; "dir with space/b.txt" ] ];
  [%expect
    {|
    $DIR/repo branch=main upstream=origin/main ahead=0 behind=0 head=true
      M. a.txt
      D. c.txt
      A. dir with space/b.txt
    |}];
  call t agent "git_unstage" [ dir; paths [ "a.txt"; "dir with space/b.txt" ] ];
  [%expect
    {|
    $DIR/repo branch=main upstream=origin/main ahead=0 behind=0 head=true
      D. c.txt
      .M a.txt
      .? dir with space/b.txt
    |}];
  call t agent "git_commit" [ dir; "message", `String "  " ];
  [%expect
    {| error: the commit message is empty: write one (or Generate one) |}];
  (* [paths] are staged first. *)
  call
    t
    agent
    "git_commit"
    [ dir; "message", `String "Change a, drop c"; paths [ "a.txt" ] ];
  [%expect
    {|
    subject: Change a, drop c
    $DIR/repo branch=main upstream=origin/main ahead=1 behind=0 head=true
      .? dir with space/b.txt
    |}];
  call t agent "git_commit" [ dir; "message", `String "Nothing" ];
  [%expect
    {| error: nothing is staged to commit: tick the files to commit (or stage them with git add) |}];
  (* Amending keeps the message when none is given. *)
  call
    t
    agent
    "git_commit"
    [ dir
    ; "amend", `True
    ; "message", `String ""
    ; paths [ "dir with space/b.txt" ]
    ];
  [%expect
    {|
    subject: Change a, drop c
    $DIR/repo branch=main upstream=origin/main ahead=1 behind=0 head=true
    |}];
  call
    t
    agent
    "git_commit"
    [ dir; "amend", `True; "message", `String "Reworded" ];
  [%expect
    {|
    subject: Reworded
    $DIR/repo branch=main upstream=origin/main ahead=1 behind=0 head=true
    |}];
  call t agent "git_push" [ dir ];
  [%expect
    {|
    remote: origin
    branch: main
    $DIR/repo branch=main upstream=origin/main ahead=0 behind=0 head=true
    |}];
  (* A new branch gets its upstream on its first push. *)
  sh ~dir:"repo" t "git switch -q -c feat/x && echo x > x.txt && git add x.txt";
  call t agent "git_commit" [ dir; "message", `String "Add x" ];
  call t agent "git_push" [ dir ];
  [%expect
    {|
    subject: Add x
    $DIR/repo branch=feat/x upstream=- ahead=0 behind=0 head=true
    remote: origin
    branch: feat/x
    $DIR/repo branch=feat/x upstream=origin/feat/x ahead=0 behind=0 head=true
    |}];
  (* [tracked]: every change to tracked files, like [git commit -a]. *)
  write t "repo/x.txt" "xx\n";
  write t "repo/y.txt" "y\n";
  call t agent "git_stage" [ dir; "tracked", `True ];
  [%expect
    {|
    $DIR/repo branch=feat/x upstream=origin/feat/x ahead=0 behind=0 head=true
      M. x.txt
      .? y.txt
    |}]
;;

let%expect_test "refusals: a rejected push, detached HEAD, no remote" =
  with_agent
  @@ fun t agent ->
  setup t;
  (* Someone else pushes first. *)
  sh
    t
    "git clone -q origin.git other && cd other && echo theirs > t.txt && git \
     add t.txt && git commit -qm theirs && git push -q";
  sh ~dir:"repo" t "echo ours > o.txt && git add o.txt && git commit -qm ours";
  (* git's hints are worded differently in each version. *)
  sh ~dir:"repo" t "git config advice.pushUpdateRejected false";
  call t agent "git_push" [ dir ];
  [%expect
    {|
    error: git push origin main was refused: To ../origin.git
     ! [rejected]        HEAD -> main (fetch first)
    error: failed to push some refs to '../origin.git' (the remote has commits this branch does not: pull first, e.g. git pull --rebase, then push again)
    |}];
  sh ~dir:"repo" t "git fetch -q";
  call t agent "git_status" [ dir ];
  [%expect
    {| $DIR/repo branch=main upstream=origin/main ahead=1 behind=1 head=true |}];
  sh ~dir:"repo" t "git checkout -q --detach && echo d > d.txt && git add d.txt";
  call t agent "git_commit" [ dir; "message", `String "Detached" ];
  call t agent "git_push" [ dir ];
  [%expect
    {|
    error: HEAD is detached in $DIR/repo: commit on a branch (git switch -c NAME there makes one)
    error: HEAD is detached in $DIR/repo: push a branch (git switch -c NAME there makes one)
    |}];
  sh
    t
    "git init -q -b main lone && cd lone && git config user.name t && git \
     config user.email t@t";
  call t agent "git_push" [ "dir", `String "lone" ];
  call t agent "git_commit" [ "dir", `String "lone"; "amend", `True ];
  [%expect
    {|
    error: there is nothing to push: commit first
    error: there is no commit to amend yet: commit without Amend first
    |}];
  sh ~dir:"lone" t "echo l > l.txt && git add l.txt && git commit -qm first";
  call t agent "git_push" [ "dir", `String "lone" ];
  [%expect
    {| error: $DIR/lone has no remote to push to: add one (git remote add origin URL) |}];
  (* Before the first commit, unstaging takes files out of the index. *)
  sh
    t
    "git init -q -b main fresh && cd fresh && echo f > f.txt && git add f.txt";
  call t agent "git_unstage" [ "dir", `String "fresh"; paths [ "f.txt" ] ];
  [%expect
    {|
    $DIR/fresh branch=main upstream=- ahead=0 behind=0 head=false
      .? f.txt
    |}]
;;
