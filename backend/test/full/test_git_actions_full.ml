open! Core
open! Prigh
open Tool_test_helpers
open! Test_git_actions

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
