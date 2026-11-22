open! Core
open! Prigh
open Tool_test_helpers
open! Test_review_compare

let%expect_test "a branch that is not checked out, read-only" =
  with_repo
  @@ fun t agent ->
  (* Since the merge base with its default base (main): its own commit. *)
  print_diff ~refs:true t (Agent.git_diff agent ~rev:"feat/x");
  [%expect
    {|
    rev=feat/x branch=- base=origin/main direct=false against=<sha> files=[x.txt] +1 -0
    branches: feat/x main
    remote: origin/feat/remote origin/main
    tags: v1
    commit <sha> feat: x
    commit <sha> init
    |}];
  (* Directly: main's later commit shows, undone. *)
  print_diff t (Agent.git_diff agent ~rev:"feat/x" ~base:"main" ~direct:true);
  [%expect
    {| rev=feat/x branch=- base=main direct=true against=<sha> files=[a.txt x.txt] +1 -1 |}];
  (* A remote-tracking branch, against its remote's default branch; a tag
     as the base. *)
  print_diff t (Agent.git_diff agent ~rev:"origin/feat/remote");
  print_diff t (Agent.git_diff agent ~rev:"origin/feat/remote" ~base:"v1");
  print_diff
    t
    (Agent.git_diff agent ~rev:"origin/feat/remote" ~base:"v1" ~direct:true);
  [%expect
    {|
    rev=origin/feat/remote branch=- base=origin/main direct=false against=<sha> files=[r.txt] +1 -0
    rev=origin/feat/remote branch=- base=v1 direct=false against=<sha> files=[a.txt r.txt] +2 -0
    rev=origin/feat/remote branch=- base=v1 direct=true against=<sha> files=[a.txt r.txt] +2 -0
    |}];
  (* Uncommitted and untracked changes are the worktree's, not the
     branch's. *)
  sh t "echo dirty >> a.txt && echo u > untracked.txt";
  print_diff t (Agent.git_diff agent ~rev:"feat/x");
  [%expect
    {| rev=feat/x branch=- base=origin/main direct=false against=<sha> files=[x.txt] +1 -0 |}];
  (* Errors say what to do, with the closest names. *)
  print_diff t (Agent.git_diff agent ~rev:"feat/y");
  print_diff t (Agent.git_diff agent ~rev:"feat/x" ~base:"HEAD");
  print_diff t (Agent.git_diff agent ~rev:"feat/x" ~base:"mian");
  print_diff t (Agent.git_diff agent ~rev:"--output=/tmp/x");
  [%expect
    {|
    error: unknown revision "feat/y": name a branch to review (closest: feat/x, main, v1)
    error: feat/x is not checked out, so it has no uncommitted changes: compare it with a branch (e.g. --base main)
    error: unknown revision "mian": name a branch, tag or commit (closest: main, v1, feat/x)
    error: unknown revision "--output=/tmp/x": name a branch to review (closest: feat/x, origin/main, main)
    |}];
  (* Its files as the branch has them. *)
  let shown r =
    match r with
    | Ok (s : Git_diff.Shown.t) -> printf "%S truncated=%b\n" s.text s.truncated
    | Error e -> printf "error: %s\n" (show t (Error.to_string_hum e))
  in
  shown (Agent.git_show agent ~rev:"feat/x" ~path:"x.txt");
  shown (Agent.git_show agent ~rev:"feat/x" ~path:"a.txt");
  shown (Agent.git_show agent ~rev:"feat/x" ~path:"r.txt");
  shown (Agent.git_show agent ~rev:"nope" ~path:"a.txt");
  [%expect
    {|
    "x\n" truncated=false
    "one\n" truncated=false
    error: feat/x has no file r.txt
    error: unknown revision "nope": name a branch (closest: main, v1, feat/x)
    |}]
;;
