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

let%expect_test
    "the worktree against any base, since the merge base or directly"
  =
  with_repo
  @@ fun t agent ->
  let feat =
    Worktree.create
      ~env:t.env
      ~cancel:Cancellation.never
      ~cwd:(Filename.concat t.dir "repo")
      ~dir:(Filename.concat t.dir "wts")
      ~branch:"feat/x"
      ~base:None
      ()
    |> Or_error.ok_exn
    |> fun (c : Worktree.Created.t) -> c.worktree.path
  in
  let rel = String.chop_prefix_exn feat ~prefix:(t.dir ^ "/") in
  sh t ~dir:rel "echo more >> x.txt";
  (* No base recorded for an existing branch: origin's default. *)
  print_diff t (Agent.git_diff agent ~dir:feat ~default_base:true);
  print_diff t (Agent.git_diff agent ~dir:feat ~base:"main" ~direct:true);
  print_diff t (Agent.git_diff agent ~dir:feat ~base:"v1");
  print_diff t (Agent.git_diff agent ~dir:feat ~base:"origin/main");
  [%expect
    {|
    rev=- branch=feat/x base=origin/main direct=false against=<sha> files=[x.txt] +2 -0
    rev=- branch=feat/x base=main direct=true against=<sha> files=[a.txt x.txt] +2 -1
    rev=- branch=feat/x base=v1 direct=false against=<sha> files=[x.txt] +2 -0
    rev=- branch=feat/x base=origin/main direct=false against=<sha> files=[x.txt] +2 -0
    |}];
  (* Directly against HEAD is the uncommitted changes, as without. *)
  print_diff t (Agent.git_diff agent ~dir:feat ~base:"HEAD" ~direct:true);
  [%expect
    {| rev=- branch=feat/x base=HEAD direct=false against=HEAD files=[x.txt] +1 -0 |}];
  (* A commit by its id, and the recent commits on what is reviewed. *)
  (match Agent.git_diff agent ~dir:feat ~default_base:true with
   | Ok { commits = _ :: init :: _; _ } ->
     print_diff t (Agent.git_diff agent ~dir:feat ~base:init.sha);
     print_diff
       t
       (Agent.git_diff agent ~dir:feat ~base:(String.prefix init.sha 8)
        |> Or_error.map ~f:(fun (r : Git_diff.Result.t) ->
          { r with base = "<short sha>" }))
   | _ -> print_endline "no commits");
  [%expect
    {|
    rev=- branch=feat/x base=<sha> direct=false against=<sha> files=[x.txt] +2 -0
    rev=- branch=feat/x base=<short sha> direct=false against=<sha> files=[x.txt] +2 -0
    |}];
  (match Agent.git_refs agent with
   | Ok r -> print_s [%sexp (r : Git_diff.Refs.t)]
   | Error e -> print_s [%sexp (e : Error.t)]);
  [%expect
    {|
    ((branches (feat/x main)) (remote_branches (origin/feat/remote origin/main))
     (tags (v1)))
    |}]
;;
