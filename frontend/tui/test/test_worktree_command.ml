open! Core
open! Expect_test_helpers_core
open Prigh_protocol
module W = Prigh_ui.Worktree_command

let worktree
      ?branch
      ?(main = false)
      ?(current = false)
      ?changes
      ?(ahead = 0)
      ?(behind = 0)
      ?(sessions = [])
      path
  =
  { Worktree.path
  ; branch
  ; head = Some "0123456789abcdef0123456789abcdef01234567"
  ; main
  ; managed = not main
  ; locked = false
  ; prunable = false
  ; changes
  ; upstream = None
  ; ahead
  ; behind
  ; current
  ; sessions
  }
;;

let session ?name id =
  { Worktree.Session_ref.id
  ; path = "/s/" ^ id
  ; name
  ; live = true
  ; running = false
  }
;;

let listing =
  { Worktree.Listing.root = "/src/repo"
  ; project_dir = "/h/.prigh/worktrees/repo"
  ; worktrees =
      [ worktree
          "/src/repo"
          ~branch:"main"
          ~main:true
          ~current:true
          ~changes:0
          ~ahead:1
      ; worktree
          "/h/.prigh/worktrees/repo/feat-a"
          ~branch:"feat/a"
          ~changes:3
          ~sessions:[ session "s1" ~name:"fixing a"; session "s2" ]
      ; worktree "/h/.prigh/worktrees/repo/detached" ~changes:0
      ]
  }
;;

let%expect_test "parse" =
  List.iter
    [ ""
    ; "list"
    ; "new feat/b"
    ; "new feat/b main"
    ; "new"
    ; "new a b c"
    ; "open feat/a"
    ; "open -n feat/a"
    ; "open feat/a -n"
    ; "open"
    ; "open a b"
    ; "open -x a"
    ; "rm -f feat/a"
    ; "remove feat/a"
    ; "rm"
    ; "prune"
    ]
    ~f:(fun s -> print_s [%sexp (s : string), (W.parse s : W.Action.t)]);
  [%expect
    {|
    ("" List)
    (list List)
    ("new feat/b" (New (branch feat/b) (base ())))
    ("new feat/b main" (New (branch feat/b) (base (main))))
    (new (
      Error
      "usage: /worktree new BRANCH [BASE]: a new branch from BASE (default: HEAD), or an existing branch"))
    ("new a b c" (
      Error
      "usage: /worktree new BRANCH [BASE]: a new branch from BASE (default: HEAD), or an existing branch"))
    ("open feat/a" (
      Open
      (target      feat/a)
      (new_session false)))
    ("open -n feat/a" (
      Open
      (target      feat/a)
      (new_session true)))
    ("open feat/a -n" (
      Open
      (target      feat/a)
      (new_session true)))
    (open (
      Error
      "usage: /worktree open [-n] TARGET (TARGET: a branch or path, as /worktree lists them)"))
    ("open a b" (
      Error "one worktree at a time: usage: /worktree open [-n] TARGET"))
    ("open -x a" (Error "unknown option -x: usage: /worktree open [-n] TARGET"))
    ("rm -f feat/a" (
      Remove
      (target feat/a)
      (force  true)))
    ("remove feat/a" (
      Remove
      (target feat/a)
      (force  false)))
    (rm (
      Error
      "usage: /worktree rm [-f] TARGET (TARGET: a branch or path, as /worktree lists them)"))
    (prune (
      Error
      "unknown subcommand \"prune\": /worktree [list|new BRANCH [BASE]|open [-n] TARGET|rm [-f] TARGET]"))
    |}]
;;

let%expect_test "find, describe, confirm" =
  List.iter
    [ "feat/a"
    ; "/h/.prigh/worktrees/repo/feat-a"
    ; "feat-a"
    ; "feat-a/"
    ; "repo"
    ; "detached"
    ; "feat/b"
    ]
    ~f:(fun name ->
      print_endline
        (match W.find listing name with
         | Ok w -> sprintf "%s -> %s" name w.path
         | Error e -> sprintf "%s -> error: %s" name e));
  List.iter listing.worktrees ~f:(fun w ->
    print_endline (W.describe w);
    printf
      "  target=%s marker=%S cannot_remove=%s\n"
      (W.target w)
      (W.marker w)
      (Option.value (W.cannot_remove w) ~default:"-");
    print_endline ("  " ^ W.confirm_remove w ~force:false);
    print_endline ("  " ^ W.confirm_remove w ~force:true));
  print_endline
    (W.created
       { worktree = List.nth_exn listing.worktrees 1; new_branch = true });
  print_endline (W.removed (List.nth_exn listing.worktrees 2));
  [%expect
    {|
    feat/a -> /h/.prigh/worktrees/repo/feat-a
    /h/.prigh/worktrees/repo/feat-a -> /h/.prigh/worktrees/repo/feat-a
    feat-a -> /h/.prigh/worktrees/repo/feat-a
    feat-a/ -> /h/.prigh/worktrees/repo/feat-a
    repo -> /src/repo
    detached -> /h/.prigh/worktrees/repo/detached
    feat/b -> error: no worktree "feat/b" (closest: feat/a, feat-a, main); /worktree lists them
    main · /src/repo · main worktree · here · ↑1 ↓0
      target=main marker="+" cannot_remove=/src/repo is the repository's main worktree: it cannot be removed
      Remove the worktree main at /src/repo? (the branch stays)
      Remove the worktree main at /src/repo? (the branch stays)
    feat/a · /h/.prigh/worktrees/repo/feat-a · 3 changed · 2 sessions
      target=feat/a marker="*" cannot_remove=-
      Remove the worktree feat/a at /h/.prigh/worktrees/repo/feat-a; it has 3 uncommitted changes; sessions working there: fixing a, s2? (the branch stays)
      Remove the worktree feat/a at /h/.prigh/worktrees/repo/feat-a; its 3 uncommitted changes are lost; sessions working there: fixing a, s2? (the branch stays)
    01234567 · /h/.prigh/worktrees/repo/detached
      target=/h/.prigh/worktrees/repo/detached marker="" cannot_remove=-
      Remove the worktree 01234567 at /h/.prigh/worktrees/repo/detached? (the branch stays)
      Remove the worktree 01234567 at /h/.prigh/worktrees/repo/detached? (the branch stays)
    worktree feat/a at /h/.prigh/worktrees/repo/feat-a (new branch)
    removed the worktree 01234567 at /h/.prigh/worktrees/repo/detached (the branch stays)
    |}]
;;

let%expect_test "complete" =
  List.iter
    [ ""
    ; "op"
    ; "open "
    ; "open -n "
    ; "open -n fe"
    ; "rm "
    ; "new x "
    ; "new x m"
    ; "list "
    ]
    ~f:(fun rest ->
      match W.complete ~worktrees:listing.worktrees rest with
      | None -> printf "%S: none\n" rest
      | Some (offset, candidates) ->
        printf "%S at %d:\n" rest offset;
        List.iter candidates ~f:(fun (id, detail) ->
          printf "  %s  %s\n" id detail));
  [%expect
    {|
    "" at 0:
      list  pick a worktree of this repository to work in
      new  new BRANCH [BASE]: create a worktree and work in it
      open  open [-n] TARGET: work in a worktree (-n: in a new session)
      rm  rm [-f] TARGET: remove a worktree (-f: with uncommitted changes)
    "op" at 0:
      list  pick a worktree of this repository to work in
      new  new BRANCH [BASE]: create a worktree and work in it
      open  open [-n] TARGET: work in a worktree (-n: in a new session)
      rm  rm [-f] TARGET: remove a worktree (-f: with uncommitted changes)
    "open " at 5:
      -n  in a new session
      main  main · /src/repo · main worktree · here · ↑1 ↓0
      feat/a  feat/a · /h/.prigh/worktrees/repo/feat-a · 3 changed · 2 sessions
      /h/.prigh/worktrees/repo/detached  01234567 · /h/.prigh/worktrees/repo/detached
    "open -n " at 8:
      main  main · /src/repo · main worktree · here · ↑1 ↓0
      feat/a  feat/a · /h/.prigh/worktrees/repo/feat-a · 3 changed · 2 sessions
      /h/.prigh/worktrees/repo/detached  01234567 · /h/.prigh/worktrees/repo/detached
    "open -n fe" at 8:
      main  main · /src/repo · main worktree · here · ↑1 ↓0
      feat/a  feat/a · /h/.prigh/worktrees/repo/feat-a · 3 changed · 2 sessions
      /h/.prigh/worktrees/repo/detached  01234567 · /h/.prigh/worktrees/repo/detached
    "rm " at 3:
      -f  also with uncommitted changes
      feat/a  feat/a · /h/.prigh/worktrees/repo/feat-a · 3 changed · 2 sessions
      /h/.prigh/worktrees/repo/detached  01234567 · /h/.prigh/worktrees/repo/detached
    "new x " at 6:
      main  base: main · /src/repo · main worktree · here · ↑1 ↓0
      feat/a  base: feat/a · /h/.prigh/worktrees/repo/feat-a · 3 changed · 2 sessions
    "new x m" at 6:
      main  base: main · /src/repo · main worktree · here · ↑1 ↓0
      feat/a  base: feat/a · /h/.prigh/worktrees/repo/feat-a · 3 changed · 2 sessions
    "list ": none
    |}]
;;
