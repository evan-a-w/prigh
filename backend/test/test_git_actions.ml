open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

let git_env =
  "export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1 GIT_AUTHOR_NAME=t \
   GIT_AUTHOR_EMAIL=t@t GIT_COMMITTER_NAME=t GIT_COMMITTER_EMAIL=t@t"
;;

let sh ?(dir = "") (t : Tool_test_helpers.t) script =
  let command =
    sprintf
      "cd %s && %s && (%s) > /dev/null 2>&1"
      (Filename.quote (Filename.concat t.dir dir))
      git_env
      script
  in
  match Sys_unix.command command with
  | 0 -> ()
  | n -> raise_s [%message "setup failed" (script : string) (n : int)]
;;

(* A repository in [repo] whose [origin] is the bare [origin.git], with one
   commit on main. The repository's own config says who commits, so that
   the backend's git (which reads the user's global config) needs nothing
   else. *)
let setup t =
  sh t "git init -q --bare -b main origin.git";
  sh
    t
    "git init -q -b main repo && cd repo && git config user.name t && git \
     config user.email t@t && git config commit.gpgsign false && git remote \
     add origin ../origin.git && printf 'one\\n' > a.txt && printf 'gone\\n' > \
     c.txt && git add . && git commit -qm 'Initial commit' && git push -q -u \
     origin main"
;;

let with_agent f =
  (* The backend's git reads no user configuration (signing, hooks). *)
  Core_unix.putenv ~key:"GIT_CONFIG_GLOBAL" ~data:"/dev/null";
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [])
      ~tools:Tools.all
      ~sessions_dir:(Filename.concat t.dir ".sessions")
      ~home:t.dir
      ~cwd:t.dir
      ()
  in
  f t agent
;;

let call t agent meth params =
  match Git_rpc.dispatch agent ~meth ~params:(`Object params) with
  | None -> print_endline "not a git method"
  | Some (Error e) -> printf "error: %s\n" (mask t (Error.to_string_hum e))
  | Some (Ok json) ->
    let status json =
      match [%of_jsonaf: Git_actions.Status.t] json with
      | s ->
        let staged, unstaged =
          List.partition_tf s.files ~f:(fun f -> Option.is_some f.index)
        in
        let show (f : Git_actions.File.t) =
          sprintf
            "%s%s%s%s"
            (Option.value f.index ~default:".")
            (Option.value f.worktree ~default:".")
            (Option.value_map f.orig ~default:"" ~f:(fun o -> " " ^ o ^ " ->"))
            (" " ^ f.path)
        in
        printf
          "%s branch=%s upstream=%s ahead=%d behind=%d head=%b\n"
          (mask t s.root)
          (Option.value s.branch ~default:"(detached)")
          (Option.value s.upstream ~default:"-")
          s.ahead
          s.behind
          (Option.is_some s.head);
        List.iter (staged @ unstaged) ~f:(fun f -> printf "  %s\n" (show f))
    in
    (match Json.member "status" json with
     | Some st ->
       List.iter [ "remote"; "branch"; "subject" ] ~f:(fun key ->
         match Json.member key json with
         | Some (`String v) -> printf "%s: %s\n" key v
         | _ -> ());
       status st
     | None -> status json)
;;

let dir = "dir", `String "repo"
let paths ps = "paths", `Array (List.map ps ~f:(fun p -> `String p))

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

let%expect_test "status parsing: renames, conflicts, upstream counts" =
  let out =
    String.concat
      ~sep:"\000"
      [ "# branch.oid 0123"
      ; "# branch.head feat/y"
      ; "# branch.upstream origin/feat/y"
      ; "# branch.ab +2 -3"
      ; "2 RM N... 100644 100644 100644 aaa bbb R100 new name.txt"
      ; "old.txt"
      ; "u UU N... 100644 100644 100644 100644 a b c both.txt"
      ; "? un tracked"
      ; ""
      ]
  in
  print_s
    [%sexp (Git_actions.Status.parse ~root:"/r" out : Git_actions.Status.t)];
  [%expect
    {|
    ((root /r) (branch feat/y) (head 0123) (upstream origin/feat/y) (ahead 2)
     (behind 3)
     (files
      (((path "new name.txt") (orig old.txt) (index R) (worktree M))
       ((path both.txt) (index U) (worktree U))
       ((path "un tracked") (worktree ?)))))
    |}]
;;

let%expect_test "sections of guidance and diff digests" =
  let doc =
    "# Project\n\
     intro\n\
     ## Pull requests\n\
     Say how it was tested.\n\
     ### Titles\n\
     Imperative.\n\
     ## Layout\n\
     dirs\n\
     ## Making a PR\n\
     Small ones.\n"
  in
  print_endline
    (Option.value
       (Git_actions.sections
          ~topic:[ "pull request"; "pull requests"; "pr" ]
          doc)
       ~default:"(none)");
  [%expect
    {|
    ## Pull requests
    Say how it was tested.
    ### Titles
    Imperative.
    ## Making a PR
    Small ones.
    |}];
  print_endline
    (Option.value
       (Git_actions.sections ~topic:[ "commit" ] doc)
       ~default:"(none)");
  [%expect {| (none) |}];
  let file name n =
    sprintf
      "diff --git a/%s b/%s\n--- a/%s\n+++ b/%s\n@@ -0,0 +1,%d @@\n%s"
      name
      name
      name
      name
      n
      (String.concat
         (List.init n ~f:(fun i -> sprintf "+line %d of %s\n" i name)))
  in
  let diff = file "small" 2 in
  print_string (Diff_digest.digest ~max_bytes:10_000 diff);
  [%expect
    {|
    diff --git a/small b/small
    --- a/small
    +++ b/small
    @@ -0,0 +1,2 @@
    +line 0 of small
    +line 1 of small
    |}];
  let big =
    String.concat (List.init 6 ~f:(fun i -> file (sprintf "f%d" i) 40))
  in
  print_string (Diff_digest.digest ~max_bytes:1_500 big);
  [%expect
    {|
    diff --git a/f0 b/f0
    --- a/f0
    +++ b/f0
    @@ -0,0 +1,40 @@
    +line 0 of f0
    +line 1 of f0
    +line 2 of f0
    +line 3 of f0
    +line 4 of f0
    +line 5 of f0
    +line 6 of f0
    +line 7 of f0
    +line 8 of f0
    +line 9 of f0
    +line 10 of f0
    +line 11 of f0
    +line 12 of f0
    +line 13 of f0
    +line 14 of f0
    +line 15 of f0
    +line 16 of f0
    +line 17 of f0
    +line 18 of f0
    +line 19 of f0
    +line 20 of f0
    +line 21 of f0
    +line 22 of f0
    +line 23 of f0
    +line 24 of f0
    +line 25 of f0
    +line 26 of f0
    +line 27 of f0
    +line 28 of f0
    +line 29 of f0
    +line 30 of f0
    +line 31 of f0
    +line 32 of f0
    +line 33 of f0
    +line 34 of f0
    +line 35 of f0
    [... 4 more lines of this file left out; it is +40 -0 in all]
    diff --git a/f1 b/f1
    --- a/f1
    +++ b/f1
    @@ -0,0 +1,40 @@
    +line 0 of f1
    +line 1 of f1
    +line 2 of f1
    +line 3 of f1
    +line 4 of f1
    +line 5 of f1
    +line 6 of f1
    +line 7 of f1
    +line 8 of f1
    +line 9 of f1
    +line 10 of f1
    +line 11 of f1
    +line 12 of f1
    +line 13 of f1
    +line 14 of f1
    +line 15 of f1
    +line 16 of f1
    +line 17 of f1
    +line 18 of f1
    +line 19 of f1
    +line 20 of f1
    +line 21 of f1
    +line 22 of f1
    +line 23 of f1
    +line 24 of f1
    +line 25 of f1
    +line 26 of f1
    +line 27 of f1
    +line 28 of f1
    +line 29 of f1
    +line 30 of f1
    +line 31 of f1
    +line 32 of f1
    +line 33 of f1
    +line 34 of f1
    +line 35 of f1
    [... 4 more lines of this file left out; it is +40 -0 in all]
    diff --git a/f2 b/f2
    --- a/f2
    +++ b/f2
    @@ -0,0 +1,40 @@
    +line 0 of f2
    +line 1 of f2
    +line 2 of f2
    +line 3 of f2
    +line 4 of f2
    +line 5 of f2
    +line 6 of f2
    +line 7 of f2
    +line 8 of f2
    +line 9 of f2
    [... 30 more lines of this file left out; it is +40 -0 in all]
    [3 more files left out:]
    diff --git a/f3 b/f3 (+40 -0)
    diff --git a/f4 b/f4 (+40 -0)
    diff --git a/f5 b/f5 (+40 -0)
    |}]
;;
