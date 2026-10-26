open! Core
open! Prigh
open Tool_test_helpers

(* Choosing what to review and what to compare it with: a branch that is not
   checked out (read-only, by git plumbing), remote-tracking branches, tags,
   commits and typed revisions as bases, since the merge base or directly. *)

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

(* main: init, then "main: two" after feat/x branched off; feat/x (not
   checked out): "feat: x"; origin (a bare repository) has main and
   feat/remote, which is not a local branch; v1 tags init. *)
let with_repo f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  sh
    t
    "git init -q -b main . && printf 'one\\n' > a.txt && git add -A && git \
     commit -q -m init && git tag v1 && git checkout -q -b feat/x && echo x > \
     x.txt && git add x.txt && git commit -q -m 'feat: x' && git checkout -q \
     main && printf 'one\\ntwo\\n' > a.txt && git commit -q -am 'main: two' && \
     git init -q --bare ../origin.git && git remote add origin ../origin.git \
     && git push -q origin main && git checkout -q -b feat/remote && echo r > \
     r.txt && git add r.txt && git commit -q -m 'remote: r' && git push -q \
     origin feat/remote && git checkout -q main && git branch -q -D \
     feat/remote && git fetch -q origin && git remote set-head origin main";
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
  f t agent
;;

let sha_re = Re.compile (Re.Perl.re "[0-9a-f]{40}")
let index_re = Re.compile (Re.Perl.re {|index [0-9a-f]+\.\.[0-9a-f]+|})

let show t s =
  Re.replace_string sha_re ~by:"<sha>" s
  |> Re.replace_string index_re ~by:"index <ids>"
  |> mask t
;;

let files diff =
  String.split_lines diff
  |> List.filter_map ~f:(fun l -> String.chop_prefix l ~prefix:"diff --git a/")
  |> List.map ~f:(fun l -> List.hd_exn (String.split l ~on:' '))
;;

let print_diff ?(refs = false) t (r : Git_diff.Result.t Or_error.t) =
  match r with
  | Error e -> printf "error: %s\n" (show t (Error.to_string_hum e))
  | Ok r ->
    let _, added, removed = Git_diff.Result.stats r in
    printf
      "rev=%s branch=%s base=%s direct=%b against=%s files=[%s] +%d -%d\n"
      (Option.value r.rev ~default:"-")
      (Option.value r.branch ~default:"-")
      (show t r.base)
      r.direct
      (show t r.against)
      (String.concat ~sep:" " (files r.diff))
      added
      removed;
    if refs
    then (
      printf "branches: %s\n" (String.concat ~sep:" " r.branches);
      printf "remote: %s\n" (String.concat ~sep:" " r.remote_branches);
      printf "tags: %s\n" (String.concat ~sep:" " r.tags);
      List.iter r.commits ~f:(fun c ->
        printf "commit %s %s\n" (show t c.sha) c.subject))
;;

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

let%expect_test "comments on a branch that is not checked out" =
  with_repo
  @@ fun _t agent ->
  let added =
    Agent.update_review agent ~f:(fun r ->
      Review_state.add
        r
        ~root:"branch:feat/x"
        ~path:"x.txt"
        ~range:
          { Review_comment.Range.first = 1
          ; last = 1
          ; side = New
          ; columns = None
          }
        ~anchor:"x"
        ~text:"Name it y"
        ~at:0.)
  in
  print_s [%sexp (Or_error.is_ok added : bool)];
  (match Review_state.submit (Agent.review_state agent) with
   | Ok (text, n, _) -> printf "%d\n%s\n" n text
   | Error e -> print_s [%sexp (e : Error.t)]);
  [%expect
    {|
    true
    1
    The user left review comments on the changes in a worktree (each comment's file is its absolute path, in the worktree it names: edit that file, even when it is not under your directory; line numbers are as when they commented: the file may have changed since). Answer every comment with reply_to_comment: say what you did, or why not. Make the changes they ask for; where the user should decide, propose the change with the reply's edits instead of making it (it is applied when they accept it). Resolve a comment (resolve: true) when it needs nothing more. Posts marked new are what you have not seen yet. A comment on text the user selected marks it ⟦like this⟧ in its code and quotes it in <selected>; one with side="old" is on lines the change removed, numbered as in the file before it. A comment with a branch is on a branch that is not checked out: its file is BRANCH:PATH, as git show reads it. To change it, check the branch out first (in a worktree of its own if it is not yours), or say what you would change.

    <comment id="c1" branch="feat/x" checked_out="no" file="feat/x:x.txt" lines="1" status="open">
    <code>
    1  x
    </code>
    <post from="user" new="true">
    Name it y
    </post>
    </comment>
    |}]
;;
