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
