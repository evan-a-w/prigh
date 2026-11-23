open! Core
open! Prigh
open Tool_test_helpers

let sh (t : Tool_test_helpers.t) script =
  let command =
    sprintf
      "cd %s && export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1 \
       GIT_AUTHOR_NAME=t GIT_AUTHOR_EMAIL=t@t GIT_COMMITTER_NAME=t \
       GIT_COMMITTER_EMAIL=t@t && (%s) > /dev/null 2>&1"
      (Filename.quote t.dir)
      script
  in
  match Sys_unix.command command with
  | 0 -> ()
  | n -> raise_s [%message "setup failed" (script : string) (n : int)]
;;

let out (t : Tool_test_helpers.t) script =
  let r =
    Process.run_collect ~env:t.env ~cwd:t.dir ~prog:"sh" ~args:[ "-c"; script ] ()
  in
  print_string (mask t r.stdout)
;;

let cancel = Cancellation.never
let ref_ n = Checkpoint.session_prefix ~session:"s1" ^ n

let snap t ?(cwd = "repo") name =
  match
    Checkpoint.capture ~env:t.env ~cancel ~cwd:(t.dir ^/ cwd) ~ref:(Some (ref_ name))
  with
  | Ok (Snapshot { commit = Some c; _ }) -> c
  | Ok other -> raise_s [%sexp (other : Checkpoint.Captured.t)]
  | Error e -> Error.raise e
;;

let files t =
  out
    t
    "cd repo && find . -path ./.git -prune -o -type f -print | sort | while \
     read f; do printf '%s: %s\\n' \"$f\" \"$(cat \"$f\")\"; done"
;;

let state t =
  print_endline "-- files";
  files t;
  print_endline "-- git status";
  out t "cd repo && git status --short && git stash list && git log --oneline | wc -l"
;;

let print_result t = function
  | Ok files -> printf "ok: [%s]\n" (String.concat ~sep:" " files)
  | Error e -> printf "error: %s\n" (mask t (Error.to_string_hum e))
;;

let%expect_test "capture and restore: modified, added, deleted and staged files" =
  with_sandbox
  @@ fun t ->
  sh
    t
    "mkdir repo && cd repo && git init -q -b main . && echo one > a.txt && \
     echo keep > k.txt && printf 'ignored\\n' > .gitignore && git add -A && \
     git commit -q -m init && echo junk > ignored && echo staged > s.txt && \
     git add s.txt && echo untracked > u.txt";
  let before = snap t "1-before" in
  (* The agent's turn: edits, adds (in a new directory), deletes. *)
  sh
    t
    "cd repo && echo two > a.txt && rm u.txt && mkdir -p d/e && echo new > \
     d/e/n.txt && rm k.txt && echo junk2 > ignored";
  let after = snap t "1-after" in
  state t;
  [%expect
    {|
    -- files
    ./a.txt: two
    ./d/e/n.txt: new
    ./.gitignore: ignored
    ./ignored: junk2
    ./s.txt: staged
    -- git status
     M a.txt
     D k.txt
    A  s.txt
    ?? d/
    1
    |}];
  print_result
    t
    (Checkpoint.changed ~env:t.env ~cancel ~cwd:(t.dir ^/ "repo") ~from:before ~to_:None);
  [%expect {| ok: [a.txt d/e/n.txt k.txt u.txt] |}];
  print_result
    t
    (Checkpoint.restore ~env:t.env ~cancel ~cwd:(t.dir ^/ "repo") ~to_:before ~expect:after);
  (* Back as before; the index (s.txt staged), HEAD and the ignored file are
     as the user left them. *)
  state t;
  [%expect
    {|
    ok: [a.txt d/e/n.txt k.txt u.txt]
    -- files
    ./a.txt: one
    ./.gitignore: ignored
    ./ignored: junk2
    ./k.txt: keep
    ./s.txt: staged
    ./u.txt: untracked
    -- git status
    A  s.txt
    ?? u.txt
    1
    |}];
  out t "cd repo && git for-each-ref --format='%(refname)' refs/prigh";
  [%expect
    {|
    refs/prigh/checkpoints/s1/1-after
    refs/prigh/checkpoints/s1/1-before
    |}];
  (* The other way, forward again. *)
  print_result
    t
    (Checkpoint.restore ~env:t.env ~cancel ~cwd:(t.dir ^/ "repo") ~to_:after ~expect:before);
  files t;
  [%expect
    {|
    ok: [a.txt d/e/n.txt k.txt u.txt]
    ./a.txt: two
    ./d/e/n.txt: new
    ./.gitignore: ignored
    ./ignored: junk2
    ./s.txt: staged
    |}];
  print_result
    t
    (Checkpoint.drop
       ~env:t.env
       ~cancel
       ~cwd:(t.dir ^/ "repo")
       ~prefix:(Checkpoint.session_prefix ~session:"s1")
     |> Or_error.map ~f:(fun n -> [ Int.to_string n ]));
  out t "cd repo && git for-each-ref refs/prigh | wc -l";
  [%expect {|
    ok: [2]
    0
    |}]
;;

let%expect_test "restore refuses when files changed since, and changes nothing" =
  with_sandbox
  @@ fun t ->
  sh
    t
    "mkdir repo && cd repo && git init -q -b main . && echo one > a.txt && \
     git add -A && git commit -q -m init";
  let before = snap t "b" in
  sh t "cd repo && echo two > a.txt";
  let after = snap t "a" in
  (* The user edits after the agent's turn. *)
  sh t "cd repo && echo mine > b.txt && echo three > a.txt";
  print_result
    t
    (Checkpoint.restore ~env:t.env ~cancel ~cwd:(t.dir ^/ "repo") ~to_:before ~expect:after);
  files t;
  [%expect
    {|
    error: files changed since the agent's last turn: a.txt, b.txt
    ./a.txt: three
    ./b.txt: mine
    |}]
;;

let%expect_test "a file becomes a directory and back; an empty repository" =
  with_sandbox
  @@ fun t ->
  sh t "mkdir repo && cd repo && git init -q -b main . && echo f > x";
  let before = snap t "b" in
  sh t "cd repo && rm x && mkdir x && echo inner > x/y";
  let after = snap t "a" in
  print_result
    t
    (Checkpoint.restore ~env:t.env ~cancel ~cwd:(t.dir ^/ "repo") ~to_:before ~expect:after);
  files t;
  print_result
    t
    (Checkpoint.restore ~env:t.env ~cancel ~cwd:(t.dir ^/ "repo") ~to_:after ~expect:before);
  files t;
  [%expect
    {|
    ok: [x x/y]
    ./x: f
    ok: [x x/y]
    ./x/y: inner
    |}]
;;

let%expect_test "outside a repository; refs outside the prefix; the op" =
  with_sandbox
  @@ fun t ->
  sh t "mkdir plain";
  print_s
    [%sexp
      (Checkpoint.capture ~env:t.env ~cancel ~cwd:(t.dir ^/ "plain") ~ref:None
       |> Or_error.map ~f:(function
         | Checkpoint.Captured.Not_git why -> mask t why
         | Snapshot _ -> "snapshot")
        : string Or_error.t)];
  [%expect {| (Ok "$DIR/plain is not in a git repository") |}];
  sh t "mkdir repo && cd repo && git init -q -b main .";
  print_s
    [%sexp
      (Checkpoint.capture
         ~env:t.env
         ~cancel
         ~cwd:(t.dir ^/ "repo")
         ~ref:(Some "refs/heads/main")
       |> Or_error.map ~f:(fun _ -> ())
        : unit Or_error.t)];
  [%expect
    {|
    (Error
     "checkpoint refs are under refs/prigh/checkpoints/, not refs/heads/main")
    |}];
  (* Through the op, as the backend calls it. *)
  let exec json =
    Checkpoint.run ~env:t.env ~cancel ~cwd:(t.dir ^/ "repo") ~arguments:json
  in
  sh t "cd repo && echo a > a";
  let c =
    match Checkpoint.Client.capture exec ~ref:(Some (ref_ "x")) with
    | Ok (Snapshot { commit = Some c; root; _ }) ->
      print_endline (mask t root);
      c
    | _ -> assert false
  in
  sh t "cd repo && echo b > a";
  print_result t (Checkpoint.Client.changed exec ~from:c ~to_:None);
  print_result t (Checkpoint.Client.restore exec ~to_:c ~expect:c);
  print_s [%sexp (Checkpoint.Client.drop exec ~prefix:Checkpoint.ref_prefix : int Or_error.t)];
  [%expect
    {|
    $DIR/repo
    ok: [a]
    error: files changed since the agent's last turn: a
    (Ok 1)
    |}]
;;
