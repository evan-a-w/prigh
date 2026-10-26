open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

(* Reviewing a worktree other than the session's, against its base: the
   branch it was made from, or the remote's default branch; and the review
   (comments, approvals, what is sent) kept apart per worktree. *)

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

let with_agent ?on_request replies f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  sh
    t
    "git init -q -b main . && printf 'one\\ntwo\\n' > a.txt && git add -A && \
     git commit -q -m init";
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create ?on_request replies)
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

let print_diff ?(full = false) t (r : Git_diff.Result.t Or_error.t) =
  match r with
  | Error e -> printf "error: %s\n" (show t (Error.to_string_hum e))
  | Ok r ->
    let files, added, removed = Git_diff.Result.stats r in
    printf
      "root=%s branch=%s main=%b base=%s against=%s files=%d +%d -%d\n"
      (show t r.root)
      (Option.value r.branch ~default:"(detached)")
      r.main
      r.base
      (show t r.against)
      files
      added
      removed;
    if full
    then (
      printf "branches: %s\n" (String.concat ~sep:" " r.branches);
      print_string (show t r.diff))
;;

let worktree t ~branch ~base =
  Worktree.create
    ~env:t.env
    ~cancel:Cancellation.never
    ~cwd:(Filename.concat t.dir "repo")
    ~dir:(Filename.concat t.dir "wts")
    ~branch
    ~base
    ()
  |> Or_error.ok_exn
  |> fun (c : Worktree.Created.t) -> c.worktree.path
;;

let%expect_test "git_diff of a worktree: committed, uncommitted and untracked \
                 changes against its base"
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
  sh t "git worktree add -q -b manual ../manual && cd ../manual && echo m > \
        m.txt && git add m.txt && git commit -q -m m";
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
    error: unknown revision "nope": name a branch, tag or commit (e.g. main)
    error: not a git repository: $DIR (name a worktree of the repository: /worktree lists them)
    |}]
;;

let comment t agent ~root ~path ~anchor text =
  match
    Agent.update_review agent ~f:(fun r ->
      Review_state.add
        r
        ~root
        ~path
        ~range:(Review_comment.Range.lines ~first:1 ~last:1)
        ~anchor
        ~text
        ~at:0.)
  with
  | Ok c -> c.id
  | Error e -> "error: " ^ mask t (Error.to_string_hum e)
;;

let dispatch t agent meth params =
  match Review_rpc.dispatch agent ~meth ~params:(Jsonaf.of_string params) with
  | None -> print_endline "not a review method"
  | Some (Ok json) -> print_endline (mask t (Jsonaf.to_string json))
  | Some (Error e) -> printf "error: %s\n" (mask t (Error.to_string_hum e))
;;

let%expect_test "the review per worktree: approvals, sending, proposals \
                 applied in the comment's worktree"
  =
  let sent = Queue.create () in
  with_agent
    ~on_request:(fun request ->
      match List.last request.messages with
      | Some (Message.User u) -> Queue.enqueue sent u.text
      | _ -> ())
    [ Reply.tool_calls
        [ ( "r1"
          , "reply_to_comment"
          , {|{"id":"c2","text":"Like this?","edits":[{"old_text":"two","new_text":"2"}]}|}
          )
        ]
    ; Reply.text "Proposed."
    ; Reply.text "Ok."
    ]
  @@ fun t agent ->
  let main = Filename.concat t.dir "repo" in
  let feat = worktree t ~branch:"feat/x" ~base:None in
  write t "repo/a.txt" "one\ntwo\nmain\n";
  write t (String.chop_prefix_exn feat ~prefix:(t.dir ^ "/") ^ "/a.txt") "one\ntwo\nfeat\n";
  print_endline (comment t agent ~root:main ~path:"a.txt" ~anchor:"one" "In main");
  print_endline
    (comment t agent ~root:feat ~path:"a.txt" ~anchor:"one" "In the worktree");
  [%expect {|
    c1
    c2
    |}];
  (* Approvals: one recorded before worktrees were told apart (no root), and
     one for each worktree's file of the same name. *)
  let call = dispatch t agent in
  call "review_approve" {|{"path":"a.txt","hash":"old"}|};
  call
    "review_approve"
    (sprintf {|{"root":%S,"path":"a.txt","hash":"h-main"}|} main);
  call
    "review_approve"
    (sprintf {|{"root":%S,"path":"a.txt","hash":"h-feat"}|} feat);
  call "review_unapprove" (sprintf {|{"root":%S,"path":"a.txt"}|} main);
  [%expect
    {|
    {"comments":[{"id":"c1","root":"$DIR/repo","path":"a.txt","first":1,"last":1,"side":"new","anchor":"one","status":"open","thread":[{"author":"user","text":"In main","at_ms":0,"delivered":false}]},{"id":"c2","root":"$DIR/wts/repo/feat-x","path":"a.txt","first":1,"last":1,"side":"new","anchor":"one","status":"open","thread":[{"author":"user","text":"In the worktree","at_ms":0,"delivered":false}]}],"approvals":[{"path":"a.txt","hash":"old"}]}
    {"comments":[{"id":"c1","root":"$DIR/repo","path":"a.txt","first":1,"last":1,"side":"new","anchor":"one","status":"open","thread":[{"author":"user","text":"In main","at_ms":0,"delivered":false}]},{"id":"c2","root":"$DIR/wts/repo/feat-x","path":"a.txt","first":1,"last":1,"side":"new","anchor":"one","status":"open","thread":[{"author":"user","text":"In the worktree","at_ms":0,"delivered":false}]}],"approvals":[{"path":"a.txt","hash":"old"},{"root":"$DIR/repo","path":"a.txt","hash":"h-main"}]}
    {"comments":[{"id":"c1","root":"$DIR/repo","path":"a.txt","first":1,"last":1,"side":"new","anchor":"one","status":"open","thread":[{"author":"user","text":"In main","at_ms":0,"delivered":false}]},{"id":"c2","root":"$DIR/wts/repo/feat-x","path":"a.txt","first":1,"last":1,"side":"new","anchor":"one","status":"open","thread":[{"author":"user","text":"In the worktree","at_ms":0,"delivered":false}]}],"approvals":[{"path":"a.txt","hash":"old"},{"root":"$DIR/repo","path":"a.txt","hash":"h-main"},{"root":"$DIR/wts/repo/feat-x","path":"a.txt","hash":"h-feat"}]}
    {"comments":[{"id":"c1","root":"$DIR/repo","path":"a.txt","first":1,"last":1,"side":"new","anchor":"one","status":"open","thread":[{"author":"user","text":"In main","at_ms":0,"delivered":false}]},{"id":"c2","root":"$DIR/wts/repo/feat-x","path":"a.txt","first":1,"last":1,"side":"new","anchor":"one","status":"open","thread":[{"author":"user","text":"In the worktree","at_ms":0,"delivered":false}]}],"approvals":[{"path":"a.txt","hash":"old"},{"root":"$DIR/wts/repo/feat-x","path":"a.txt","hash":"h-feat"}]}
    |}];
  (* Sending the worktree's comments leaves main's unsent; the agent reads
     the worktree and the file's absolute path. *)
  call "review_submit" (sprintf {|{"root":%S}|} feat);
  Agent.wait_idle agent;
  print_endline (mask t (Queue.last_exn sent));
  [%expect
    {|
    {"count":1}
    The user left review comments on the changes in a worktree (each comment's file is its absolute path, in the worktree it names: edit that file, even when it is not under your directory; line numbers are as when they commented: the file may have changed since). Answer every comment with reply_to_comment: say what you did, or why not. Make the changes they ask for; where the user should decide, propose the change with the reply's edits instead of making it (it is applied when they accept it). Resolve a comment (resolve: true) when it needs nothing more. Posts marked new are what you have not seen yet. A comment on text the user selected marks it ⟦like this⟧ in its code and quotes it in <selected>; one with side="old" is on lines the change removed, numbered as in the file before it.

    <comment id="c2" worktree="$DIR/wts/repo/feat-x" file="$DIR/wts/repo/feat-x/a.txt" lines="1" status="open">
    <code>
    1  one
    </code>
    <post from="user" new="true">
    In the worktree
    </post>
    </comment>
    |}];
  print_s
    [%sexp
      (List.map (Agent.review_state agent).comments ~f:Review_comment.unsent
       : bool list)];
  [%expect {| (true false) |}];
  (* Accepting the proposal edits the worktree's file, not main's. *)
  (match Agent.decide_proposal agent ~id:"c2" ~post:1 ~accept:true with
   | Ok c -> print_s [%sexp (c.thread |> List.length : int)]
   | Error e -> print_s [%sexp (e : Error.t)]);
  print_string (read t "repo/a.txt");
  print_string (In_channel.read_all (Filename.concat feat "a.txt"));
  [%expect
    {|
    3
    one
    two
    main
    one
    2
    feat
    |}];
  (* The rest goes with a submission without a root. *)
  call "review_submit" "{}";
  Agent.wait_idle agent;
  [%expect {| {"count":2} |}]
;;
