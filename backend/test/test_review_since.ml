open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

(* What changed in a file since the user's last action on it in the review:
   each action (approving, commenting, replying, deciding a proposal, saving
   in the review's editor) snapshots the file, its text kept once per
   content in the session, and [review_since] diffs a snapshot with the file
   as it is now. *)

let sh (t : Tool_test_helpers.t) script =
  let command =
    sprintf
      "cd %s && export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1 \
       GIT_AUTHOR_NAME=t GIT_AUTHOR_EMAIL=t@t GIT_COMMITTER_NAME=t \
       GIT_COMMITTER_EMAIL=t@t && (%s) > /dev/null 2>&1"
      (Filename.quote (Filename.concat t.dir "repo"))
      script
  in
  match Sys_unix.command command with
  | 0 -> ()
  | n -> raise_s [%message "setup failed" (script : string) (n : int)]
;;

let with_agent replies f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  Core_unix.mkdir_p (Filename.concat t.dir "repo");
  sh
    t
    "git init -q -b main . && printf 'one\\ntwo\\nthree\\n' > a.txt && git add \
     -A && git commit -q -m init";
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create replies)
      ~tools:Tools.all
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:(Filename.concat t.dir "repo")
      ()
  in
  f t agent
;;

let root t = Filename.concat t.dir "repo"
let at_ms = Re.compile (Re.Perl.re {|"at_ms":[0-9]+|})

let call ?(by = "lloyd") ?(quiet = false) t agent meth params =
  match
    Option.first_some
      (Review_rpc.dispatch ~by agent ~meth ~params:(Jsonaf.of_string params))
      (Files_rpc.dispatch ~by agent ~meth ~params:(Jsonaf.of_string params))
  with
  | None -> print_endline "not a method"
  | Some (Ok json) ->
    if not quiet
    then
      print_endline
        (Re.replace_string
           at_ms
           ~by:{|"at_ms":T|}
           (mask t (Jsonaf.to_string json)))
  | Some (Error e) -> printf "error: %s\n" (mask t (Error.to_string_hum e))
;;

(* The file's history: each snapshot's action, by whom, and what it held. *)
let history t agent path =
  let review = Agent.review_state agent in
  List.iteri
    (Review_state.snapshots review ~root:(root t) ~path)
    ~f:(fun i (s : Review_snapshot.t) ->
      printf
        "%d %s by %s: %s\n"
        i
        (Review_snapshot.Action.to_string s.action)
        (Option.value s.by ~default:"-")
        (match s.content with
         | Text { md5 } ->
           sprintf
             "text %s%s"
             (String.prefix md5 8)
             (match Review_state.blob review ~md5 with
              | Some text -> sprintf " %S" text
              | None -> " (lost)")
         | Absent -> "absent"
         | Binary _ -> "binary"
         | Too_large { bytes } -> sprintf "too large (%d bytes)" bytes))
;;

let since ?path ?index t agent =
  match Agent.review_since agent ~root:(root t) ?path ?index () with
  | Error e -> printf "error: %s\n" (mask t (Error.to_string_hum e))
  | Ok files ->
    List.iter files ~f:(fun (s : Review_state.Since.t) ->
      printf
        "%s: since #%d %s changed=%b +%d -%d%s\n%s"
        s.path
        s.index
        (Review_snapshot.Action.to_string s.snapshot.action)
        s.changed
        s.diff.added
        s.diff.removed
        (Option.value_map s.note ~default:"" ~f:(fun n -> " (" ^ n ^ ")"))
        s.diff.diff)
;;

let%expect_test "approve, change the file, see what changed since" =
  with_agent []
  @@ fun t agent ->
  write t "repo/a.txt" "one\ntwo\nthree\nfour\n";
  call
    ~quiet:true
    t
    agent
    "review_approve"
    (sprintf {|{"root":%S,"path":"a.txt","hash":"h1"}|} (root t));
  history t agent "a.txt";
  since t agent;
  [%expect
    {|
    0 approved by lloyd: text 6fe10b4f "one\ntwo\nthree\nfour\n"
    a.txt: since #0 approved changed=false +0 -0
    |}];
  (* The agent changes it: the since-view is snapshot -> now. *)
  write t "repo/a.txt" "one\n2\nthree\nfour\nfive\n";
  call t agent "review_since" (sprintf {|{"root":%S,"path":"a.txt"}|} (root t));
  [%expect
    {| {"files":[{"path":"a.txt","index":0,"snapshot":{"root":"$DIR/repo","path":"a.txt","action":"approved","by":"lloyd","at_ms":T,"content":{"kind":"text","md5":"<id><id>"}},"current":{"kind":"text","md5":"<id><id>"},"changed":true,"added":2,"removed":1,"diff":"diff --git a/a.txt b/a.txt\n--- a/a.txt\n+++ b/a.txt\n@@ -1,4 +1,5 @@\n one\n-two\n+2\n three\n four\n+five\n"}]} |}];
  (* A comment snapshots it again; its text, the same as before, is kept
     once. Comparing with the older action still works. *)
  write t "repo/a.txt" "one\ntwo\nthree\nfour\n";
  call
    ~quiet:true
    t
    agent
    "review_comment"
    (sprintf
       {|{"root":%S,"path":"a.txt","first":1,"last":1,"anchor":"one","text":"Why?"}|}
       (root t));
  write t "repo/a.txt" "one\ntwo\nthree\nfour\n4.5\n";
  history t agent "a.txt";
  since t agent;
  since ~path:"a.txt" ~index:0 t agent;
  [%expect
    {|
    0 approved by lloyd: text 6fe10b4f "one\ntwo\nthree\nfour\n"
    1 commented by lloyd: text 6fe10b4f "one\ntwo\nthree\nfour\n"
    a.txt: since #1 commented changed=true +1 -0
    diff --git a/a.txt b/a.txt
    --- a/a.txt
    +++ b/a.txt
    @@ -2,3 +2,4 @@
     two
     three
     four
    +4.5
    a.txt: since #0 approved changed=true +1 -0
    diff --git a/a.txt b/a.txt
    --- a/a.txt
    +++ b/a.txt
    @@ -2,3 +2,4 @@
     two
     three
     four
    +4.5
    |}];
  let texts =
    List.count
      (Session.entries (Agent.session agent))
      ~f:(fun (e : Session.Entry.t) ->
        match e.payload with
        | Review (Snapshot { text = Some _; _ }) -> true
        | _ -> false)
  in
  print_s [%message (texts : int)];
  [%expect {| (texts 1) |}];
  (* Unknown actions and files say what to do. *)
  since ~path:"a.txt" ~index:5 t agent;
  since ~path:"b.txt" t agent;
  [%expect
    {|
    error: a.txt has no review action 5 to compare with: it has 2 (0 to 1)
    error: b.txt has no review action to compare with: approve it or comment on it first
    |}]
;;

let%expect_test "every action snapshots the file; kept across restarts" =
  with_agent
    [ Reply.tool_calls
        [ ( "r1"
          , "reply_to_comment"
          , {|{"id":"c1","text":"Like this?","edits":[{"old_text":"two","new_text":"2"}]}|}
          )
        ]
    ; Reply.text "Proposed."
    ]
  @@ fun t agent ->
  let r = root t in
  call
    ~quiet:true
    t
    agent
    "review_comment"
    (sprintf
       {|{"root":%S,"path":"a.txt","first":2,"last":2,"anchor":"two","text":"Digits?"}|}
       r);
  call ~quiet:true t agent "review_submit" "{}";
  Agent.wait_idle agent;
  call ~quiet:true t agent "review_reply" {|{"id":"c1","text":"Hmm"}|};
  call
    ~quiet:true
    t
    agent
    "review_decide"
    {|{"id":"c1","post":1,"accept":true}|};
  call
    ~quiet:true
    t
    agent
    "review_approve"
    (sprintf {|{"root":%S,"path":"a.txt","hash":"h"}|} r);
  call
    ~quiet:true
    t
    agent
    "review_unapprove"
    (sprintf {|{"root":%S,"path":"a.txt"}|} r);
  (* Saved in the review's editor (with [review_root]); a save elsewhere is
     not an action of the review's. *)
  call
    t
    agent
    "fs_write"
    (sprintf
       {|{"path":%S,"text":"one\n2\nthree\nedited\n","review_root":%S}|}
       (Filename.concat r "a.txt")
       r);
  call
    ~quiet:true
    t
    agent
    "fs_write"
    (sprintf {|{"path":%S,"text":"other\n"}|} (Filename.concat r "b.txt"));
  history t agent "a.txt";
  history t agent "b.txt";
  [%expect
    {|
    {"path":"$DIR/repo/a.txt","md5":"<id><id>"}
    0 commented by lloyd: text deed54b8 "one\ntwo\nthree\n"
    1 replied by lloyd: text deed54b8 "one\ntwo\nthree\n"
    2 accepted by lloyd: text 2046b60c "one\n2\nthree\n"
    3 approved by lloyd: text 2046b60c "one\n2\nthree\n"
    4 unapproved by lloyd: text 2046b60c "one\n2\nthree\n"
    5 edited by lloyd: text 57106145 "one\n2\nthree\nedited\n"
    |}];
  (* get_review lists the snapshots without their texts. *)
  call t agent "get_review" "{}";
  [%expect
    {| {"comments":[{"id":"c1","root":"$DIR/repo","path":"a.txt","first":2,"last":2,"side":"new","anchor":"two","status":"open","thread":[{"author":"user","text":"Digits?","at_ms":T,"delivered":true},{"author":"agent","text":"Like this?","at_ms":T,"delivered":true,"proposal":{"edits":[{"old_text":"two","new_text":"2"}],"state":"accepted"}},{"author":"user","text":"Hmm","at_ms":T,"delivered":false},{"author":"user","text":"Accepted your proposed change: it is applied.","at_ms":T,"delivered":false}]}],"approvals":[],"snapshots":[{"root":"$DIR/repo","path":"a.txt","action":"commented","by":"lloyd","at_ms":T,"content":{"kind":"text","md5":"<id><id>"}},{"root":"$DIR/repo","path":"a.txt","action":"replied","by":"lloyd","at_ms":T,"content":{"kind":"text","md5":"<id><id>"}},{"root":"$DIR/repo","path":"a.txt","action":"accepted","by":"lloyd","at_ms":T,"content":{"kind":"text","md5":"<id><id>"}},{"root":"$DIR/repo","path":"a.txt","action":"approved","by":"lloyd","at_ms":T,"content":{"kind":"text","md5":"<id><id>"}},{"root":"$DIR/repo","path":"a.txt","action":"unapproved","by":"lloyd","at_ms":T,"content":{"kind":"text","md5":"<id><id>"}},{"root":"$DIR/repo","path":"a.txt","action":"edited","by":"lloyd","at_ms":T,"content":{"kind":"text","md5":"<id><id>"}}]} |}];
  (* All of it is in the session file: a restart reads it back, and a fork
     keeps the texts. *)
  let loaded =
    Or_error.ok_exn (Session.load (Session.path (Agent.session agent)))
  in
  let forked =
    Or_error.ok_exn
      (Session.fork loaded ~dir:(Filename.concat t.dir "sessions"))
  in
  print_s
    [%message
      ""
        ~restart:
          (Review_state.equal (Session.review loaded) (Agent.review_state agent)
           : bool)
        ~fork:
          (Review_state.equal (Session.review forked) (Agent.review_state agent)
           : bool)];
  [%expect {| ((restart true) (fork true)) |}]
;;

let%expect_test "deleted, new, binary and large files" =
  with_agent []
  @@ fun t agent ->
  let r = root t in
  let approve path =
    call
      ~quiet:true
      t
      agent
      "review_approve"
      (sprintf {|{"root":%S,"path":%S,"hash":"h"}|} r path)
  in
  (* Approved while absent (deleted), then back. *)
  Core_unix.unlink (Filename.concat r "a.txt");
  approve "a.txt";
  write t "repo/a.txt" "back\n";
  write t "repo/bin.dat" "a\000b";
  approve "bin.dat";
  write t "repo/bin.dat" "a\000c";
  write t "repo/big.txt" (String.make (Review_snapshot.max_bytes + 1) 'x');
  approve "big.txt";
  write t "repo/new.txt" "new\n";
  approve "new.txt";
  Core_unix.unlink (Filename.concat r "new.txt");
  history t agent "a.txt";
  history t agent "bin.dat";
  history t agent "big.txt";
  since t agent;
  [%expect
    {|
    0 approved by lloyd: absent
    0 approved by lloyd: binary
    0 approved by lloyd: too large (1048577 bytes)
    a.txt: since #0 approved changed=true +1 -0
    diff --git a/a.txt b/a.txt
    --- a/a.txt
    +++ b/a.txt
    @@ -0,0 +1,1 @@
    +back
    big.txt: since #0 approved changed=false +0 -0
    bin.dat: since #0 approved changed=true +0 -0 (it is a binary file)
    new.txt: since #0 approved changed=true +0 -1
    diff --git a/new.txt b/new.txt
    --- a/new.txt
    +++ b/new.txt
    @@ -1,1 +0,0 @@
    -new
    |}];
  (* Approvals of old frontends (no root) record no snapshot. *)
  call ~quiet:true t agent "review_approve" {|{"path":"c.txt","hash":"h"}|};
  print_s [%sexp (Map.length (Agent.review_state agent).snapshots : int)];
  [%expect {| 4 |}]
;;
