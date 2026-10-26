open! Core
open! Expect_test_helpers_core
open Prigh_ui
open Fixtures
module H = Test_app.H
module P = Prigh_protocol

(* /worktree: the repository's git worktrees, as [list_worktrees] gives them;
   the session works in fix/b. *)

let cwd = "/h/.prigh/worktrees/repo/fix-b"
let key = "abc123 backend:" ^ cwd

let listing_json =
  {|{"root":"/src/repo","project_dir":"/h/.prigh/worktrees/repo","worktrees":[
     {"path":"/src/repo","branch":"main","head":"0123456789abcdef","main":true,"changes":0,"ahead":1},
     {"path":"/h/.prigh/worktrees/repo/feat-a","branch":"feat/a","head":"1111111111","managed":true,"changes":3,
      "sessions":[{"id":"s1","path":"/s/s1","name":"fixing a","live":true},{"id":"s2","path":"/s/s2"}]},
     {"path":"/h/.prigh/worktrees/repo/fix-b","branch":"fix/b","head":"2222222222","managed":true,"changes":0,"current":true,
      "sessions":[{"id":"abc123","path":"/home/u/.prigh/sessions/1.jsonl","live":true}]},
     {"path":"/h/.prigh/worktrees/repo/old","branch":"old","head":"3333333333","managed":true,"prunable":true}]}|}
;;

let feat_a_json =
  {|{"path":"/h/.prigh/worktrees/repo/feat-a","branch":"feat/a","head":"1111111111","managed":true,"changes":3}|}
;;

let connected ?(height = 16) () =
  let h = Test_app.connected ~width:80 ~height () in
  H.event h (State (state ~cwd ~git_branch:"fix/b" ()));
  h
;;

(* As typed, closing the completion first so that Enter runs it. *)
let run ?(quiet = true) h text =
  String.iter text ~f:(fun c -> H.step ~quiet h (Key (Key.char c)));
  if Option.is_some h.H.model.autocomplete then H.esc h;
  H.enter h
;;

let editor h = print_endline ("editor: " ^ Editor.text h.H.model.editor)

let target ~command action : App.Reply_tag.t =
  Worktree_target { command; action }
;;

let picker () =
  let h = connected () in
  run h "/worktree";
  H.reply h Worktrees_picker listing_json;
  h
;;

let%expect_test "/worktree picks among the worktrees; Enter works there" =
  let h = connected () in
  run h "/worktree";
  H.reply h Worktrees_picker listing_json;
  H.show h;
  H.keys h "feat";
  H.show h;
  H.enter h;
  H.mode h;
  H.reply
    h
    (Worktree_entered
       "working in the worktree feat/a at /h/.prigh/worktrees/repo/feat-a")
    {|{"cwd":"/h/.prigh/worktrees/repo/feat-a","created":false}|};
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_worktrees) (params ()) (tag Worktrees_picker))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Worktrees of /src/repo  (4)
    / ▏
       main    /src/repo · main worktree · ↑1 ↓0
       feat/a  /h/.prigh/worktrees/repo/feat-a · 3 changed · 2 sessions
    ▸* fix/b   /h/.prigh/worktrees/repo/fix-b · here · 1 session
       old     /h/.prigh/worktrees/repo/old · gone
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter opens · Ctrl+D removes






      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Worktrees of /src/repo  (1)
    / feat▏
    ▸  feat/a  /h/.prigh/worktrees/repo/feat-a · 3 changed · 2 sessions
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter opens · Ctrl+D removes
    (Rpc
      (method_ set_cwd)
      (params (
        (path     /h/.prigh/worktrees/repo/feat-a)
        (worktree true)))
      (tag (
        Worktree_entered
        "working in the worktree feat/a at /h/.prigh/worktrees/repo/feat-a")))
    editing









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────── working in the worktree feat/a at /h/.prigh/worktrees/repo/feat-a ───
    > ▏
    deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  ()
;;

let%expect_test "Ctrl+N in the picker works there in a new session" =
  let h = picker () in
  H.keys h "feat";
  H.key h (Key.ctrl 'n');
  H.show h;
  H.reply
    h
    (Worktree_session_started
       { path = "/h/.prigh/worktrees/repo/feat-a"
       ; notice =
           "new session in the worktree feat/a at \
            /h/.prigh/worktrees/repo/feat-a"
       })
    "null";
  H.reply
    h
    (Worktree_entered
       "new session in the worktree feat/a at /h/.prigh/worktrees/repo/feat-a")
    {|{"cwd":"/h/.prigh/worktrees/repo/feat-a","created":false}|};
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_worktrees) (params ()) (tag Worktrees_picker))
    (Rpc
      (method_ new_session)
      (params ())
      (tag (
        Worktree_session_started
        (path /h/.prigh/worktrees/repo/feat-a)
        (notice
         "new session in the worktree feat/a at /h/.prigh/worktrees/repo/feat-a"))))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────────────────────────┐
    │ starting a session in the worktree feat/a at                                 │
    │ /h/.prigh/worktrees/repo/feat-a…                                             │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc
      (method_ set_cwd)
      (params ((path /h/.prigh/worktrees/repo/feat-a)))
      (tag (
        Worktree_entered
        "new session in the worktree feat/a at /h/.prigh/worktrees/repo/feat-a")))













    ────── new session in the worktree feat/a at /h/.prigh/worktrees/repo/feat-a ───
    > ▏
    deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "Ctrl+D in the picker asks first; Esc goes back to it" =
  let h = picker () in
  H.keys h "feat";
  H.key h (Key.ctrl 'd');
  H.show h;
  H.esc h;
  H.show h;
  H.key h (Key.ctrl 'd');
  H.enter h;
  H.show h;
  (* Uncommitted changes: refused, and [rm -f] is in the editor. *)
  H.reply_error
    h
    (Worktree_removed { target = "feat/a"; force = false })
    "/h/.prigh/worktrees/repo/feat-a has uncommitted changes (3 files): commit \
     or stash them first, or force the removal";
  H.show h;
  H.enter h;
  H.reply
    h
    (target
       ~command:"/worktree rm -f feat/a"
       (Remove { target = "feat/a"; force = true }))
    listing_json;
  H.show h;
  H.key h (Key.char 'y');
  H.reply
    h
    (Worktree_removed { target = "feat/a"; force = true })
    (sprintf {|{"worktree":%s}|} feat_a_json);
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_worktrees) (params ()) (tag Worktrees_picker))




      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────────────────────────────────────────────┐
    │ Remove the worktree feat/a at /h/.prigh/worktrees/repo/feat-a; it has 3      │
    │ uncommitted changes; sessions working there: fixing a, s2? (the branch       │
    │ stays) (y/n)                                                                 │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  confirm: y / n






      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Worktrees of /src/repo  (1)
    / feat▏
    ▸  feat/a  /h/.prigh/worktrees/repo/feat-a · 3 changed · 2 sessions
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter opens · Ctrl+D removes
    (Rpc
      (method_ remove_worktree)
      (params ((path /h/.prigh/worktrees/repo/feat-a)))
      (tag (
        Worktree_removed
        (target feat/a)
        (force  false))))









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────── removing the worktree at /h/.prigh/worktrees/repo/feat-a… ───
    > ▏
    deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01




      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ /h/.prigh/worktrees/repo/feat-a has uncommitted changes (3 files): commit or │
    │ stash them first, or force the removal; /worktree rm -f feat/a removes it    │
    │ with its changes                                                             │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > /worktree rm -f feat/a▏
    deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    (Rpc
      (method_ list_worktrees)
      (params ())
      (tag (
        Worktree_target
        (command "/worktree rm -f feat/a")
        (action (
          Remove
          (target feat/a)
          (force  true))))))




      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────────────────────────────────────────────┐
    │ Remove the worktree feat/a at /h/.prigh/worktrees/repo/feat-a; its 3         │
    │ uncommitted changes are lost; sessions working there: fixing a, s2? (the     │
    │ branch stays) (y/n)                                                          │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  confirm: y / n
    (Rpc
      (method_ remove_worktree)
      (params (
        (path  /h/.prigh/worktrees/repo/feat-a)
        (force true)))
      (tag (
        Worktree_removed
        (target feat/a)
        (force  true))))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────────────────────────┐
    │ removed the worktree feat/a at /h/.prigh/worktrees/repo/feat-a (the branch   │
    │ stays)                                                                       │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "the main worktree and the session's own are not removed" =
  let h = picker () in
  H.key h (Key.ctrl 'd');
  H.show h;
  H.esc h;
  run h "/worktree rm fix/b";
  H.reply
    h
    (target
       ~command:"/worktree rm fix/b"
       (Remove { target = "fix/b"; force = false }))
    listing_json;
  H.show h;
  H.mode h;
  [%expect
    {|
    (Rpc (method_ list_worktrees) (params ()) (tag Worktrees_picker))

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ this session works in /h/.prigh/worktrees/repo/fix-b: /worktree open another │
    │ one (or /cd elsewhere) first                                                 │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    Worktrees of /src/repo  (4)
    / ▏
       main    /src/repo · main worktree · ↑1 ↓0
       feat/a  /h/.prigh/worktrees/repo/feat-a · 3 changed · 2 sessions
    ▸* fix/b   /h/.prigh/worktrees/repo/fix-b · here · 1 session
       old     /h/.prigh/worktrees/repo/old · gone
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter opens · Ctrl+D removes
    (Rpc
      (method_ list_worktrees)
      (params ())
      (tag (
        Worktree_target
        (command "/worktree rm fix/b")
        (action (
          Remove
          (target fix/b)
          (force  false))))))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ this session works in /h/.prigh/worktrees/repo/fix-b: /worktree open another │
    │ one (or /cd elsewhere) first                                                 │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    editing
    |}]
;;

let%expect_test "/worktree rm asks first: Esc cancels, Enter removes" =
  let h = connected () in
  run ~quiet:false h "/worktree rm feat/a";
  H.reply
    h
    (target
       ~command:"/worktree rm feat/a"
       (Remove { target = "feat/a"; force = false }))
    listing_json;
  H.show h;
  H.esc h;
  H.show h;
  run h "/worktree rm -f feat-a";
  H.reply
    h
    (target
       ~command:"/worktree rm -f feat-a"
       (Remove { target = "feat-a"; force = true }))
    listing_json;
  H.mode h;
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ list_worktrees)
      (params ())
      (tag (
        Worktrees_for_autocomplete
        "abc123 backend:/h/.prigh/worktrees/repo/fix-b")))
    (Rpc
      (method_ list_worktrees)
      (params ())
      (tag (
        Worktree_target
        (command "/worktree rm feat/a")
        (action (
          Remove
          (target feat/a)
          (force  false))))))




      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────────────────────────────────────────────┐
    │ Remove the worktree feat/a at /h/.prigh/worktrees/repo/feat-a; it has 3      │
    │ uncommitted changes; sessions working there: fixing a, s2? (the branch       │
    │ stays) (y/n)                                                                 │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  confirm: y / n









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────────── cancelled ───
    > ▏
    deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    (Rpc
      (method_ list_worktrees)
      (params ())
      (tag (
        Worktree_target
        (command "/worktree rm -f feat-a")
        (action (
          Remove
          (target feat-a)
          (force  true))))))
    confirm
    (Rpc
      (method_ remove_worktree)
      (params (
        (path  /h/.prigh/worktrees/repo/feat-a)
        (force true)))
      (tag (
        Worktree_removed
        (target feat/a)
        (force  true))))
    |}]
;;

let%expect_test "/worktree new creates one and works there" =
  let h = connected () in
  run ~quiet:false h "/worktree new feat/c";
  run ~quiet:false h "/worktree new feat/d main";
  H.show h;
  H.reply
    h
    (Retry_in_editor
       { command = "/worktree new feat/c"; hint = ""; then_ = Worktree_created })
    {|{"worktree":{"path":"/h/.prigh/worktrees/repo/feat-c","branch":"feat/c","head":"4444","managed":true,"changes":0},"new_branch":true}|};
  H.reply
    h
    (Worktree_entered
       "worktree feat/c at /h/.prigh/worktrees/repo/feat-c (new branch)")
    {|{"cwd":"/h/.prigh/worktrees/repo/feat-c","created":false}|};
  H.show h;
  (* A refusal puts the command back to fix. *)
  H.reply_error
    h
    (Retry_in_editor
       { command = "/worktree new feat/d main"
       ; hint = ""
       ; then_ = Worktree_created
       })
    "not a valid branch name: \"feat/d main\" (e.g. feat/my-change)";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ list_worktrees)
      (params ())
      (tag (
        Worktrees_for_autocomplete
        "abc123 backend:/h/.prigh/worktrees/repo/fix-b")))
    (Rpc
      (method_ create_worktree)
      (params ((branch feat/c)))
      (tag (
        Retry_in_editor
        (command "/worktree new feat/c")
        (hint    "")
        (then_   Worktree_created))))
    (Rpc
      (method_ create_worktree)
      (params (
        (branch feat/d)
        (base   main)))
      (tag (
        Retry_in_editor
        (command "/worktree new feat/d main")
        (hint    "")
        (then_   Worktree_created))))









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────────── creating a worktree for feat/d… ───
    > ▏
    deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    (Rpc
      (method_ set_cwd)
      (params (
        (path     /h/.prigh/worktrees/repo/feat-c)
        (worktree true)))
      (tag (
        Worktree_entered
        "worktree feat/c at /h/.prigh/worktrees/repo/feat-c (new branch)")))









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────── worktree feat/c at /h/.prigh/worktrees/repo/feat-c (new branch) ───
    > ▏
    deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ─────────────── not a valid branch name: "feat/d main" (e.g. feat/my-change) ───
    > /worktree new feat/d main▏
    deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/worktree open -n works there in a new session" =
  let h = connected () in
  run ~quiet:false h "/worktree open -n main";
  H.reply
    h
    (target
       ~command:"/worktree open -n main"
       (Open { target = "main"; new_session = true }))
    listing_json;
  H.reply
    h
    (Worktree_session_started
       { path = "/src/repo"
       ; notice = "new session in the worktree main at /src/repo"
       })
    "null";
  run ~quiet:false h "/worktree open /src/repo/";
  H.reply
    h
    (target
       ~command:"/worktree open /src/repo/"
       (Open { target = "/src/repo/"; new_session = false }))
    listing_json;
  [%expect
    {|
    (Rpc
      (method_ list_worktrees)
      (params ())
      (tag (
        Worktrees_for_autocomplete
        "abc123 backend:/h/.prigh/worktrees/repo/fix-b")))
    (Rpc
      (method_ list_worktrees)
      (params ())
      (tag (
        Worktree_target
        (command "/worktree open -n main")
        (action (
          Open
          (target      main)
          (new_session true))))))
    (Rpc
      (method_ new_session)
      (params ())
      (tag (
        Worktree_session_started
        (path /src/repo)
        (notice "new session in the worktree main at /src/repo"))))
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc
      (method_ set_cwd)
      (params ((path /src/repo)))
      (tag (Worktree_entered "new session in the worktree main at /src/repo")))
    (Rpc
      (method_ list_worktrees)
      (params ())
      (tag (
        Worktrees_for_autocomplete
        "abc123 backend:/h/.prigh/worktrees/repo/fix-b")))
    (Rpc
      (method_ list_worktrees)
      (params ())
      (tag (
        Worktree_target
        (command "/worktree open /src/repo/")
        (action (
          Open
          (target      /src/repo/)
          (new_session false))))))
    (Rpc
      (method_ set_cwd)
      (params (
        (path     /src/repo)
        (worktree true)))
      (tag (Worktree_entered "working in the worktree main at /src/repo")))
    |}]
;;

let%expect_test "/worktree's mistakes say what to do, the command kept to fix" =
  let h = connected () in
  let attempt text =
    run h text;
    editor h;
    H.show h;
    H.key h (Key.ctrl 'u')
  in
  attempt "/worktree bogus";
  attempt "/worktree open";
  attempt "/worktree rm -x feat/a";
  run h "/worktree open feet/a";
  H.reply
    h
    (target
       ~command:"/worktree open feet/a"
       (Open { target = "feet/a"; new_session = false }))
    listing_json;
  editor h;
  H.show h;
  H.key h (Key.ctrl 'u');
  (* A worktree whose directory is gone. *)
  run h "/worktree open old";
  H.reply
    h
    (target
       ~command:"/worktree open old"
       (Open { target = "old"; new_session = false }))
    listing_json;
  H.show h;
  (* Not in a repository. *)
  run h "/worktree";
  H.reply_error
    h
    Worktrees_picker
    "not a git repository: /work (git init starts one, or /cd into one)";
  H.show h;
  H.mode h;
  [%expect
    {|
    editor: /worktree bogus





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ unknown subcommand "bogus": /worktree [list|new BRANCH [BASE]|open [-n]      │
    │ TARGET|back [main]|rm [-f] TARGET]                                           │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > /worktree bogus▏
    deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    editor: /worktree open




      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ usage: /worktree open [-n] TARGET (TARGET: a branch or path, as /worktree    │
    │ lists them)                                                                  │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ▸ open  open [-n] TARGET: work in a worktree (-n: in a new session)
    ────────────────────────────────────────────────────────────────────────────────
    > /worktree open▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  Tab/Enter accept · Alt+S as typed
    editor: /worktree rm -x feat/a









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ───────────────────────── unknown option -x: usage: /worktree rm [-f] TARGET ───
    > /worktree rm -x feat/a▏
    deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    (Rpc
      (method_ list_worktrees)
      (params ())
      (tag (
        Worktree_target
        (command "/worktree open feet/a")
        (action (
          Open
          (target      feet/a)
          (new_session false))))))
    editor: /worktree open feet/a






      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ no worktree "feet/a" (closest: feat/a, feat-a, fix/b); /worktree lists them  │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > /worktree open feet/a▏
    deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    (Rpc
      (method_ list_worktrees)
      (params ())
      (tag (
        Worktree_target
        (command "/worktree open old")
        (action (
          Open
          (target      old)
          (new_session false))))))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ the worktree old is gone (/h/.prigh/worktrees/repo/old was deleted);         │
    │ /worktree rm old forgets it                                                  │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    (Rpc (method_ list_worktrees) (params ()) (tag Worktrees_picker))









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ───────── not a git repository: /work (git init starts one, or /cd into one) ───
    > ▏
    deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    editing
    |}]
;;

let%expect_test
    "completion: subcommands, then worktrees; fetched once per session"
  =
  let h = connected ~height:20 () in
  let popup () =
    let ac = Option.value_exn h.H.model.autocomplete in
    print_s
      [%sexp
        (List.map (Autocomplete.items ac) ~f:(fun i -> i.id, i.detail)
         : (string * string) list)]
  in
  H.keys h "/worktree ";
  popup ();
  H.reply h (Worktrees_for_autocomplete key) listing_json;
  H.keys h "open ";
  popup ();
  H.keys h "fe";
  H.show h;
  H.key h (Key.plain Tab);
  editor h;
  H.key h (Key.ctrl 'u');
  H.keys h "/worktree rm ";
  popup ();
  H.key h (Key.ctrl 'u');
  H.keys h "/worktree new feat/x ";
  popup ();
  H.key h (Key.ctrl 'u');
  (* Another session: its repository is asked again. *)
  H.reply ~quiet:true h Reload_messages "null";
  H.reply
    ~quiet:true
    h
    Initial_state
    (state_json ~cwd:"/src/repo" ~session:("other", "/s/other") ());
  H.keys h "/worktree open ";
  [%expect
    {|
    (Rpc
      (method_ list_worktrees)
      (params ())
      (tag (
        Worktrees_for_autocomplete
        "abc123 backend:/h/.prigh/worktrees/repo/fix-b")))
    ((list "pick a worktree of this repository to work in")
     (new "new BRANCH [BASE]: create a worktree and work in it")
     (open "open [-n] TARGET: work in a worktree (-n: in a new session)")
     (back "back [main]: back where the session switched into worktrees from")
     (rm "rm [-f] TARGET: remove a worktree (-f: with uncommitted changes)"))
    ((-n "in a new session")
     (main
      "main \194\183 /src/repo \194\183 main worktree \194\183 \226\134\1451 \226\134\1470")
     (feat/a
      "feat/a \194\183 /h/.prigh/worktrees/repo/feat-a \194\183 3 changed \194\183 2 sessions")
     (fix/b
      "fix/b \194\183 /h/.prigh/worktrees/repo/fix-b \194\183 here \194\183 1 session")
     (old "old \194\183 /h/.prigh/worktrees/repo/old \194\183 gone"))












      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ feat/a  feat/a · /h/.prigh/worktrees/repo/feat-a · 3 changed · 2 sessions
    ────────────────────────────────────────────────────────────────────────────────
    > /worktree open fe▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  Tab/Enter accept · Alt+S as typed
    editor: /worktree open feat/a
    ((-f "also with uncommitted changes")
     (feat/a
      "feat/a \194\183 /h/.prigh/worktrees/repo/feat-a \194\183 3 changed \194\183 2 sessions")
     (old "old \194\183 /h/.prigh/worktrees/repo/old \194\183 gone"))
    ((main
      "base: main \194\183 /src/repo \194\183 main worktree \194\183 \226\134\1451 \226\134\1470")
     (feat/a
      "base: feat/a \194\183 /h/.prigh/worktrees/repo/feat-a \194\183 3 changed \194\183 2 sessions")
     (fix/b
      "base: fix/b \194\183 /h/.prigh/worktrees/repo/fix-b \194\183 here \194\183 1 session")
     (old "base: old \194\183 /h/.prigh/worktrees/repo/old \194\183 gone"))
    (Rpc
      (method_ list_worktrees)
      (params ())
      (tag (Worktrees_for_autocomplete "other backend:/src/repo")))
    |}]
;;

let%expect_test
    "/review completion: worktrees and branches not checked out, then remote \
     branches and tags after --base"
  =
  let h = connected ~height:20 () in
  let popup () =
    let ac = Option.value_exn h.H.model.autocomplete in
    print_s
      [%sexp
        (List.map (Autocomplete.items ac) ~f:(fun i -> i.id)
         : string list)]
  in
  H.keys h "/review ";
  H.reply h (Worktrees_for_autocomplete key) listing_json;
  H.reply
    h
    (Refs_for_autocomplete key)
    {|{"branches":["feat/a","fix/b","main","old-work"],"remote_branches":["origin/main","origin/feat/r"],"tags":["v2","v1"]}|};
  popup ();
  H.keys h "old-work --base ";
  popup ();
  H.keys h "origin/m";
  H.key h (Key.plain Tab);
  H.keys h " --d";
  H.key h (Key.plain Tab);
  editor h;
  [%expect {|
    (Rpc
      (method_ list_worktrees)
      (params ())
      (tag (
        Worktrees_for_autocomplete
        "abc123 backend:/h/.prigh/worktrees/repo/fix-b")))
    (Rpc
      (method_ git_refs)
      (params ())
      (tag (Refs_for_autocomplete "abc123 backend:/h/.prigh/worktrees/repo/fix-b")))
    (--base --direct main feat/a fix/b old old-work origin/main origin/feat/r)
    (HEAD feat/a fix/b main old old-work origin/main origin/feat/r v2 v1)
    editor: /review old-work --base origin/main --direct
    |}]
;;

let%expect_test "the backend's refusals are shown, the command kept to retry" =
  let h = connected () in
  H.reply_error
    h
    (Worktree_removed { target = "fix/b"; force = false })
    "this session works in /h/.prigh/worktrees/repo/fix-b: /cd out of it (e.g. \
     to the main worktree) first";
  editor h;
  H.show h;
  H.key h (Key.ctrl 'u');
  H.reply_error
    h
    (Worktree_entered "working in the worktree feat/a at /x")
    "no such directory: /x";
  H.show h;
  (* No listing for completion: only the flag completes. *)
  H.keys h "/worktree ";
  H.reply_error h (Worktrees_for_autocomplete key) "not a git repository: /x";
  H.keys h "open ";
  print_s
    [%sexp
      (List.map
         (Autocomplete.items (Option.value_exn h.model.autocomplete))
         ~f:(fun i -> i.id)
       : string list)];
  [%expect
    {|
    editor: /worktree rm fix/b





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ this session works in /h/.prigh/worktrees/repo/fix-b: /cd out of it (e.g. to │
    │ the main worktree) first                                                     │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > /worktree rm fix/b▏
    deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────── no such directory: /x ───
    > ▏
    deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    (Rpc
      (method_ list_worktrees)
      (params ())
      (tag (
        Worktrees_for_autocomplete
        "abc123 backend:/h/.prigh/worktrees/repo/fix-b")))
    (-n)
    |}]
;;

let%expect_test "another session closes the picker and forgets the listing" =
  let h = picker () in
  H.reply ~quiet:true h Reload_messages "null";
  H.mode h;
  print_s
    [%sexp
      (Prigh_ui.Location_cache.find h.model.worktrees ~key
       : P.Worktree.Listing.t option)];
  [%expect
    {|
    (Rpc (method_ list_worktrees) (params ()) (tag Worktrees_picker))
    editing
    ()
    |}]
;;

(* /review WORKTREE: that worktree's files and their approvals, against its
   base; Alt+R in the picker does the same for the highlighted one. *)

let feat_review_json =
  {|{"comments":[
      {"id":"c1","root":"/src/repo","path":"a.ml","first":1,"last":1,"anchor":"x","status":"open",
       "thread":[{"author":"user","text":"In main","at_ms":1,"delivered":false}]},
      {"id":"c2","root":"/h/.prigh/worktrees/repo/feat-a","path":"a.ml","first":1,"last":1,"anchor":"x","status":"open",
       "thread":[{"author":"user","text":"In feat/a","at_ms":2,"delivered":false}]}],
     "approvals":[{"path":"a.ml","hash":"legacy"},
                  {"root":"/h/.prigh/worktrees/repo/feat-a","path":"b.ml","hash":"stale"}]}|}
;;

let feat_diff_json =
  {|{"root":"/h/.prigh/worktrees/repo/feat-a","against":"0123456789abcdef0123456789abcdef01234567",
     "base":"main","branch":"feat/a","main":false,"branches":["feat/a","main"],"truncated":false,
     "diff":"diff --git a/a.ml b/a.ml\n--- a/a.ml\n+++ b/a.ml\n@@ -1 +1 @@\n-x\n+y\ndiff --git a/b.ml b/b.ml\n--- a/b.ml\n+++ b/b.ml\n@@ -1 +1 @@\n-p\n+q\n"}|}
;;

let show_feat_review h ~dir ~base =
  H.reply
    ~quiet:true
    h
    (Review_comments { target = Dir dir; base; direct = false })
    feat_review_json;
  let review =
    P.Review.of_json (P.Json.parse feat_review_json |> Or_error.ok_exn)
    |> Or_error.ok_exn
  in
  H.reply
    h
    (Retry_in_editor
       { command = Review_command.command ~target:(Dir dir) ?base ()
       ; hint = ""
       ; then_ = Review_status { review; target = Dir dir }
       })
    feat_diff_json;
  H.show h
;;

let%expect_test "/review WORKTREE: its approvals and comments, not the others'" =
  let h = connected ~height:20 () in
  run h "/review feat/a";
  H.reply
    h
    (Review_named
       { command = "/review feat/a"
       ; name = "feat/a"
       ; base = None
       ; direct = false
       ; model = None
       })
    listing_json;
  show_feat_review h ~dir:(Some "/h/.prigh/worktrees/repo/feat-a") ~base:None;
  [%expect
    {|
    (Rpc
      (method_ list_worktrees)
      (params ())
      (tag (
        Review_named
        (command "/review feat/a")
        (name    feat/a)
        (base ())
        (direct false)
        (model (feat/a)))))
    (Rpc
      (method_ get_review)
      (params ())
      (tag (
        Review_comments
        (target (Dir (/h/.prigh/worktrees/repo/feat-a)))
        (base ())
        (direct false))))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Review ─────────────────────────────────────────────────────────────────────┐
    │ review feat/a (/h/.prigh/worktrees/repo/feat-a) against main (merge base     │
    │ 01234567)  0 of 2 files approved                                             │
    │   ○ a.ml  +1 −1  not approved                                                │
    │     c2 line 1, not sent yet                                                  │
    │       you: In feat/a                                                         │
    │   ● b.ml  +1 −1  changed since approved                                      │
    │ approve files and comment on code you select in prigh-web's review page;     │
    │ /review HEAD has prigh review the changes itself                             │
    └───────────────────────────────────────────────────────────────── Esc closes ─┘
    ──────────────────────────────────────────────────────── reading the review… ───
    > ▏
    deepseek-flash  think:off  ↑↓ scroll · Esc closes · typing goes to the editor
    |}]
;;

(* With review actions recorded, /review also reads what changed in each
   file since its last action. *)
let%expect_test "/review: what changed since the last action on each file" =
  let h = connected ~height:20 () in
  let root = "/h/.prigh/worktrees/repo/feat-a" in
  let snapshot path action =
    sprintf
      {|{"root":"%s","path":"%s","action":"%s","at_ms":1,"content":{"kind":"text","md5":"m"}}|}
      root
      path
      action
  in
  let review_json =
    sprintf
      {|{"comments":[],"approvals":[{"root":"%s","path":"b.ml","hash":"stale"}],"snapshots":[%s,%s]}|}
      root
      (snapshot "a.ml" "commented")
      (snapshot "b.ml" "approved")
  in
  let since_file path action ~added ~removed =
    sprintf
      {|{"path":"%s","index":0,"snapshot":%s,"current":{"kind":"text","md5":"n"},"changed":%b,"added":%d,"removed":%d,"diff":""}|}
      path
      (snapshot path action)
      (added + removed > 0)
      added
      removed
  in
  let review =
    P.Review.of_json (P.Json.parse review_json |> Or_error.ok_exn)
    |> Or_error.ok_exn
  in
  let diff =
    P.Git_diff.of_json (P.Json.parse feat_diff_json |> Or_error.ok_exn)
    |> Or_error.ok_exn
  in
  let dir = Some root in
  let show_diff () =
    H.reply
      h
      (Retry_in_editor
         { command = Review_command.command ?dir ()
         ; hint = ""
         ; then_ = Review_status { review; dir }
         })
      feat_diff_json
  in
  show_diff ();
  [%expect
    {|
    (Rpc
      (method_ review_since)
      (params ((root /h/.prigh/worktrees/repo/feat-a)))
      (tag (
        Review_since
        (review (
          (comments ())
          (approvals ((
            (root (/h/.prigh/worktrees/repo/feat-a))
            (path b.ml)
            (hash stale))))
          (snapshots (
            ((root   /h/.prigh/worktrees/repo/feat-a)
             (path   a.ml)
             (action commented)
             (by ())
             (at ((1970-01-01 10:00:00.001000000+10:00)))
             (content (Text (md5 m))))
            ((root   /h/.prigh/worktrees/repo/feat-a)
             (path   b.ml)
             (action approved)
             (by ())
             (at ((1970-01-01 10:00:00.001000000+10:00)))
             (content (Text (md5 m))))))))
        (diff (
          (root    /h/.prigh/worktrees/repo/feat-a)
          (against 0123456789abcdef0123456789abcdef01234567)
          (diff
           "diff --git a/a.ml b/a.ml\n--- a/a.ml\n+++ b/a.ml\n@@ -1 +1 @@\n-x\n+y\ndiff --git a/b.ml b/b.ml\n--- a/b.ml\n+++ b/b.ml\n@@ -1 +1 @@\n-p\n+q\n")
          (truncated false)
          (branch (feat/a))
          (base main)
          (main false)
          (branches (feat/a main)))))))
    |}];
  H.reply
    h
    (Review_since { review; diff })
    (sprintf
       {|{"files":[%s,%s]}|}
       (since_file "a.ml" "commented" ~added:1 ~removed:0)
       (since_file "b.ml" "approved" ~added:2 ~removed:1));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Review ─────────────────────────────────────────────────────────────────────┐
    │ review feat/a (/h/.prigh/worktrees/repo/feat-a) against main (merge base     │
    │ 01234567)  0 of 2 files approved                                             │
    │   ○ a.ml  +1 −1  not approved, +1 −0 since commented                         │
    │   ● b.ml  +1 −1  changed since approved (+2 −1)                              │
    │ approve files and comment on code you select in prigh-web's review page;     │
    │ /review HEAD has prigh review the changes itself                             │
    └───────────────────────────────────────────────────────────────── Esc closes ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  ↑↓ scroll · Esc closes · typing goes to the editor
    |}];
  (* An older backend without review_since: the list as before. *)
  show_diff ();
  H.reply_error h (Review_since { review; diff }) "unknown method review_since";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ review_since)
      (params ((root /h/.prigh/worktrees/repo/feat-a)))
      (tag (
        Review_since
        (review (
          (comments ())
          (approvals ((
            (root (/h/.prigh/worktrees/repo/feat-a))
            (path b.ml)
            (hash stale))))
          (snapshots (
            ((root   /h/.prigh/worktrees/repo/feat-a)
             (path   a.ml)
             (action commented)
             (by ())
             (at ((1970-01-01 10:00:00.001000000+10:00)))
             (content (Text (md5 m))))
            ((root   /h/.prigh/worktrees/repo/feat-a)
             (path   b.ml)
             (action approved)
             (by ())
             (at ((1970-01-01 10:00:00.001000000+10:00)))
             (content (Text (md5 m))))))))
        (diff (
          (root    /h/.prigh/worktrees/repo/feat-a)
          (against 0123456789abcdef0123456789abcdef01234567)
          (diff
           "diff --git a/a.ml b/a.ml\n--- a/a.ml\n+++ b/a.ml\n@@ -1 +1 @@\n-x\n+y\ndiff --git a/b.ml b/b.ml\n--- a/b.ml\n+++ b/b.ml\n@@ -1 +1 @@\n-p\n+q\n")
          (truncated false)
          (branch (feat/a))
          (base main)
          (main false)
          (branches (feat/a main)))))))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Review ─────────────────────────────────────────────────────────────────────┐
    │ review feat/a (/h/.prigh/worktrees/repo/feat-a) against main (merge base     │
    │ 01234567)  0 of 2 files approved                                             │
    │   ○ a.ml  +1 −1  not approved                                                │
    │   ● b.ml  +1 −1  changed since approved                                      │
    │ approve files and comment on code you select in prigh-web's review page;     │
    │ /review HEAD has prigh review the changes itself                             │
    └───────────────────────────────────────────────────────────────── Esc closes ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  ↑↓ scroll · Esc closes · typing goes to the editor
    |}]
;;

let%expect_test
    "/review: a path and --base go straight there; a word that names no \
     worktree has the model review"
  =
  let h = connected () in
  run h "/review ../feat-a --base main";
  run h "/review tests";
  H.reply
    h
    (Review_named
       { command = "/review tests"
       ; name = "tests"
       ; base = None
       ; direct = false
       ; model = Some "tests"
       })
    listing_json;
  run h "/review nope --base main";
  H.reply
    h
    (Review_named
       { command = "/review nope --base main"
       ; name = "nope"
       ; base = Some "main"
       ; direct = false
       ; model = None
       })
    listing_json;
  (* No worktree of that name: a branch to review read-only, which the
     backend does not know either. *)
  H.reply
    ~quiet:true
    h
    (Review_comments { target = Rev "nope"; base = Some "main"; direct = false })
    feat_review_json;
  H.reply_error
    h
    (Retry_in_editor
       { command = "/review nope --base main"
       ; hint = ""
       ; then_ =
           Review_status
             { review =
                 P.Review.of_json
                   (P.Json.parse feat_review_json |> Or_error.ok_exn)
                 |> Or_error.ok_exn
             ; target = Rev "nope"
             }
       })
    "unknown revision \"nope\": name a branch to review (closest: old, main)";
  H.show h;
  editor h;
  [%expect
    {|
    (Rpc
      (method_ get_review)
      (params ())
      (tag (
        Review_comments (target (Dir (../feat-a))) (base (main)) (direct false))))
    (Rpc
      (method_ list_worktrees)
      (params ())
      (tag (
        Review_named
        (command "/review tests")
        (name    tests)
        (base ())
        (direct false)
        (model (tests)))))
    (Rpc
      (method_ review)
      (params ((args tests)))
      (tag (
        Retry_in_editor
        (command "/review tests")
        (hint    "")
        (then_   Review_started))))
    (Rpc
      (method_ list_worktrees)
      (params ())
      (tag (
        Review_named
        (command "/review nope --base main")
        (name    nope)
        (base (main))
        (direct false)
        (model ()))))
    (Rpc
      (method_ get_review)
      (params ())
      (tag (Review_comments (target (Rev nope)) (base (main)) (direct false))))









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────── unknown revision "nope": name a branch to review (closest: old, main) ───
    > /review nope --base main▏
    deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    editor: /review nope --base main
    |}]
;;

let%expect_test
    "/review BRANCH --base REV --direct: a branch not checked out, read-only, \
     compared directly"
  =
  let h = connected () in
  run h "/review feat/b --base main --direct";
  H.reply
    h
    (Review_named
       { command = "/review feat/b --base main --direct"
       ; name = "feat/b"
       ; base = Some "main"
       ; direct = true
       ; model = None
       })
    listing_json;
  let review_json =
    {|{"comments":[{"id":"c3","root":"branch:feat/b","path":"b.ml","first":1,"last":1,"anchor":"q","status":"open",
       "thread":[{"author":"user","text":"On the branch","at_ms":3,"delivered":false}]}],"approvals":[]}|}
  in
  H.reply
    h
    (Review_comments
       { target = Rev "feat/b"; base = Some "main"; direct = true })
    review_json;
  H.reply
    h
    (Retry_in_editor
       { command = "/review feat/b --base main --direct"
       ; hint = ""
       ; then_ =
           Review_status
             { review =
                 P.Review.of_json (P.Json.parse review_json |> Or_error.ok_exn)
                 |> Or_error.ok_exn
             ; target = Rev "feat/b"
             }
       })
    {|{"root":"/src/repo","against":"0123456789abcdef0123456789abcdef01234567","rev":"feat/b","direct":true,
       "base":"main","main":false,"branches":["feat/b","main"],"truncated":false,
       "diff":"diff --git a/b.ml b/b.ml\n--- a/b.ml\n+++ b/b.ml\n@@ -1 +1 @@\n-p\n+q\n"}|};
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ list_worktrees)
      (params ())
      (tag (
        Review_named
        (command "/review feat/b --base main --direct")
        (name    feat/b)
        (base (main))
        (direct true)
        (model ()))))
    (Rpc
      (method_ get_review)
      (params ())
      (tag (Review_comments (target (Rev feat/b)) (base (main)) (direct true))))
    (Rpc
      (method_ git_diff)
      (params (
        (base   main)
        (rev    feat/b)
        (direct true)))
      (tag (
        Retry_in_editor
        (command "/review feat/b --base main --direct")
        (hint    "")
        (then_ (
          Review_status
          (review (
            (comments ((
              (id   c3)
              (root branch:feat/b)
              (path b.ml)
              (range (
                (first 1)
                (last  1)
                (side  New)))
              (anchor q)
              (status Open)
              (thread ((
                (author User)
                (text   "On the branch")
                (at ((1970-01-01 10:00:00.003000000+10:00)))
                (proposal ())
                (delivered false)))))))
            (approvals ())))
          (target (Rev feat/b)))))))

      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Review ─────────────────────────────────────────────────────────────────────┐
    │ review feat/b (not checked out) against main directly  0 of 1 file approved  │
    │   ○ b.ml  +1 −1  not approved                                                │
    │     c3 line 1, not sent yet                                                  │
    │       you: On the branch                                                     │
    │ approve files and comment on code you select in prigh-web's review page;     │
    │ /review HEAD has prigh review the changes itself                             │
    └───────────────────────────────────────────────────────────────── Esc closes ─┘
    ──────────────────────────────────────────────────────── reading the review… ───
    > ▏
    deepseek-flash  think:off  ↑↓ scroll · Esc closes · typing goes to the editor
    |}]
;;

let%expect_test "Review_command: parsing, resolving and completing /review" =
  List.iter
    [ ""
    ; "feat/a"
    ; "~/wt/x"
    ; "./sub --base origin/main"
    ; "--base main"
    ; "feat/a --base main"
    ; "main focus on errors"
    ; "--base"
    ; "a b --base main"
    ; "--direct"
    ; "feat/b --base main --direct"
    ; "origin/feat/b --direct --base"
    ]
    ~f:(fun args ->
      print_s
        [%message args ~_:(Review_command.parse args : Review_command.Action.t)]);
  let listing =
    P.Worktree.Listing.of_json (P.Json.parse listing_json |> Or_error.ok_exn)
    |> Or_error.ok_exn
  in
  List.iter
    [ "feat/a", None; "feat-a", None; "nope", Some "nope" ]
    ~f:(fun (name, model) ->
      print_s
        [%sexp
          (Review_command.resolve listing ~name ~base:None ~direct:false ~model
           : Review_command.Action.t)]);
  print_s
    [%sexp
      (Review_command.resolve
         listing
         ~name:"origin/feat/b"
         ~base:(Some "main")
         ~direct:true
         ~model:None
       : Review_command.Action.t)];
  print_endline
    (Review_command.command ~target:(Dir (Some "/w/x")) ~base:"main" ());
  print_endline
    (Review_command.command
       ~target:(Rev "origin/feat/b")
       ~base:"v1"
       ~direct:true
       ());
  print_endline (Review_command.command ());
  List.iter
    [ ""
    ; "fe"
    ; "feat/a "
    ; "--base "
    ; "feat/a --base m"
    ; "feat/a --base main --direct "
    ]
    ~f:(fun rest ->
      print_s
        [%message
          rest
            ~_:
              (Review_command.complete
                 ~worktrees:(List.take listing.worktrees 2)
                 ~refs:
                   { branches = [ "feat/a"; "feat/b"; "main" ]
                   ; remote_branches = [ "origin/main"; "origin/feat/b" ]
                   ; tags = [ "v1" ]
                   }
                 rest
               : (int * (string * string) list) option)]);
  [%expect
    {|
    ("" (Show (target (Dir ())) (base ()) (direct false)))
    (feat/a (Named (name feat/a) (base ()) (direct false) (model (feat/a))))
    (~/wt/x (Show (target (Dir (~/wt/x))) (base ()) (direct false)))
    ("./sub --base origin/main" (
      Show (target (Dir (./sub))) (base (origin/main)) (direct false)))
    ("--base main" (Show (target (Dir ())) (base (main)) (direct false)))
    ("feat/a --base main" (
      Named (name feat/a) (base (main)) (direct false) (model ())))
    ("main focus on errors" (Model "main focus on errors"))
    (--base (Error "--base needs a revision: /review --base main"))
    ("a b --base main" (
      Error
      "one worktree or branch at a time: /review [WORKTREE|BRANCH] [--base REV] [--direct], or /review [BASE] [instructions]"))
    (--direct (Show (target (Dir ())) (base ()) (direct true)))
    ("feat/b --base main --direct" (
      Named (name feat/b) (base (main)) (direct true) (model ())))
    ("origin/feat/b --direct --base" (
      Error "--base needs a revision: /review --base main"))
    (Show
      (target (Dir (/h/.prigh/worktrees/repo/feat-a)))
      (base ())
      (direct false))
    (Show
      (target (Dir (/h/.prigh/worktrees/repo/feat-a)))
      (base ())
      (direct false))
    (Model nope)
    (Show (target (Rev origin/feat/b)) (base (main)) (direct true))
    /review /w/x --base main
    /review origin/feat/b --base v1 --direct
    /review
    ("" ((
      0 (
        (--base "compare with another branch, tag or commit")
        (--direct "compare with the base itself, not the merge base")
        (main
         "main \194\183 /src/repo \194\183 main worktree \194\183 \226\134\1451 \226\134\1470")
        (feat/a
         "feat/a \194\183 /h/.prigh/worktrees/repo/feat-a \194\183 3 changed \194\183 2 sessions")
        (feat/b "not checked out: review it read-only (needs --base)")
        (origin/main "not checked out: review it read-only (needs --base)")
        (origin/feat/b "not checked out: review it read-only (needs --base)")))))
    (fe ((
      0 (
        (--base "compare with another branch, tag or commit")
        (--direct "compare with the base itself, not the merge base")
        (main
         "main \194\183 /src/repo \194\183 main worktree \194\183 \226\134\1451 \226\134\1470")
        (feat/a
         "feat/a \194\183 /h/.prigh/worktrees/repo/feat-a \194\183 3 changed \194\183 2 sessions")
        (feat/b "not checked out: review it read-only (needs --base)")
        (origin/main "not checked out: review it read-only (needs --base)")
        (origin/feat/b "not checked out: review it read-only (needs --base)")))))
    ("feat/a " ((
      7 (
        (--base "compare with another branch, tag or commit")
        (--direct "compare with the base itself, not the merge base")))))
    ("--base " ((
      7 (
        (HEAD   "the uncommitted changes only")
        (feat/a "compare with feat/a")
        (feat/b "compare with feat/b")
        (main   "compare with main")
        (origin/main "compare with the remote branch origin/main")
        (origin/feat/b "compare with the remote branch origin/feat/b")
        (v1 "compare with the tag v1")))))
    ("feat/a --base m" ((
      14 (
        (HEAD   "the uncommitted changes only")
        (feat/a "compare with feat/a")
        (feat/b "compare with feat/b")
        (main   "compare with main")
        (origin/main "compare with the remote branch origin/main")
        (origin/feat/b "compare with the remote branch origin/feat/b")
        (v1 "compare with the tag v1")))))
    ("feat/a --base main --direct " ())
    |}]
;;

(* /worktree back: where the session switched into worktrees from. *)

let home_json =
  {|{"cwd":"/src/repo","host":{"id":"backend","name":"srv"},"repo":{"root":"/src/repo","worktree":"/src/repo","branch":"main","main":true}}|}
;;

let with_home () =
  let h = Test_app.connected ~width:80 ~height:12 () in
  H.event
    h
    (State (state ~cwd ~git_branch:"fix/b" ~extra:[ "home", home_json ] ()));
  h
;;

let%expect_test
    "/worktree back goes home; without a home it says how to get one"
  =
  let h = connected ~height:12 () in
  run h "/worktree back";
  editor h;
  H.show h;
  let h = with_home () in
  run h "/worktree back";
  H.reply
    h
    (Retry_in_editor
       { command = "/worktree back"
       ; hint = ""
       ; then_ =
           Worktree_went_back (Option.bind h.H.model.state ~f:(fun s -> s.home))
       })
    {|{"cwd":"/src/repo","created":false}|};
  H.show h;
  [%expect
    {|
    editor: /worktree back

      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ this session hasn't switched into a worktree; /worktree open TARGET switches │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ▸ back  back [main]: back where the session switched into worktrees from
    ────────────────────────────────────────────────────────────────────────────────
    > /worktree back▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  Tab/Enter accept · Alt+S as typed
    (Rpc
      (method_ worktree_back)
      (params ())
      (tag (
        Retry_in_editor
        (command "/worktree back")
        (hint    "")
        (then_ (
          Worktree_went_back ((
            (cwd /src/repo)
            (host      (backend))
            (host_name (srv))
            (repo ((
              (root     /src/repo)
              (worktree /src/repo)
              (branch (main))
              (main true)))))))))))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ─────────────────────────────────────────────────── back in main (/src/repo) ───
    > ▏
    deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/worktree back main; a failure puts the command back" =
  let h = with_home () in
  run h "/worktree back main";
  let tag : App.Reply_tag.t =
    Retry_in_editor
      { command = "/worktree back main"
      ; hint = ""
      ; then_ =
          Worktree_went_back (Option.bind h.H.model.state ~f:(fun s -> s.home))
      }
  in
  H.reply_error h tag "the tool host srv that /src/repo is on is not connected";
  editor h;
  H.show h;
  let h = with_home () in
  run h "/worktree back home";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ worktree_back)
      (params ((main true)))
      (tag (
        Retry_in_editor
        (command "/worktree back main")
        (hint    "")
        (then_ (
          Worktree_went_back ((
            (cwd /src/repo)
            (host      (backend))
            (host_name (srv))
            (repo ((
              (root     /src/repo)
              (worktree /src/repo)
              (branch (main))
              (main true)))))))))))
    editor: /worktree back main





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────── the tool host srv that /src/repo is on is not connected ───
    > /worktree back main▏
    deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01

      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ usage: /worktree back [main] (main: the repository's main checkout, when the │
    │ place it came from is gone)                                                  │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > /worktree back home▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  Tab/Enter accept · Alt+S as typed
    |}]
;;

let%expect_test "/worktree completes back, then main" =
  let h = with_home () in
  H.keys h "/worktree b";
  H.show h;
  H.keys h "ack ";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ list_worktrees)
      (params ())
      (tag (
        Worktrees_for_autocomplete
        "abc123 backend:/h/.prigh/worktrees/repo/fix-b")))




      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ back  back [main]: back where the session switched into worktrees from
    ────────────────────────────────────────────────────────────────────────────────
    > /worktree b▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  Tab/Enter accept · Alt+S as typed




      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ main  the repository's main checkout instead
    ────────────────────────────────────────────────────────────────────────────────
    > /worktree back ▏
    deepseek-flash  ctx:0.1%/1.0M  $0.01  Tab accepts · Enter as typed · Esc hides
    |}]
;;
