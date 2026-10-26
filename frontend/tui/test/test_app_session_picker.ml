open! Core
open! Expect_test_helpers_core
open Prigh_ui
module H = Test_app.H

(* An archived session listed first, as [list_sessions] may (by recency). The
   current one ([Fixtures.state_json]'s) is archived too. *)
let sessions_json =
  {|[
  {"id":"1","path":"/home/u/.prigh/sessions/1.jsonl","name":"build fix","cwd":"/work","created_at":"2025-06-05T10:00:00Z","updated_at":"2025-06-06T10:00:00Z","first_prompt":"fix the build","message_count":12,"archived":true,"bytes":2048},
  {"id":"3a","path":"/s/3a.jsonl","name":null,"cwd":"/old","created_at":"2025-05-01T10:00:00Z","updated_at":"2025-06-05T10:00:00Z","first_prompt":"try a spike","message_count":4,"archived":true,"bytes":1500000},
  {"id":"2","path":"/s/2.jsonl","name":"deploy","cwd":"/work","created_at":"2025-06-03T10:00:00Z","updated_at":"2025-06-03T10:00:00Z","first_prompt":"deploy it","message_count":6,"bytes":4096},
  {"id":"4","path":"/s/4.jsonl","name":null,"cwd":"/tmp","created_at":"2025-06-02T10:00:00Z","updated_at":"2025-06-02T10:00:00Z","first_prompt":"scratch","message_count":2,"bytes":100},
  {"id":"5","path":"/s/5.jsonl","name":"release","cwd":"/work","created_at":"2025-06-01T10:00:00Z","updated_at":"2025-06-01T10:00:00Z","first_prompt":"cut it","message_count":3,"bytes":300}
  ]|}
;;

let rows (h : H.t) =
  List.iter (Render.screen h.model).lines ~f:(fun line ->
    let plain = Content.Line.to_plain line in
    if
      String.is_prefix plain ~prefix:" "
      || String.is_prefix plain ~prefix:"▸"
      || String.is_prefix plain ~prefix:">"
    then print_endline (String.rstrip plain))
;;

let%expect_test "the picker lists active sessions first, whatever the order" =
  let h = Test_app.connected ~width:100 ~height:16 () in
  H.keys h "/sessions";
  H.enter h;
  H.reply h Sessions_picker sessions_json;
  rows h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_picker))
      earlier answer
      ── work ──
       deploy ∣ 2025-06-03T10:00 ∣ 6 msgs ∣ deploy it      /work
       release ∣ 2025-06-01T10:00 ∣ 3 msgs ∣ cut it        /work
      ── tmp ──
       (unnamed) ∣ 2025-06-02T10:00 ∣ 2 msgs ∣ scratch     /tmp
      ── archived ──
    ▸* build fix ∣ 2025-06-06T10:00 ∣ 12 msgs ∣ fix the build  /work
       (unnamed) ∣ 2025-06-05T10:00 ∣ 4 msgs ∣ try a spike  /old
    >
    |}]
;;

let%expect_test
    "the picker groups sessions by project and worktree; the filter finds them \
     by project"
  =
  let h = Test_app.connected ~width:100 ~height:20 () in
  H.keys h "/sessions";
  H.enter h;
  let repo root worktree branch =
    sprintf
      {|"repo":{"root":"%s","worktree":"%s","branch":"%s","main":%b}|}
      root
      worktree
      branch
      (String.equal root worktree)
  in
  let session id name cwd repo =
    sprintf
      {|{"id":"%s","path":"/s/%s.jsonl","name":"%s","cwd":"%s","created_at":"2025-06-01T10:00:00Z","updated_at":"2025-06-0%sT10:00:00Z","first_prompt":"go","message_count":2%s}|}
      id
      id
      name
      cwd
      id
      (if String.is_empty repo then "" else "," ^ repo)
  in
  H.reply
    h
    Sessions_picker
    (sprintf
       "[%s]"
       (String.concat
          ~sep:","
          [ session
              "9"
              "login form"
              "/wt/prigh/feat-login"
              (repo "/src/prigh" "/wt/prigh/feat-login" "feat/login")
          ; session
              "8"
              "main work"
              "/src/prigh"
              (repo "/src/prigh" "/src/prigh" "main")
          ; session
              "7"
              "landing"
              "/src/site"
              (repo "/src/site" "/src/site" "trunk")
          ; session
              "6"
              "backend"
              "/src/prigh/backend"
              (repo "/src/prigh" "/src/prigh" "main")
          ; session "5" "notes" "/home/u/notes" ""
          ]));
  rows h;
  H.keys h "site";
  rows h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_picker))
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
      earlier answer
      ── prigh ──
    ▸  main work ∣ 2025-06-08T10:00 ∣ 2 msgs ∣ go   /src/prigh
       backend ∣ 2025-06-06T10:00 ∣ 2 msgs ∣ go     /src/prigh/backend
      ── prigh ⎇ feat/login ──
       login form ∣ 2025-06-09T10:00 ∣ 2 msgs ∣ go  /wt/prigh/feat-login
      ── site ──
       landing ∣ 2025-06-07T10:00 ∣ 2 msgs ∣ go     /src/site
      ── notes ──
       notes ∣ 2025-06-05T10:00 ∣ 2 msgs ∣ go       /home/u/notes
    >
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
      earlier answer
      ── site ──
    ▸  landing ∣ 2025-06-07T10:00 ∣ 2 msgs ∣ go  /src/site
    >
    |}]
;;

let%expect_test
    "Ctrl+N and back keeps the marks on hidden sessions and moves a hidden \
     highlight to its neighbour"
  =
  let h = Test_app.connected ~width:100 ~height:16 () in
  H.keys h "/sessions";
  H.enter h;
  H.reply ~quiet:true h Sessions_picker sessions_json;
  (* Mark the unnamed scratch session; the filter hides it. *)
  H.key h (Key.plain Up);
  H.key h (Key.plain Tab);
  H.key h (Key.ctrl 'n');
  H.key h (Key.ctrl 'n');
  rows h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_picker))
      earlier answer
      ── work ──
       deploy ∣ 2025-06-03T10:00 ∣ 6 msgs ∣ deploy it      /work
       release ∣ 2025-06-01T10:00 ∣ 3 msgs ∣ cut it        /work
      ── tmp ──
     ✓ (unnamed) ∣ 2025-06-02T10:00 ∣ 2 msgs ∣ scratch     /tmp
      ── archived ──
    ▸* build fix ∣ 2025-06-06T10:00 ∣ 12 msgs ∣ fix the build  /work
       (unnamed) ∣ 2025-06-05T10:00 ∣ 4 msgs ∣ try a spike  /old
    >
    |}];
  (* A highlighted session the filter hides gives way to the next one shown. *)
  H.key h (Key.plain Up);
  H.key h (Key.ctrl 'n');
  rows h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
      earlier answer
      ── work ──
       deploy ∣ 2025-06-03T10:00 ∣ 6 msgs ∣ deploy it      /work
       release ∣ 2025-06-01T10:00 ∣ 3 msgs ∣ cut it        /work
      ── archived ──
    ▸* build fix ∣ 2025-06-06T10:00 ∣ 12 msgs ∣ fix the build  /work
    >
    |}];
  (* The hidden mark still counts, and is deleted with the others. *)
  H.key h (Key.plain Up);
  H.key h (Key.plain Tab);
  H.key h (Key.ctrl 'd');
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ delete_sessions)
      (params ((paths (/s/4.jsonl /s/5.jsonl))))
      (tag Sessions_deleted))
    |}]
;;

let%expect_test
    "session completions: by name, active first, archived marked; accepting \
     inserts the id; each command offers what it can act on"
  =
  let h = Test_app.connected ~width:80 ~height:14 () in
  H.keys h "/switch ";
  H.reply h Sessions_cache sessions_json;
  rows h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
      earlier answer
    ▸ deploy       deploy it · /work
      scratch      /tmp
      release      cut it · /work
      build fix    archived · fix the build · /work
      try a spike  archived · /old
    > /switch
    |}];
  (* By name, and the id goes in, which every session command accepts. *)
  H.keys h "rel";
  rows h;
  H.key h (Key.plain Tab);
  rows h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
      earlier answer
    ▸ release      cut it · /work
      try a spike  archived · /old
    > /switch rel
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
      earlier answer
    > /switch 5
    |}];
  H.key h (Key.ctrl 'u');
  H.keys h "/archive ";
  rows h;
  H.key h (Key.ctrl 'u');
  H.keys h "/unarchive ";
  rows h;
  (* Not the session we are in. *)
  H.key h (Key.ctrl 'u');
  H.keys h "/delete ";
  rows h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
      earlier answer
    ▸ deploy   deploy it · /work
      scratch  /tmp
      release  cut it · /work
    > /archive
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
      earlier answer
    ▸ build fix    archived · fix the build · /work
      try a spike  archived · /old
    > /unarchive
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
      earlier answer
    ▸ deploy       deploy it · /work
      scratch      /tmp
      release      cut it · /work
      try a spike  archived · /old
    > /delete
    |}]
;;

let%expect_test
    "/delete refuses the current session up front; /delete-archived leaves it \
     out"
  =
  let h = Test_app.connected ~width:80 ~height:14 () in
  H.keys h "/delete 1 2";
  H.enter h;
  H.reply h (Sessions_to_delete [ "1"; "2" ]) sessions_json;
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag (Sessions_to_delete (1 2))))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ session "build fix" is the session you are in: switch to another one first,  │
    │ or leave it out                                                              │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > /delete 1 2▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Key.ctrl 'u');
  H.keys h "/delete-archived";
  H.enter h;
  H.reply h Archived_to_delete sessions_json;
  H.show h;
  H.enter h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Archived_to_delete))




      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────────────────────────────────────────────┐
    │ Delete the archived session 3a (1.4 MB)? Enter deletes, Esc keeps it         │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  confirm: y / n
    (Rpc
      (method_ delete_sessions)
      (params ((paths (/s/3a.jsonl))))
      (tag Sessions_deleted))
    |}]
;;
