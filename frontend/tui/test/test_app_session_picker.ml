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
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
      earlier answer
       deploy ∣ 2025-06-03T10:00 ∣ 6 msgs ∣ deploy it      /work
       (unnamed) ∣ 2025-06-02T10:00 ∣ 2 msgs ∣ scratch     /tmp
       release ∣ 2025-06-01T10:00 ∣ 3 msgs ∣ cut it        /work
      ── archived ──
    ▸* build fix ∣ 2025-06-06T10:00 ∣ 12 msgs ∣ fix the build  /work
       (unnamed) ∣ 2025-06-05T10:00 ∣ 4 msgs ∣ try a spike  /old
    >
    |}]
;;

let%expect_test
    "the picker: each session's repository and branch, its worktrees in the \
     detail; the filter finds them by repository and worktree"
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
              (repo "/src/prigh" "/src/prigh" "main"
               ^ {|,"worktrees":[{"path":"/wt/prigh/feat-login","branch":"feat/login","root":"/src/prigh","source":"created"},{"path":"/wt/prigh/agent-fix","branch":"agent/fix","root":"/src/prigh","source":"agent","agent":"1","task":"fix it"}]|}
              )
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
  for _ = 1 to 4 do
    H.key h (Key.plain Backspace)
  done;
  H.keys h "agent/fix";
  rows h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_picker))
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
      earlier answer
    ▸  login form ∣ 2025-06-09T10:00 ∣ 2 msgs ∣ prigh ⎇ feat/login ∣ go  /wt/prigh/feat-login
       main work ∣ 2025-06-08T10:00 ∣ 2 msgs ∣ prigh ⎇ main ∣ go  /src/prigh · ⎇ feat/login · ⎇ agent/f…
       landing ∣ 2025-06-07T10:00 ∣ 2 msgs ∣ site ⎇ trunk ∣ go  /src/site
       backend ∣ 2025-06-06T10:00 ∣ 2 msgs ∣ prigh ⎇ main ∣ go  /src/prigh/backend
       notes ∣ 2025-06-05T10:00 ∣ 2 msgs ∣ go              /home/u/notes
    >
    (Rpc
      (method_ search_sessions)
      (params (
        (query s)
        (limit 50)))
      (tag Session_hits))
    (Rpc
      (method_ search_sessions)
      (params (
        (query si)
        (limit 50)))
      (tag Session_hits))
    (Rpc
      (method_ search_sessions)
      (params (
        (query sit)
        (limit 50)))
      (tag Session_hits))
    (Rpc
      (method_ search_sessions)
      (params (
        (query site)
        (limit 50)))
      (tag Session_hits))
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
      earlier answer
    ▸  landing ∣ 2025-06-07T10:00 ∣ 2 msgs ∣ site ⎇ trunk ∣ go  /src/site
    >
    (Rpc
      (method_ search_sessions)
      (params (
        (query sit)
        (limit 50)))
      (tag Session_hits))
    (Rpc
      (method_ search_sessions)
      (params (
        (query si)
        (limit 50)))
      (tag Session_hits))
    (Rpc
      (method_ search_sessions)
      (params (
        (query s)
        (limit 50)))
      (tag Session_hits))
    (Rpc
      (method_ search_sessions)
      (params (
        (query a)
        (limit 50)))
      (tag Session_hits))
    (Rpc
      (method_ search_sessions)
      (params (
        (query ag)
        (limit 50)))
      (tag Session_hits))
    (Rpc
      (method_ search_sessions)
      (params (
        (query age)
        (limit 50)))
      (tag Session_hits))
    (Rpc
      (method_ search_sessions)
      (params (
        (query agen)
        (limit 50)))
      (tag Session_hits))
    (Rpc
      (method_ search_sessions)
      (params (
        (query agent)
        (limit 50)))
      (tag Session_hits))
    (Rpc
      (method_ search_sessions)
      (params (
        (query agent/f)
        (limit 50)))
      (tag Session_hits))
    (Rpc
      (method_ search_sessions)
      (params (
        (query agent/fi)
        (limit 50)))
      (tag Session_hits))
    (Rpc
      (method_ search_sessions)
      (params (
        (query agent/fix)
        (limit 50)))
      (tag Session_hits))
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
      earlier answer
    ▸  main work ∣ 2025-06-08T10:00 ∣ 2 msgs ∣ prigh ⎇ main ∣ go  /src/prigh · ⎇ feat/login · ⎇ agent/f…
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
  H.key h (Key.plain Up);
  H.key h (Key.plain Tab);
  H.key h (Key.ctrl 'n');
  H.key h (Key.ctrl 'n');
  rows h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_picker))
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
      earlier answer
       deploy ∣ 2025-06-03T10:00 ∣ 6 msgs ∣ deploy it      /work
     ✓ (unnamed) ∣ 2025-06-02T10:00 ∣ 2 msgs ∣ scratch     /tmp
    ▸  release ∣ 2025-06-01T10:00 ∣ 3 msgs ∣ cut it        /work
      ── archived ──
     * build fix ∣ 2025-06-06T10:00 ∣ 12 msgs ∣ fix the build  /work
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
       deploy ∣ 2025-06-03T10:00 ∣ 6 msgs ∣ deploy it      /work
    ▸  release ∣ 2025-06-01T10:00 ∣ 3 msgs ∣ cut it        /work
      ── archived ──
     * build fix ∣ 2025-06-06T10:00 ∣ 12 msgs ∣ fix the build  /work
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
      (params ((paths (/s/2.jsonl /s/4.jsonl))))
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
    ▸ undo         bring the latest deleted message back
      deploy       deploy it · /work
      scratch      /tmp
      release      cut it · /work
      build fix    archived · fix the build · /work
      try a spike  archived · /old
    > /delete
    |}]
;;

(* A session as [list_sessions] gives it, by id ([/s/<id>.jsonl]). *)
let session ?(archived = false) ?(running = false) ?(clients = 0) id name =
  sprintf
    {|{"id":"%s","path":"/s/%s.jsonl","name":"%s","cwd":"/work","created_at":"2025-06-01T10:00:00Z","updated_at":"2025-06-01T10:00:00Z","first_prompt":"go","message_count":2,"archived":%b,"running":%b,"live":%b,"clients":%d,"bytes":1000}|}
    id
    id
    name
    archived
    running
    (clients > 0)
    clients
;;

let list sessions = sprintf "[%s]" (String.concat ~sep:"," sessions)

(* In session "here" ([/s/here.jsonl]). *)
let in_here () =
  let h = Test_app.connected ~width:80 ~height:14 () in
  H.reply
    ~quiet:true
    h
    Initial_state
    (Fixtures.state_json ~session:("here", "/s/here.jsonl") ());
  h
;;

let delete h args sessions =
  H.key h (Key.ctrl 'u');
  H.keys h ("/delete " ^ args);
  H.enter h;
  H.reply
    ~quiet:true
    h
    (Sessions_to_delete (String.split args ~on:' '))
    sessions
;;

let%expect_test
    "/delete of the session you are in: you move to the next one down, else \
     the one above, else a new session"
  =
  let h = in_here () in
  delete
    h
    "here"
    (list
       [ session "up" "above"
       ; session ~clients:1 "here" "here"
       ; session "down" "below"
       ]);
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag (Sessions_to_delete (here))))


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    ┌─ Confirm ────────────────────────────────────────────────────────────────────┐
    │ Delete session "here" (1000 B)? You'll move to session "below". Enter        │
    │ deletes, Esc keeps it                                                        │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  confirm: y / n
    |}];
  H.esc h;
  delete
    h
    "here"
    (list [ session "up" "above"; session ~clients:1 "here" "here" ]);
  H.show h;
  H.esc h;
  delete h "here" (list [ session ~clients:1 "here" "here" ]);
  H.show h;
  H.enter h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag (Sessions_to_delete (here))))


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    ┌─ Confirm ────────────────────────────────────────────────────────────────────┐
    │ Delete session "here" (1000 B)? You'll move to session "above". Enter        │
    │ deletes, Esc keeps it                                                        │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  confirm: y / n
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag (Sessions_to_delete (here))))


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    ┌─ Confirm ────────────────────────────────────────────────────────────────────┐
    │ Delete session "here" (1000 B)? You'll move to a new session. Enter deletes, │
    │ Esc keeps it                                                                 │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  confirm: y / n
    (Rpc
      (method_ delete_sessions)
      (params ((paths (/s/here.jsonl)) (switch_to_new true)))
      (tag Sessions_deleted))
    |}]
;;

let%expect_test "the move, then the list without it" =
  let h = in_here () in
  delete
    h
    "here"
    (list
       [ session "up" "above"
       ; session ~clients:1 "here" "here"
       ; session "down" "below"
       ]);
  H.enter h;
  H.reply
    h
    Sessions_deleted
    (sprintf
       {|{"state":%s,"deleted":[{"path":"/s/here.jsonl","id":"here","bytes":1000}],"refused":[]}|}
       (Fixtures.state_json
          ~session:("down", "/s/down.jsonl")
          ~session_name:"below"
          ()));
  H.reply
    h
    Initial_state
    (Fixtures.state_json
       ~session:("down", "/s/down.jsonl")
       ~session_name:"below"
       ());
  H.reply h Initial_messages {|[{"role":"user","text":"below's question"}]|};
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag (Sessions_to_delete (here))))
    (Rpc
      (method_ delete_sessions)
      (params ((paths (/s/here.jsonl)) (switch_to /s/down.jsonl)))
      (tag Sessions_deleted))
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ get_pending) (params ()) (tag Pending))








      prigh in /work "below" · /help · Esc aborts · Ctrl+C twice quits

    ▌ below's question
    ───────────────────────────────────────────────── Deleted 1 session (1000 B) ───
    > ▏
    /work "below"  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Key.ctrl 'u');
  H.keys h "/sessions";
  H.enter h;
  H.reply
    h
    Sessions_picker
    (list [ session "up" "above"; session ~clients:1 "down" "below" ]);
  rows h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_picker))
      prigh in /work "below" · /help · Esc aborts · Ctrl+C twice quits
       above ∣ 2025-06-01T10:00 ∣ 2 msgs ∣ go    /work
    ▸* ● below ∣ 2025-06-01T10:00 ∣ 2 msgs ∣ go  /work
    >
    |}]
;;

let%expect_test
    "several marked in the picker, the current among them; kept while running \
     or open elsewhere"
  =
  let h = in_here () in
  let sessions ?(running = false) ?(clients = 1) () =
    list
      [ session ~running ~clients "here" "here"
      ; session "gone" "also gone"
      ; session "next" "the next one"
      ]
  in
  H.keys h "/sessions";
  H.enter h;
  H.reply ~quiet:true h Sessions_picker (sessions ());
  H.key h (Key.plain Tab);
  H.key h (Key.plain Tab);
  H.key h (Key.ctrl 'd');
  H.show h;
  H.enter h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_picker))


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    ┌─ Confirm ────────────────────────────────────────────────────────────────────┐
    │ Delete 2 sessions (2.0 KB)? You'll move to session "the next one". Enter     │
    │ deletes, Esc keeps them                                                      │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  confirm: y / n
    (Rpc
      (method_ delete_sessions)
      (params ((paths (/s/here.jsonl /s/gone.jsonl)) (switch_to /s/next.jsonl)))
      (tag Sessions_deleted))
    |}];
  H.esc h;
  delete h "here" (sessions ~running:true ());
  H.show h;
  H.esc h;
  delete h "here" (sessions ~clients:2 ());
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag (Sessions_to_delete (here))))


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ session "here" is the session you are in and is running: wait for it or      │
    │ /abort it first, or leave it out                                             │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > /delete here▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag (Sessions_to_delete (here))))


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ session "here" is the session you are in and is open in another frontend:    │
    │ close it there first, or leave it out                                        │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > /delete here▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/delete-archived with the current session archived" =
  let h = Test_app.connected ~width:80 ~height:14 () in
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
    │ Delete all 2 archived sessions (1.4 MB)? You'll move to session "release".   │
    │ Enter deletes, Esc keeps them                                                │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  confirm: y / n
    (Rpc
      (method_ delete_sessions)
      (params (
        (paths (/home/u/.prigh/sessions/1.jsonl /s/3a.jsonl))
        (switch_to /s/5.jsonl)))
      (tag Sessions_deleted))
    |}]
;;
