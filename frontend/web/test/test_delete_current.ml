open! Core
module H = Harness

(* Deleting the session you are in (s1): you move to the next one down the
   sidebar first, which the backend does in the same request. *)

let list sessions = sprintf "[%s]" (String.concat ~sep:"," sessions)
let current = H.session_json ~name:"Main work" ~live:true ~clients:1 "s1"

let sidebar h =
  H.text h ~selector:".sidebar .sessions";
  H.text h ~selector:".sidebar .selection-bar"
;;

let confirmation h = H.text h ~selector:".modal"

let where h =
  print_s
    [%message
      ""
        ~in_:
          (Option.map (H.model h).state ~f:(fun s -> s.session_id)
           : string option)
        ~pending:
          ((H.model h).pending.session : Prigh_web.In_flight.Session.t option)]
;;

let%expect_test "the next one down: asked, moved, then deleted" =
  let h =
    H.create
      ~sessions:
        (list
           [ H.session_json ~name:"Before" "s0"
           ; current
           ; H.session_json ~name:"Below" "s2"
           ; H.session_json ~name:"Further" "s3"
           ])
      ()
  in
  H.act h (Ask_delete "/sessions/s1.jsonl");
  confirmation h;
  [%expect
    {|
    (Focus dialog)
    Delete “Main work”?
    (Close (Esc))
    You'll move to “Below”.
    It will be deleted, with its subagent logs: 0 B on disk. This can't be undone.
    Main work 0 B
    (Cancel) (Delete)
    |}];
  (* One request: it moves us and deletes; the chat says it is switching at
     once and the move's messages are asked for. *)
  H.act h Dialog_accept;
  where h;
  [%expect
    {|
    (Focus editor)
    (Rpc (method_ delete_sessions)
     (params ((paths (/sessions/s1.jsonl)) (switch_to /sessions/s2.jsonl)))
     (tag (Moved_and_deleted (Session /sessions/s2.jsonl))))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before ()))))
    (Focus editor)
    ((in_ (s1)) (pending ((Switching /sessions/s2.jsonl))))
    |}];
  H.reply
    h
    "delete_sessions"
    (sprintf
       {|{"state":%s,"deleted":[{"path":"/sessions/s1.jsonl","id":"s1","bytes":0}],"refused":[]}|}
       (H.state_json ~fields:[ "session_id", `String "s2" ] ()));
  where h;
  sidebar h;
  H.text h ~selector:".toast";
  [%expect
    {|
    (Set_url_session s2)
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Rpc (method_ get_review) (params ()) (tag (Review Review)))
    (Expire_toast (id 0) (after_ms 2500))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s2\
                            \nbackend\
                            \n/work") (purpose (Sidebar 2)))))
    ((in_ (s2)) (pending ()))
    (Before 2m ago /work) (More (right-click, Shift+F10)) (Archive) (Delete)
    (Below 2m ago /work) (More (right-click, Shift+F10)) (Archive) (Delete)
    (Further 2m ago /work) (More (right-click, Shift+F10)) (Archive) (Delete)
    Deleted 1 session (0 B)
    |}]
;;

let%expect_test "the last one: the one above; the only one: a new session" =
  let h =
    H.create ~sessions:(list [ H.session_json ~name:"Above" "s0"; current ]) ()
  in
  H.key h "Delete" ~target:(Session "s1");
  H.text h ~selector:".modal .delete-moving";
  [%expect
    {|
    (Ask_delete /sessions/s1.jsonl)
    (Focus dialog)
    You'll move to “Above”.
    |}];
  let h = H.create ~sessions:(list [ current ]) () in
  H.act h (Ask_delete "/sessions/s1.jsonl");
  H.text h ~selector:".modal .delete-moving";
  H.act h Dialog_accept;
  where h;
  [%expect
    {|
    (Focus dialog)
    You'll move to a new session.
    (Focus editor)
    (Rpc (method_ delete_sessions)
     (params ((paths (/sessions/s1.jsonl)) (switch_to_new true)))
     (tag (Moved_and_deleted New_session)))
    (Focus editor)
    ((in_ (s1)) (pending (Starting)))
    |}];
  H.reply
    h
    "delete_sessions"
    (sprintf
       {|{"state":%s,"deleted":[{"path":"/sessions/s1.jsonl","id":"s1","bytes":0}],"refused":[]}|}
       (H.state_json
          ~fields:[ "session_id", `String "s9"; "message_count", `Number "0" ]
          ()));
  where h;
  sidebar h;
  [%expect
    {|
    (Set_url_session s9)
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s9.jsonl) (limit 60)))
     (tag (Page (path /sessions/s9.jsonl) (before ()))))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Rpc (method_ get_review) (params ()) (tag (Review Review)))
    (Expire_toast (id 0) (after_ms 2500))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s9\
                            \nbackend\
                            \n/work") (purpose (Sidebar 2)))))
    ((in_ (s9)) (pending ()))
    No sessions yet: send a message to start one.
    |}]
;;

let%expect_test
    "several marked, the current among them: the first below not deleted"
  =
  let h =
    H.create
      ~sessions:
        (list
           [ current
           ; H.session_json ~name:"Also gone" "s2"
           ; H.session_json ~name:"Kept" ~running:true "s3"
           ; H.session_json ~name:"Last" "s4"
           ])
      ()
  in
  H.quiet h (fun () ->
    H.act h (Select_sessions true);
    H.act h (Mark_session { path = "/sessions/s1.jsonl"; range = false });
    H.act h (Mark_session { path = "/sessions/s3.jsonl"; range = true }));
  H.act h Delete_marked;
  confirmation h;
  [%expect
    {|
    (Focus dialog)
    Delete 2 sessions?
    (Close (Esc))
    “Main work” is the session you are in. You'll move to “Kept”.
    They will be deleted, with their subagent logs: 0 B on disk. This can't be undone.
    Main work 0 B
    Also gone 0 B
    Kept 0 B
    “Kept” is running: it will be kept (wait for it or stop it first)
    (Cancel) (Delete 2)
    |}];
  H.act h Dialog_accept;
  [%expect
    {|
    (Focus editor)
    (Rpc (method_ delete_sessions)
     (params
      ((paths (/sessions/s1.jsonl /sessions/s2.jsonl))
       (switch_to /sessions/s3.jsonl)))
     (tag (Moved_and_deleted (Session /sessions/s3.jsonl))))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s3.jsonl) (limit 60)))
     (tag (Page (path /sessions/s3.jsonl) (before ()))))
    (Focus editor)
    |}];
  (* Marking s1 to s3 then deleting all but the running one: past the
     ones going, s4 when s3 goes too. *)
  let h =
    H.create
      ~sessions:
        (list
           [ current
           ; H.session_json ~name:"Also gone" "s2"
           ; H.session_json ~name:"Last" "s4"
           ])
      ()
  in
  H.quiet h (fun () ->
    H.act h (Select_sessions true);
    H.act h (Mark_session { path = "/sessions/s1.jsonl"; range = false });
    H.act h (Mark_session { path = "/sessions/s4.jsonl"; range = true });
    H.act h Delete_marked);
  H.text h ~selector:".modal .delete-moving";
  [%expect
    {| “Main work” is the session you are in. You'll move to a new session. |}]
;;

let%expect_test "a session that runs is kept, and so is where you are" =
  let h =
    H.create
      ~sessions:
        (list
           [ H.session_json
               ~name:"Main work"
               ~live:true
               ~clients:1
               ~running:true
               "s1"
           ; H.session_json ~name:"Below" "s2"
           ])
      ()
  in
  H.act h (Ask_delete "/sessions/s1.jsonl");
  confirmation h;
  H.act h Dialog_accept;
  where h;
  [%expect
    {|
    (Focus dialog)
    Can't delete this session
    (Close (Esc))
    Main work 0 B
    “Main work” is running: it will be kept (wait for it or stop it first)
    (Close)
    (Focus editor)
    ((in_ (s1)) (pending ()))
    |}];
  (* Open in another tab too: kept. *)
  let h =
    H.create
      ~sessions:
        (list
           [ H.session_json ~name:"Main work" ~live:true ~clients:2 "s1"
           ; H.session_json ~name:"Below" "s2"
           ])
      ()
  in
  H.act h (Ask_delete "/sessions/s1.jsonl");
  H.text h ~selector:".modal .delete-warnings";
  [%expect
    {|
    (Focus dialog)
    “Main work” is open in another tab or terminal: it will be kept (close it there first)
    |}];
  (* It started running meanwhile: the backend keeps it, and us in it. *)
  let h = H.create ~sessions:(list [ current; H.session_json "s2" ]) () in
  H.quiet h (fun () ->
    H.act h (Ask_delete "/sessions/s1.jsonl");
    H.act h Dialog_accept);
  H.reply
    h
    "delete_sessions"
    {|{"deleted":[],"refused":[{"path":"/sessions/s1.jsonl","error":"session \"Main work\" is running: wait for it or /abort it first"}]}|};
  where h;
  H.text h ~selector:".toast";
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    ((in_ (s1)) (pending ()))
    Not deleted: session "Main work" is running: wait for it or /abort it first (Dismiss)
    |}];
  (* An error: nothing happened. *)
  let h = H.create ~sessions:(list [ current; H.session_json "s2" ]) () in
  H.quiet h (fun () ->
    H.act h (Ask_delete "/sessions/s1.jsonl");
    H.act h Dialog_accept);
  H.fail h "delete_sessions" "no session \"/sessions/s2.jsonl\"";
  where h;
  H.text h ~selector:".toast";
  [%expect
    {|
    ((in_ (s1)) (pending ()))
    Couldn't delete: no session "/sessions/s2.jsonl" (Dismiss)
    |}]
;;
