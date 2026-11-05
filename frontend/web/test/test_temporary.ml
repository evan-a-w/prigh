open! Core
module H = Harness

let temporary_state ?(id = "t1") ?(fields = []) () =
  H.state_json
    ~fields:
      ([ "session_id", `String id
       ; "session_path", `String (sprintf "/sessions/%s.jsonl" id)
       ; "temporary", `True
       ]
       @ fields)
    ()
;;

(* The new-session reply: the state, and the key to come back with. *)
let started ?id ?fields () =
  sprintf
    {|{"state":%s,"temporary_key":"k-%s"}|}
    (temporary_state ?id ?fields ())
    (Option.value id ~default:"t1")
;;

(* A temporary session with a turn in it, as the page shows it once its
   transcript has loaded. *)
let inside ?(fields = []) () =
  let h =
    H.create
      ~state:
        (temporary_state ~fields:([ "message_count", `Number "2" ] @ fields) ())
      ()
  in
  H.quiet h (fun () ->
    H.event
      h
      {|{"event":"message_start","message":{"role":"user","text":"What does git rebase --onto do?"}}|});
  h
;;

let%expect_test
    "the new-session screen: its heading, and the tray under the composer"
  =
  let h = H.create () in
  H.elements h ~selector:".main";
  H.text h ~selector:".chat";
  H.names h ~selector:".tray button";
  [%expect
    {|
    <main class="fresh main">
    What are we building?
    button "Folder: /work. Change it" -- Start in another folder (/cd)
    button "Branch: main. Change it" -- Work in another worktree (/worktree)
    [switch] button "Temporary" -- Make it temporary: not saved or listed (Ctrl+Shift+O)
    |}];
  (* The folder and the branch are their pickers. *)
  H.click h ~selector:".tray-where";
  [%expect
    {|
    (Run /cd)
    (Focus dialog-input)
    (Rpc (method_ list_dirs) (params ((prefix /work) (host backend)))
     (tag (Prompt_paths /work)))
    |}];
  H.quiet h (fun () -> H.act h Close_dialog);
  H.click ~nth:1 h ~selector:".tray-where";
  [%expect
    {|
    (Run /worktree)
    (Focus dialog)
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nbackend\
                            \n/work") (purpose Picker))))
    |}]
;;

let%expect_test
    "the Temporary chip makes the empty session a temporary one, and back"
  =
  let h = H.create () in
  H.click h ~selector:".temporary-chip";
  (* On at once, while the session is made. *)
  H.elements h ~selector:".temporary-chip";
  [%expect
    {|
    (Set_temporary true)
    (Rpc (method_ new_session) (params ((temporary true))) (tag Reload_state))
    (Focus editor)
    <button class="btn on temporary-chip">
    |}];
  H.quiet h (fun () -> H.reply h "new_session" (started ()));
  [%expect {| |}];
  H.text h ~selector:".chat";
  H.elements h ~selector:".main";
  [%expect
    {|
    Temporary session
    Not saved and not listed. Deleted when you leave.
    <main class="fresh main temporary">
    |}];
  (* Its key is kept for this page only. *)
  let h' = H.create () in
  H.quiet h' (fun () -> H.act h' (Set_temporary true));
  H.reply h' "new_session" (started ());
  [%expect
    {|
    (Set_url_session t1)
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/t1.jsonl) (limit 60)))
     (tag (Page (path /sessions/t1.jsonl) (before ()))))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Rpc (method_ get_review) (params ()) (tag (Review Review)))
    (Remember_temporary ((t1 k-t1)))
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "t1\
                            \nbackend\
                            \n/work") (purpose (Sidebar 2)))))
    |}];
  (* Off: a saved session again (the empty temporary one goes quietly). *)
  H.click h ~selector:".temporary-chip";
  [%expect
    {|
    (Set_temporary false)
    (Rpc (method_ new_session) (params ((temporary false))) (tag Reload_state))
    (Focus editor)
    |}];
  H.event
    h
    {|{"type":"event","event":"temporary_ended","session":"t1","messages":0,"kept":[]}|};
  H.text h ~selector:".toasts";
  [%expect {| (Remember_temporary ()) |}]
;;

let%expect_test "Ctrl+Shift+O and /temporary" =
  let h = H.create () in
  (* On the new-session screen, the chip. *)
  H.key h ~ctrl:true ~shift:true ~code:"KeyO" "O";
  [%expect
    {|
    New_temporary
    (Rpc (method_ new_session) (params ((temporary true))) (tag Reload_state))
    (Focus editor)
    |}];
  H.quiet h (fun () -> H.reply h "new_session" (started ()));
  H.key h ~ctrl:true ~shift:true ~code:"KeyO" "O";
  [%expect
    {|
    New_temporary
    (Rpc (method_ new_session) (params ((temporary false))) (tag Reload_state))
    (Focus editor)
    |}];
  (* In a session with something in it: a new temporary one. *)
  let h = inside () in
  H.key h ~ctrl:true ~shift:true ~code:"KeyO" "O";
  [%expect
    {|
    New_temporary
    (Rpc (method_ new_session) (params ((temporary true))) (tag Reload_state))
    (Focus editor)
    |}];
  (* /temporary in a saved session starts one... *)
  let h =
    H.create
      ~state:(H.state_json ~fields:[ "message_count", `Number "3" ] ())
      ()
  in
  H.act h (Run "/temporary");
  [%expect
    {|
    (Rpc (method_ new_session) (params ((temporary true))) (tag Reload_state))
    (Focus editor)
    |}];
  (* ...and in a temporary one, opens its menu. *)
  let h = inside () in
  H.act h (Run "/temporary");
  H.text h ~selector:".modal";
  [%expect
    {|
    (Focus dialog)
    Temporary session
    (Close (Esc))
    This session isn't saved. It's deleted, with its subagents, when you leave it.
    (Keep this session) (Delete it now)
    |}]
;;

let%expect_test "a temporary session's prompts stay out of the history" =
  let h = H.create () in
  H.quiet h (fun () ->
    H.act h (Set_temporary true);
    H.reply h "new_session" (started ()));
  H.type_ h "a secret question";
  H.act h Send;
  [%expect
    {|
    Scroll_to_bottom
    (Rpc (method_ prompt) (params ((text "a secret question")))
     (tag (Sent (text "a secret question") (images ()))))
    |}];
  print_s [%sexp (Prigh_web.History.to_list (H.model h).history : string list)];
  (* The composer goes down to its place. *)
  H.elements h ~selector:".main";
  [%expect
    {|
    ()
    <main class="landing main temporary">
    |}];
  (* A saved session's go in. *)
  let h = H.create () in
  H.type_ h "a question";
  H.act h Send;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "a question") (max 500))) (tag Ignore))
    Scroll_to_bottom
    (Rpc (method_ prompt) (params ((text "a question")))
     (tag (Sent (text "a question") (images ()))))
    |}]
;;

let%expect_test "in a temporary session: the pill and its menu" =
  let h = inside () in
  H.elements h ~selector:".main";
  H.names h ~selector:".chrome-center button";
  [%expect
    {|
    <main class="main temporary">
    button "Temporary" -- Not saved: deleted when you leave (/temporary)
    |}];
  H.click h ~selector:".temporary-pill";
  H.names h ~selector:".modal";
  [%expect
    {|
    Temporary_menu
    (Focus dialog)
    [dialog] div "Temporary session"
    |}];
  (* Esc closes it, with nothing done. *)
  H.key h "Escape";
  H.elements h ~selector:".modal";
  [%expect
    {|
    Close_dialog
    (Focus editor)
    |}];
  H.quiet h (fun () -> H.act h Temporary_menu);
  H.click h ~selector:".temporary-keep";
  H.text h ~selector:".toasts";
  [%expect
    {|
    Keep_session
    (Rpc (method_ keep_session) (params ()) (tag Reload_state))
    (Expire_toast (id 0) (after_ms 3460))
    (Focus editor)
    Kept: this session is saved, and in your sessions
    |}];
  H.reply
    h
    "keep_session"
    (sprintf
       {|{"state":%s}|}
       (H.state_json
          ~fields:[ "session_id", `String "t1"; "message_count", `Number "2" ]
          ()));
  H.elements h ~selector:".main";
  H.elements h ~selector:".temporary-pill";
  [%expect {| <main class="main"> |}];
  (* Delete it now: off to a new session, at once. *)
  let h = inside ~fields:[ "running", `True ] () in
  H.quiet h (fun () -> H.act h Temporary_menu);
  H.click h ~selector:".temporary-delete";
  [%expect
    {|
    Delete_temporary
    (Rpc (method_ new_session) (params ((temporary false))) (tag Reload_state))
    (Focus editor)
    |}];
  H.event
    h
    {|{"type":"event","event":"temporary_ended","session":"t1","messages":2,"kept":[{"path":"/work/.worktrees/x","kind":"worktree","why":"/work/.worktrees/x has uncommitted changes (1 file)"}]}|};
  H.text h ~selector:".toasts";
  [%expect
    {|
    (Remember_temporary ())
    (Expire_toast (id 0) (after_ms 2500))
    (Expire_toast (id 1) (after_ms 4060))
    Temporary session deleted
    Kept the worktree /work/.worktrees/x: it has uncommitted changes
    |}]
;;

let%expect_test "on a phone, the pill is the top strip's title" =
  let h = inside () in
  H.quiet h (fun () -> H.act h (Set_narrow true));
  H.names h ~selector:".chrome-center button";
  [%expect
    {| button "Temporary session" -- Not saved: deleted when you leave (/temporary) |}]
;;

let%expect_test "leaving: at once when idle, asked first while it works" =
  let h = inside () in
  H.act h (Switch_session "/sessions/s2.jsonl");
  [%expect
    {|
    (Rpc (method_ switch_session) (params ((path /sessions/s2.jsonl)))
     (tag Reload_state))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before ()))))
    (Focus editor)
    |}];
  let working =
    [ "running", `True
    ; ( "subagents"
      , Jsonaf.of_string {|[{"id":"a1","task":"look","running":true}]|} )
    ; ( "jobs"
      , Jsonaf.of_string
          {|[{"id":"j1","command":"make","running":true},{"id":"j2","command":"ls","running":false}]|}
      )
    ]
  in
  let h = inside ~fields:working () in
  H.act h (Switch_session "/sessions/s2.jsonl");
  H.text h ~selector:".modal";
  [%expect
    {|
    (Focus dialog)
    Delete this temporary session?
    (Close (Esc))
    It's still working (its turn, a subagent, a job). Leaving deletes the transcript and stops them.
    (Keep it instead) (Stay) (Delete and leave)
    |}];
  (* Stay: nothing happens. *)
  H.click h ~selector:".leave-temporary .modal-buttons .btn" ~nth:1;
  H.elements h ~selector:".modal";
  [%expect
    {|
    Close_dialog
    (Focus editor)
    |}];
  (* The others lead where the user was going. *)
  H.quiet h (fun () -> H.act h New_session);
  H.click h ~selector:".leave-keep";
  [%expect
    {|
    Leave_keep
    (Rpc (method_ keep_session) (params ()) (tag Ignore))
    (Expire_toast (id 0) (after_ms 3460))
    (Rpc (method_ new_session) (params ()) (tag Reload_state))
    (Focus editor)
    |}];
  let h = inside ~fields:working () in
  H.quiet h (fun () -> H.act h (Run "/new"));
  (* Enter accepts. *)
  H.act h Dialog_accept;
  [%expect {| (Rpc (method_ new_session) (params ()) (tag Reload_state)) |}];
  (* Staying drops the command typed that would have left. *)
  let h = inside ~fields:working () in
  H.quiet h (fun () ->
    H.type_ h "/new";
    H.act h Send);
  H.quiet h (fun () -> H.act h Close_dialog);
  printf
    "dialog: %b, draft: %S\n"
    (Option.is_some (H.model h).dialog)
    (H.model h).draft;
  [%expect {| dialog: false, draft: "" |}]
;;
