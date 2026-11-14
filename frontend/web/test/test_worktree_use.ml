open! Core
module H = Harness

(* When the sidebar shows a session's worktrees: when the session was used
   in the last hours (12 by default), "used" being its last change, the
   user going into it (here or in another browser), or any session working
   in one of its worktrees; and always for a pinned session, a running one
   and the one the user is in. A fold or unfold by hand holds until the
   session is next opened. *)

let prigh = "/home/u/src/prigh"
let trees = "/home/u/.prigh/worktrees/prigh"
let days_ago = "2026-10-02 08:00:00Z"

let state ?(cwd = prigh) id =
  H.state_json
    ~fields:
      [ "session_id", `String id
      ; "session_path", `String (sprintf "/sessions/%s.jsonl" id)
      ; "cwd", `String cwd
      ]
    ()
;;

(* [Here] is the session the page is in; [Old], [Pinned] and [Owner] are
   three days old, each with a worktree. *)
let session ?(updated_at = days_ago) ?pinned ?running ?(cwd = prigh) ~name id =
  H.session_json
    ~name
    ~cwd
    ~repo:(prigh, cwd, Some "main")
    ~updated_at
    ?pinned
    ?running
    ~worktrees:[ trees ^ "/" ^ id, Some ("feat/" ^ id), prigh, "created" ]
    id
;;

let list sessions = sprintf "[%s]" (String.concat ~sep:"," sessions)

let base =
  [ session ~name:"Here" ~updated_at:"2026-10-05 09:58:00Z" "here"
  ; session ~name:"Old" "old"
  ; session ~name:"Pinned" ~pinned:true "pinned"
  ]
;;

let create ?(sessions = base) ?(current = "here") () =
  H.create ~state:(state current) ~sessions:(list sessions) ~verbose:false ()
;;

(* What the user sees: each row, and the worktrees under it when they
   show. *)
let sidebar h = H.text h ~selector:".sessions"

let switch h id =
  H.quiet h (fun () ->
    H.act h (Switch_session (sprintf "/sessions/%s.jsonl" id));
    H.reply h "switch_session" (sprintf {|{"state":%s}|} (state id)))
;;

let%expect_test "a pinned session's worktrees show, however old" =
  let h = create () in
  sidebar h;
  [%expect {|
    Pinned 1
    (Pinned 3d ago prigh main) (1) (More (right-click, Shift+F10)) (Archive) (Delete)
    Recent 2
    (Here 2m ago prigh main) (1) (More (right-click, Shift+F10)) (Archive) (Delete)
    (feat/here)
    (Old 3d ago prigh main) (1) (More (right-click, Shift+F10)) (Archive) (Delete)
    |}]
;;

let%expect_test "opening a session is using it" =
  let h = create () in
  switch h "old";
  switch h "here";
  sidebar h;
  [%expect {|
    Pinned 1
    (Pinned 3d ago prigh main) (1) (More (right-click, Shift+F10)) (Archive) (Delete)
    Recent 2
    (Here 2m ago prigh main) (1) (More (right-click, Shift+F10)) (Archive) (Delete)
    (feat/here)
    (Old 3d ago prigh main) (1) (More (right-click, Shift+F10)) (Archive) (Delete)
    |}]
;;

let%expect_test "the session the user is in shows its worktrees" =
  let h = create ~current:"old" () in
  sidebar h;
  [%expect {|
    Pinned 1
    (Pinned 3d ago prigh main) (1) (More (right-click, Shift+F10)) (Archive) (Delete)
    Recent 2
    (Here 2m ago prigh main) (1) (More (right-click, Shift+F10)) (Archive) (Delete)
    (feat/here)
    (Old 3d ago prigh main) (1) (More (right-click, Shift+F10)) (Archive) (Delete)
    |}]
;;

let%expect_test "a session working in another's worktree uses it" =
  let h =
    create
      ~sessions:
        (base
         @ [ session ~name:"Owner" "owner"
           ; H.session_json
               ~name:"Parser fix"
               ~cwd:(trees ^ "/owner/src")
               ~updated_at:"2026-10-05 09:30:00Z"
               "worker"
           ])
      ()
  in
  sidebar h;
  [%expect {|
    Pinned 1
    (Pinned 3d ago prigh main) (1) (More (right-click, Shift+F10)) (Archive) (Delete)
    Recent 4
    (Here 2m ago prigh main) (1) (More (right-click, Shift+F10)) (Archive) (Delete)
    (feat/here)
    (Old 3d ago prigh main) (1) (More (right-click, Shift+F10)) (Archive) (Delete)
    (Owner 3d ago prigh main) (1) (More (right-click, Shift+F10)) (Archive) (Delete)
    (Parser fix 30m ago ~/.prigh/worktrees/prigh/owner/src) (More (right-click, Shift+F10)) (Archive) (Delete)
    |}]
;;

let%expect_test "a running session stays used while the page is open" =
  let h =
    create
      ~sessions:
        (base
         @ [ session
               ~name:"Long run"
               ~running:true
               ~updated_at:"2026-10-05 09:30:00Z"
               "busy"
           ])
      ()
  in
  H.quiet h (fun () ->
    H.act h (Tick (Time_ns.add H.now (Time_ns.Span.of_hr 13.))));
  sidebar h;
  [%expect {|
    Pinned 1
    (Pinned 3d ago prigh main) (1) (More (right-click, Shift+F10)) (Archive) (Delete)
    Recent 3
    (Here 13h ago prigh main) (1) (More (right-click, Shift+F10)) (Archive) (Delete)
    (Old 3d ago prigh main) (1) (More (right-click, Shift+F10)) (Archive) (Delete)
    (Long run 13h ago prigh main) (1) (More (right-click, Shift+F10)) (Archive) (Delete)
    |}]
;;

let%expect_test "a fold from before (the old preference) does not stick" =
  let h = create () in
  H.event h {|{"event":"prefs_changed","prefs":{"web.sidebar-folded":["here"]}}|};
  sidebar h;
  [%expect {|
    (Cache_prefs (((key web.sidebar-folded) (value ((here))))))
    Pinned 1
    (Pinned 3d ago prigh main) (1) (More (right-click, Shift+F10)) (Archive) (Delete)
    Recent 2
    (Here 2m ago prigh main) (1) (More (right-click, Shift+F10)) (Archive) (Delete)
    (Old 3d ago prigh main) (1) (More (right-click, Shift+F10)) (Archive) (Delete)
    |}]
;;

