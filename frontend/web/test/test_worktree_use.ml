open! Core
open Prigh_web
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
(* The sessions, then the worktrees each shows. *)
let sidebar h =
  H.text h ~selector:".sessions";
  Worktree_lines.print h
;;

let switch ?cwd h id =
  H.quiet h (fun () ->
    H.act h (Switch_session (sprintf "/sessions/%s.jsonl" id));
    H.reply h "switch_session" (sprintf {|{"state":%s}|} (state ?cwd id)))
;;

let%expect_test "a pinned session's worktrees show, however old" =
  let h = create () in
  sidebar h;
  [%expect
    {|
    Pinned 1
    (prigh 3d Pinned) (More (right-click, Shift+F10)) (Archive) (Delete)
    Recent 2
    (prigh 2m Here) (More (right-click, Shift+F10)) (Archive) (Delete)
    (prigh 3d Old) (More (right-click, Shift+F10)) (Archive) (Delete)
    (feat/here)
    (feat/pinned)
    |}]
;;

let%expect_test "opening a session is using it" =
  let h = create () in
  switch h "old";
  switch h "here";
  sidebar h;
  [%expect
    {|
    Pinned 1
    (prigh 3d Pinned) (More (right-click, Shift+F10)) (Archive) (Delete)
    Recent 2
    (prigh 2m Here) (More (right-click, Shift+F10)) (Archive) (Delete)
    (prigh 3d Old) (More (right-click, Shift+F10)) (Archive) (Delete)
    (feat/here)
    (feat/old)
    (feat/pinned)
    |}]
;;

let%expect_test "the session the user is in shows its worktrees" =
  let h = create ~current:"old" () in
  sidebar h;
  [%expect
    {|
    Pinned 1
    (prigh 3d Pinned) (More (right-click, Shift+F10)) (Archive) (Delete)
    Recent 2
    (prigh 2m Here) (More (right-click, Shift+F10)) (Archive) (Delete)
    (prigh 3d Old) (More (right-click, Shift+F10)) (Archive) (Delete)
    (feat/here)
    (feat/old)
    (feat/pinned)
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
  [%expect
    {|
    Pinned 1
    (prigh 3d Pinned) (More (right-click, Shift+F10)) (Archive) (Delete)
    Recent 4
    (prigh 2m Here) (More (right-click, Shift+F10)) (Archive) (Delete)
    (prigh 3d Old) (More (right-click, Shift+F10)) (Archive) (Delete)
    (prigh 3d Owner) (More (right-click, Shift+F10)) (Archive) (Delete)
    (src 30m Parser fix) (More (right-click, Shift+F10)) (Archive) (Delete)
    (feat/here)
    (feat/pinned)
    (feat/owner)
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
  [%expect
    {|
    Pinned 1
    (prigh 3d Pinned) (More (right-click, Shift+F10)) (Archive) (Delete)
    Recent 3
    (prigh 13h Here) (More (right-click, Shift+F10)) (Archive) (Delete)
    (prigh 3d Old) (More (right-click, Shift+F10)) (Archive) (Delete)
    (prigh working… Long run) (More (right-click, Shift+F10)) (Archive) (Delete)
    (feat/here)
    (feat/pinned)
    (feat/busy)
    |}]
;;

let%expect_test "a fold from before (the old preference) does not stick" =
  let h = create () in
  H.event
    h
    {|{"event":"prefs_changed","prefs":{"web.sidebar-folded":["here"]}}|};
  sidebar h;
  [%expect
    {|
    (Cache_prefs (((key web.sidebar-folded) (value ((here))))))
    Pinned 1
    (prigh 3d Pinned) (More (right-click, Shift+F10)) (Archive) (Delete)
    Recent 2
    (prigh 2m Here) (More (right-click, Shift+F10)) (Archive) (Delete)
    (prigh 3d Old) (More (right-click, Shift+F10)) (Archive) (Delete)
    (feat/here)
    (feat/pinned)
    |}]
;;

(* Each session, [+] its worktrees shown or [-] folded. *)
let folds h =
  let m = H.model h in
  let shown = App.Model.worktrees_shown m in
  List.iter m.sessions ~f:(fun s ->
    printf
      "%s %s\n"
      (if Set.mem shown s.id then "+" else "-")
      (Option.value s.name ~default:s.id))
;;

let at h time =
  H.quiet h (fun () -> H.act h (Tick (Time_ns.of_string_with_utc_offset time)))
;;

let toggle h id = H.quiet h (fun () -> H.act h (Toggle_folded id))

let opened h =
  print_endline
    (Option.value_map
       (Worktree_folds.opened_pref (H.model h).worktree_folds)
       ~default:"none"
       ~f:Jsonaf.to_string)
;;

let%expect_test "a hand fold holds until the session is next opened" =
  let h = create () in
  at h "2026-10-05 10:01:00Z";
  toggle h "here";
  at h "2026-10-05 12:00:00Z";
  folds h;
  [%expect
    {|
    - Here
    - Old
    + Pinned
    |}];
  (* Going elsewhere keeps it. *)
  switch h "old";
  folds h;
  [%expect
    {|
    - Here
    + Old
    + Pinned
    |}];
  (* Going back in: the fold has had its day. *)
  switch h "here";
  folds h;
  [%expect
    {|
    + Here
    + Old
    + Pinned
    |}]
;;

let%expect_test "a fold made in the session holds, there and after" =
  let h = create () in
  (* Opened at 10:00 (the page's first tick), folded a minute later. *)
  at h "2026-10-05 10:01:00Z";
  toggle h "here";
  at h "2026-10-05 10:02:00Z";
  folds h;
  at h "2026-10-06 10:00:00Z";
  folds h;
  [%expect
    {|
    - Here
    - Old
    + Pinned
    - Here
    - Old
    + Pinned
    |}]
;;

let%expect_test
    "an old session unfolded by hand folds again after its next visit"
  =
  let h = create () in
  at h "2026-10-05 10:01:00Z";
  toggle h "old";
  at h "2026-10-07 10:00:00Z";
  folds h;
  [%expect
    {|
    + Here
    + Old
    + Pinned
    |}];
  switch h "old";
  switch h "here";
  (* Used by the visit for 12 hours, then folded: the unfold is spent. *)
  at h "2026-10-07 21:00:00Z";
  folds h;
  at h "2026-10-07 23:00:00Z";
  folds h;
  [%expect
    {|
    + Here
    + Old
    + Pinned
    + Here
    - Old
    + Pinned
    |}]
;;

let%expect_test "going into a session in its worktree clears the owner's fold" =
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
  at h "2026-10-05 10:01:00Z";
  toggle h "owner";
  at h "2026-10-05 10:02:00Z";
  folds h;
  switch h ~cwd:(trees ^ "/owner/src") "worker";
  folds h;
  [%expect
    {|
    + Here
    - Old
    + Pinned
    - Owner
    + Parser fix
    + Here
    - Old
    + Pinned
    + Owner
    + Parser fix
    |}]
;;

let%expect_test "visits in another browser count; the later of each wins" =
  let h = create () in
  at h "2026-10-05 10:01:00Z";
  toggle h "pinned";
  folds h;
  [%expect
    {|
    + Here
    - Old
    - Pinned
    |}];
  (* There: Old an hour ago, Pinned just now, Here long ago (this browser's
     is later). *)
  H.event
    h
    {|{"event":"prefs_changed","prefs":{"web.sidebar-opened":{"old":"2026-10-05 09:00:00Z","pinned":"2026-10-05 10:05:00Z","here":"2026-10-01 00:00:00Z"}}}|};
  at h "2026-10-05 10:06:00Z";
  folds h;
  opened h;
  [%expect
    {|
    (Cache_prefs
     (((key web.sidebar-opened)
       (value
        (((old "2026-10-05 09:00:00Z") (pinned "2026-10-05 10:05:00Z")
          (here "2026-10-01 00:00:00Z")))))))
    + Here
    + Old
    + Pinned
    {"here":"2026-10-05 10:00:00Z","old":"2026-10-05 09:00:00Z","pinned":"2026-10-05 10:05:00Z"}
    |}]
;;

let%expect_test "visits before the preferences came are uploaded, if newer" =
  let start ~cached ~backend =
    let h = H.connecting () in
    H.quiet h (fun () ->
      H.reply h "get_state" (state "here");
      H.reply h "list_sessions" (list base);
      H.act
        h
        (Restore_prefs
           { prefs =
               [ { key = Pref_sync.Key.opened
                 ; value = Some (Jsonaf.of_string cached)
                 }
               ]
           ; legacy = false
           });
      H.act h (Tick H.now));
    (* Of what loading them does, only the upload. *)
    let m, commands =
      App.update
        (H.model h)
        (Reply
           ( Prefs
           , Ok
               (Jsonaf.of_string
                  (sprintf
                     {|{"prefs":{"web.migrated":true,"web.sidebar-opened":%s}}|}
                     backend)) ))
    in
    List.iter commands ~f:(function
      | Rpc { method_ = "set_prefs"; params; _ } ->
        List.iter params ~f:(fun (key, value) ->
          printf "set_prefs %s %s\n" key (Jsonaf.to_string value))
      | _ -> ());
    print_endline
      (Option.value_map
         (Worktree_folds.opened_pref m.worktree_folds)
         ~default:"none"
         ~f:Jsonaf.to_string)
  in
  (* This browser went into Here, and saw Old earlier than the backend
     knows: Here's goes up. *)
  start
    ~cached:{|{"old":"2026-10-05 08:00:00Z"}|}
    ~backend:{|{"old":"2026-10-05 09:00:00Z"}|};
  [%expect
    {|
    set_prefs prefs {"web.sidebar-opened":{"here":"2026-10-05 10:00:00Z","old":"2026-10-05 09:00:00Z"}}
    {"here":"2026-10-05 10:00:00Z","old":"2026-10-05 09:00:00Z"}
    |}];
  (* The backend knows of later visits to both: nothing to send. *)
  start
    ~cached:{|{"old":"2026-10-05 08:00:00Z"}|}
    ~backend:{|{"old":"2026-10-05 09:00:00Z","here":"2026-10-05 10:00:00Z"}|};
  [%expect {| {"here":"2026-10-05 10:00:00Z","old":"2026-10-05 09:00:00Z"} |}];
  (* A visit cached from an earlier page, later than the backend's, goes up
     too. *)
  start
    ~cached:{|{"old":"2026-10-05 09:30:00Z"}|}
    ~backend:{|{"old":"2026-10-05 09:00:00Z","here":"2026-10-05 10:00:00Z"}|};
  [%expect
    {|
    set_prefs prefs {"web.sidebar-opened":{"here":"2026-10-05 10:00:00Z","old":"2026-10-05 09:30:00Z"}}
    {"here":"2026-10-05 10:00:00Z","old":"2026-10-05 09:30:00Z"}
    |}]
;;

let%expect_test "0 hours folds them all, pinned and the open session too" =
  let h = create () in
  H.quiet h (fun () ->
    H.event
      h
      {|{"event":"prefs_changed","prefs":{"web.sidebar-worktree-hours":0}}|});
  folds h;
  [%expect
    {|
    - Here
    - Old
    - Pinned
    |}]
;;
