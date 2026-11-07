open! Core
module H = Harness

(* Pinned sessions: a group of their own at the top of the sidebar, marked
   with a pin, never archived for being idle; pinning from the menu, P and
   /pin, at once and back if the backend refuses. *)

let sessions =
  sprintf
    "[%s]"
    (String.concat
       ~sep:","
       [ H.session_json ~first_prompt:"fix the parser bug" ~live:true "s1"
       ; H.session_json ~name:"Release notes" ~bytes:12_000 "s2"
       ; H.session_json ~name:"Long-running notes" ~pinned:true "s3"
       ; H.session_json ~name:"Old lexer" ~archived:true "s4"
       ])
;;

let sidebar h = H.text h ~selector:".sidebar .sessions"
let titles h = H.text h ~selector:".sidebar .session-title"

let menu h path =
  H.quiet h (fun () ->
    H.act h (Open_menu { target = Session path; anchor = Element "x" }));
  H.text h ~selector:".context-menu [data-item=pin]";
  H.text h ~selector:".context-menu [data-item=unpin]";
  H.quiet h (fun () -> H.act h Close_dialog)
;;

let%expect_test "pinned sessions first, under Pinned and Recent headings" =
  let h = H.create ~sessions () in
  sidebar h;
  H.show h ~selector:".session-pin";
  H.elements h ~selector:".sessions-group";
  [%expect
    {|
    Pinned 1
    (Long-running notes 2m ago /work) (More (right-click, Shift+F10)) (Archive) (Delete)
    Recent 2
    (fix the parser bug 2m ago /work) (More (right-click, Shift+F10)) (Archive) (Delete)
    (Release notes 2m ago /work) (More (right-click, Shift+F10)) (Archive) (Delete)
    (Archived 1)
    <span role="img" aria-label="Pinned" title="Pinned to the top (P unpins)" class="session-pin">
      <icon class="pin"> </icon>
    </span>
    <div class="sessions-divider sessions-group">
    <div class="sessions-divider sessions-group">
    |}];
  (* Nothing pinned: no headings. *)
  H.act h (Set_pinned { path = "/sessions/s3.jsonl"; pinned = false });
  sidebar h;
  [%expect
    {|
    (Rpc (method_ set_pinned) (params ((path /sessions/s3.jsonl) (pinned false)))
     (tag (Pinned_set (title "Long-running notes") (pinned false))))
    (fix the parser bug 2m ago /work) (More (right-click, Shift+F10)) (Archive) (Delete)
    (Release notes 2m ago /work) (More (right-click, Shift+F10)) (Archive) (Delete)
    (Long-running notes 2m ago /work) (More (right-click, Shift+F10)) (Archive) (Delete)
    (Archived 1)
    |}];
  (* Only pinned sessions: no Recent heading. *)
  let h =
    H.create
      ~sessions:
        (sprintf "[%s]" (H.session_json ~name:"Only one" ~pinned:true "s1"))
      ()
  in
  sidebar h;
  [%expect
    {|
    Pinned 1
    (Only one 2m ago /work) (More (right-click, Shift+F10)) (Archive) (Delete)
    |}]
;;

let%expect_test "the keyboard walks pinned, then recent, then archived" =
  let h = H.create ~sessions () in
  H.act h Toggle_archived;
  H.key h "ArrowDown" ~target:(Session "s3");
  H.key h "ArrowDown" ~target:(Session "s1");
  H.key h "ArrowDown" ~target:(Session "s2");
  [%expect
    {|
    (Session_nav (from (s3)) (by 1))
    (Focus session-s1)
    (Session_nav (from (s1)) (by 1))
    (Focus session-s2)
    (Session_nav (from (s2)) (by 1))
    (Focus session-s4)
    |}]
;;

let%expect_test "a search keeps the groups, best match first in each" =
  let h = H.create ~sessions () in
  H.act h (Set_session_query "notes");
  sidebar h;
  [%expect
    {|
    (Search_sessions_later (search 1) (after_ms 150))
    Pinned 1
    (Long-running notes 2m ago /work) (More (right-click, Shift+F10)) (Archive) (Delete)
    Recent 1
    (Release notes 2m ago /work) (More (right-click, Shift+F10)) (Archive) (Delete)
    |}]
;;

let%expect_test
    "a search's title and content hits each show in their group, pinned ones \
     under Pinned with their pin, marks and passage"
  =
  let sessions =
    sprintf
      "[%s]"
      (String.concat
         ~sep:","
         [ H.session_json
             ~name:"Deploy the docs"
             ~updated_at:"2026-10-05 09:00:00Z"
             "s1"
         ; H.session_json
             ~name:"Weekly notes"
             ~pinned:true
             ~updated_at:"2026-10-05 08:00:00Z"
             "s2"
         ; H.session_json
             ~name:"Deploy checklist"
             ~pinned:true
             ~updated_at:"2026-10-04 08:00:00Z"
             "s3"
         ; H.session_json ~name:"Lexer" ~updated_at:"2026-10-03 08:00:00Z" "s4"
         ; H.session_json
             ~name:"Old deploy"
             ~archived:true
             ~updated_at:"2026-10-02 08:00:00Z"
             "s5"
         ])
  in
  let h = H.create ~sessions () in
  H.act h (Set_session_query "deploy");
  H.act h (Search_sessions 1);
  sidebar h;
  [%expect
    {|
    (Search_sessions_later (search 1) (after_ms 150))
    (Rpc (method_ search_sessions) (params ((query deploy) (limit 50)))
     (tag (Session_hits 1)))
    Pinned 1
    (Deploy checklist 1d ago /work) (More (right-click, Shift+F10)) (Archive) (Delete)
    Recent 1
    (Deploy the docs 1h ago /work) (More (right-click, Shift+F10)) (Archive) (Delete)
    (Archived 1)
    (Old deploy 3d ago /work) (More (right-click, Shift+F10)) (Unarchive) (Delete)
    (Delete archived)
    |}];
  (* The messages of a pinned and an unpinned session mention it too. *)
  H.reply
    h
    "search_sessions"
    {|{"query":"deploy","hits":[
      {"id":"s2","path":"/sessions/s2.jsonl","score":20,
       "snippet":{"text":"…before we deploy on Friday","ranges":[[13,6]]}},
      {"id":"s4","path":"/sessions/s4.jsonl","score":20,
       "snippet":{"text":"…deploy the lexer","ranges":[[3,6]]}},
      {"id":"s3","path":"/sessions/s3.jsonl","score":40,"snippet":null}]}|};
  sidebar h;
  [%expect
    {|
    Pinned 2
    (Deploy checklist 1d ago /work) (More (right-click, Shift+F10)) (Archive) (Delete)
    (Weekly notes …before we deploy on Friday 2h ago /work) (More (right-click, Shift+F10)) (Archive) (Delete)
    Recent 2
    (Deploy the docs 1h ago /work) (More (right-click, Shift+F10)) (Archive) (Delete)
    (Lexer … deploy the lexer 2d ago /work) (More (right-click, Shift+F10)) (Archive) (Delete)
    (Archived 1)
    (Old deploy 3d ago /work) (More (right-click, Shift+F10)) (Unarchive) (Delete)
    (Delete archived)
    |}];
  H.show h ~selector:"[data-session=s2] .session-main";
  H.show h ~selector:"[data-session=s3] .session-title";
  [%expect
    {|
    <div class="session-main">
      <span dir="auto" class="session-title">
        <span role="img" aria-label="Pinned" title="Pinned to the top (P unpins)" class="session-pin">
          <icon class="pin"> </icon>
        </span>
        Weekly notes
      </span>
      <span dir="auto" class="session-snippet">
        …before we
        <mark> deploy </mark>
         on Friday
      </span>
      <div class="session-meta">
        <span class="session-age"> 2h ago </span>
        <span title="/work" class="session-cwd"> /work </span>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      </div>
    </div>
    <span dir="auto" class="session-title">
      <span role="img" aria-label="Pinned" title="Pinned to the top (P unpins)" class="session-pin">
        <icon class="pin"> </icon>
      </span>
      <mark> Deploy </mark>
       checklist
    </span>
    |}]
;;

let%expect_test "pinning and unpinning: at once, and back if refused" =
  let h = H.create ~sessions () in
  menu h "/sessions/s2.jsonl";
  menu h "/sessions/s3.jsonl";
  menu h "/sessions/s4.jsonl";
  [%expect
    {|
    Pin to top P
    Unpin P
    Pin to top P
    |}];
  H.act h (Set_pinned { path = "/sessions/s2.jsonl"; pinned = true });
  titles h;
  H.reply h "set_pinned" "{}";
  H.text h ~selector:".toast";
  [%expect
    {|
    (Rpc (method_ set_pinned) (params ((path /sessions/s2.jsonl) (pinned true)))
     (tag (Pinned_set (title "Release notes") (pinned true))))
    Release notes
    Long-running notes
    fix the parser bug
    (Expire_toast (id 0) (after_ms 2980))
    Pinned “Release notes” to the top
    |}];
  (* Pinning an archived session unarchives it. *)
  H.act h (Set_pinned { path = "/sessions/s4.jsonl"; pinned = true });
  titles h;
  H.fail h "set_pinned" "no such session";
  H.text h ~selector:".toast.error";
  H.reply h "list_sessions" sessions;
  titles h;
  [%expect
    {|
    (Rpc (method_ set_pinned) (params ((path /sessions/s4.jsonl) (pinned true)))
     (tag (Pinned_set (title "Old lexer") (pinned true))))
    Release notes
    Long-running notes
    Old lexer
    fix the parser bug
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    Couldn't pin “Old lexer”: no such session (Dismiss)
    Long-running notes
    fix the parser bug
    Release notes
    |}];
  (* Archiving a pinned one unpins it. *)
  H.act h (Set_archived { path = "/sessions/s3.jsonl"; archived = true });
  H.act h Toggle_archived;
  sidebar h;
  [%expect
    {|
    (Rpc (method_ set_archived)
     (params ((path /sessions/s3.jsonl) (archived true)))
     (tag (Archived_set (title "Long-running notes") (archived true))))
    (fix the parser bug 2m ago /work) (More (right-click, Shift+F10)) (Archive) (Delete)
    (Release notes 2m ago /work) (More (right-click, Shift+F10)) (Archive) (Delete)
    (Archived 2)
    (Long-running notes 2m ago /work) (More (right-click, Shift+F10)) (Unarchive) (Delete)
    (Old lexer 2m ago /work) (More (right-click, Shift+F10)) (Unarchive) (Delete)
    (Delete archived)
    |}]
;;

let%expect_test "P on a session, /pin and /unpin" =
  let h = H.create ~sessions () in
  H.key h "p" ~target:(Session "s2");
  H.key h "P" ~target:(Session "s3");
  [%expect
    {|
    (Set_pinned (path /sessions/s2.jsonl) (pinned true))
    (Rpc (method_ set_pinned) (params ((path /sessions/s2.jsonl) (pinned true)))
     (tag (Pinned_set (title "Release notes") (pinned true))))
    (Set_pinned (path /sessions/s3.jsonl) (pinned false))
    (Rpc (method_ set_pinned) (params ((path /sessions/s3.jsonl) (pinned false)))
     (tag (Pinned_set (title "Long-running notes") (pinned false))))
    |}];
  let run command =
    H.type_ h command;
    H.act h Send
  in
  run "/pin";
  run "/unpin Release notes";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /pin) (max 500)))
     (tag Ignore))
    (Rpc (method_ set_pinned) (params ((path /sessions/s1.jsonl) (pinned true)))
     (tag (Pinned_set (title "fix the parser bug") (pinned true))))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/unpin Release notes") (max 500)))
     (tag Ignore))
    (Rpc (method_ set_pinned) (params ((path /sessions/s2.jsonl) (pinned false)))
     (tag (Pinned_set (title "Release notes") (pinned false))))
    |}]
;;

let%expect_test "a backend that cannot pin: no menu item, P says so" =
  let sessions =
    String.substr_replace_all sessions ~pattern:{|"pinned":false,|} ~with_:""
  in
  let h = H.create ~sessions () in
  menu h "/sessions/s2.jsonl";
  H.key h "p" ~target:(Session "s2");
  H.text h ~selector:".toast.error";
  [%expect
    {|
    (Set_pinned (path /sessions/s2.jsonl) (pinned true))
    This backend can't pin sessions: update it to pin. (Dismiss)
    |}]
;;

let%expect_test "the delete dialog says a session is pinned" =
  let h = H.create ~sessions () in
  H.act h (Ask_delete "/sessions/s3.jsonl");
  H.text h ~selector:".modal .delete-row";
  [%expect
    {|
    (Focus dialog)
    Long-running notes pinned 0 B
    |}]
;;
