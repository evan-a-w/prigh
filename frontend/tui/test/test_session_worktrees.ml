open! Core
open! Expect_test_helpers_core
open Prigh_ui
module P = Prigh_protocol

(* Where a session works and the worktrees that matter to it, as both
   frontends' session lists show them. *)

let session ?repo ?(worktrees = []) id cwd =
  let repo =
    match repo with
    | None -> ""
    | Some (root, worktree, branch) ->
      sprintf
        {|,"repo":{"root":"%s","worktree":"%s","branch":%s,"main":%b}|}
        root
        worktree
        (Option.value_map branch ~default:"null" ~f:(sprintf "%S"))
        (String.equal root worktree)
  in
  P.Json.parse
    (sprintf
       {|{"id":"%s","path":"/s/%s.jsonl","name":"%s","cwd":"%s","created_at":"x","message_count":1,"worktrees":[%s]%s}|}
       id
       id
       id
       cwd
       (String.concat ~sep:"," worktrees)
       repo)
  |> Or_error.ok_exn
  |> P.Session_summary.of_json
  |> Or_error.ok_exn
;;

let worktree ?branch ~root ~source path =
  sprintf
    {|{"path":"%s","branch":%s,"root":"%s",%s}|}
    path
    (Option.value_map branch ~default:"null" ~f:(sprintf "%S"))
    root
    source
;;

let listing root paths =
  P.Json.parse
    (sprintf
       {|{"root":"%s","project_dir":"/wt/p","worktrees":[%s]}|}
       root
       (String.concat
          ~sep:","
          (List.map paths ~f:(fun (path, branch, changes) ->
             sprintf
               {|{"path":"%s","branch":"%s","head":"abc","main":false,"managed":true,"locked":false,"prunable":false,"changes":%d,"upstream":null,"ahead":0,"behind":0,"current":false,"sessions":[]}|}
               path
               branch
               changes))))
  |> Or_error.ok_exn
  |> P.Worktree.Listing.of_json
  |> Or_error.ok_exn
;;

let s1 =
  session
    "s1"
    "/wt/p/feat-x/src"
    ~repo:("/src/p", "/wt/p/feat-x", Some "feat/x")
    ~worktrees:
      [ worktree
          "/wt/p/feat-x"
          ~branch:"feat/x"
          ~root:"/src/p"
          ~source:{|"source":"entered"|}
      ; worktree
          "/wt/p/agent-a"
          ~branch:"agent/a"
          ~root:"/src/p"
          ~source:{|"source":"agent","agent":"1","task":"tidy"|}
      ; worktree
          "/wt/p/pr-3"
          ~branch:"pr-3"
          ~root:"/src/p"
          ~source:{|"source":"pull_request","pr":3|}
      ; worktree "/wt/q/detached" ~root:"/src/q" ~source:{|"source":"created"|}
      ]
;;

let print nodes =
  List.iter nodes ~f:(fun (n : Session_worktrees.Node.t) ->
    printf
      "⎇ %s%s%s%s\n"
      (Session_worktrees.Node.label n)
      (if Session_worktrees.Node.agent n then " (subagent)" else "")
      (if n.here then " here" else "")
      (Option.value_map n.listed ~default:"" ~f:(fun l ->
         sprintf " listed, %d changed" (Option.value l.changes ~default:0))))
;;

let%expect_test "a session's place: its repository and branch" =
  List.iter
    [ s1
    ; session "s2" "/src/p" ~repo:("/src/p.git", "/src/p", None)
    ; session "s3" "/tmp"
    ]
    ~f:(fun s ->
      printf
        "%s: %s\n"
        s.id
        (Option.value (Session_worktrees.place s) ~default:"(none)"));
  [%expect
    {|
    s1: p ⎇ feat/x
    s2: p
    s3: (none)
    |}]
;;

let%expect_test "its worktrees: without listings, as recorded" =
  print (Session_worktrees.of_session s1);
  print_endline (Session_worktrees.summary (Session_worktrees.of_session s1));
  print_s
    [%sexp (Session_worktrees.roots [ s1; session "s3" "/tmp" ] : string list)];
  [%expect
    {|
    ⎇ feat/x here
    ⎇ agent/a (subagent)
    ⎇ pr-3
    ⎇ detached
    ⎇ feat/x · ⎇ agent/a (subagent) · ⎇ pr-3 · ⎇ detached
    (/src/p /src/q)
    |}]
;;

let%expect_test
    "with its repository's listing: what is in them, the branch now, removed \
     ones gone; another repository's untouched"
  =
  let listings =
    [ listing
        "/src/p"
        [ "/wt/p/feat-x", "feat/x-renamed", 2; "/wt/p/agent-a", "agent/a", 0 ]
    ]
  in
  print (Session_worktrees.of_session ~listings s1);
  (* Where the session is now, when known better than its summary. *)
  print (Session_worktrees.of_session ~listings ~cwd:"/src/p" s1);
  [%expect
    {|
    ⎇ feat/x-renamed here listed, 2 changed
    ⎇ agent/a (subagent) listed, 0 changed
    ⎇ detached
    ⎇ feat/x-renamed listed, 2 changed
    ⎇ agent/a (subagent) listed, 0 changed
    ⎇ detached
    |}]
;;

let%expect_test "where a worktree switch took it from: in /sessions' details" =
  let with_home home =
    P.Json.parse
      (sprintf
         {|{"id":"s4","path":"/s/s4.jsonl","name":"s4","cwd":"/wt/p/feat-x","created_at":"x","message_count":1,"home":%s}|}
         home)
    |> Or_error.ok_exn
    |> P.Session_summary.of_json
    |> Or_error.ok_exn
  in
  let sessions =
    [ with_home
        {|{"cwd":"/src/p","host":{"id":"backend","name":"srv"},"repo":{"root":"/src/p","worktree":"/src/p","branch":"main","main":true}}|}
    ; with_home {|{"cwd":"/home/u/notes","host":null,"repo":null}|}
    ; with_home "null"
    ]
  in
  List.iter sessions ~f:(fun s ->
    print_endline
      (Option.value
         (Session_worktrees.home
            ~tilde:(String.substr_replace_first ~pattern:"/home/u" ~with_:"~")
            s.home)
         ~default:"(none)"));
  List.iter
    (Picker.visible
       (Session_selection.picker
          (Session_selection.create sessions)
          ~current:None))
    ~f:(fun item -> print_endline item.detail);
  [%expect
    {|
    from main
    from ~/notes
    (none)
    /wt/p/feat-x · from main
    /wt/p/feat-x · from /home/u/notes
    /wt/p/feat-x
    |}]
;;
