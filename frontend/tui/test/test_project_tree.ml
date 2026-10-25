open! Core
open! Expect_test_helpers_core
open Prigh_ui
module P = Prigh_protocol

(* Sessions grouped into projects and worktrees, as both frontends show
   them. *)

let session ?repo ?(agent_worktrees = "") ?(archived = false) id cwd =
  let repo =
    match repo with
    | None -> ""
    | Some (root, worktree, branch) ->
      sprintf
        {|,"repo":{"root":"%s","worktree":"%s","branch":"%s","main":%b}|}
        root
        worktree
        branch
        (String.equal root worktree)
  in
  P.Json.parse
    (sprintf
       {|{"id":"%s","path":"/s/%s.jsonl","name":"%s","cwd":"%s","created_at":"x","message_count":1,"archived":%b,"agent_worktrees":[%s]%s}|}
       id
       id
       id
       cwd
       archived
       agent_worktrees
       repo)
  |> Or_error.ok_exn
  |> P.Session_summary.of_json
  |> Or_error.ok_exn
;;

let agent_worktree ~agent ~path ~branch ~root =
  sprintf
    {|{"agent":"%s","task":"t","path":"%s","branch":"%s","root":"%s"}|}
    agent
    path
    branch
    root
;;

let print (tree : Project_tree.t) =
  List.iter tree ~f:(fun p ->
    printf
      "%s%s%s (%s)\n"
      (Project_tree.Project.name p)
      (if p.git then " [git]" else "")
      (Option.value_map p.branch ~default:"" ~f:(fun b -> " " ^ b))
      p.root;
    List.iter p.sessions ~f:(fun s -> printf "  %s\n" s.id);
    List.iter p.worktrees ~f:(fun w ->
      printf
        "  ⎇ %s (%s)%s%s\n"
        (Project_tree.Worktree_node.label w)
        w.path
        (match w.agent with
         | Some (a, owner) -> sprintf " subagent %s of %s" a.agent owner.id
         | None -> "")
        (Option.value_map w.listed ~default:"" ~f:(fun l ->
           sprintf " listed, %d changed" (Option.value l.changes ~default:0)));
      List.iter w.sessions ~f:(fun s -> printf "    %s\n" s.id)))
;;

let sessions =
  [ session "s1" "/wt/x" ~repo:("/src/p", "/wt/x", "feat/x")
  ; session
      "s2"
      "/src/p"
      ~repo:("/src/p", "/src/p", "main")
      ~agent_worktrees:
        (agent_worktree
           ~agent:"a1"
           ~path:"/wt/agent-y"
           ~branch:"agent/y"
           ~root:"/src/p")
  ; session "s3" "/notes"
  ; session "s4" "/src/q/sub" ~repo:("/src/q", "/src/q", "trunk")
  ; session "s5" "/src/p/backend" ~repo:("/src/p", "/src/p", "main")
  ; session "s6" "/wt/x/lib" ~repo:("/src/p", "/wt/x", "feat/x")
  ]
;;

let%expect_test "projects in the order of first mention, worktrees under them" =
  print (Project_tree.build sessions);
  [%expect
    {|
    p [git] main (/src/p)
      s2
      s5
      ⎇ feat/x (/wt/x)
        s1
        s6
      ⎇ agent/y (/wt/agent-y) subagent a1 of s2
    notes (/notes)
      s3
    q [git] trunk (/src/q)
      s4
    |}];
  let tree = Project_tree.build sessions in
  List.iter sessions ~f:(fun s ->
    printf "%s: %s\n" s.id (Project_tree.place_label tree s));
  [%expect
    {|
    s1: p ⎇ feat/x
    s2: p
    s3: notes
    s4: q
    s5: p
    s6: p ⎇ feat/x
    |}];
  let order collapsed =
    Project_tree.sessions ~collapsed:(String.Set.of_list collapsed) tree
    |> List.map ~f:(fun (s : P.Session_summary.t) -> s.id)
    |> String.concat ~sep:" "
    |> print_endline
  in
  order [];
  order [ "/wt/x" ];
  order [ "/src/p"; "/notes" ];
  [%expect
    {|
    s2 s5 s1 s6 s3 s4
    s2 s5 s3 s4
    s4
    |}]
;;

let%expect_test
    "a listing adds its worktrees; its project comes first when no session is \
     in it; archived owners keep their subagents' worktrees"
  =
  let listing root =
    P.Json.parse
      (sprintf
         {|{"root":"%s","project_dir":"/wt","worktrees":[{"path":"%s","branch":"main","main":true,"current":true},{"path":"/wt/x","branch":"feat/x","changes":2},{"path":"/wt/docs","branch":"docs"}]}|}
         root
         root)
    |> Or_error.ok_exn
    |> P.Worktree.Listing.of_json
    |> Or_error.ok_exn
  in
  print (Project_tree.build ~listing:(listing "/src/p") sessions);
  [%expect
    {|
    p [git] main (/src/p)
      s2
      s5
      ⎇ feat/x (/wt/x) listed, 2 changed
        s1
        s6
      ⎇ agent/y (/wt/agent-y) subagent a1 of s2
      ⎇ docs (/wt/docs) listed, 0 changed
    notes (/notes)
      s3
    q [git] trunk (/src/q)
      s4
    |}];
  print
    (Project_tree.build ~listing:(listing "/src/new") [ session "s3" "/notes" ]);
  [%expect
    {|
    new [git] main (/src/new)
      ⎇ feat/x (/wt/x) listed, 2 changed
      ⎇ docs (/wt/docs) listed, 0 changed
    notes (/notes)
      s3
    |}];
  let archived =
    session
      "s9"
      "/src/q"
      ~archived:true
      ~repo:("/src/q", "/src/q", "trunk")
      ~agent_worktrees:
        (agent_worktree
           ~agent:"a2"
           ~path:"/wt/q-agent"
           ~branch:"agent/z"
           ~root:"/src/q")
  in
  print (Project_tree.build ~owners:[ archived ] [ session "s3" "/notes" ]);
  [%expect
    {|
    notes (/notes)
      s3
    q [git] (/src/q)
      ⎇ agent/z (/wt/q-agent) subagent a2 of s9
    |}];
  print_s
    [%sexp
      (Option.map
         (Project_tree.find_worktree
            (Project_tree.build sessions)
            "/wt/agent-y")
         ~f:(fun (p, w) -> p.root, w.branch)
       : (string * string option) option)];
  [%expect {| ((/src/p (agent/y))) |}]
;;
