open! Core
open! Prigh
open Tool_test_helpers
open! Test_agent_worktrees

let%expect_test "git worktree in bash: recorded, with a note when outside" =
  with_server (fun ~trees ->
    ( [ bash ~id:"b1" "git worktree add -q -b feat/c ../outside"
      ; bash
          ~id:"b2"
          (sprintf
             "git worktree add -q -b feat/d %s"
             (Filename.concat trees "repo/feat-d"))
      ; bash ~id:"b3" "echo unrelated"
      ; Reply.text "made two"
      ; bash ~id:"b4" "git worktree remove ../outside"
      ; call_tool ~id:"s1" ~name:"subagent" {|{"task":"By hand"}|}
      ; Reply.text "started"
      ; Reply.text "ok"
      ]
    , [ ( "By hand"
        , [ bash ~id:"n1" "git worktree add -q -b feat/e ../by-hand"
          ; Reply.text "done"
          ] )
      ] ))
  @@ fun t ~prompt ->
  (* The project directory exists once prigh made a worktree there. *)
  ignore
    (Worktree.create
       ~env:t.env
       ~cancel:Cancellation.never
       ~cwd:(Filename.concat t.dir "repo")
       ~dir:(Filename.concat t.dir "trees")
       ~branch:"feat/first"
       ~base:None
       ()
     : Worktree.Created.t Or_error.t);
  prompt "make two by hand";
  [%expect
    {|
    bash:
    [prigh: git worktree add made $DIR/outside outside the standard place ($DIR/trees/repo). It is recorded on the session, but next time use the worktree tool (or the subagent tool's worktree option) rather than git worktree add.]
    bash:
    bash: unrelated

    -- worktrees:
    {"path":"$DIR/outside","branch":"feat/c","root":"$DIR/repo","source":"created"}
    {"path":"$DIR/trees/repo/feat-d","branch":"feat/d","root":"$DIR/repo","source":"created"}
    |}];
  prompt "remove one, and a subagent";
  [%expect
    {|
    bash:
    subagent: started agent a1 (By hand); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    [a1] bash:
    [prigh: git worktree add made $DIR/by-hand outside the standard place ($DIR/trees/repo). It is recorded on the session, but next time use the worktree tool (or the subagent tool's worktree option) rather than git worktree add.]
    -- worktrees:
    {"path":"$DIR/trees/repo/feat-d","branch":"feat/d","root":"$DIR/repo","source":"created"}
    {"path":"$DIR/by-hand","branch":"feat/e","root":"$DIR/repo","source":"agent","agent":"a1","task":"By hand"}
    |}]
;;
