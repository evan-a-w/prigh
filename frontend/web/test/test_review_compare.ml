open! Core
module H = Harness

(* The review page's pickers: what to review (a worktree, or a branch that is
   not checked out, read-only) and what to compare it with (HEAD, branches,
   remote branches, tags, recent commits, or any revision typed), since the
   merge base or directly; remembered in the user's preferences. *)

let feat = "/home/u/.prigh/worktrees/work/feat-login"

let state () =
  H.state_json
    ~fields:
      [ "session_id", `String "s1"
      ; "session_path", `String "/sessions/s1.jsonl"
      ; "cwd", `String "/work"
      ; "git_branch", `String "main"
      ]
    ()
;;

let listing =
  sprintf
    {|{"root":"/work","project_dir":"/home/u/.prigh/worktrees/work","worktrees":[
       {"path":"/work","branch":"main","head":"0123456789abcdef","main":true,"current":true,"changes":1},
       {"path":"%s","branch":"feat/login","head":"1111111111","managed":true,"changes":1,"ahead":1}]}|}
    feat
;;

let a_diff =
  "diff --git a/a.ml b/a.ml\n\
   index 1111111..2222222 100644\n\
   --- a/a.ml\n\
   +++ b/a.ml\n\
   @@ -1 +1 @@\n\
   -let x = 1\n\
   +let x = 2\n"
;;

let sha c = String.make 40 c

let diff_json
      ?(root = "/work")
      ?(branch = Some "main")
      ?rev
      ?(base = "HEAD")
      ?(direct = false)
      ?(against = "HEAD")
      diff
  =
  Jsonaf.to_string
    (`Object
        (List.filter_opt
           [ Some ("root", `String root)
           ; Some ("against", `String against)
           ; Some ("diff", `String diff)
           ; Some ("truncated", `False)
           ; Option.map branch ~f:(fun b -> "branch", `String b)
           ; Option.map rev ~f:(fun r -> "rev", `String r)
           ; Some ("base", `String base)
           ; Some ("direct", if direct then `True else `False)
           ; Some ("main", if String.equal root "/work" then `True else `False)
           ; Some
               ( "branches"
               , `Array
                   [ `String "feat/login"; `String "feat/old"; `String "main" ]
               )
           ; Some
               ( "remote_branches"
               , `Array [ `String "origin/main"; `String "origin/feat/remote" ]
               )
           ; Some ("tags", `Array [ `String "v1.2"; `String "v1.1" ])
           ; Some
               ( "commits"
               , `Array
                   [ `Object
                       [ "sha", `String (sha 'a')
                       ; "subject", `String "Fix the login"
                       ]
                   ; `Object
                       [ "sha", `String (sha 'b')
                       ; "subject", `String "Add a.ml"
                       ]
                   ] )
           ]))
;;

let started () =
  let h = H.create ~state:(state ()) () in
  H.quiet h (fun () ->
    H.reply h "list_worktrees" listing;
    H.reply h "get_review" {|{"comments":[],"approvals":[]}|};
    H.act h (Review Toggle);
    H.reply h "git_diff" (diff_json a_diff));
  h
;;

let target h = H.text h ~selector:".review-target"
let popover h = H.text h ~selector:"#review-popover"

let%expect_test "the pickers: grouped, filtered, keyboard and Esc" =
  let h = started () in
  target h;
  [%expect
    {|
    Reviewing
    (main /work)
    against
    (HEAD the uncommitted changes)
    |}];
  (* What to review: the worktrees, then the branches not checked out. *)
  H.key h "r" ~target:Review;
  popover h;
  [%expect
    {|
    (Review_pick Target)
    (Focus review-pick-filter)
    [] r
    Worktrees
    ✓ main /work · this session's
    feat/login /home/u/.prigh/worktrees/work/feat-login
    Branches
    feat/old not checked out: read-only
    Remote branches
    origin/main not checked out: read-only
    origin/feat/remote not checked out: read-only
    ↑ ↓ and Enter choose, Esc closes
    |}];
  H.names h ~selector:"#review-popover [role]";
  [%expect
    {|
    [combobox] input "What to review"
    [listbox] div "What to review"
    [group] div "Worktrees"
    [option] div "main /work \194\183 this session's"
    [option] div "feat/login /home/u/.prigh/worktrees/work/feat-login"
    [group] div "Branches"
    [option] div "feat/old not checked out: read-only"
    [group] div "Remote branches"
    [option] div "origin/main not checked out: read-only"
    [option] div "origin/feat/remote not checked out: read-only"
    |}];
  (* Its button says it is open; the rest of the page is out of reach. *)
  H.elements h ~selector:"#review-target-button[aria-expanded=true]";
  H.elements h ~selector:".review-head[inert]";
  [%expect
    {|
    <button class="open review-pick-button">
    <header class="review-head">
    |}];
  (* Typed: best first, ungrouped. *)
  H.input h ~selector:"#review-pick-filter" "remote";
  popover h;
  [%expect
    {|
    (Dialog_input remote)
    [remote] r
    origin/feat/remote not checked out: read-only
    ↑ ↓ and Enter choose, Esc closes
    |}];
  (* Esc closes it without a change, back on its button. *)
  H.key h "Escape";
  H.elements h ~selector:"#review-popover";
  [%expect
    {|
    Close_dialog
    (Focus review-target-button)
    |}];
  (* Against: HEAD, branches, remote branches, tags, the last commits. *)
  H.click h ~selector:"#review-base-button";
  popover h;
  [%expect
    {|
    (Review_pick Base)
    (Focus review-pick-filter)
    [] b
    Uncommitted
    ✓ HEAD the uncommitted changes only
    Branches
    feat/login local branch
    feat/old local branch
    Remote branches
    origin/main remote-tracking branch
    origin/feat/remote remote-tracking branch
    Tags
    v1.2 tag
    v1.1 tag
    Recent commits
    aaaaaaaa Fix the login
    bbbbbbbb Add a.ml
    ↑ ↓ and Enter choose, Esc closes
    |}];
  (* The same button again closes it. *)
  H.click h ~selector:"#review-base-button";
  H.elements h ~selector:"#review-popover";
  [%expect
    {|
    (Review_pick Base)
    (Focus review-base-button)
    |}]
;;

let%expect_test "against origin/main, since the merge base or directly" =
  let h = started () in
  H.key h "b" ~target:Review;
  H.input h ~selector:"#review-pick-filter" "origin/ma";
  H.key h "Enter";
  [%expect
    {|
    (Review_pick Base)
    (Focus review-pick-filter)
    (Dialog_input origin/ma)
    Dialog_accept
    (Rpc (method_ git_diff) (params ((base origin/main))) (tag (Review Diff)))
    (Focus review-page)
    |}];
  H.reply h "git_diff" (diff_json ~base:"origin/main" ~against:(sha 'c') a_diff);
  target h;
  [%expect
    {|
    (Rpc (method_ fs_preview) (params ((path /work/a.ml)))
     (tag (Review (Content a.ml))))
    (Cache_prefs
     (((key web.review)
       (value
        (((compare ((/work ((base origin/main) (direct false))))) (last ())))))))
    (Rpc (method_ set_prefs)
     (params
      ((prefs
        ((web.review
          ((compare ((/work ((base origin/main) (direct false))))) (last ())))))))
     (tag Show_error))
    Reviewing
    (main /work)
    against
    (origin/main)
    (merge base cccccccc)
    (Since merge base) (Directly)
    |}];
  H.elements h ~selector:".review-target[title]";
  [%expect {| <div class="review-target"> |}];
  (* Directly: two-dot, said in the header. *)
  H.key h "d" ~target:Review;
  [%expect
    {|
    (Review_direct true)
    (Rpc (method_ git_diff) (params ((base origin/main) (direct true)))
     (tag (Review Diff)))
    (Focus review-page)
    |}];
  H.reply
    h
    "git_diff"
    (diff_json ~base:"origin/main" ~direct:true ~against:(sha 'd') a_diff);
  H.quiet h (fun () -> target h);
  H.elements h ~selector:".review-target[title]";
  [%expect
    {|
    (Rpc (method_ fs_preview) (params ((path /work/a.ml)))
     (tag (Review (Content a.ml))))
    (Cache_prefs
     (((key web.review)
       (value
        (((compare ((/work ((base origin/main) (direct true))))) (last ())))))))
    (Rpc (method_ set_prefs)
     (params
      ((prefs
        ((web.review
          ((compare ((/work ((base origin/main) (direct true))))) (last ())))))))
     (tag Show_error))
    Reviewing
    (main /work)
    against
    (origin/main)
    directly
    (Since merge base) (Directly)
    <div class="review-target">
    |}];
  H.click h ~selector:".review-target .segment";
  [%expect
    {|
    (Review_direct false)
    (Rpc (method_ git_diff) (params ((base origin/main))) (tag (Review Diff)))
    (Focus review-page)
    |}]
;;

let%expect_test "a typed revision: checked, the closest names when unknown" =
  let h = started () in
  H.key h "b" ~target:Review;
  H.input h ~selector:"#review-pick-filter" "mian";
  popover h;
  [%expect
    {|
    (Review_pick Base)
    (Focus review-pick-filter)
    (Dialog_input mian)
    [mian] b
    ✓ HEAD the uncommitted changes only
    origin/main remote-tracking branch
    origin/feat/remote remote-tracking branch
    mian compare with this branch, tag or commit
    ↑ ↓ and Enter choose, Esc closes
    |}];
  H.click h ~selector:"[data-id='base:mian']";
  [%expect
    {|
    (Picker_choose base:mian)
    (Rpc (method_ git_diff) (params ((base mian))) (tag (Review Diff)))
    (Focus review-page)
    |}];
  H.fail
    h
    "git_diff"
    "unknown revision \"mian\": name a branch, tag or commit (closest: main, \
     origin/main, v1.1)";
  (* Back to what was reviewed, the closest names in a toast. *)
  H.text h ~selector:".toast";
  H.reply h "git_diff" (diff_json a_diff);
  H.quiet h (fun () -> target h);
  [%expect
    {|
    (Rpc (method_ git_diff) (params ()) (tag (Review Diff)))
    (Focus review-page)
    Unknown revision "mian": name a branch, tag or commit (closest: main, origin/main, v1.1) (Dismiss)
    (Rpc (method_ fs_preview) (params ((path /work/a.ml)))
     (tag (Review (Content a.ml))))
    Reviewing
    (main /work)
    against
    (HEAD the uncommitted changes)
    |}]
;;

let%expect_test
    "a branch that is not checked out: read-only, then checked out into a \
     worktree"
  =
  let h = started () in
  H.key h "r" ~target:Review;
  H.input h ~selector:"#review-pick-filter" "old";
  H.key h "Enter";
  [%expect
    {|
    (Review_pick Target)
    (Focus review-pick-filter)
    (Dialog_input old)
    Dialog_accept
    (Rpc (method_ git_diff) (params ((rev feat/old))) (tag (Review Diff)))
    (Focus review-page)
    |}];
  H.reply
    h
    "git_diff"
    (diff_json
       ~branch:None
       ~rev:"feat/old"
       ~base:"main"
       ~against:(sha 'e')
       a_diff);
  target h;
  [%expect
    {|
    (Rpc (method_ git_show) (params ((rev feat/old) (path a.ml)))
     (tag (Review (Shown a.ml))))
    (Cache_prefs
     (((key web.review) (value (((compare ()) (last ((/work rev:feat/old)))))))))
    (Rpc (method_ set_prefs)
     (params
      ((prefs ((web.review ((compare ()) (last ((/work rev:feat/old)))))))))
     (tag Show_error))
    Reviewing
    (feat/old not checked out)
    against
    (main)
    (merge base eeeeeeee)
    (Since merge base) (Directly)
    Read-only (Check out into a worktree)
    |}];
  (* Its files as the branch has them; editing is refused with a hint. *)
  H.reply h "git_show" {|{"text":"let x = 2\n","truncated":false}|};
  H.key h "e" ~target:Review;
  H.text h ~selector:".toast";
  [%expect
    {|
    (Review (Set_mode Edit))
    (Expire_toast (id 0) (after_ms 5220))
    feat/old is not checked out, so it is read-only here: Check out into a worktree to change it.
    |}];
  (* Checked out into a worktree (recorded on the session), then reviewed
     there, compared the same way. *)
  H.click h ~selector:".review-checkout";
  [%expect
    {|
    Review_checkout
    (Expire_toast (id 1) (after_ms 2820))
    (Rpc (method_ create_worktree) (params ((branch feat/old)))
     (tag
      (Review_checked_out ((reviewed (Dir ())) (base (main)) (direct false)))))
    |}];
  H.reply
    h
    "create_worktree"
    {|{"worktree":{"path":"/home/u/.prigh/worktrees/work/feat-old","branch":"feat/old","head":"eeee","managed":true,"changes":0},"new_branch":false,"root":"/work","cwd":"/work"}|};
  [%expect
    {|
    (Expire_toast (id 2) (after_ms 4700))
    (Rpc (method_ git_diff)
     (params ((base main) (dir /home/u/.prigh/worktrees/work/feat-old)))
     (tag (Review Diff)))
    (Focus review-page)
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nbackend\
                            \n/work") (purpose (Sidebar 2)))))
    |}]
;;

let%expect_test "a remote branch checks out as its local branch" =
  let h = started () in
  H.key h "r" ~target:Review;
  H.click h ~selector:"[data-id='rev:origin/feat/remote']";
  H.reply
    h
    "git_diff"
    (diff_json
       ~branch:None
       ~rev:"origin/feat/remote"
       ~base:"origin/main"
       ~against:(sha 'f')
       a_diff);
  H.quiet h (fun () -> ());
  H.click h ~selector:".review-checkout";
  [%expect
    {|
    (Review_pick Target)
    (Focus review-pick-filter)
    (Picker_choose rev:origin/feat/remote)
    (Rpc (method_ git_diff) (params ((rev origin/feat/remote)))
     (tag (Review Diff)))
    (Focus review-page)
    (Rpc (method_ git_show) (params ((rev origin/feat/remote) (path a.ml)))
     (tag (Review (Shown a.ml))))
    (Cache_prefs
     (((key web.review)
       (value (((compare ()) (last ((/work rev:origin/feat/remote)))))))))
    (Rpc (method_ set_prefs)
     (params
      ((prefs
        ((web.review ((compare ()) (last ((/work rev:origin/feat/remote)))))))))
     (tag Show_error))
    Review_checkout
    (Expire_toast (id 0) (after_ms 2940))
    (Rpc (method_ create_worktree) (params ((branch feat/remote)))
     (tag
      (Review_checked_out
       ((reviewed (Dir ())) (base (origin/main)) (direct false)))))
    |}]
;;

let%expect_test "remembered: the next visit opens where the last one was" =
  let h = H.create ~state:(state ()) () in
  H.quiet h (fun () ->
    H.reply h "list_worktrees" listing;
    H.reply h "get_review" {|{"comments":[],"approvals":[]}|};
    (* The session's own diff, for the file browser: its root is known. *)
    H.act h (Review Refresh);
    H.reply h "git_diff" (diff_json a_diff));
  H.event
    h
    {|{"event":"prefs_changed","prefs":{"web.review":{"compare":{"branch:feat/old":{"base":"v1.1","direct":true}},"last":{"/work":"rev:feat/old"}}}}|};
  H.key h "r" ~alt:true ~code:"KeyR";
  [%expect
    {|
    (Cache_prefs
     (((key web.review)
       (value
        (((compare ((branch:feat/old ((base v1.1) (direct true)))))
          (last ((/work rev:feat/old)))))))))
    (Review Toggle)
    (Rpc (method_ git_diff) (params ((base v1.1) (rev feat/old) (direct true)))
     (tag (Review Diff)))
    (Focus review-page)
    |}];
  (* Back to the session's own worktree, then the branch again: compared
     as it was. *)
  H.quiet h (fun () ->
    H.reply
      h
      "git_diff"
      (diff_json ~branch:None ~rev:"feat/old" ~base:"v1.1" ~direct:true a_diff));
  H.key h "r" ~target:Review;
  H.click h ~selector:"[data-id='own']";
  H.quiet h (fun () -> H.reply h "git_diff" (diff_json a_diff));
  H.key h "r" ~target:Review;
  H.click h ~selector:"[data-id='rev:feat/old']";
  [%expect
    {|
    (Review_pick Target)
    (Focus review-pick-filter)
    (Picker_choose own)
    (Rpc (method_ git_diff) (params ()) (tag (Review Diff)))
    (Focus review-page)
    (Review_pick Target)
    (Focus review-pick-filter)
    (Picker_choose rev:feat/old)
    (Rpc (method_ git_diff) (params ((base v1.1) (rev feat/old) (direct true)))
     (tag (Review Diff)))
    (Focus review-page)
    |}]
;;

let%expect_test
    "/review completes branches not checked out, remote ones and tags"
  =
  let h = started () in
  H.quiet h (fun () -> H.key h "Escape" ~target:Review);
  H.type_ h "/review ";
  H.text h ~selector:"#completion-list";
  H.type_ h "/review feat/old --base ";
  H.text h ~selector:"#completion-list";
  [%expect
    {|
    (Review Close)
    --base compare with another branch, tag or commit
    --direct compare with the base itself, not the merge base
    main /work · main worktree · here · 1 changed
    feat/login /home/u/.prigh/worktrees/work/feat-login · 1 changed · ↑1 ↓0
    feat/old not checked out: review it read-only (needs --base)
    origin/main not checked out: review it read-only (needs --base)
    origin/feat/remote not checked out: review it read-only (needs --base)
    HEAD the uncommitted changes only
    feat/login compare with feat/login
    feat/old compare with feat/old
    main compare with main
    origin/main compare with the remote branch origin/main
    origin/feat/remote compare with the remote branch origin/feat/remote
    v1.2 compare with the tag v1.2
    v1.1 compare with the tag v1.1
    |}]
;;
