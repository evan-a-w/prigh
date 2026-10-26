open! Core
module H = Harness

(* Reviewing another worktree of the repository: [/review WORKTREE], the
   worktree picker's Alt+R and [Review_worktree] (the sidebar's "Review
   changes"); against its base or one picked; its own comments and
   approvals only. *)

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

let login_diff =
  "diff --git a/login.ml b/login.ml\n\
   new file mode 100644\n\
   index 0000000..3333333\n\
   --- /dev/null\n\
   +++ b/login.ml\n\
   @@ -0,0 +1 @@\n\
   +let login () = ()\n"
;;

let diff_json ?(base = "HEAD") ?(against = "HEAD") ~root ~branch diff =
  Jsonaf.to_string
    (`Object
        [ "root", `String root
        ; "against", `String against
        ; "diff", `String diff
        ; "truncated", `False
        ; "branch", `String branch
        ; "base", `String base
        ; "main", if String.equal root "/work" then `True else `False
        ; "branches", `Array [ `String "feat/login"; `String "main"; `String "origin/main" ]
        ])
;;

let feat_diff ?(base = "main") () =
  diff_json
    ~root:feat
    ~branch:"feat/login"
    ~base
    ~against:
      (if String.equal base "HEAD"
       then "HEAD"
       else "abcdef0123456789abcdef0123456789abcdef01")
    (a_diff ^ login_diff)
;;

let comment ~id ~root text =
  sprintf
    {|{"id":"%s","root":"%s","path":"a.ml","first":1,"last":1,"anchor":"let x = 2","status":"open","thread":[{"author":"user","text":"%s","at_ms":1791194400000,"delivered":false}]}|}
    id
    root
    text
;;

(* Comments on a.ml in both worktrees, and approvals: a.ml at the hash of
   main's change before worktrees were told apart (no root), login.ml in
   the worktree at an older hash. *)
let review_json ~a_hash =
  sprintf
    {|{"comments":[%s,%s],"approvals":[{"path":"a.ml","hash":"%s"},{"root":"%s","path":"login.ml","hash":"older"}]}|}
    (comment ~id:"c1" ~root:"/work" "In main")
    (comment ~id:"c2" ~root:feat "In the worktree")
    a_hash
    feat
;;

let a_hash () =
  match Prigh_ui.Diff_files.parse a_diff with
  | [ f ] -> f.hash
  | _ -> assert false
;;

let page h = H.text h ~selector:".review-page"
let target h = H.text h ~selector:".review-target"

let started () =
  let h = H.create ~state:(state ()) () in
  H.quiet h (fun () ->
    H.reply h "list_worktrees" listing;
    H.reply h "get_review" (review_json ~a_hash:(a_hash ())));
  h
;;

let%expect_test "/review WORKTREE: its changes against its base, its own \
                 comments and approvals"
  =
  let h = started () in
  H.type_ h "/review feat/login";
  H.act h Send;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/review feat/login") (max 500)))
     (tag Ignore))
    (Rpc (method_ git_diff)
     (params ((dir /home/u/.prigh/worktrees/work/feat-login)))
     (tag (Review Diff)))
    (Focus review-page)
    |}];
  H.reply h "git_diff" (feat_diff ());
  page h;
  [%expect {|
    (Rpc (method_ fs_preview)
     (params ((path /home/u/.prigh/worktrees/work/feat-login/a.ml)))
     (tag (Review (Content a.ml))))
    Review
    0/2 approved
    (Read the changes again (they also update as prigh edits))
    (Comment) (Edit)
    (Changes) (Whole file)
    (Send 1 comment) (Close the review (Esc, Alt+R))
    Reviewing main (/work) feat/login (/home/u/.prigh/worktrees/work/feat-login) against HEAD (the uncommitted changes) main origin/main (merge base abcdef01)
    (○ a.ml 1 +1 −1)
    (⚠ login.ml +1 −0)
    2 files changed, +2 −1
    (Previous file ([, p, k)) (Next file (], n, j))
    a.ml
    (Approve)
    @@ -1 +1 @@
    1 - let x = 1
    1 + let x = 2
    c2 line 1
    (Resolve) (Delete the comment and its thread)
    You 10:00 not sent yet
    In the worktree
    []
    |}];
  H.show h ~selector:".review-target";
  [%expect {|
    <div title="Reviewing feat/login (/home/u/.prigh/worktrees/work/feat-login) against main (merge base abcdef01)"
         class="review-target">
      <span class="review-target-text"> Reviewing </span>
      <select id="review-worktree"
              aria-label="The worktree to review"
              title="The worktree to review"
              class="review-select"
              @on_change>
        <option #value="/work" #selected="false"> main (/work) </option>
        <option #value="/home/u/.prigh/worktrees/work/feat-login" #selected="true"> feat/login (/home/u/.prigh/worktrees/work/feat-login) </option>
      </select>
      <span class="review-target-text"> against </span>
      <select id="review-base"
              aria-label="What to compare it with"
              title="What to compare it with"
              class="review-select"
              @on_change>
        <option #value="HEAD" #selected="false"> HEAD (the uncommitted changes) </option>
        <option #value="main" #selected="true"> main </option>
        <option #value="origin/main" #selected="false"> origin/main </option>
      </select>
      <span class="review-merge-base"> (merge base abcdef01) </span>
    </div>
    |}];
  (* Approving and sending name the worktree. *)
  H.key h "a" ~target:Review;
  H.key h "s" ~target:Review;
  [%expect {|
    (Review (Toggle_approval a.ml))
    (Rpc (method_ review_approve)
     (params
      ((root /home/u/.prigh/worktrees/work/feat-login) (path a.ml)
       (hash 8e7f85d53940537408ab49f00ee82988)))
     (tag (Review (Changed ()))))
    (Review Submit)
    (Rpc (method_ review_submit)
     (params ((root /home/u/.prigh/worktrees/work/feat-login)))
     (tag (Review Submitted)))
    |}]
;;

let%expect_test "the session's own worktree keeps the approvals from before \
                 worktrees were told apart"
  =
  let h = started () in
  H.type_ h "/review";
  H.act h Send;
  H.reply h "git_diff" (diff_json ~root:"/work" ~branch:"main" a_diff);
  page h;
  [%expect {|
    (Rpc (method_ push_pref) (params ((key history) (value /review) (max 500)))
     (tag Ignore))
    (Rpc (method_ git_diff) (params ()) (tag (Review Diff)))
    (Focus review-page)
    (Rpc (method_ fs_preview) (params ((path /work/a.ml)))
     (tag (Review (Content a.ml))))
    Review
    1/1 approved
    (Read the changes again (they also update as prigh edits))
    (Comment) (Edit)
    (Changes) (Whole file)
    (Send 1 comment) (Close the review (Esc, Alt+R))
    Reviewing main (/work) feat/login (/home/u/.prigh/worktrees/work/feat-login) against HEAD (the uncommitted changes) feat/login origin/main
    (✓ a.ml 1 +1 −1)
    1 file changed, +1 −1
    (Previous file ([, p, k)) (Next file (], n, j))
    a.ml
    (Approved)
    @@ -1 +1 @@
    1 - let x = 1
    1 + let x = 2
    c1 line 1
    (Resolve) (Delete the comment and its thread)
    You 10:00 not sent yet
    In main
    []
    |}];
  (* Taking that approval back names no worktree, as it was made. *)
  H.key h "a" ~target:Review;
  [%expect {|
    (Review (Toggle_approval a.ml))
    (Rpc (method_ review_unapprove) (params ((path a.ml)))
     (tag (Review (Changed ()))))
    |}]
;;

let%expect_test "another base, another worktree: from the pickers; replies \
                 for the one before are dropped"
  =
  let h = started () in
  H.act h (Review_worktree { path = feat; base = None });
  [%expect {|
    (Rpc (method_ git_diff)
     (params ((dir /home/u/.prigh/worktrees/work/feat-login)))
     (tag (Review Diff)))
    (Focus review-page)
    |}];
  (* The base picker: the uncommitted changes only. *)
  H.act h (Review (Show { dir = Some feat; base = Some "HEAD" }));
  [%expect {| (Focus review-page) |}];
  (* The first reply is for the default base: read again for HEAD. *)
  H.reply h "git_diff" (feat_diff ());
  [%expect {|
    (Rpc (method_ git_diff)
     (params ((base HEAD) (dir /home/u/.prigh/worktrees/work/feat-login)))
     (tag (Review Diff)))
    |}];
  H.reply h "git_diff" (feat_diff ~base:"HEAD" ());
  target h;
  [%expect {|
    (Rpc (method_ fs_preview)
     (params ((path /home/u/.prigh/worktrees/work/feat-login/a.ml)))
     (tag (Review (Content a.ml))))
    Reviewing main (/work) feat/login (/home/u/.prigh/worktrees/work/feat-login) against HEAD (the uncommitted changes) main origin/main
    |}];
  (* The agent or a subagent edits: the reviewed worktree is read again. *)
  H.event
    h
    {|{"event":"subagent","call_id":"c1","agent_id":"a1","inner":{"event":"tool_end","call":{"id":"e1","name":"write","arguments":"{}"},"result":{"role":"tool_result","tool_call_id":"e1","tool_name":"write","text":"ok","is_error":false}}}|};
  [%expect {|
    (Rpc (method_ git_diff)
     (params ((base HEAD) (dir /home/u/.prigh/worktrees/work/feat-login)))
     (tag (Review Diff)))
    |}];
  (* Another session starts over with its own worktree. *)
  H.event
    h
    (sprintf
       {|{"event":"state","state":%s}|}
       (H.state_json
          ~fields:
            [ "session_id", `String "s2"
            ; "session_path", `String "/sessions/s2.jsonl"
            ; "cwd", `String "/work"
            ]
          ()));
  print_s
    [%sexp ((H.model h).review.target : Prigh_web.Code_review.Target.t)];
  [%expect {|
    (Set_url_session s2)
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before ()))))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Rpc (method_ get_review) (params ()) (tag (Review Review)))
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s2\
                            \nbackend\
                            \n/work") (purpose (Sidebar 2)))))
    ((dir ()) (base ()))
    |}]
;;

let%expect_test "/worktree's picker: Alt+R reviews the highlighted worktree" =
  let h = started () in
  H.type_ h "/worktree";
  H.act h Send;
  H.reply h "list_worktrees" listing;
  H.act h (Picker_query "feat");
  H.key h "r" ~alt:true ~code:"KeyR";
  [%expect {|
    (Rpc (method_ push_pref) (params ((key history) (value /worktree) (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nbackend\
                            \n/work") (purpose Picker))))
    (Focus picker-input)
    (Review_worktree (path /home/u/.prigh/worktrees/work/feat-login) (base ()))
    (Focus editor)
    (Rpc (method_ git_diff)
     (params ((dir /home/u/.prigh/worktrees/work/feat-login)))
     (tag (Review Diff)))
    (Focus review-page)
    |}]
;;

let%expect_test "/review completes worktrees, --base and branches" =
  let h = started () in
  H.quiet h (fun () ->
    H.type_ h "/review";
    H.act h Send;
    H.reply h "git_diff" (diff_json ~root:"/work" ~branch:"main" a_diff);
    H.key h "Escape");
  let completions () =
    print_s
      [%sexp
        (Option.value_map (H.model h).completion ~default:[] ~f:(fun c ->
           List.map (Prigh_web.Completion.items c) ~f:(fun i -> i.id))
         : string list)]
  in
  H.type_ h "/review ";
  completions ();
  H.type_ h "/review --base ";
  completions ();
  [%expect {|
    (browser default)
    (--base main feat/login)
    (HEAD feat/login main origin/main)
    |}]
;;
