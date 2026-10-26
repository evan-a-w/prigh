open! Core
module H = Harness

(* The review page's git bar: the worktree's branch and what is staged,
   ticking files to stage, committing with a drafted message, amending,
   pushing, and a pull request for the branch. *)

let state () =
  H.state_json
    ~fields:
      [ "session_id", `String "s1"
      ; "session_path", `String "/sessions/s1.jsonl"
      ; "cwd", `String "/home/me/proj"
      ]
    ()
;;

let main_ml_diff =
  "diff --git a/src/main.ml b/src/main.ml\n\
   index 1111111..2222222 100644\n\
   --- a/src/main.ml\n\
   +++ b/src/main.ml\n\
   @@ -2,3 +2,4 @@\n\
  \ let b = 2\n\
   -let c = 3\n\
   +let c = 3 * 2\n\
   +let d = 4\n\
  \ let e = 5\n"
;;

let util_diff =
  "diff --git a/src/util.ml b/src/util.ml\n\
   new file mode 100644\n\
   index 0000000..3333333\n\
   --- /dev/null\n\
   +++ b/src/util.ml\n\
   @@ -0,0 +1,2 @@\n\
   +let id x = x\n\
   +let twice f x = f (f x)\n"
;;

let diff_json ?(root = "/home/me/proj") diff =
  Jsonaf.to_string
    (`Object
        [ "root", `String root
        ; "against", `String "HEAD"
        ; "diff", `String diff
        ; "truncated", `False
        ])
;;

(* src/main.ml's hash, as the review approves it. *)
let approved_main =
  {|{"comments":[],"approvals":[{"path":"src/main.ml","hash":"7f57d42d80a860a5b495b9ed7f7e860e"}]}|}
;;

let status_json
      ?(root = "/home/me/proj")
      ?(branch = Some "feat/x")
      ?(ahead = 1)
      files
  =
  sprintf
    {|{"root":"%s",%s"head":"abcdef0123","upstream":"origin/feat/x","ahead":%d,"behind":0,"files":[%s]}|}
    root
    (Option.value_map branch ~default:"" ~f:(sprintf {|"branch":"%s",|}))
    ahead
    (String.concat ~sep:"," files)
;;

let file ?index ?worktree path =
  sprintf
    {|{"path":"%s"%s%s}|}
    path
    (Option.value_map index ~default:"" ~f:(sprintf {|,"index":"%s"|}))
    (Option.value_map worktree ~default:"" ~f:(sprintf {|,"worktree":"%s"|}))
;;

let opened () =
  let h = H.create ~state:(state ()) () in
  H.quiet h (fun () ->
    H.reply h "get_review" approved_main;
    H.type_ h "/review";
    H.key h "Enter";
    H.reply h "git_diff" (diff_json (main_ml_diff ^ util_diff)));
  h
;;

let show_bar h =
  H.text h ~selector:".git-bar";
  H.text h ~selector:".review-files"
;;

let%expect_test "the bar: branch, upstream, counts; approved files come ticked" =
  let h = opened () in
  H.reply
    h
    "git_status"
    (status_json
       [ file "src/main.ml" ~worktree:"M"; file "src/util.ml" ~worktree:"?" ]);
  show_bar h;
  [%expect
    {|
    (Complete_accept (run true))
    feat/x ↑1 ↓0 origin/feat/x
    0 staged 1 unstaged 1 untracked
    (Stage approved) (Push ↑1) (Create PR)
    (☑) (✓ src/ main.ml +2 −1)
    (☐) (○ src/ util.ml +2 −0)
    2 files changed, +4 −1
    Commit message
    (✦ Generate)
    []
    (☐ Amend) (Commit 1 file)
    |}];
  H.names h ~selector:".git-tick";
  [%expect
    {|
    [checkbox] button "Commit src/main.ml" -- Approved: staged when you commit (untick to leave it out)
    [checkbox] button "Commit src/util.ml" -- Not staged: tick to stage it
    |}];
  (* A status it cannot read: no actions, and why. *)
  H.quiet h (fun () -> H.act h (Review Refresh));
  H.fail h "git_status" "not a git repository: /x (/cd into one)";
  H.text h ~selector:".git-bar";
  [%expect {| No git actions here: not a git repository: /x (/cd into one) |}]
;;

let%expect_test "ticking stages and unstages; Stage approved" =
  let h = opened () in
  H.reply
    h
    "git_status"
    (status_json
       [ file "src/main.ml" ~worktree:"M"; file "src/util.ml" ~worktree:"?" ]);
  (* Unticking an approved file leaves it out: nothing to tell git. *)
  H.click h ~selector:".git-tick";
  H.click h ~selector:".git-tick" ~nth:1;
  [%expect
    {|
    (Complete_accept (run true))
    (Git (Toggle src/main.ml))
    (Git (Toggle src/util.ml))
    (Rpc (method_ git_stage) (params ((paths (src/util.ml)))) (tag (Git Staged)))
    |}];
  H.text h ~selector:".git-bar";
  [%expect
    {|
    feat/x ↑1 ↓0 origin/feat/x
    0 staged 1 unstaged 1 untracked
    (Stage approved) (Push ↑1) (Create PR)
    |}];
  H.reply
    h
    "git_stage"
    (status_json
       [ file "src/main.ml" ~worktree:"M"; file "src/util.ml" ~index:"A" ]);
  H.text h ~selector:".review-files ul";
  [%expect
    {|
    (☐) (✓ src/ main.ml +2 −1)
    (☑) (○ src/ util.ml +2 −0)
    |}];
  (* A staged file unticked is unstaged. *)
  H.click h ~selector:".git-tick" ~nth:1;
  [%expect
    {|
    (Git (Toggle src/util.ml))
    (Rpc (method_ git_unstage) (params ((paths (src/util.ml))))
     (tag (Git Staged)))
    |}];
  H.reply
    h
    "git_unstage"
    (status_json
       [ file "src/main.ml" ~worktree:"M"; file "src/util.ml" ~worktree:"?" ]);
  H.click h ~selector:".git-actions button";
  [%expect
    {|
    (Git Stage_approved)
    (Rpc (method_ git_stage) (params ((paths (src/main.ml)))) (tag (Git Staged)))
    |}];
  H.reply
    h
    "git_stage"
    (status_json
       [ file "src/main.ml" ~index:"M" ~worktree:"M"
       ; file "src/util.ml" ~worktree:"?"
       ]);
  H.text h ~selector:".review-files ul";
  H.names h ~selector:".git-tick";
  [%expect
    {|
    (☑) (✓ src/ main.ml +2 −1)
    (☐) (○ src/ util.ml +2 −0)
    [checkbox] button "Commit src/main.ml" -- Partly staged: tick to stage all of it
    [checkbox] button "Commit src/util.ml" -- Not staged: tick to stage it
    |}]
;;

let%expect_test
    "commit with a drafted message: staged first, streamed, undone, committed"
  =
  let h = opened () in
  H.reply
    h
    "git_status"
    (status_json
       [ file "src/main.ml" ~worktree:"M"; file "src/util.ml" ~worktree:"?" ]);
  (* Refusals say what to do. *)
  H.click h ~selector:".git-commit-button";
  H.text h ~selector:".git-failure";
  [%expect
    {|
    (Complete_accept (run true))
    (Git Commit)
    Write a commit message, or Generate one (Alt+G).
    |}];
  (* The draft is of the staged diff: the ticked file is staged first. *)
  H.click h ~selector:".git-commit .generate-go";
  [%expect
    {|
    (Generate Commit_message)
    (Rpc (method_ git_stage) (params ((paths (src/main.ml))))
     (tag (Git Staged_to_draft)))
    |}];
  H.reply
    h
    "git_stage"
    (status_json
       [ file "src/main.ml" ~index:"M"; file "src/util.ml" ~worktree:"?" ]);
  [%expect
    {|
    (Rpc (method_ generate_text) (params ((gen_id gen-1) (kind commit_message)))
     (tag (Generated gen-1)))
    |}];
  H.event h {|{"event":"generate_delta","gen_id":"gen-1","delta":"Double "}|};
  H.event h {|{"event":"generate_delta","gen_id":"gen-1","delta":"c"}|};
  H.text h ~selector:".git-commit";
  [%expect
    {|
    Commit message
    (Stop)
    [Double c]
    (☐ Amend) (Commit 1 file)
    |}];
  H.reply
    h
    "generate_text"
    {|{"gen_id":"gen-1","kind":"commit_message","text":"Double c and add d","model":"m","cost_usd":0.001}|};
  H.text h ~selector:".git-commit";
  [%expect
    {|
    Commit message
    (✦ Generate) (Undo)
    [Double c and add d]
    (☐ Amend) (Commit 1 file)
    |}];
  H.click h ~selector:".generate-undo";
  H.text h ~selector:".git-commit";
  [%expect
    {|
    (Generate_undo Commit_message)
    Commit message
    (✦ Generate)
    []
    (☐ Amend) (Commit 1 file)
    |}];
  H.input h ~selector:"#git-message" "Double c";
  H.click h ~selector:".git-commit-button";
  [%expect
    {|
    (Git (Message "Double c"))
    (Git Commit)
    (Rpc (method_ git_commit) (params ((message "Double c")))
     (tag (Git Committed)))
    |}];
  H.text h ~selector:".git-commit-button";
  [%expect {| (Committing…) |}];
  H.reply
    h
    "git_commit"
    (sprintf
       {|{"sha":"1234567890abcdef","subject":"Double c","status":%s}|}
       (status_json ~ahead:2 [ file "src/util.ml" ~worktree:"?" ]));
  [%expect
    {|
    (Rpc (method_ git_diff) (params ()) (tag (Review Diff)))
    (Rpc (method_ git_status) (params ()) (tag (Git Status)))
    |}];
  H.text h ~selector:".git-notice";
  H.text h ~selector:".git-bar";
  [%expect
    {|
    Committed 1234567 Double c on feat/x
    feat/x ↑2 ↓0 origin/feat/x
    0 staged 0 unstaged 1 untracked
    (Stage approved) (Push ↑2) (Create PR)
    Committed 1234567 Double c on feat/x
    |}];
  (* Amending without a message keeps the commit's. *)
  H.click h ~selector:".git-amend";
  [%expect {| (Git Toggle_amend) |}];
  H.click h ~selector:".git-commit-button";
  [%expect
    {|
    (Git Commit)
    (Rpc (method_ git_commit) (params ((message "") (amend true)))
     (tag (Git Committed)))
    |}];
  H.fail h "git_commit" "git commit failed: hook said no";
  [%expect {| (Rpc (method_ git_status) (params ()) (tag (Git Status))) |}];
  H.text h ~selector:".git-failure";
  [%expect {| git commit failed: hook said no |}]
;;

let%expect_test "a draft that fails puts the text back and says why" =
  let h = opened () in
  H.reply
    h
    "git_status"
    (status_json
       [ file "src/main.ml" ~index:"M"; file "src/util.ml" ~worktree:"?" ]);
  H.input h ~selector:"#git-message" "mine";
  H.click h ~selector:".git-commit .generate-go";
  H.event h {|{"event":"generate_delta","gen_id":"gen-1","delta":"Half"}|};
  H.fail h "generate_text" "deepseek/deepseek-flash: overloaded";
  H.text h ~selector:".git-commit";
  H.text h ~selector:".toast";
  [%expect
    {|
    (Complete_accept (run true))
    (Git (Message mine))
    (Generate Commit_message)
    (Rpc (method_ generate_text) (params ((gen_id gen-1) (kind commit_message)))
     (tag (Generated gen-1)))
    Commit message
    (✦ Generate)
    [mine]
    (☐ Amend) (Commit 1 file)
    deepseek/deepseek-flash: overloaded (Dismiss)
    |}];
  (* Stopped: what came stays, Undo puts the text back. *)
  H.click h ~selector:".git-commit .generate-go";
  H.event h {|{"event":"generate_delta","gen_id":"gen-2","delta":"Part"}|};
  H.click h ~selector:".generate-stop";
  H.event h {|{"event":"generate_delta","gen_id":"gen-2","delta":" more"}|};
  H.fail h "generate_text" "cancelled";
  H.text h ~selector:".git-commit";
  [%expect
    {|
    (Generate Commit_message)
    (Rpc (method_ generate_text) (params ((gen_id gen-2) (kind commit_message)))
     (tag (Generated gen-2)))
    Generate_stop
    (Rpc (method_ generate_cancel) (params ((gen_id gen-2))) (tag Ignore))
    Commit message
    (✦ Generate) (Undo)
    [Part]
    (☐ Amend) (Commit 1 file)
    |}]
;;

let%expect_test "push: refused, then pushed; detached HEAD" =
  let h = opened () in
  H.reply h "git_status" (status_json [ file "src/main.ml" ~worktree:"M" ]);
  H.click h ~selector:".git-actions button" ~nth:1;
  [%expect
    {|
    (Complete_accept (run true))
    (Git Push)
    (Rpc (method_ git_push) (params ()) (tag (Git Pushed)))
    |}];
  H.fail
    h
    "git_push"
    "git push origin feat/x was refused: ! [rejected] (the remote has commits \
     this branch does not: pull first, e.g. git pull --rebase, then push \
     again)";
  H.text h ~selector:".git-failure";
  [%expect
    {|
    (Rpc (method_ git_status) (params ()) (tag (Git Status)))
    git push origin feat/x was refused: ! [rejected] (the remote has commits this branch does not: pull first, e.g. git pull --rebase, then push again)
    |}];
  H.reply h "git_status" (status_json [ file "src/main.ml" ~worktree:"M" ]);
  H.click h ~selector:".git-actions button" ~nth:1;
  H.reply
    h
    "git_push"
    (sprintf
       {|{"remote":"origin","branch":"feat/x","status":%s}|}
       (status_json ~ahead:0 [ file "src/main.ml" ~worktree:"M" ]));
  H.text h ~selector:".git-notice";
  H.text h ~selector:".git-bar";
  [%expect
    {|
    (Git Push)
    (Rpc (method_ git_push) (params ()) (tag (Git Pushed)))
    Pushed feat/x to origin
    feat/x ↑0 ↓0 origin/feat/x
    0 staged 1 unstaged 0 untracked
    (Stage approved) (Push) (Create PR)
    Pushed feat/x to origin
    |}];
  H.quiet h (fun () -> H.act h (Review Refresh));
  H.reply
    h
    "git_status"
    (status_json ~branch:None [ file "src/main.ml" ~index:"M" ]);
  H.input h ~selector:"#git-message" "x";
  H.click h ~selector:".git-commit-button";
  H.text h ~selector:".git-bar";
  H.text h ~selector:".git-failure";
  [%expect
    {|
    (Git (Message x))
    (Git Commit)
    detached HEAD ↑1 ↓0 origin/feat/x
    1 staged 0 unstaged 0 untracked
    (Stage approved) (Push ↑1) (Create PR)
    HEAD is detached: commit on a branch (git switch -c NAME).
    HEAD is detached: commit on a branch (git switch -c NAME).
    |}]
;;

let%expect_test
    "another worktree: its status, and a pull request for its branch"
  =
  let h = H.create ~state:(state ()) () in
  let tree = "/home/me/.prigh/worktrees/proj/feat-y" in
  H.quiet h (fun () ->
    H.reply h "get_review" {|{"comments":[],"approvals":[]}|};
    H.act h (Review_worktree { path = tree; base = Some "main" }));
  H.reply h "git_diff" (diff_json ~root:tree util_diff);
  H.reply
    h
    "git_status"
    (status_json ~root:tree [ file "src/util.ml" ~worktree:"?" ]);
  H.click h ~selector:".git-actions button" ~nth:2;
  [%expect
    {|
    (Rpc (method_ fs_preview)
     (params ((path /home/me/.prigh/worktrees/proj/feat-y/src/util.ml)))
     (tag (Review (Content src/util.ml))))
    (Git Create_pr)
    (Focus dialog)
    (Rpc (method_ pr_defaults)
     (params ((dir /home/me/.prigh/worktrees/proj/feat-y)))
     (tag (Forge (Defaults s1))))
    |}];
  H.reply
    h
    "pr_defaults"
    {|{"repo":{"host":"github.com","kind":"github","owner":"me","name":"proj","remote":"origin","url":"https://github.com/me/proj"},"head":"feat/y","base":"main","title":"Add util","body":"","commits":["Add util"],"pushed":true}|};
  H.key h "g" ~alt:true ~code:"KeyG" ~target:Field;
  [%expect
    {|
    (Focus pr-title)
    (Generate Pr)
    (Rpc (method_ generate_text)
     (params
      ((gen_id gen-1) (kind pr) (dir /home/me/.prigh/worktrees/proj/feat-y)
       (base main)))
     (tag (Generated gen-1)))
    |}];
  H.event
    h
    {|{"event":"generate_delta","gen_id":"gen-1","delta":"Add the util module\n\nIt adds "}|};
  H.text h ~selector:".modal-body";
  [%expect
    {|
    feat/y into [main] me/proj
    (Stop)
    Title
    [Add the util module]
    Description (Markdown)
    [It adds]
    1 commit
    Add util
    (☐ Open it as a draft) (☐ Push feat/y first)
    |}];
  (* Esc stops the draft first, then closes. *)
  H.key h "Escape" ~target:Field;
  [%expect
    {|
    Close_dialog
    (Rpc (method_ generate_cancel) (params ((gen_id gen-1))) (tag Ignore))
    |}];
  (* The backend answers the cancelled draft. *)
  H.fail h "generate_text" "cancelled";
  H.text h ~selector:".pr-generate";
  [%expect {| (✦ Generate title and description) (Undo) |}];
  H.click h ~selector:".generate-undo";
  H.text h ~selector:".pr-field";
  [%expect
    {|
    (Generate_undo Pr)
    Title
    [Add util]
    Description (Markdown)
    []
    |}];
  H.click h ~selector:".modal .generate-go";
  H.reply
    h
    "generate_text"
    {|{"gen_id":"gen-2","kind":"pr","text":"Add util\n\nBody.","title":"Add util","body":"Body.","model":"m","cost_usd":0}|};
  H.key h "Enter" ~ctrl:true ~target:Multiline;
  [%expect
    {|
    (Generate Pr)
    (Rpc (method_ generate_text)
     (params
      ((gen_id gen-2) (kind pr) (dir /home/me/.prigh/worktrees/proj/feat-y)
       (base main)))
     (tag (Generated gen-2)))
    Dialog_accept
    (Rpc (method_ pr_create)
     (params
      ((dir /home/me/.prigh/worktrees/proj/feat-y) (title "Add util")
       (body Body.) (base main) (head feat/y) (draft false) (push false)))
     (tag (Forge Created)))
    |}]
;;

let%expect_test "Generate for a new worktree's branch and the session's name" =
  let h = H.create ~state:(state ()) () in
  H.quiet h (fun () -> H.act h New_worktree);
  H.input h ~selector:"#worktree-branch" "add a dark mode";
  H.click h ~selector:".generate-go";
  [%expect
    {|
    (New_worktree_input (field Branch) (text "add a dark mode"))
    (Generate Worktree_branch)
    (Rpc (method_ generate_text)
     (params ((gen_id gen-1) (kind branch_name) (task "add a dark mode")))
     (tag (Generated gen-1)))
    |}];
  H.reply
    h
    "generate_text"
    {|{"gen_id":"gen-1","kind":"branch_name","text":"feat/dark-mode","model":"m","cost_usd":0}|};
  H.text h ~selector:".worktree-dialog .field";
  [%expect
    {|
    Branch
    (✦ Generate name) (Undo)
    [feat/dark-mode] A new branch, or an existing one to check out (✦ names one from the task typed here)
    Base
    [] Where a new branch starts (empty: main, where this session is)
    |}];
  H.key h "Escape" ~target:Field;
  H.quiet h (fun () -> H.act h Open_rename);
  H.key h "g" ~alt:true ~code:"KeyG" ~target:Field;
  H.event h {|{"event":"generate_delta","gen_id":"gen-2","delta":"Dark mode"}|};
  H.reply
    h
    "generate_text"
    {|{"gen_id":"gen-2","kind":"session_title","text":"Adding a dark mode","model":"m","cost_usd":0}|};
  H.text h ~selector:".modal";
  [%expect
    {|
    Close_dialog
    (Focus editor)
    (Generate Session_title)
    (Rpc (method_ generate_text) (params ((gen_id gen-2) (kind session_title)))
     (tag (Generated gen-2)))
    Rename session
    (Close (Esc))
    [Adding a dark mode]
    (✦ Generate) (Undo)
    (Cancel) (Rename)
    |}]
;;
