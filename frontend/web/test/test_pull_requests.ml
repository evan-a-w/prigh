open! Core
module H = Harness

(* The session's repository's pull requests: /pr's list (filtered by state
   and by text), one pull request with its checks, description and diff,
   checking one out, and creating one. *)

let repo =
  {|{"host":"github.com","kind":"github","owner":"evan-a-w","name":"prigh","remote":"origin","url":"https://github.com/evan-a-w/prigh"}|}
;;

let pr
      ?(state = "open")
      ?(draft = false)
      ?(body = "")
      ~number
      ~title
      ~author
      ~head
      ()
  =
  sprintf
    {|{"number":%d,"title":%S,"state":"%s","draft":%b,"author":"%s","head":"%s","base":"main","url":"https://github.com/evan-a-w/prigh/pull/%d","body":%S,"created_at":"2026-10-25T10:00:00Z","updated_at":"2026-10-25T11:00:00Z"}|}
    number
    title
    state
    draft
    author
    head
    number
    body
;;

let git_hosting =
  pr
    ~draft:true
    ~body:"Closes #1.\n\n- GitHub and **Forgejo** accounts\n- pull requests"
    ~number:24
    ~title:"Git hosting"
    ~author:"lloyd"
    ~head:"feat/git-hosting"
    ()
;;

let list_json =
  sprintf
    {|{"repo":%s,"prs":[%s,%s,%s]}|}
    repo
    git_hosting
    (pr
       ~number:23
       ~title:"Fix the narrow top bar"
       ~author:"evan"
       ~head:"fix/topbar"
       ())
    (pr ~number:21 ~title:"Faster pages" ~author:"evan" ~head:"perf/pages" ())
;;

let diff =
  "diff --git a/README.md b/README.md\n\
   index 1111111..2222222 100644\n\
   --- a/README.md\n\
   +++ b/README.md\n\
   @@ -1,2 +1,3 @@\n\
  \ # prigh\n\
   +Git hosting.\n\
  \ A coding agent.\n"
;;

let detail_json =
  sprintf
    {|{"repo":%s,"pr":%s,"checks":{"state":"failure","items":[{"name":"build","state":"success","url":"https://ci/1"},{"name":"lint","state":"failure","url":null},{"name":"e2e","state":"pending","url":null}]},"diff":%s,"truncated":false}|}
    repo
    git_hosting
    (Jsonaf.to_string (`String diff))
;;

let open_list h =
  H.type_ h "/pr";
  H.act h Send
;;

let%expect_test "/pr: loading, the list, filtering, a pull request and back" =
  let h = H.create () in
  open_list h;
  H.text h ~selector:".modal";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /pr) (max 500)))
     (tag Ignore))
    (Focus picker-input)
    (Rpc (method_ pr_list) (params ((state open)))
     (tag (Forge (List (session s1) (filter Open)))))
    Pull requests
    (Close (Esc))
    (Open) (Closed) (All)
    []
    Loading the open pull requests…
    ↑↓ to move, Enter to open, Esc to close (New pull request)
    |}];
  H.reply h "pr_list" list_json;
  H.text h ~selector:".modal-body";
  [%expect
    {|
    (evan-a-w/prigh)
    (Open) (Closed) (All)
    []
    Draft #24 Git hosting lloyd · feat/git-hosting → main
    Open #23 Fix the narrow top bar evan · fix/topbar → main
    Open #21 Faster pages evan · perf/pages → main
    |}];
  (* Fuzzy, over the number, author and branches too. *)
  H.act h (Picker_query "topbar");
  H.text h ~selector:".picker-items";
  H.act h (Picker_query "evan");
  H.key h "ArrowDown" ~target:Field;
  H.key h "Enter" ~target:Field;
  [%expect
    {|
    Open #23 Fix the narrow top bar evan · fix/topbar → main
    (Dialog_move 1)
    Dialog_accept
    (Focus dialog)
    (Rpc (method_ pr_view) (params ((number 23)))
     (tag (Forge (View (session s1) (number 23)))))
    |}];
  (* What the list knew shows at once. *)
  H.text h ~selector:".modal-head";
  H.text h ~selector:".modal-body";
  [%expect
    {|
    #23 Fix the narrow top bar
    (Close (Esc))
    Loading pull request #23…
    |}];
  H.key h "Escape" ~target:Page;
  H.text h ~selector:".modal-body";
  [%expect
    {|
    Close_dialog
    (Focus picker-input)
    (evan-a-w/prigh)
    (Open) (Closed) (All)
    [evan]
    Open #21 Faster pages evan · perf/pages → main
    Open #23 Fix the narrow top bar evan · fix/topbar → main
    |}];
  (* Its late answer opens nothing. *)
  H.reply h "pr_view" detail_json;
  H.key h "Escape" ~target:Field;
  H.elements h ~selector:".modal";
  [%expect
    {|
    Close_dialog
    (Focus editor)
    |}]
;;

let%expect_test "a pull request: state, branches, checks, description, diff" =
  let h = H.create () in
  H.type_ h "/pr #24";
  H.quiet h (fun () -> H.act h Send);
  H.reply h "pr_view" detail_json;
  H.text h ~selector:".modal";
  [%expect
    {|
    #24 Git hosting
    (Close (Esc))
    Draft lloyd feat/git-hosting → main evan-a-w/prigh
    Checks 1 passed, 1 failed, 1 running
    ✓ (build) success
    ✕ lint failure
    e2e pending
    Description
    Closes #1.
    GitHub and Forgejo accounts
    pull requests
    Changes 1 file changed, +1 −0
    README.md +1 −0
    @@ -1,2 +1,3 @@
    1 # prigh
    2 + Git hosting.
    3 A coding agent.
    Esc closes (Open on GitHub) (Check out) (Done)
    |}];
  H.show h ~selector:".modal-buttons a";
  [%expect
    {|
    <a href="https://github.com/evan-a-w/prigh/pull/24"
       target="_blank"
       rel="noopener noreferrer"
       class="btn link-button">
      Open on GitHub
      <icon class="external"> </icon>
    </a>
    |}];
  H.key h "Enter" ~target:Page;
  [%expect
    {|
    Dialog_accept
    (Focus editor)
    |}]
;;

let%expect_test "the state filter; errors as the backend says them" =
  let h = H.create () in
  H.quiet h (fun () -> open_list h);
  H.click h ~selector:".segment:nth-child(2)";
  H.text h ~selector:".modal-body";
  [%expect
    {|
    (Pr_filter Closed)
    (Focus picker-input)
    (Rpc (method_ pr_list) (params ((state closed)))
     (tag (Forge (List (session s1) (filter Closed)))))
    (Open) (Closed) (All)
    []
    Loading the closed pull requests…
    |}];
  (* The open ones' answer came too late. *)
  H.reply h "pr_list" list_json;
  H.text h ~selector:".loading-row";
  [%expect {| Loading the closed pull requests… |}];
  H.reply h "pr_list" (sprintf {|{"repo":%s,"prs":[]}|} repo);
  H.text h ~selector:".modal-body";
  [%expect
    {|
    (evan-a-w/prigh)
    (Open) (Closed) (All)
    []
    No closed pull requests in evan-a-w/prigh. New pull request opens one from this branch.
    |}];
  H.act h (Pr_filter All);
  H.fail h "pr_list" "not signed in to github.com: /login github signs in";
  H.text h ~selector:".modal-body";
  [%expect
    {|
    (Focus picker-input)
    (Rpc (method_ pr_list) (params ((state all)))
     (tag (Forge (List (session s1) (filter All)))))
    (Open) (Closed) (All)
    []
    not signed in to github.com: /login github signs in
    |}];
  H.type_ h "/pr view 99";
  H.quiet h (fun () -> H.act h Send);
  H.fail h "pr_view" "github.com: pull request #99 not found in evan-a-w/prigh";
  H.text h ~selector:".modal";
  [%expect
    {|
    Pull request #99
    (Close (Esc))
    github.com: pull request #99 not found in evan-a-w/prigh
    Esc closes (Check out) (Done)
    |}]
;;

let%expect_test "bad arguments say the usage" =
  let h = H.create () in
  List.iter
    [ "/pr view"; "/pr checkout x"; "/pr list merged"; "/pr frob" ]
    ~f:(fun command ->
      H.quiet h (fun () ->
        H.type_ h command;
        H.act h Send);
      H.text h ~selector:".toast.error:last-child span");
  [%expect
    {|
    /pr view needs a number: /pr view 24 (/pr lists them)
    /pr checkout needs a number: /pr checkout 24 (/pr lists them)
    /pr list takes open, closed or all: /pr list closed
    Unknown /pr argument “frob”: /pr [list [open|closed|all] | view N | new [title] | checkout N]
    |}]
;;

let checkout_json =
  {|{"path":"/work-wt/pr-24","branch":"pr-24","created":true}|}
;;

let%expect_test "checking out from the pull request, then working there" =
  let h = H.create () in
  H.type_ h "/pr 24";
  H.quiet h (fun () -> H.act h Send);
  H.reply h "pr_view" detail_json;
  H.click h ~selector:".modal-buttons .btn:nth-child(3)";
  H.text h ~selector:".loading-row";
  [%expect
    {|
    (Pr_checkout 24)
    (Rpc (method_ pr_checkout) (params ((number 24)))
     (tag (Forge (Checkout (session s1) (number 24)))))
    Checking out #24…
    |}];
  H.reply h "pr_checkout" checkout_json;
  H.text h ~selector:".pr-checkout";
  H.click h ~selector:".pr-checkout .btn";
  [%expect
    {|
    Checked out in a new worktree: /work-wt/pr-24 (branch pr-24).
    (Work there)
    (Run "/cd /work-wt/pr-24")
    (Rpc (method_ set_cwd) (params ((path /work-wt/pr-24) (if_missing ask)))
     (tag (Dir_changed Cwd)))
    |}]
;;

let%expect_test "/pr checkout N from the editor offers to work there" =
  let h = H.create () in
  H.type_ h "/pr checkout 24";
  H.quiet h (fun () -> H.act h Send);
  H.text h ~selector:".toast";
  H.reply h "pr_checkout" checkout_json;
  H.text h ~selector:".modal";
  [%expect
    {|
    Checking out pull request #24…
    (Focus dialog)
    Checked out #24
    (Close (Esc))
    Pull request #24 is in a new worktree at /work-wt/pr-24 , on branch pr-24.
    Work there? The session's tools then run in it.
    (Stay here) (Work there)
    |}];
  H.key h "Escape" ~target:Page;
  [%expect
    {|
    Close_dialog
    (Focus editor)
    |}];
  H.type_ h "/pr checkout 24";
  H.quiet h (fun () -> H.act h Send);
  H.reply h "pr_checkout" checkout_json;
  H.key h "Enter" ~target:Page;
  [%expect
    {|
    (Focus dialog)
    Dialog_accept
    (Focus editor)
    (Rpc (method_ set_cwd) (params ((path /work-wt/pr-24) (if_missing ask)))
     (tag (Dir_changed Cwd)))
    |}];
  H.type_ h "/pr checkout 7";
  H.quiet h (fun () -> H.act h Send);
  H.fail h "pr_checkout" "not a git repository: /x (/cd into one)";
  H.text h ~selector:".toast.error";
  [%expect {| not a git repository: /x (/cd into one) (Dismiss) |}]
;;

let defaults_json =
  sprintf
    {|{"repo":%s,"head":"feat/x","base":"main","title":"Add x","body":"- Add x\n- Test x","commits":["Add x","Test x"],"pushed":false}|}
    repo
;;

let%expect_test "/pr new: prefilled, edited, refused, then created" =
  let h = H.create () in
  H.type_ h "/pr new Add the x feature";
  H.act h Send;
  H.text h ~selector:".modal";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/pr new Add the x feature") (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ pr_defaults) (params ()) (tag (Forge (Defaults s1))))
    New pull request
    (Close (Esc))
    Reading the branch…
    Ctrl+Enter creates it (Cancel) (Create pull request)
    |}];
  H.reply h "pr_defaults" defaults_json;
  H.text h ~selector:".modal";
  [%expect
    {|
    (Focus pr-title)
    New pull request
    (Close (Esc))
    feat/x into [main] evan-a-w/prigh
    Title
    [Add the x feature]
    Description (Markdown)
    [- Add x
    - Test x]
    2 commits
    Add x
    Test x
    (☐ Open it as a draft) (☑ Push feat/x first)
    feat/x has commits the remote doesn't: push it first.
    Ctrl+Enter creates it (Cancel) (Create pull request)
    |}];
  H.click h ~selector:".pr-toggle";
  H.input h ~selector:"#pr-body" "Adds x.\n\nTested.";
  (* Enter in the description is a new line; Ctrl+Enter creates. *)
  H.key h "Enter" ~target:Multiline;
  H.key h "Enter" ~ctrl:true ~target:Multiline;
  H.text h ~selector:".modal-buttons";
  [%expect
    {|
    (Pr_form Toggle_draft)
    (Pr_form (Body  "Adds x.\
                   \n\
                   \nTested."))
    (browser default)
    Dialog_accept
    (Rpc (method_ pr_create)
     (params
      ((title "Add the x feature") (body  "Adds x.\
                                         \n\
                                         \nTested.")
       (base main) (head feat/x) (draft true) (push true)))
     (tag (Forge Created)))
    Ctrl+Enter creates it (Cancel) (Creating…)
    |}];
  H.fail
    h
    "pr_create"
    "git push -u origin feat/x failed: rejected (fetch first)";
  H.text h ~selector:".modal-body";
  [%expect
    {|
    feat/x into [main] evan-a-w/prigh
    Title
    [Add the x feature]
    Description (Markdown)
    [Adds x.

    Tested.]
    2 commits
    Add x
    Test x
    (☑ Open it as a draft) (☑ Push feat/x first)
    feat/x has commits the remote doesn't: push it first.
    git push -u origin feat/x failed: rejected (fetch first)
    |}];
  H.key h "Enter" ~target:Field;
  H.reply
    h
    "pr_create"
    (sprintf
       {|{"repo":%s,"pr":%s}|}
       repo
       (pr
          ~draft:true
          ~number:25
          ~title:"Add the x feature"
          ~author:"lloyd"
          ~head:"feat/x"
          ()));
  H.elements h ~selector:".modal";
  H.text h ~selector:".toast";
  [%expect
    {|
    Dialog_accept
    (Rpc (method_ pr_create)
     (params
      ((title "Add the x feature") (body  "Adds x.\
                                         \n\
                                         \nTested.")
       (base main) (head feat/x) (draft true) (push true)))
     (tag (Forge Created)))
    (Focus editor)
    (Expire_toast (id 0) (after_ms 3660))
    #25 created: https://github.com/evan-a-w/prigh/pull/25
    |}]
;;

let%expect_test
    "New pull request from the list: Esc goes back; created, the list refreshes"
  =
  let h = H.create () in
  H.quiet h (fun () -> open_list h);
  H.reply h "pr_list" list_json;
  H.click h ~selector:".modal-buttons .btn";
  H.reply h "pr_defaults" defaults_json;
  H.input h ~selector:"#pr-title" "";
  H.key h "Enter" ~target:Field;
  H.text h ~selector:".dialog-error";
  [%expect
    {|
    New_pr
    (Focus dialog)
    (Rpc (method_ pr_defaults) (params ()) (tag (Forge (Defaults s1))))
    (Focus pr-title)
    (Pr_form (Title ""))
    Dialog_accept
    Give the pull request a title.
    |}];
  H.key h "Escape" ~target:Field;
  H.text h ~selector:".picker-items";
  [%expect
    {|
    Close_dialog
    (Focus picker-input)
    Draft #24 Git hosting lloyd · feat/git-hosting → main
    Open #23 Fix the narrow top bar evan · fix/topbar → main
    Open #21 Faster pages evan · perf/pages → main
    |}];
  H.quiet h (fun () ->
    H.act h New_pr;
    H.reply h "pr_defaults" defaults_json);
  H.key h "Enter" ~target:Field;
  H.reply
    h
    "pr_create"
    (sprintf
       {|{"repo":%s,"pr":%s}|}
       repo
       (pr ~number:25 ~title:"Add x" ~author:"lloyd" ~head:"feat/x" ()));
  H.text h ~selector:".modal-body";
  [%expect
    {|
    Dialog_accept
    (Rpc (method_ pr_create)
     (params
      ((title "Add x") (body  "- Add x\
                             \n- Test x")
       (base main) (head feat/x) (draft false) (push true)))
     (tag (Forge Created)))
    (Focus picker-input)
    (Rpc (method_ pr_list) (params ((state open)))
     (tag (Forge (List (session s1) (filter Open)))))
    (Expire_toast (id 0) (after_ms 3660))
    (Open) (Closed) (All)
    []
    Loading the open pull requests…
    |}]
;;

let%expect_test "outside a repository the create dialog says what to do" =
  let h = H.create () in
  H.type_ h "/pr new";
  H.quiet h (fun () -> H.act h Send);
  H.fail
    h
    "pr_defaults"
    "no GitHub or Forgejo remote in /work (git remote -v lists none): add one \
     with git remote add origin URL";
  H.text h ~selector:".modal";
  H.key h "Enter" ~target:Page;
  [%expect
    {|
    New pull request
    (Close (Esc))
    no GitHub or Forgejo remote in /work (git remote -v lists none): add one with git remote add origin URL
    (Close)
    Dialog_accept
    |}]
;;

let%expect_test
    "the session card's branch opens the list; switching session closes it and \
     ignores its answers"
  =
  let h = H.create () in
  H.click h ~selector:".session-card .branch";
  [%expect
    {|
    (Run /pr)
    (Focus picker-input)
    (Rpc (method_ pr_list) (params ((state open)))
     (tag (Forge (List (session s1) (filter Open)))))
    |}];
  H.quiet h (fun () ->
    H.event
      h
      (sprintf
         {|{"event":"state","state":%s}|}
         (H.state_json ~fields:[ "session_id", `String "s2" ] ())));
  H.elements h ~selector:".modal";
  H.quiet h (fun () -> open_list h);
  (* s1's answer is not s2's. *)
  H.reply h "pr_list" list_json;
  H.text h ~selector:".loading-row";
  [%expect {| Loading the open pull requests… |}]
;;

let%expect_test
    "keys: Enter is a new line in the description, Ctrl+Enter accepts"
  =
  let h = H.create () in
  H.quiet h (fun () ->
    H.type_ h "/pr new";
    H.act h Send;
    H.reply h "pr_defaults" defaults_json);
  H.key h "Enter" ~target:Multiline;
  H.key h "Enter" ~shift:true ~target:Multiline;
  H.key h "Enter" ~meta:true ~target:Multiline;
  [%expect
    {|
    (browser default)
    (browser default)
    Dialog_accept
    (Rpc (method_ pr_create)
     (params
      ((title "Add x") (body  "- Add x\
                             \n- Test x")
       (base main) (head feat/x) (draft false) (push true)))
     (tag (Forge Created)))
    |}]
;;
