open! Core
open! Prigh
open Tool_test_helpers
open! Test_forge_tools

let%expect_test "issue changes on GitHub" =
  with_tools
    Github
    [ route Post "/repos/o/r/issues" ~status:201 (gh_issue ())
    ; route
        Post
        "/repos/o/r/issues/12/comments"
        ~status:201
        (comment_json ~host:"github.com")
    ; route Patch "/repos/o/r/issues/12" (gh_issue ~state:"closed" ())
    ; route
        Post
        "/repos/o/r/issues/12/labels"
        {|[{"name":"bug"},{"name":"ui"}]|}
    ; route Delete "/repos/o/r/issues/12/labels/ui" {|[{"name":"bug"}]|}
    ; route
        Get
        "/repos/o/r/issues/12"
        (gh_issue ~labels:{|[{"name":"bug"}]|} ())
    ; route Post "/repos/o/r/issues/12/assignees" ~status:201 (gh_issue ())
    ]
  @@ fun ~run ->
  run
    "issue"
    {|{"action":"create","title":"Crash on start","body":"It crashes.","labels":["bug"],"assignees":["bob"]}|};
  [%expect
    {|
    POST https://api.github.com/repos/o/r/issues
      Authorization: Bearer tok
      {"title":"Crash on start","body":"It crashes.","labels":["bug"],"assignees":["bob"]}
    opened #12 Crash on start (open)
    https://github.com/o/r/issues/12
    |}];
  run "issue" {|{"action":"comment","number":12,"body":"Looking."}|};
  [%expect
    {|
    POST https://api.github.com/repos/o/r/issues/12/comments
      Authorization: Bearer tok
      {"body":"Looking."}
    commented on #12 (comment 99)
    https://github.com/o/r/issues/12#issuecomment-99
    |}];
  run
    "issue"
    {|{"action":"close","number":12,"reason":"not_planned","body":"Won't fix."}|};
  [%expect
    {|
    POST https://api.github.com/repos/o/r/issues/12/comments
      Authorization: Bearer tok
      {"body":"Won't fix."}
    PATCH https://api.github.com/repos/o/r/issues/12
      Authorization: Bearer tok
      {"state":"closed","state_reason":"not_planned"}
    closed #12 Crash on start (closed)
    https://github.com/o/r/issues/12
    |}];
  run "issue" {|{"action":"label","number":12,"labels":["bug","ui"]}|};
  [%expect
    {|
    POST https://api.github.com/repos/o/r/issues/12/labels
      Authorization: Bearer tok
      {"labels":["bug","ui"]}
    GET https://api.github.com/repos/o/r/issues/12
      Authorization: Bearer tok
    #12 Crash on start: labels bug
    |}];
  run "issue" {|{"action":"unlabel","number":12,"labels":["ui"]}|};
  [%expect
    {|
    DELETE https://api.github.com/repos/o/r/issues/12/labels/ui
      Authorization: Bearer tok
    GET https://api.github.com/repos/o/r/issues/12
      Authorization: Bearer tok
    #12 Crash on start: labels bug
    |}];
  run "issue" {|{"action":"assign","number":12,"assignees":["bob"]}|};
  [%expect
    {|
    POST https://api.github.com/repos/o/r/issues/12/assignees
      Authorization: Bearer tok
      {"assignees":["bob"]}
    GET https://api.github.com/repos/o/r/issues/12
      Authorization: Bearer tok
    #12 Crash on start: assignees none
    |}];
  run "issue" {|{"action":"frobnicate","number":12}|};
  [%expect
    {| ERROR: invalid arguments: unknown action "frobnicate": use list, view, create, comment, edit, close, reopen, label, unlabel, assign or unassign |}]
;;

let%expect_test "issues on Forgejo: labels by id, assignees by edit" =
  with_tools
    Forgejo
    [ route Get "/api/v1/repos/o/r/issues" (sprintf "[%s]" (fj_issue ()))
    ; route
        Get
        "/api/v1/repos/o/r/labels"
        {|[{"id":3,"name":"bug"},{"id":4,"name":"ui"}]|}
    ; route Post "/api/v1/repos/o/r/issues" ~status:201 (fj_issue ())
    ; route
        Post
        "/api/v1/repos/o/r/issues/12/labels"
        {|[{"id":3,"name":"bug"},{"id":4,"name":"ui"}]|}
    ; route Delete "/api/v1/repos/o/r/issues/12/labels/4" ~status:204 ""
    ; route
        Get
        "/api/v1/repos/o/r/issues/12"
        (fj_issue ~labels:{|[{"name":"bug"}]|} ())
    ; route Patch "/api/v1/repos/o/r/issues/12" (fj_issue ~state:"closed" ())
    ]
  @@ fun ~run ->
  run "issue" {|{"action":"list","labels":["bug"],"search":"crash"}|};
  [%expect
    {|
    GET https://codeberg.org/api/v1/repos/o/r/issues?limit=50&state=open&type=issues&labels=bug&q=crash
      Authorization: token tok
    1 open issue in o/r on codeberg.org:
    #12 Crash on start (open, by alice, 0 comments, updated 2026-10-21)
    |}];
  run
    "issue"
    {|{"action":"create","title":"Crash on start","labels":["bug","nope"]}|};
  [%expect
    {|
    GET https://codeberg.org/api/v1/repos/o/r/labels?limit=50
      Authorization: token tok
    GET https://codeberg.org/api/v1/orgs/o/labels?limit=50
      Authorization: token tok
    ERROR: issue create: o/r has no label nope (it has bug, ui)
    |}];
  run "issue" {|{"action":"label","number":12,"labels":["bug","ui"]}|};
  [%expect
    {|
    GET https://codeberg.org/api/v1/repos/o/r/labels?limit=50
      Authorization: token tok
    POST https://codeberg.org/api/v1/repos/o/r/issues/12/labels
      Authorization: token tok
      {"labels":[3,4]}
    GET https://codeberg.org/api/v1/repos/o/r/issues/12
      Authorization: token tok
    #12 Crash on start: labels bug
    |}];
  run "issue" {|{"action":"unlabel","number":12,"labels":["ui"]}|};
  [%expect
    {|
    GET https://codeberg.org/api/v1/repos/o/r/labels?limit=50
      Authorization: token tok
    DELETE https://codeberg.org/api/v1/repos/o/r/issues/12/labels/4
      Authorization: token tok
    GET https://codeberg.org/api/v1/repos/o/r/issues/12
      Authorization: token tok
    #12 Crash on start: labels bug
    |}];
  run "issue" {|{"action":"assign","number":12,"assignees":["bob"]}|};
  [%expect
    {|
    GET https://codeberg.org/api/v1/repos/o/r/issues/12
      Authorization: token tok
    PATCH https://codeberg.org/api/v1/repos/o/r/issues/12
      Authorization: token tok
      {"assignees":["bob"]}
    #12 Crash on start: assignees none
    |}];
  run "issue" {|{"action":"close","number":12,"reason":"not_planned"}|};
  [%expect
    {|
    PATCH https://codeberg.org/api/v1/repos/o/r/issues/12
      Authorization: token tok
      {"state":"closed"}
    closed #12 Crash on start (closed)
    https://codeberg.org/o/r/issues/12
    |}]
;;

let%expect_test "pull request changes on GitHub" =
  with_tools
    Github
    [ route Get "/repos/o/r/pulls/7" (gh_pr ~draft:true ())
    ; route Patch "/repos/o/r/pulls/7" (gh_pr ())
    ; route
        Post
        "/repos/o/r/issues/7/comments"
        ~status:201
        (comment_json ~host:"github.com")
    ; route
        Post
        "/graphql"
        {|{"data":{"markPullRequestReadyForReview":{"pullRequest":{"isDraft":false}}}}|}
    ; route Put "/repos/o/r/pulls/7/merge" {|{"merged":true,"sha":"def"}|}
    ; route Get "/repos/o/r" {|{"full_name":"o/r","default_branch":"main"}|}
    ; route Delete "/repos/o/r/git/refs/heads/feat/x" ~status:204 ""
    ; route
        Post
        "/repos/o/r/pulls/7/reviews"
        {|{"id":5,"user":{"login":"octocat"},"state":"APPROVED","body":"LGTM","submitted_at":"2026-10-22T10:00:00Z","html_url":"u"}|}
    ; route
        Post
        "/repos/o/r/pulls/7/comments/40/replies"
        ~status:201
        {|{"id":41,"user":{"login":"octocat"},"path":"a","line":3,"body":"Done.","in_reply_to_id":40,"created_at":"2026-10-22T10:00:00Z","html_url":"u41"}|}
    ; route Post "/repos/o/r/pulls/7/requested_reviewers" ~status:201 (gh_pr ())
    ]
  @@ fun ~run ->
  run "pull_request" {|{"action":"edit","number":7,"closes":[12,13]}|};
  [%expect
    {|
    GET https://api.github.com/repos/o/r/pulls/7
      Authorization: Bearer tok
    PATCH https://api.github.com/repos/o/r/pulls/7
      Authorization: Bearer tok
      {"body":"Does x.\n\nCloses #12\nCloses #13"}
    edited #7 Add x (open)
    https://github.com/o/r/pull/7
    |}];
  run "pull_request" {|{"action":"comment","number":7,"body":"Looking."}|};
  [%expect
    {|
    POST https://api.github.com/repos/o/r/issues/7/comments
      Authorization: Bearer tok
      {"body":"Looking."}
    commented on #7 (comment 99)
    https://github.com/o/r/issues/12#issuecomment-99
    |}];
  run "pull_request" {|{"action":"ready","number":7}|};
  [%expect
    {|
    GET https://api.github.com/repos/o/r/pulls/7
      Authorization: Bearer tok
    POST https://api.github.com/graphql
      Authorization: Bearer tok
      {"query":"mutation($id: ID!) { markPullRequestReadyForReview(input: {pullRequestId: $id}) { pullRequest { isDraft } } }","variables":{"id":"PR_7"}}
    ready for review: #7 Add x (open)
    https://github.com/o/r/pull/7
    |}];
  run
    "pull_request"
    {|{"action":"merge","number":7,"method":"squash","delete_branch":true}|};
  [%expect
    {|
    GET https://api.github.com/repos/o/r/pulls/7
      Authorization: Bearer tok
    GET https://api.github.com/repos/o/r
      Authorization: Bearer tok
    PUT https://api.github.com/repos/o/r/pulls/7/merge
      Authorization: Bearer tok
      {"merge_method":"squash","sha":"abc123"}
    DELETE https://api.github.com/repos/o/r/git/refs/heads/feat/x
      Authorization: Bearer tok
    merged #7 Add x (squash) into main, deleted feat/x
    https://github.com/o/r/pull/7
    |}];
  run
    "pull_request"
    {|{"action":"review","number":7,"event":"approve","body":"LGTM","comments":[{"path":"a","line":3,"body":"nit"}]}|};
  [%expect
    {|
    GET https://api.github.com/repos/o/r/pulls/7
      Authorization: Bearer tok
    POST https://api.github.com/repos/o/r/pulls/7/reviews
      Authorization: Bearer tok
      {"event":"APPROVE","body":"LGTM","commit_id":"abc123","comments":[{"path":"a","line":3,"side":"RIGHT","body":"nit"}]}
    reviewed #7 Add x: APPROVED, 1 line comment (review 5)
    https://github.com/o/r/pull/7
    |}];
  run
    "pull_request"
    {|{"action":"reply","number":7,"comment_id":40,"body":"Done."}|};
  [%expect
    {|
    POST https://api.github.com/repos/o/r/pulls/7/comments/40/replies
      Authorization: Bearer tok
      {"body":"Done."}
    replied to comment 40 on #7 (comment 41)
    u41
    |}];
  run
    "pull_request"
    {|{"action":"request_reviewers","number":7,"reviewers":["bob"]}|};
  [%expect
    {|
    POST https://api.github.com/repos/o/r/pulls/7/requested_reviewers
      Authorization: Bearer tok
      {"reviewers":["bob"]}
    asked bob to review #7
    |}]
;;

let%expect_test "pull request changes on Forgejo" =
  with_tools
    Forgejo
    [ route Get "/api/v1/repos/o/r/pulls/7" (fj_pr ())
    ; route Patch "/api/v1/repos/o/r/pulls/7" (fj_pr ~title:"Add x" ())
    ; route Post "/api/v1/repos/o/r/pulls/7/merge" ""
    ; route
        Get
        "/api/v1/repos/o/r"
        {|{"full_name":"o/r","default_branch":"main"}|}
    ; route
        Post
        "/api/v1/repos/o/r/pulls/7/reviews"
        {|{"id":5,"user":{"login":"octocat"},"state":"REQUEST_CHANGES","body":"Fix a.","submitted_at":"2026-10-22T10:00:00Z","html_url":"u"}|}
    ; route
        Get
        "/api/v1/repos/o/r/pulls/7/reviews"
        {|[{"id":5,"user":{"login":"bob"},"state":"COMMENT","body":"","submitted_at":"2026-10-22T10:00:00Z","html_url":"u"}]|}
    ; route
        Get
        "/api/v1/repos/o/r/pulls/7/reviews/5/comments"
        {|[{"id":40,"pull_request_review_id":5,"user":{"login":"bob"},"path":"a","position":3,"body":"Why?","created_at":"2026-10-22T10:00:00Z","html_url":"u40"}]|}
    ; route Get "/api/v1/repos/o/r/issues/7/comments" "[]"
    ]
  @@ fun ~run ->
  run "pull_request" {|{"action":"ready","number":7}|};
  [%expect
    {|
    GET https://codeberg.org/api/v1/repos/o/r/pulls/7
      Authorization: token tok
    PATCH https://codeberg.org/api/v1/repos/o/r/pulls/7
      Authorization: token tok
      {"title":"Add x"}
    ready for review: #7 Add x (open)
    https://codeberg.org/o/r/pull/7
    |}];
  run
    "pull_request"
    {|{"action":"merge","number":7,"method":"rebase","delete_branch":true,"title":"Add x (#7)"}|};
  [%expect
    {|
    GET https://codeberg.org/api/v1/repos/o/r/pulls/7
      Authorization: token tok
    GET https://codeberg.org/api/v1/repos/o/r
      Authorization: token tok
    POST https://codeberg.org/api/v1/repos/o/r/pulls/7/merge
      Authorization: token tok
      {"Do":"rebase","delete_branch_after_merge":true,"MergeTitleField":"Add x (#7)","head_commit_id":"abc123"}
    merged #7 WIP: Add x (rebase) into main, deleted feat/x
    https://codeberg.org/o/r/pull/7
    |}];
  run
    "pull_request"
    {|{"action":"review","number":7,"event":"request_changes","body":"Fix a.","comments":[{"path":"a","line":3,"body":"nit"}]}|};
  [%expect
    {|
    GET https://codeberg.org/api/v1/repos/o/r/pulls/7
      Authorization: token tok
    POST https://codeberg.org/api/v1/repos/o/r/pulls/7/reviews
      Authorization: token tok
      {"event":"REQUEST_CHANGES","body":"Fix a.","commit_id":"abc123","comments":[{"path":"a","new_position":3,"body":"nit"}]}
    reviewed #7 WIP: Add x: CHANGES_REQUESTED, 1 line comment (review 5)
    https://codeberg.org/o/r/pull/7
    |}];
  run "pull_request" {|{"action":"comments","number":7}|};
  [%expect
    {|
    GET https://codeberg.org/api/v1/repos/o/r/issues/7/comments?limit=50
      Authorization: token tok
    GET https://codeberg.org/api/v1/repos/o/r/pulls/7/reviews?limit=50
      Authorization: token tok
    GET https://codeberg.org/api/v1/repos/o/r/pulls/7/reviews/5/comments?limit=50
      Authorization: token tok
    comments on #7

    reviews:
    --- bob COMMENTED, 2026-10-22T10:00:00Z (review 5)

    line comments:
    --- bob on a:3, 2026-10-22T10:00:00Z (comment_id 40)
    Why?
    |}];
  run
    "pull_request"
    {|{"action":"reply","number":7,"comment_id":40,"body":"Done."}|};
  [%expect
    {| ERROR: pull_request reply: codeberg.org (Forgejo) cannot reply in a review thread: comment on the pull request instead, quoting the comment |}]
;;

let%expect_test
    "merge never deletes a branch outside the repository, nor its default or \
     base branch"
  =
  let pr ~number ?head_owner ?head_name ?head_deleted ?(head = "feat/x") () =
    Fake_forge.pr_json
      ~host:"github.com"
      ~number
      ~title:"Add x"
      ?head_owner
      ?head_name
      ?head_deleted
      ~head
      ()
  in
  with_tools
    Github
    [ route
        Get
        "/repos/o/r/pulls/7"
        (pr ~number:7 ~head_deleted:true ~head:"main" ())
    ; route
        Get
        "/repos/o/r/pulls/8"
        (pr ~number:8 ~head_name:"r-fork" ~head:"main" ())
    ; route Get "/repos/o/r/pulls/9" (pr ~number:9 ~head:"dev" ())
    ; route Get "/repos/o/r/pulls/10" (pr ~number:10 ~head:"main" ())
    ; route Get "/repos/o/r/pulls/11" (pr ~number:11 ~head:"fix/a#b" ())
    ; route Put "/repos/o/r/pulls/7/merge" {|{"merged":true}|}
    ; route Put "/repos/o/r/pulls/8/merge" {|{"merged":true}|}
    ; route Put "/repos/o/r/pulls/11/merge" {|{"merged":true}|}
    ; route Delete "/repos/o/r/git/refs/heads/fix/a%23b" ~status:204 ""
    ; route Get "/repos/o/r" {|{"full_name":"o/r","default_branch":"dev"}|}
    ]
  @@ fun ~run ->
  let merge n =
    run
      "pull_request"
      (sprintf {|{"action":"merge","number":%d,"delete_branch":true}|} n)
  in
  (* a deleted fork's head: [head.repo] is null *)
  merge 7;
  [%expect
    {|
    GET https://api.github.com/repos/o/r/pulls/7
      Authorization: Bearer tok
    PUT https://api.github.com/repos/o/r/pulls/7/merge
      Authorization: Bearer tok
      {"merge_method":"merge","sha":"abc123"}
    merged #7 Add x (merge) into main, kept main: it is in another repository (a fork, or one that was deleted), not this one
    https://github.com/o/r/pull/7
    |}];
  (* a fork with the same owner *)
  merge 8;
  [%expect
    {|
    GET https://api.github.com/repos/o/r/pulls/8
      Authorization: Bearer tok
    PUT https://api.github.com/repos/o/r/pulls/8/merge
      Authorization: Bearer tok
      {"merge_method":"merge","sha":"abc123"}
    merged #8 Add x (merge) into main, kept main: it is in another repository (a fork, or one that was deleted), not this one
    https://github.com/o/r/pull/8
    |}];
  (* the default branch, then the base: refused before merging *)
  merge 9;
  [%expect
    {|
    GET https://api.github.com/repos/o/r/pulls/9
      Authorization: Bearer tok
    GET https://api.github.com/repos/o/r
      Authorization: Bearer tok
    ERROR: pull_request merge: refusing to delete dev: it is the repository's default branch; merge without delete_branch
    |}];
  merge 10;
  [%expect
    {|
    GET https://api.github.com/repos/o/r/pulls/10
      Authorization: Bearer tok
    GET https://api.github.com/repos/o/r
      Authorization: Bearer tok
    ERROR: pull_request merge: refusing to delete main: it is the pull request's base branch; merge without delete_branch
    |}];
  (* a branch name with [#]: encoded, its [/] kept *)
  merge 11;
  [%expect
    {|
    GET https://api.github.com/repos/o/r/pulls/11
      Authorization: Bearer tok
    GET https://api.github.com/repos/o/r
      Authorization: Bearer tok
    PUT https://api.github.com/repos/o/r/pulls/11/merge
      Authorization: Bearer tok
      {"merge_method":"merge","sha":"abc123"}
    DELETE https://api.github.com/repos/o/r/git/refs/heads/fix/a%23b
      Authorization: Bearer tok
    merged #11 Add x (merge) into main, deleted fix/a#b
    https://github.com/o/r/pull/11
    |}]
;;

let%expect_test "repo view, runs, run, logs and rerun on GitHub" =
  with_tools
    Github
    [ route
        Get
        "/repos/o/r"
        {|{"full_name":"o/r","description":"Does r.","default_branch":"main","private":false,"fork":false,"archived":false,"stargazers_count":5,"forks_count":1,"open_issues_count":2,"html_url":"https://github.com/o/r","homepage":""}|}
    ; route Get "/repos/o/r/readme" "# r\n\nDoes r.\n"
    ; route
        Get
        "/repos/o/r/actions/runs"
        {|{"workflow_runs":[{"id":100,"name":"CI","display_title":"Add x","status":"completed","conclusion":"failure","head_branch":"feat/x","head_sha":"abc123","event":"pull_request","html_url":"https://github.com/o/r/actions/runs/100","created_at":"2026-10-22T10:00:00Z"}]}|}
    ; route
        Get
        "/repos/o/r/actions/runs/100"
        {|{"id":100,"name":"CI","display_title":"Add x","status":"completed","conclusion":"failure","head_branch":"feat/x","head_sha":"abc123","event":"pull_request","html_url":"https://github.com/o/r/actions/runs/100","created_at":"2026-10-22T10:00:00Z"}|}
    ; route
        Get
        "/repos/o/r/actions/runs/100/jobs"
        {|{"jobs":[{"id":200,"name":"build","status":"completed","conclusion":"failure","html_url":"u","steps":[{"name":"Checkout","status":"completed","conclusion":"success"},{"name":"Test","status":"completed","conclusion":"failure"}]}]}|}
    ; route
        Get
        "/repos/o/r/actions/jobs/200/logs"
        ~status:302
        ~headers:[ "Location", "https://blob.example/logs/200?sig=x" ]
        ""
    ; route Get "/logs/200" "line 1\nFAIL: test_x\n"
    ; route
        Post
        "/repos/o/r/actions/runs/100/rerun-failed-jobs"
        ~status:201
        "{}"
    ]
  @@ fun ~run ->
  run "repo" {|{"action":"view"}|};
  [%expect
    {|
    GET https://api.github.com/repos/o/r
      Authorization: Bearer tok
    GET https://api.github.com/repos/o/r/readme
      Authorization: Bearer tok
      Accept: application/vnd.github.raw
    o/r: Does r.
    public, default branch main, 5 stars, 1 fork, 2 open issues
    https://github.com/o/r

    README:
    # r

    Does r.
    |}];
  run "repo" {|{"action":"runs","branch":"feat/x"}|};
  [%expect
    {|
    GET https://api.github.com/repos/o/r/actions/runs?per_page=100&branch=feat/x
      Authorization: Bearer tok
    CI runs in o/r on github.com, newest first:
    100 CI: Add x [failure] on feat/x (pull_request, 2026-10-22T10:00) https://github.com/o/r/actions/runs/100
    |}];
  run "repo" {|{"action":"run","run":100}|};
  [%expect
    {|
    GET https://api.github.com/repos/o/r/actions/runs/100
      Authorization: Bearer tok
    GET https://api.github.com/repos/o/r/actions/runs/100/jobs?per_page=100
      Authorization: Bearer tok
    100 CI: Add x [failure] on feat/x (pull_request, 2026-10-22T10:00) https://github.com/o/r/actions/runs/100
      job 200 build [failure]
        Checkout [success]
        Test [failure]
    |}];
  run "repo" {|{"action":"logs","job":200}|};
  [%expect
    {|
    GET https://api.github.com/repos/o/r/actions/jobs/200/logs
      Authorization: Bearer tok
      Accept: */*
    GET https://blob.example/logs/200?sig=x
    line 1
    FAIL: test_x
    |}];
  run "repo" {|{"action":"rerun","run":100}|};
  [%expect
    {|
    POST https://api.github.com/repos/o/r/actions/runs/100/rerun-failed-jobs
      Authorization: Bearer tok
    re-running the failed jobs of run 100 (action=run shows its progress)
    |}]
;;
