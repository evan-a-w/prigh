open! Core
open! Prigh
open Tool_test_helpers
open! Test_git_review

let%expect_test "/review: uncommitted changes, a base, instructions, nothing" =
  let sent = Queue.create () in
  with_agent
    ~on_request:(fun request ->
      match List.last request.messages with
      | Some (Message.User u) -> Queue.enqueue sent u.text
      | _ -> ())
    [ Reply.text
        "**[P1] Check the count**\n\
         `a.txt:2` ...\n\n\
         **Verdict:** patch is incorrect"
    ; Reply.text "No findings."
    ; Reply.text "No findings either."
    ]
  @@ fun t agent ->
  result t (Agent.review agent ~args:"");
  [%expect
    {| error: not a git repository: $DIR (git init starts one, or /cd into one) |}];
  sh
    t
    "git init -q -b main . && echo .sessions > .gitignore && echo one > a.txt \
     && git add -A && git commit -q -m init && git checkout -q -b feature";
  result t (Agent.review agent ~args:"");
  result t (Agent.review agent ~args:"main");
  [%expect
    {|
    error: nothing to review: $DIR has no uncommitted changes (/review BRANCH reviews the changes against a branch, e.g. /review main)
    error: nothing to review: no changes against main in $DIR
    |}];
  write t "a.txt" "one\ntwo\n";
  result t (Agent.review agent ~args:"");
  Agent.wait_idle agent;
  user_messages t agent;
  [%expect
    {|
    ok
    (((notes
       (((kind (Client (client Other)))
         (text
          "[Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]"))))
      (source (Review (target "uncommitted changes")))))
    [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]

    [prigh review] Review the uncommitted changes (staged, unstaged and untracked files) in the git repository at $DIR and give prioritised findings.

    This is a review only: do not modify any files.

    Flag an issue only when all of these hold:
    1. It meaningfully affects the correctness, performance, security or maintainability of the code.
    2. It is discrete and actionable, not a general complaint about the codebase.
    3. It was introduced by these changes; pre-existing problems are out of scope.
    4. The author would fix it if they knew about it, and it is clearly not intentional.
    5. It does not rest on unstated assumptions: if you claim other code breaks, name the code that provably does.
    6. Fixing it does not demand more rigour than the rest of the codebase has.
    Ignore trivial style unless it obscures meaning or breaks the project's documented conventions (AGENTS.md, CLAUDE.md and the like, which win over these guidelines).

    Report every finding the author would want, most severe first, each as:

    **[P1] Imperative title of at most 80 characters**
    `path/to/file.ext:LINE` (or `LINE-LINE`: the shortest range, inside the changed lines)
    One matter-of-fact paragraph: why it is a bug and the inputs or circumstances it needs. No code longer than 3 lines; a ```suggestion block only for a concrete replacement.

    Priorities: [P0] drop everything to fix, blocks release or major usage regardless of inputs; [P1] urgent, fix next; [P2] normal, fix eventually; [P3] low, nice to have.

    If nothing qualifies, say "No findings." rather than stretching. End with a line **Verdict:** "patch is correct" or "patch is incorrect" (correct means existing code and tests will not break and there are no blocking bugs; ignore style, typos and docs), and one to three sentences why.

    The changes (git diff HEAD, untracked files as new files):

    ```diff
    diff --git a/a.txt b/a.txt
    index <ids> 100644
    --- a/a.txt
    +++ b/a.txt
    @@ -1 +1,2 @@
     one
    +two
    ```
    assistant: **[P1] Check the count**
    `a.txt:2` ...

    **Verdict:** patch is incorrect
    |}];
  (* A first word that names a revision is the base; otherwise all of it is
     instructions. *)
  sh t "git commit -q -am two";
  result t (Agent.review agent ~args:"main  look at the error handling");
  Agent.wait_idle agent;
  result t (Agent.review agent ~args:"look at the error handling");
  [%expect
    {|
    ok
    error: nothing to review: $DIR has no uncommitted changes (/review BRANCH reviews the changes against a branch, e.g. /review main)
    |}];
  List.iter (Queue.to_list sent) ~f:(fun text ->
    print_endline
      (String.concat
         ~sep:"\n"
         (List.filter
            (String.split_lines
               (mask
                  t
                  (Re.replace_string
                     (Re.compile (Re.Perl.re {|[0-9a-f]{40}|}))
                     ~by:"<sha>"
                     text)))
            ~f:(fun line ->
              List.exists
                [ "[prigh review]"; "Additional"; "The changes" ]
                ~f:(fun prefix -> String.is_prefix line ~prefix)))));
  [%expect
    {|
    [prigh review] Review the uncommitted changes (staged, unstaged and untracked files) in the git repository at $DIR and give prioritised findings.
    The changes (git diff HEAD, untracked files as new files):
    [prigh review] Review the changes against the base branch main in the git repository at $DIR and give prioritised findings. The merge base is <sha>: the changes are the working tree compared with it (git diff <sha>) plus untracked files.
    Additional instructions from the user: look at the error handling
    The changes (git diff <sha>, untracked files as new files):
    |}]
;;
