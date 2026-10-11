open! Core

module Target = struct
  type t =
    | Uncommitted
    | Base of
        { base : string
        ; merge_base : string
        }
  [@@deriving sexp_of, equal]

  let describe = function
    | Uncommitted -> "uncommitted changes"
    | Base { base; _ } -> sprintf "changes against %s" base
  ;;
end

let split_args args =
  match String.lsplit2 (String.strip args) ~on:' ' with
  | Some (first, rest) -> Some first, String.strip rest
  | None ->
    (match String.strip args with
     | "" -> None, ""
     | word -> Some word, "")
;;

let inline_limit = 30_000

let rubric =
  {|This is a review only: do not modify any files.

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

If nothing qualifies, say "No findings." rather than stretching. End with a line **Verdict:** "patch is correct" or "patch is incorrect" (correct means existing code and tests will not break and there are no blocking bugs; ignore style, typos and docs), and one to three sentences why.|}
;;

let prompt ~target ~instructions (diff : Git_diff.Result.t) =
  let what, command =
    match (target : Target.t) with
    | Uncommitted ->
      ( sprintf
          "Review the uncommitted changes (staged, unstaged and untracked \
           files) in the git repository at %s and give prioritised findings."
          diff.root
      , sprintf "git diff %s" diff.against )
    | Base { base; merge_base } ->
      ( sprintf
          "Review the changes against the base branch %s in the git repository \
           at %s and give prioritised findings. The merge base is %s: the \
           changes are the working tree compared with it (git diff %s) plus \
           untracked files."
          base
          diff.root
          merge_base
          merge_base
      , sprintf "git diff %s" merge_base )
  in
  let files, added, removed = Git_diff.Result.stats diff in
  let changes =
    if String.length diff.diff <= inline_limit && not diff.truncated
    then
      sprintf
        "The changes (%s, untracked files as new files):\n\n```diff\n%s```"
        command
        diff.diff
    else
      sprintf
        "The changes are too large to include (%d files, +%d -%d): read them \
         with %s and git status, file by file."
        files
        added
        removed
        command
  in
  String.concat
    ~sep:"\n\n"
    (List.filter_opt
       [ Some ("[prigh review] " ^ what)
       ; Some rubric
       ; Option.map instructions ~f:(fun i ->
           "Additional instructions from the user: " ^ i)
       ; Some changes
       ])
;;

let nothing_to_review (target : Target.t) ~root =
  match target with
  | Uncommitted ->
    sprintf
      "nothing to review: %s has no uncommitted changes (/review BRANCH \
       reviews the changes against a branch, e.g. /review main)"
      root
  | Base { base; _ } ->
    sprintf "nothing to review: no changes against %s in %s" base root
;;
