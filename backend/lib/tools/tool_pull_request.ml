open! Core
open! Import

let name = "pull_request"

let changes action =
  List.mem [ "create"; "checkout" ] action ~equal:String.equal
;;

let spec =
  { Tool_spec.name
  ; parallel_safe = false
  ; on_host = false
  ; freeform = None
  ; destructive =
      Depends
        (fun args ->
          Option.value_map
            (Tool_args.string_opt args "action")
            ~default:true
            ~f:changes)
  ; description =
      "Pull requests of the working directory's repository on its git host \
       (GitHub or Forgejo), through the user's sign-in to that host in prigh: \
       no gh CLI or git credentials needed. list shows them (state open, \
       closed or all). view shows one (number): its branches, author, \
       description, CI checks and URL; checks shows only its CI checks; diff \
       its unified diff. create opens one from head (default: the current \
       branch) into base (default: the repository's default branch), pushing \
       the branch first unless push is false; draft marks it a draft. \
       checkout fetches one into the branch pr-N in a worktree at the \
       standard location and records it on the session: work there by giving \
       its path (cd <path> in bash, paths under it in file tools, a \
       subagent's cwd)."
  ; parameters =
      Tool_args.schema
        ~required:[ "action" ]
        [ "action", `String, "list, view, checks, diff, create or checkout"
        ; "state", `String, "list: open (default), closed or all"
        ; "number", `Integer, "view, checks, diff, checkout: the pull request"
        ; ( "title"
          , `String
          , "create: its title (default: from the branch's commits: the \
             subject of a single one, else the branch name)" )
        ; ( "body"
          , `String
          , "create: its description (Markdown): what changed, why, how it was \
             tested (when the title is left out too: from the commits)" )
        ; ( "base"
          , `String
          , "create: the branch to merge into (default: the repository's \
             default branch)" )
        ; ( "head"
          , `String
          , "create: the branch to merge (default: the current branch; \
             OWNER:BRANCH for a fork's)" )
        ; "draft", `Boolean, "create: as a draft (default false)"
        ; ( "push"
          , `Boolean
          , "create: push head to the remote first (default true)" )
        ; ( "cwd"
          , `String
          , "A directory in the repository, e.g. another worktree (default: \
             this agent's working directory)" )
        ]
  }
;;

(* On the session's tool host, through the context's executor. *)
let host_op op ~run =
  { Tool.spec =
      { Tool_spec.name = op
      ; description = "git hosting"
      ; parameters = `Object []
      ; parallel_safe = true
      ; destructive = Never
      ; on_host = true
      ; freeform = None
      }
  ; run
  }
;;

let git_forge =
  host_op Git_forge.op ~run:(fun (context : Tool.Context.t) arguments ->
    Git_forge.run
      ~env:context.env
      ~cancel:context.cancel
      ~cwd:context.cwd
      ~arguments)
;;

let git_location =
  host_op Git_location.op ~run:(fun (context : Tool.Context.t) _ ->
    Git_location.run ~cwd:context.cwd)
;;

let on_host (context : Tool.Context.t) ~cwd tool arguments =
  Tool.execute_via { context with cwd; on_output = ignore } tool arguments
;;

let number args =
  match Tool_args.int_opt args "number" with
  | Some n -> n
  | None ->
    raise
      (Tool_args.Invalid
         "missing \"number\": the pull request's number (action=list shows \
          them)")
;;

let repo_name (repo : Forge_repo.t) =
  sprintf "%s/%s on %s" repo.owner repo.name repo.instance.host
;;

let flags (pr : Pull_request.t) =
  String.concat
    ~sep:", "
    (Pull_request.State.to_string pr.state
     :: List.filter_opt [ Option.some_if pr.draft "draft" ])
;;

let list_text ~state ({ repo; prs } : Forges.Pulls.t) =
  let which = if String.equal state "all" then "" else state ^ " " in
  match prs with
  | [] ->
    sprintf
      "no %spull requests in %s (%s/pulls)%s"
      which
      (repo_name repo)
      (Forge_repo.web_url repo)
      (if String.equal state "all" then "" else "; state=all lists them all")
  | prs ->
    String.concat
      ~sep:"\n"
      (sprintf "%spull requests in %s:" which (repo_name repo)
       :: List.map prs ~f:(fun (pr : Pull_request.t) ->
         sprintf
           "#%d %s (%s; %s, %s -> %s) %s"
           pr.number
           pr.title
           (flags pr)
           pr.author
           pr.head
           pr.base
           pr.url))
;;

let checks_text = function
  | None -> "checks: unknown (no head commit)"
  | Some (Error e) -> sprintf "checks: unavailable (%s)" (Error.to_string_hum e)
  | Some (Ok ({ state = "none"; _ } : Pr_checks.t)) -> "checks: none"
  | Some (Ok ({ state; items } : Pr_checks.t)) ->
    String.concat
      ~sep:"\n"
      (sprintf "checks: %s" state
       :: List.map items ~f:(fun (i : Pr_checks.Item.t) ->
         sprintf
           "- %s: %s%s"
           i.name
           i.state
           (Option.value_map i.url ~default:"" ~f:(sprintf " %s"))))
;;

let header ({ pr; repo; _ } : Forges.Pull.t) =
  sprintf
    "#%d %s (%s) in %s\n%s\n%s wants to merge %s into %s"
    pr.number
    pr.title
    (flags pr)
    (repo_name repo)
    pr.url
    pr.author
    pr.head
    pr.base
;;

let view_text (pull : Forges.Pull.t) =
  let body =
    if String.is_empty (String.strip pull.pr.body)
    then "(no description)"
    else (
      let shown = Truncate.head (String.strip pull.pr.body) in
      if shown.truncated
      then
        sprintf
          "%s\n[description cut at %d of %d lines]"
          shown.text
          (Truncate.count_lines shown.text)
          shown.total_lines
      else shown.text)
  in
  sprintf
    "%s\ncreated %s, updated %s\n%s\n\n%s"
    (header pull)
    pull.pr.created_at
    pull.pr.updated_at
    (checks_text pull.checks)
    body
;;

let diff_text (pull : Forges.Pull.t) =
  let diff, cut = Option.value pull.diff ~default:("", false) in
  if String.is_empty diff
  then sprintf "#%d changes nothing (an empty diff)" pull.pr.number
  else (
    let shown = Truncate.head diff in
    sprintf
      "diff of #%d %s (%s -> %s):\n%s%s"
      pull.pr.number
      pull.pr.title
      pull.pr.head
      pull.pr.base
      shown.text
      (if shown.truncated || cut
       then
         sprintf
           "\n\
            [diff cut at %d lines: check the pull request out \
            (action=checkout) to read all of it]"
           (Truncate.count_lines shown.text)
       else ""))
;;

let create_text ~push (pull : Forges.Pull.t) =
  sprintf
    "%s %s\n%s"
    (if push then sprintf "pushed %s to %s, then opened" pull.pr.head pull.repo.remote
     else "opened")
    (header pull)
    (if pull.pr.draft then "It is a draft." else "")
  |> String.rstrip
;;

let checkout context ~forges ~cwd ~git ~worktrees_dir number =
  let%map.Or_error checkout =
    Forges.checkout_pull forges ~git ~worktrees_dir number
  in
  let root =
    match
      Git_location.of_result
        (on_host context ~cwd:checkout.path git_location (`Object []))
    with
    | Ok (Some l) -> l.root
    | Ok None | Error _ -> cwd
  in
  context.Tool.emit
    (Agent_event.Worktrees_changed
       { added =
           [ { path = checkout.path
             ; branch = Some checkout.branch
             ; root
             ; source = Pull_request { number }
             }
           ]
       ; removed = []
       });
  sprintf
    "checked #%d out in %s, on the branch %s (%s)\n\
     Work there: cd %s in bash commands, paths under it in file tools, or pass \
     it as a subagent's cwd. Remove it with the worktree tool when it is done \
     with."
    number
    checkout.path
    checkout.branch
    (if checkout.created
     then "a new worktree"
     else "its worktree already, fast-forwarded")
    (Filename.quote checkout.path)
;;

(* {!Forges}' errors point users at slash commands; the model has this
   tool's actions instead, and only the user can sign in. *)
let for_the_model error =
  List.fold
    [ "/pr lists", "action=list lists"
    ; "/cd into one", "cwd names one"
    ; ": sign in under", ": ask the user to sign in under"
    ; ": sign in again with", ": ask the user to sign in again with"
    ]
    ~init:error
    ~f:(fun error (pattern, with_) ->
      String.substr_replace_all error ~pattern ~with_)
;;

(* When [number] fails and is not one of the repository's pull requests, the
   nearest numbers that are. *)
let closest forges ~git number =
  match Forges.list_pulls forges ~git ~state:"all" with
  | Error _ -> None
  | Ok { prs; _ } ->
    if List.exists prs ~f:(fun (pr : Pull_request.t) -> pr.number = number)
    then None
    else (
      match
        List.sort prs ~compare:(fun (a : Pull_request.t) (b : Pull_request.t) ->
          Comparable.lexicographic
            [ (fun a b -> Int.compare (abs (a - number)) (abs (b - number)))
            ; Int.compare
            ]
            a.number
            b.number)
      with
      | [] -> Some "the repository has no pull requests"
      | prs ->
        Some
          (sprintf
             "the closest: %s"
             (String.concat
                ~sep:", "
                (List.map (List.take prs 3) ~f:(fun (pr : Pull_request.t) ->
                   sprintf "#%d %s (%s)" pr.number pr.title (flags pr))))))
;;

(* What [create] fills in from the branch's commits when the model leaves the
   title (and body) out. *)
let with_defaults forges ~git ~title ~body =
  match title with
  | Some title when not (String.is_empty (String.strip title)) ->
    Ok (title, Option.value body ~default:"")
  | Some _ | None ->
    let%map.Or_error d = Forges.pull_defaults forges ~git in
    d.title, Option.value body ~default:d.body
;;

let create ~forges ?(worktrees_dir = fun () -> "~/.prigh/worktrees") () =
  let run (context : Tool.Context.t) args =
    let cwd =
      match Tool_args.string_opt args "cwd" with
      | None -> context.cwd
      | Some cwd -> Tool.resolve_path context cwd
    in
    let git request =
      on_host context ~cwd git_forge (Git_forge.Request.to_json request)
    in
    let action = Tool_args.string args "action" in
    let result =
      match action with
      | "list" ->
        let state =
          Option.value (Tool_args.string_opt args "state") ~default:"open"
        in
        Forges.list_pulls forges ~git ~state |> Or_error.map ~f:(list_text ~state)
      | "view" ->
        Forges.view_pull forges ~git ~diff:false (number args)
        |> Or_error.map ~f:view_text
      | "checks" ->
        Forges.view_pull forges ~git ~diff:false (number args)
        |> Or_error.map ~f:(fun (pull : Forges.Pull.t) ->
          sprintf
            "#%d %s (head %s)\n%s"
            pull.pr.number
            pull.pr.title
            (String.prefix pull.pr.head_sha 12)
            (checks_text pull.checks))
      | "diff" ->
        Forges.view_pull forges ~git (number args) |> Or_error.map ~f:diff_text
      | "create" ->
        let push = Option.value (Tool_args.bool_opt args "push") ~default:true in
        let%bind.Or_error title, body =
          with_defaults
            forges
            ~git
            ~title:(Tool_args.string_opt args "title")
            ~body:(Tool_args.string_opt args "body")
        in
        Forges.create_pull
          forges
          ~git
          ~title
          ~body
          ~base:(Tool_args.string_opt args "base")
          ~head:(Tool_args.string_opt args "head")
          ~draft:(Option.value (Tool_args.bool_opt args "draft") ~default:false)
          ~push
        |> Or_error.map ~f:(create_text ~push)
      | "checkout" ->
        checkout
          context
          ~forges
          ~cwd
          ~git
          ~worktrees_dir:(worktrees_dir ())
          (number args)
      | other ->
        raise
          (Tool_args.Invalid
             (sprintf
                "unknown action %S: use list, view, checks, diff, create or \
                 checkout"
                other))
    in
    match result with
    | Ok text -> Tool.Result.ok text
    | Error e ->
      let hint =
        if List.mem [ "view"; "checks"; "diff"; "checkout" ] action ~equal:String.equal
        then
          Option.value_map (closest forges ~git (number args)) ~default:"" ~f:(fun c ->
            "; " ^ c)
        else ""
      in
      Tool.Result.error
        (sprintf
           "pull_request %s: %s%s"
           action
           (for_the_model (Error.to_string_hum e))
           hint)
  in
  { Tool.spec; run }
;;
