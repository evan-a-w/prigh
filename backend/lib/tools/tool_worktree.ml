open! Core
open! Import

let name = "worktree"

let spec =
  { Tool_spec.name
  ; parallel_safe = false
  ; on_host = false
  ; freeform = None
  ; destructive = true
  ; description =
      "Git worktrees of the working directory's repository: create, list or \
       remove. create checks a branch out in a new worktree at the standard \
       location (an existing branch as it is, otherwise a new one from base) \
       and records it on the session; work there by giving its path (cd <path> \
       in bash, paths under it in file tools, a subagent's cwd). list shows \
       the worktrees with their branch and changes. remove deletes one (its \
       branch stays); never the main checkout or the one you work in, and only \
       with force when it has uncommitted changes."
  ; parameters =
      Tool_args.schema
        ~required:[ "action" ]
        [ "action", `String, "create, list or remove"
        ; "branch", `String, "create: the branch to check out or make"
        ; ( "base"
          , `String
          , "create: what a new branch starts from (default: HEAD of the \
             working directory)" )
        ; "path", `String, "remove: the worktree's path"
        ; ( "force"
          , `Boolean
          , "remove: even with uncommitted changes or locked (they are lost)" )
        ; ( "cwd"
          , `String
          , "A directory in the repository (default: this agent's working \
             directory)" )
        ]
  }
;;

let run_op (context : Tool.Context.t) ~cwd tool arguments =
  Tool.execute_via { context with cwd; on_output = ignore } tool arguments
;;

let source_text : Session_worktree.Source.t -> string = function
  | Created -> "made in this session"
  | Entered -> "this session moved into it"
  | Pull_request { number } ->
    sprintf "pull request #%d, checked out here" number
  | Agent { agent; task = _ } -> sprintf "made for subagent %s" agent
;;

let changes_text (w : Worktree.t) =
  let changes =
    match w.changes with
    | None -> []
    | Some 0 -> [ "clean" ]
    | Some 1 -> [ "1 changed file" ]
    | Some n -> [ sprintf "%d changed files" n ]
  in
  let upstream =
    match w.upstream with
    | None -> []
    | Some upstream ->
      (match w.ahead, w.behind with
       | 0, 0 -> [ "up to date with " ^ upstream ]
       | ahead, 0 -> [ sprintf "%d ahead of %s" ahead upstream ]
       | 0, behind -> [ sprintf "%d behind %s" behind upstream ]
       | ahead, behind ->
         [ sprintf "%d ahead, %d behind %s" ahead behind upstream ])
  in
  String.concat ~sep:", " (changes @ upstream)
;;

let list_text
      ~(context : Tool.Context.t)
      ~recorded
      (listing : Worktree.Listing.t)
  =
  let line (w : Worktree.t) =
    let marks =
      List.filter_opt
        [ Option.some_if w.main "main checkout"
        ; Option.some_if (Worktree.contains w ~dir:context.cwd) "you are here"
        ; List.find recorded ~f:(fun (r : Session_worktree.t) ->
            String.equal r.path w.path)
          |> Option.map ~f:(fun (r : Session_worktree.t) ->
            source_text r.source)
        ; Option.some_if (not (w.main || w.managed)) "not under worktrees_dir"
        ; Option.some_if w.locked "locked"
        ; Option.some_if w.prunable "its directory is gone"
        ]
    in
    sprintf
      "- %s  %s  %s%s"
      w.path
      (Option.value w.branch ~default:"(detached)")
      (changes_text w)
      (String.concat (List.map marks ~f:(sprintf "  [%s]")))
  in
  String.concat
    ~sep:"\n"
    (sprintf
       "worktrees of %s (new ones go in %s):"
       listing.root
       listing.project_dir
     :: List.map listing.worktrees ~f:line)
;;

let create_worktree (context : Tool.Context.t) ~cwd ~dir args =
  let branch = Tool_args.string args "branch" in
  let base = Tool_args.string_opt args "base" in
  match
    run_op
      context
      ~cwd
      Worktree.create_tool
      (Worktree.create_args ~dir ~branch ~base ())
    |> Worktree.created_of_result
  with
  | Error e ->
    Tool.Result.error
      (sprintf
         "could not make the worktree: %s\n\
          (worktree action=list shows the worktrees and their branches)"
         (Error.to_string_hum e))
  | Ok created ->
    let w = created.worktree in
    context.emit
      (Agent_event.Worktrees_changed
         { added =
             [ { path = w.path
               ; branch = w.branch
               ; root = created.root
               ; source = Created
               }
             ]
         ; removed = []
         });
    Tool.Result.ok
      (sprintf
         "made worktree %s, a checkout of %s on %s\n\
          Work there: cd %s in bash commands, paths under it in file tools, or \
          pass it as a subagent's cwd. Remove it with action=remove when it is \
          done with (its branch stays)."
         w.path
         created.root
         (match created.new_branch, w.branch, base with
          | true, Some b, Some base ->
            sprintf "the new branch %s (from %s)" b base
          | true, Some b, None -> sprintf "the new branch %s" b
          | false, Some b, _ -> "the branch " ^ b
          | _, None, _ -> "a detached HEAD")
         (Filename.quote w.path))
;;

let remove_worktree (context : Tool.Context.t) ~cwd ~dir ~session args =
  let path = Tool_args.string args "path" in
  let force = Option.value (Tool_args.bool_opt args "force") ~default:false in
  let keep =
    List.dedup_and_sort
      ~compare:String.compare
      (context.cwd
       :: Option.value_map session ~default:[] ~f:(fun s -> [ Session.cwd s ]))
  in
  match
    run_op
      context
      ~cwd
      Worktree.remove_tool
      (Worktree.remove_args ~keep ~dir ~path ~force ())
    |> Worktree.removed_of_result
  with
  | Error e ->
    Tool.Result.error
      (sprintf
         "could not remove the worktree: %s\n\
          (worktree action=list shows the worktrees)"
         (Error.to_string_hum e))
  | Ok removed ->
    context.emit
      (Agent_event.Worktrees_changed { added = []; removed = [ removed.path ] });
    Tool.Result.ok
      (sprintf
         "removed worktree %s%s"
         removed.path
         (Option.value_map removed.branch ~default:"" ~f:(fun b ->
            sprintf " (the branch %s stays)" b)))
;;

let create
      ?(worktrees_dir = fun () -> "~/.prigh/worktrees")
      ?(session = fun () -> None)
      ()
  =
  let run (context : Tool.Context.t) args =
    let cwd =
      match Tool_args.string_opt args "cwd" with
      | None -> context.cwd
      | Some cwd -> Tool.resolve_path context cwd
    in
    let dir = worktrees_dir () in
    match Tool_args.string args "action" with
    | "create" -> create_worktree context ~cwd ~dir args
    | "remove" -> remove_worktree context ~cwd ~dir ~session:(session ()) args
    | "list" ->
      (match
         run_op context ~cwd Worktree.list_tool (Worktree.list_args ~dir)
         |> Worktree.listing_of_result
       with
       | Error e -> Tool.Result.error (Error.to_string_hum e)
       | Ok listing ->
         let recorded =
           Option.value_map (session ()) ~default:[] ~f:Session.worktrees
         in
         Tool.Result.ok (list_text ~context ~recorded listing))
    | other ->
      raise
        (Tool_args.Invalid
           (sprintf "unknown action %S: use create, list or remove" other))
  in
  { Tool.spec; run }
;;
