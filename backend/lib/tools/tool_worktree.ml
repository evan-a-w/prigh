open! Core
open! Import

let name = "worktree"

let spec =
  { Tool_spec.name
  ; parallel_safe = false
  ; on_host = false
  ; freeform = None
  ; native = None
  ; destructive =
      Depends
        (fun args ->
          not
            ([%equal: string option]
               (Tool_args.string_opt args "action")
               (Some "list")))
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
             directory; with host, that host's home, and a relative cwd is \
             from there)" )
        ; ( "host"
          , `String
          , "Another connected tool host (by name or id) whose repository's \
             worktrees these are, e.g. for a subagent working there (default: \
             where your tools run); only when the user asked for work on that \
             machine" )
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

(* The session records the worktrees of its active host only. *)
let announce (context : Tool.Context.t) event =
  if Option.is_none context.host then context.emit event
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
    announce
      context
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
    announce
      context
      (Agent_event.Worktrees_changed { added = []; removed = [ removed.path ] });
    Tool.Result.ok
      (sprintf
         "removed worktree %s%s"
         removed.path
         (Option.value_map removed.branch ~default:"" ~f:(fun b ->
            sprintf " (the branch %s stays)" b)))
;;

let or_invalid = function
  | Ok x -> x
  | Error message -> raise (Tool_args.Invalid message)
;;

(* With [host], the context and directory on that host. *)
let on_host ~hosts (context : Tool.Context.t) args =
  let given = Tool_args.string_opt args "cwd" in
  let here () =
    match given with
    | None -> context.cwd
    | Some cwd -> Tool.resolve_path context cwd
  in
  match Tool_args.string_opt args "host" with
  | None -> context, here ()
  | Some key ->
    let host = or_invalid (Tool_on_host.find (hosts ()) key) in
    let current =
      match context.host with
      | Some id -> String.equal id host.id
      | None -> host.active
    in
    if current
    then context, here ()
    else
      ( { context with
          host = Option.some_if (not host.active) host.id
        ; home = (fun () -> host.home)
        }
      , or_invalid (Tool_on_host.cwd_on host given) )
;;

let create
      ?(worktrees_dir = fun () -> "~/.prigh/worktrees")
      ?(session = fun () -> None)
      ?(hosts = fun () -> [])
      ()
  =
  let run (context : Tool.Context.t) args =
    let context, cwd = on_host ~hosts context args in
    (* The session's worktrees are those of its active host. *)
    let session () =
      if Option.is_none context.host then session () else None
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
