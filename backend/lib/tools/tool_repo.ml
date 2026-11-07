open! Core
open! Import

let name = "repo"
let changes action = String.equal action "rerun"
let max_log_bytes = 40_000

let spec =
  { Tool_spec.name
  ; parallel_safe = false
  ; on_host = false
  ; freeform = None
  ; native = None
  ; destructive =
      Depends
        (fun args ->
          Option.value_map
            (Tool_args.string_opt args "action")
            ~default:true
            ~f:changes)
  ; description =
      "The working directory's repository on its git host (GitHub or Forgejo), \
       through the user's sign-in to that host in prigh. view shows its \
       description, default branch, visibility and README. runs lists recent \
       CI (Actions) runs (branch narrows them). run shows one run's jobs and \
       steps (run: its id). logs shows the end of a job's log (job: its id, \
       from run). rerun re-runs a run (failed_only: only its failed jobs). On \
       Forgejo only view and runs work: a pull request's CI is in pull_request \
       checks."
  ; parameters =
      Tool_args.schema
        ~required:[ "action" ]
        [ "action", `String, "view, runs, run, logs or rerun"
        ; "run", `Integer, "run, rerun: the run's id (from runs)"
        ; "job", `Integer, "logs: the job's id (from run)"
        ; "branch", `String, "runs: only this branch's"
        ; "limit", `Integer, "runs: at most this many (default 10)"
        ; "failed_only", `Boolean, "rerun: only the failed jobs (default true)"
        ; ( "cwd"
          , `String
          , "A directory in the repository (default: this agent's working \
             directory)" )
        ]
  }
;;

let count n what = sprintf "%d %s%s" n what (if n = 1 then "" else "s")

let view_text (i : Forge_ci.Info.t) =
  String.concat
    ~sep:"\n"
    ([ (i.full_name
        ^ if String.is_empty i.description then "" else ": " ^ i.description)
     ; sprintf
         "%s%s%s, default branch %s, %s, %s, %s"
         (if i.private_ then "private" else "public")
         (if i.fork then ", a fork" else "")
         (if i.archived then ", archived" else "")
         i.default_branch
         (count i.stars "star")
         (count i.forks "fork")
         (count i.open_issues "open issue")
     ; i.url
     ]
     @ (if String.is_empty i.homepage then [] else [ i.homepage ])
     @
     match i.readme with
     | None -> []
     | Some r -> [ ""; "README:"; r ])
;;

let state (r : Forge_ci.Run.t) =
  match r.conclusion with
  | Some c when not (String.is_empty c) -> c
  | _ -> r.status
;;

let run_line (r : Forge_ci.Run.t) =
  sprintf
    "%d %s: %s [%s] on %s (%s, %s) %s"
    r.id
    r.name
    r.title
    (state r)
    r.branch
    r.event
    (String.prefix r.created_at 16)
    r.url
;;

let create ~forges () =
  let run (context : Tool.Context.t) args =
    let cwd = Tool_forge.cwd context args in
    let git = Tool_forge.git context ~cwd in
    let action = Tool_args.string args "action" in
    let id name what =
      match Tool_args.int_opt args name with
      | Some n -> n
      | None -> raise (Tool_args.Invalid (sprintf "missing %S: %s" name what))
    in
    let with_repo f = Forges.with_repo forges ~git ~f in
    let open Or_error.Let_syntax in
    let result =
      match action with
      | "view" ->
        with_repo (fun api repo -> Forge_ci.view api repo >>| view_text)
      | "runs" ->
        let limit =
          Option.value_map
            (Tool_args.int_opt args "limit")
            ~default:10
            ~f:(Int.max 1)
        in
        let branch = Tool_args.string_opt args "branch" in
        with_repo (fun api repo ->
          Forge_ci.runs api repo ~branch ~limit
          >>| function
          | [] -> sprintf "no CI runs in %s" (Tool_forge.repo_name repo)
          | runs ->
            sprintf
              "CI runs in %s, newest first:\n%s"
              (Tool_forge.repo_name repo)
              (String.concat ~sep:"\n" (List.map runs ~f:run_line)))
      | "run" ->
        let n = id "run" "the run's id (action=runs lists them)" in
        with_repo (fun api repo ->
          Forge_ci.run api repo n
          >>| fun (r, jobs) ->
          String.concat
            ~sep:"\n"
            (run_line r
             :: List.concat_map jobs ~f:(fun (j : Forge_ci.Job.t) ->
               sprintf
                 "  job %d %s [%s]"
                 j.id
                 j.name
                 (Option.value j.conclusion ~default:j.status)
               :: List.map j.steps ~f:(fun (name, state) ->
                 sprintf "    %s [%s]" name state))))
      | "logs" ->
        let n = id "job" "the job's id (action=run lists a run's jobs)" in
        with_repo (fun api repo ->
          Forge_ci.job_log api repo n ~max_bytes:max_log_bytes
          >>| fun (log, cut) ->
          (if cut
           then sprintf "[the log's last %d bytes]\n" max_log_bytes
           else "")
          ^ log)
      | "rerun" ->
        let n = id "run" "the run's id (action=runs lists them)" in
        let failed_only =
          Option.value (Tool_args.bool_opt args "failed_only") ~default:true
        in
        with_repo (fun api repo ->
          Forge_ci.rerun api repo n ~failed_only
          >>| fun () ->
          sprintf
            "re-running %s of run %d (action=run shows its progress)"
            (if failed_only then "the failed jobs" else "every job")
            n)
      | other ->
        raise
          (Tool_args.Invalid
             (sprintf
                "unknown action %S: use view, runs, run, logs or rerun"
                other))
    in
    match result with
    | Ok text -> Tool.Result.ok text
    | Error e ->
      Tool.Result.error
        (sprintf
           "repo %s: %s"
           action
           (Tool_forge.for_the_model (Error.to_string_hum e)))
  in
  { Tool.spec; run }
;;
