open! Core
open! Import
module J = Forge_json

let max_readme_bytes = 8_000

let cut s ~max_bytes =
  if String.length s <= max_bytes
  then s
  else String.prefix s max_bytes ^ "\n[... cut]"
;;

module Info = struct
  type t =
    { full_name : string
    ; description : string
    ; default_branch : string
    ; private_ : bool
    ; fork : bool
    ; archived : bool
    ; stars : int
    ; forks : int
    ; open_issues : int
    ; url : string
    ; homepage : string
    ; readme : string option
    }
  [@@deriving sexp_of]

  let of_json json ~readme =
    let int names =
      Option.value
        (List.find_map names ~f:(fun n -> J.int json [ n ]))
        ~default:0
    in
    { full_name = J.str json [ "full_name" ]
    ; description = J.str json [ "description" ]
    ; default_branch = J.str json [ "default_branch" ]
    ; private_ = J.bool json [ "private" ]
    ; fork = J.bool json [ "fork" ]
    ; archived = J.bool json [ "archived" ]
    ; stars = int [ "stargazers_count"; "stars_count" ]
    ; forks = int [ "forks_count" ]
    ; open_issues = int [ "open_issues_count" ]
    ; url = J.str json [ "html_url" ]
    ; homepage =
        Option.value
          (List.find_map [ "homepage"; "website" ] ~f:(fun n ->
             Option.filter (J.str_opt json [ n ]) ~f:(Fn.non String.is_empty)))
          ~default:""
    ; readme
    }
  ;;

  let to_json t =
    let b v = if v then `True else `False in
    `Object
      ([ "full_name", `String t.full_name
       ; "description", `String t.description
       ; "default_branch", `String t.default_branch
       ; "private", b t.private_
       ; "fork", b t.fork
       ; "archived", b t.archived
       ; "stars", J.number t.stars
       ; "forks", J.number t.forks
       ; "open_issues", J.number t.open_issues
       ; "url", `String t.url
       ; "homepage", `String t.homepage
       ]
       @ Option.value_map t.readme ~default:[] ~f:(fun r ->
         [ "readme", `String r ]))
  ;;
end

let view (api : Forge_api.t) repo =
  let open Or_error.Let_syntax in
  let%map json =
    Forge_api.get_json
      api
      ~what:(sprintf "%s/%s" repo.Forge_repo.owner repo.name)
      ~not_found:(Forge_api.repo_hint repo)
      (Forge_repo.api_url repo)
  in
  let readme =
    let result =
      match api.instance.kind with
      | Github ->
        Forge_api.send
          api
          ~accept:"application/vnd.github.raw"
          ~what:"the README"
          Get
          (Forge_repo.api_url repo ^ "/readme")
      | Forgejo ->
        Forge_api.send
          api
          ~accept:"text/plain"
          ~what:"the README"
          Get
          (Forge_repo.api_url repo ^ "/raw/README.md")
    in
    match result with
    | Ok text -> Some (cut (String.strip text) ~max_bytes:max_readme_bytes)
    | Error _ -> None
  in
  Info.of_json json ~readme
;;

module Run = struct
  type t =
    { id : int
    ; name : string
    ; title : string
    ; status : string
    ; conclusion : string option
    ; branch : string
    ; sha : string
    ; event : string
    ; url : string
    ; created_at : string
    }
  [@@deriving sexp_of]

  let of_json json =
    match J.int json [ "id" ] with
    | None -> Or_error.error_string "run: missing id"
    | Some id ->
      Ok
        { id
        ; name = J.str json [ "name" ]
        ; title = J.str json [ "display_title" ]
        ; status = J.str json [ "status" ]
        ; conclusion = J.str_opt json [ "conclusion" ]
        ; branch = J.str json [ "head_branch" ]
        ; sha = J.str json [ "head_sha" ]
        ; event = J.str json [ "event" ]
        ; url =
            (match J.str_opt json [ "html_url" ] with
             | Some u -> u
             | None -> J.str json [ "url" ])
        ; created_at = J.str json [ "created_at" ]
        }
  ;;

  let to_json t =
    `Object
      ([ "id", J.number t.id
       ; "name", `String t.name
       ; "title", `String t.title
       ; "status", `String t.status
       ]
       @ Option.value_map t.conclusion ~default:[] ~f:(fun c ->
         [ "conclusion", `String c ])
       @ [ "branch", `String t.branch
         ; "sha", `String t.sha
         ; "event", `String t.event
         ; "url", `String t.url
         ; "created_at", `String t.created_at
         ])
  ;;
end

module Job = struct
  type t =
    { id : int
    ; name : string
    ; status : string
    ; conclusion : string option
    ; url : string
    ; steps : (string * string) list
    }
  [@@deriving sexp_of]

  let of_json json =
    match J.int json [ "id" ] with
    | None -> Or_error.error_string "job: missing id"
    | Some id ->
      let steps =
        match J.path json [ "steps" ] with
        | Some (`Array steps) ->
          List.map steps ~f:(fun s ->
            ( J.str s [ "name" ]
            , match J.str_opt s [ "conclusion" ] with
              | Some c -> c
              | None -> J.str s [ "status" ] ))
        | _ -> []
      in
      Ok
        { id
        ; name = J.str json [ "name" ]
        ; status = J.str json [ "status" ]
        ; conclusion = J.str_opt json [ "conclusion" ]
        ; url = J.str json [ "html_url" ]
        ; steps
        }
  ;;

  let to_json t =
    `Object
      ([ "id", J.number t.id
       ; "name", `String t.name
       ; "status", `String t.status
       ]
       @ Option.value_map t.conclusion ~default:[] ~f:(fun c ->
         [ "conclusion", `String c ])
       @ [ "url", `String t.url
         ; ( "steps"
           , `Array
               (List.map t.steps ~f:(fun (name, state) ->
                  `Object [ "name", `String name; "state", `String state ])) )
         ])
  ;;
end

let all f items = Or_error.all (List.map items ~f)

let forgejo_only_statuses (api : Forge_api.t) what =
  Or_error.errorf
    "%s (Forgejo) has no API for %s: pull_request checks shows a pull \
     request's commit statuses, whose links lead to the CI's pages"
    api.instance.host
    what
;;

let runs (api : Forge_api.t) repo ~branch ~limit =
  let actions = Forge_repo.api_url repo ^ "/actions" in
  match api.instance.kind with
  | Github ->
    let query =
      Option.value_map branch ~default:"" ~f:(fun b ->
        "?branch=" ^ Uri.pct_encode b)
    in
    Or_error.bind
      (Forge_api.get_list
         api
         ~limit
         ~items:(Json.member "workflow_runs")
         ~what:"the workflow runs"
         (actions ^ "/runs" ^ query))
      ~f:(all Run.of_json)
  | Forgejo ->
    let open Or_error.Let_syntax in
    let%map runs =
      Forge_api.get_list
        api
        ~limit
        ~items:(Json.member "workflow_runs")
        ~what:"the Actions runs"
        ~not_found:
          ": this Forgejo has no Actions API (older than 7.0?); pull_request \
           checks shows commit statuses instead"
        (actions ^ "/tasks")
      >>= all Run.of_json
    in
    (* tasks are jobs, several per run, and the API cannot filter them *)
    List.filter runs ~f:(fun r ->
      Option.value_map branch ~default:true ~f:(String.equal r.branch))
;;

let run (api : Forge_api.t) repo id =
  match api.instance.kind with
  | Forgejo -> forgejo_only_statuses api "a run's jobs"
  | Github ->
    let open Or_error.Let_syntax in
    let url = sprintf "%s/actions/runs/%d" (Forge_repo.api_url repo) id in
    let%bind run =
      Forge_api.get_json
        api
        ~what:(sprintf "run %d" id)
        ~not_found:": repo runs lists the recent ones"
        url
      >>= Run.of_json
    in
    let%map jobs =
      Forge_api.get_list
        api
        ~items:(Json.member "jobs")
        ~what:(sprintf "the jobs of run %d" id)
        (url ^ "/jobs")
      >>= all Job.of_json
    in
    run, jobs
;;

let job_log (api : Forge_api.t) repo id ~max_bytes =
  match api.instance.kind with
  | Forgejo -> forgejo_only_statuses api "job logs"
  | Github ->
    Or_error.map
      (Forge_api.get_following
         api
         ~what:(sprintf "the log of job %d" id)
         (sprintf "%s/actions/jobs/%d/logs" (Forge_repo.api_url repo) id))
      ~f:(fun log ->
        if String.length log <= max_bytes
        then log, false
        else (
          let tail = String.suffix log max_bytes in
          match String.index tail '\n' with
          | Some i -> String.drop_prefix tail (i + 1), true
          | None -> tail, true))
;;

let rerun (api : Forge_api.t) repo id ~failed_only =
  match api.instance.kind with
  | Forgejo -> forgejo_only_statuses api "re-running a run"
  | Github ->
    Forge_api.send
      api
      ~what:(sprintf "re-running run %d" id)
      ~not_found:": repo runs lists the recent ones"
      Post
      (sprintf
         "%s/actions/runs/%d/%s"
         (Forge_repo.api_url repo)
         id
         (if failed_only then "rerun-failed-jobs" else "rerun"))
    |> Or_error.map ~f:ignore
;;
