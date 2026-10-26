open! Core
open! Import

type t =
  { http : Forge_http.t
  ; instance : Forge_instance.t
  ; authorization : string
  ; cancel : Cancellation.t
  }

let create ?(cancel = Cancellation.never) ~http instance credential =
  { http
  ; instance
  ; authorization = Forge_credential.authorization credential
  ; cancel
  }
;;

let json_accept (t : t) =
  match t.instance.kind with
  | Github -> "application/vnd.github+json"
  | Forgejo -> "application/json"
;;

let headers ?accept ?(json_body = false) t =
  [ "Authorization", t.authorization
  ; "Accept", Option.value accept ~default:(json_accept t)
  ; "User-Agent", "prigh"
  ]
  @ (match t.instance.kind with
     | Github -> [ "X-GitHub-Api-Version", "2022-11-28" ]
     | Forgejo -> [])
  @ if json_body then [ "Content-Type", "application/json" ] else []
;;

let message_of_body body =
  match Json.parse body with
  | Ok json ->
    let message =
      match Json.member "message" json with
      | Some (`String s) -> Some s
      | _ -> None
    in
    let details =
      match Json.member "errors" json with
      | Some (`Array errors) ->
        List.filter_map errors ~f:(fun e ->
          match Json.member "message" e, e with
          | Some (`String s), _ -> Some s
          | _, `String s -> Some s
          | _ ->
            (match Json.member "field" e, Json.member "code" e with
             | Some (`String f), Some (`String c) -> Some (f ^ " " ^ c)
             | _ -> None))
      | _ -> []
    in
    (match message, details with
     | None, [] -> None
     | None, d -> Some (String.concat ~sep:"; " d)
     | Some m, [] -> Some m
     | Some m, d -> Some (m ^ ": " ^ String.concat ~sep:"; " d))
  | Error _ ->
    let s = String.strip body in
    if String.is_empty s || String.length s > 300 then None else Some s
;;

let http_error
      t
      ~what
      ?(not_found = "")
      ?on_404
      (response : Forge_http.Response.t)
  =
  let host = t.instance.host in
  let detail =
    Option.value_map (message_of_body response.body) ~default:"" ~f:(fun m ->
      ": " ^ m)
  in
  match response.status with
  | 401 ->
    Or_error.errorf
      "%s refused the sign-in (HTTP 401%s): sign in again with /login %s"
      host
      detail
      (Forge_instance.login_arg t.instance)
  | 404 ->
    (match on_404 with
     | Some message -> Or_error.error_string message
     | None ->
       Or_error.errorf "%s: %s not found (HTTP 404)%s" host what not_found)
  | status -> Or_error.errorf "%s: %s: HTTP %d%s" host what status detail
;;

let send t ?accept ?body ~what ?not_found ?on_404 meth url =
  let request : Forge_http.Request.t =
    { meth
    ; url
    ; headers = headers ?accept ~json_body:(Option.is_some body) t
    ; body = Option.map body ~f:Json.to_string
    }
  in
  Or_error.bind (t.http ~cancel:t.cancel request) ~f:(fun response ->
    if response.status / 100 = 2
    then Ok response.body
    else http_error t ~what ?not_found ?on_404 response)
;;

let parse ~what body =
  match Json.parse body with
  | Ok json -> Ok json
  | Error _ -> Or_error.errorf "%s: the reply is not JSON" what
;;

let get_json t ~what ?not_found ?on_404 url =
  Or_error.bind (send t ~what ?not_found ?on_404 Get url) ~f:(parse ~what)
;;

let repo_hint (repo : Forge_repo.t) =
  sprintf
    ": check the URL of the git remote %s, or sign in as an account that can \
     see %s/%s (/login %s)"
    repo.remote
    repo.owner
    repo.name
    (Forge_instance.login_arg repo.instance)
;;

let current_user t =
  Or_error.bind
    (get_json
       t
       ~what:"the signed-in user"
       ~on_404:
         (sprintf
            "%s has no %s API at %s: if it is a %s, set forges.\"%s\".kind to \
             %s in ~/.prigh/config.json"
            t.instance.host
            (Forge_kind.label t.instance.kind)
            (Forge_instance.api_url t.instance)
            (Forge_kind.label
               (match t.instance.kind with
                | Github -> Forgejo
                | Forgejo -> Github))
            t.instance.host
            (Forge_kind.to_string
               (match t.instance.kind with
                | Github -> Forgejo
                | Forgejo -> Github)))
       (Forge_instance.api_url t.instance ^ "/user"))
    ~f:(fun json ->
      match Json.member "login" json with
      | Some (`String login) -> Ok login
      | _ -> Or_error.error_string "the user's reply has no login")
;;

let list_pulls t repo ~state =
  let query =
    match t.instance.kind with
    | Github ->
      [ "state", state
      ; "per_page", "100"
      ; "sort", "updated"
      ; "direction", "desc"
      ]
    | Forgejo -> [ "state", state; "limit", "50"; "sort", "recentupdate" ]
  in
  let url =
    Forge_repo.api_url repo
    ^ "/pulls?"
    ^ Uri.encoded_of_query (List.map query ~f:(fun (k, v) -> k, [ v ]))
  in
  let what = sprintf "the pull requests of %s/%s" repo.owner repo.name in
  Or_error.bind
    (get_json t ~what ~not_found:(repo_hint repo) url)
    ~f:(function
      | `Array items ->
        Or_error.all (List.map items ~f:(Pull_request.of_json t.instance.kind))
      | _ -> Or_error.errorf "%s: expected a list" what)
;;

let get_pull t (repo : Forge_repo.t) number =
  let what = sprintf "pull request #%d of %s/%s" number repo.owner repo.name in
  Or_error.bind
    (get_json
       t
       ~what
       ~not_found:(sprintf ": /pr lists the open ones")
       (sprintf "%s/pulls/%d" (Forge_repo.api_url repo) number))
    ~f:(Pull_request.of_json t.instance.kind)
;;

let max_diff_bytes = 1_000_000

let pull_diff t (repo : Forge_repo.t) number =
  let what = sprintf "the diff of #%d" number in
  let result =
    match t.instance.kind with
    | Github ->
      send
        t
        ~accept:"application/vnd.github.diff"
        ~what
        Get
        (sprintf "%s/pulls/%d" (Forge_repo.api_url repo) number)
    | Forgejo ->
      send
        t
        ~accept:"text/plain"
        ~what
        Get
        (sprintf "%s/pulls/%d.diff" (Forge_repo.api_url repo) number)
  in
  Or_error.map result ~f:(fun diff ->
    if String.length diff <= max_diff_bytes
    then diff, false
    else (
      let cut = String.prefix diff max_diff_bytes in
      match String.rindex cut '\n' with
      | Some i -> String.prefix cut (i + 1), true
      | None -> cut, true))
;;

let checks t repo ~sha =
  let commit = sprintf "%s/commits/%s" (Forge_repo.api_url repo) sha in
  let statuses () =
    Or_error.map
      (get_json t ~what:"the commit's statuses" (commit ^ "/status"))
      ~f:Pr_checks.of_statuses
  in
  let open Or_error.Let_syntax in
  let%map items =
    match t.instance.kind with
    | Forgejo -> statuses ()
    | Github ->
      let%bind runs =
        Or_error.map
          (get_json
             t
             ~what:"the commit's check runs"
             (commit ^ "/check-runs?per_page=100"))
          ~f:Pr_checks.of_github_check_runs
      in
      let%map statuses = statuses () in
      runs @ statuses
  in
  Pr_checks.of_items items
;;

let default_branch t repo =
  let what = sprintf "%s/%s" repo.Forge_repo.owner repo.name in
  Or_error.bind
    (get_json t ~what ~not_found:(repo_hint repo) (Forge_repo.api_url repo))
    ~f:(fun json ->
      match Json.member "default_branch" json with
      | Some (`String b) when not (String.is_empty b) -> Ok b
      | _ -> Or_error.errorf "%s has no default branch" what)
;;

let create_pull t (repo : Forge_repo.t) ~title ~body ~head ~base ~draft =
  let title, draft_field =
    match t.instance.kind with
    | Github -> title, [ ("draft", if draft then `True else `False) ]
    | Forgejo ->
      (if draft then Pull_request.forgejo_draft_title title else title), []
  in
  let request =
    `Object
      ([ "title", `String title
       ; "body", `String body
       ; "head", `String head
       ; "base", `String base
       ]
       @ draft_field)
  in
  let what = sprintf "the new pull request %s → %s" head base in
  match
    t.http
      ~cancel:t.cancel
      { meth = Post
      ; url = Forge_repo.api_url repo ^ "/pulls"
      ; headers = headers ~json_body:true t
      ; body = Some (Json.to_string request)
      }
  with
  | Error _ as e -> e
  | Ok response when response.status / 100 = 2 ->
    Or_error.bind
      (parse ~what response.body)
      ~f:(Pull_request.of_json t.instance.kind)
  | Ok response when response.status = 422 || response.status = 409 ->
    let detail = Option.value (message_of_body response.body) ~default:"" in
    if
      String.is_substring (String.lowercase detail) ~substring:"head"
      && (String.is_substring detail ~substring:"invalid"
          || String.is_substring detail ~substring:"not exist")
    then
      Or_error.errorf
        "%s: the branch %s is not on %s: push it first (the push option, or \
         git push -u %s %s)"
        t.instance.host
        head
        t.instance.host
        repo.remote
        head
    else
      Or_error.errorf
        "%s refused the pull request %s → %s: %s"
        t.instance.host
        head
        base
        (if String.is_empty detail
         then sprintf "HTTP %d" response.status
         else detail)
  | Ok response -> http_error t ~what response
;;
