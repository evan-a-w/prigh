open! Core
open! Import

module Account = struct
  type t =
    { instance : Forge_instance.t
    ; credential : Forge_credential.t option
    }
  [@@deriving sexp_of]

  let to_json { instance; credential } =
    let opt f = Option.value_map ~default:`Null ~f in
    `Object
      [ "host", `String instance.host
      ; "kind", `String (Forge_kind.to_string instance.kind)
      ; "label", `String (Forge_kind.label instance.kind)
      ; ( "user"
        , opt (fun s -> `String s) (Option.bind credential ~f:(fun c -> c.user))
        )
      ; ( "method"
        , opt
            (fun (c : Forge_credential.t) ->
               `String (Forge_credential.Method.to_string c.method_))
            credential )
      ; ( "expires_ms"
        , opt
            (fun ms -> `Number (Int.to_string ms))
            (Option.bind credential ~f:(fun c -> c.expires_ms)) )
      ; ("oauth", if Option.is_some instance.client_id then `True else `False)
      ; "oauth_note", opt (fun s -> `String s) (Forge_login.oauth_note instance)
      ]
  ;;
end

type t =
  { http : Forge_http.t
  ; sleep : float -> unit
  ; listen : Forge_login.Listen.t option
  ; getenv : string -> string option
  ; home : string option
  ; now_ms : unit -> int
  ; store : Auth_store.t
  }

type git = Git_forge.Request.t -> Tool_result.t

let create
      ~http
      ~sleep
      ?listen
      ?(getenv = Sys.getenv)
      ?home
      ?(now_ms = Credential.now_ms)
      ~store
      ()
  =
  { http; sleep; listen; getenv; home; now_ms; store }
;;

let settings t =
  Option.value_map t.home ~default:Forge_instance.Settings.empty ~f:(fun home ->
    Forge_instance.Settings.load ~home)
;;

let read_credential t host =
  Or_error.bind
    (Auth_store.read_entry t.store (Forge_credential.key ~host))
    ~f:(function
      | None -> Ok None
      | Some json ->
        (match Forge_credential.of_json json with
         | Ok c -> Ok (Some c)
         | Error e ->
           Or_error.errorf
             "%s: the sign-in to %s is unreadable (%s): sign in again"
             (Auth_store.path t.store)
             host
             (Error.to_string_hum e)))
;;

let resolve t ?kind ~stored host =
  Forge_instance.resolve ~getenv:t.getenv ?kind ?stored (settings t) ~host
;;

let instance t ?kind host =
  let open Or_error.Let_syntax in
  let%bind host = Forge_instance.normalize_host host in
  let%map stored = read_credential t host in
  resolve
    t
    ?kind
    ~stored:(Option.map stored ~f:(fun (c : Forge_credential.t) -> c.kind))
    host
;;

let accounts t =
  let open Or_error.Let_syntax in
  let%map stored =
    Auth_store.entries t.store ~prefix:Forge_credential.key_prefix
  in
  let stored =
    List.filter_map stored ~f:(fun (key, json) ->
      match Forge_credential.of_json json with
      | Ok c ->
        Some (String.chop_prefix_exn key ~prefix:Forge_credential.key_prefix, c)
      | Error _ -> None)
  in
  let hosts =
    [ "github.com"; "codeberg.org" ]
    @ Forge_instance.Settings.hosts (settings t)
    @ List.map stored ~f:fst
    |> List.stable_dedup ~compare:String.compare
  in
  List.map hosts ~f:(fun host ->
    let credential = List.Assoc.find stored ~equal:String.equal host in
    { Account.instance =
        resolve
          t
          ~stored:
            (Option.map credential ~f:(fun (c : Forge_credential.t) -> c.kind))
          host
    ; credential
    })
;;

let login t (instance : Forge_instance.t) ~method_ interaction =
  let open Or_error.Let_syntax in
  let%bind credential =
    Forge_login.login
      ~http:t.http
      ~sleep:t.sleep
      ?listen:t.listen
      ~now_ms:t.now_ms
      instance
      ~method_
      interaction
  in
  let%map () =
    Or_error.ignore_m
      (Auth_store.modify_entry
         t.store
         (Forge_credential.key ~host:instance.host)
         ~f:(fun _ -> Ok (Some (Forge_credential.to_json credential))))
  in
  credential
;;

let logout t host =
  let open Or_error.Let_syntax in
  let%bind host = Forge_instance.normalize_host host in
  let%bind stored = read_credential t host in
  match stored with
  | None -> Or_error.errorf "not signed in to %s" host
  | Some _ ->
    Or_error.ignore_m
      (Auth_store.modify_entry t.store (Forge_credential.key ~host) ~f:(fun _ ->
         Ok None))
;;

let not_signed_in (instance : Forge_instance.t) =
  Or_error.errorf
    "not signed in to %s: sign in under Settings → Git hosting in prigh-web, \
     or with /login %s"
    instance.host
    (Forge_instance.login_arg instance)
;;

let api t ?cancel (instance : Forge_instance.t) =
  let open Or_error.Let_syntax in
  let%bind stored = read_credential t instance.host in
  match stored with
  | None -> not_signed_in instance
  | Some credential
    when not (Forge_credential.needs_refresh ~now_ms:(t.now_ms ()) credential)
    -> Ok (Forge_api.create ?cancel ~http:t.http instance credential)
  | Some _ ->
    (* Re-checked under the lock: another process may have refreshed it. *)
    let%bind json =
      Auth_store.modify_entry
        t.store
        (Forge_credential.key ~host:instance.host)
        ~f:(fun json ->
          match Option.map json ~f:Forge_credential.of_json with
          | None | Some (Error _) -> Ok json
          | Some (Ok current) ->
            if Forge_credential.needs_refresh ~now_ms:(t.now_ms ()) current
            then
              Or_error.map
                (Forge_login.refresh
                   ~http:t.http
                   ?cancel
                   ~now_ms:(t.now_ms ())
                   instance
                   current)
                ~f:(fun fresh -> Some (Forge_credential.to_json fresh))
            else Ok json)
    in
    (match Option.map json ~f:Forge_credential.of_json with
     | Some (Ok credential) ->
       Ok (Forge_api.create ?cancel ~http:t.http instance credential)
     | None | Some (Error _) -> not_signed_in instance)
;;

(* ---- the session's repository --------------------------------------- *)

let host_result r ~of_json = Git_forge.result r ~of_json

let pick_remote t (info : Git_forge.Info.t) =
  let parsed =
    List.filter_map info.remotes ~f:(fun (r : Git_forge.Remote.t) ->
      Option.map (List.find_map r.urls ~f:Forge_remote.parse) ~f:(fun p -> r, p))
  in
  let named name =
    List.find parsed ~f:(fun ((r : Git_forge.Remote.t), _) ->
      String.equal r.name name)
  in
  let known () =
    match accounts t with
    | Error _ -> None
    | Ok accounts ->
      List.find parsed ~f:(fun (_, (p : Forge_remote.t)) ->
        List.exists accounts ~f:(fun a -> String.equal a.instance.host p.host))
  in
  match Option.bind info.upstream_remote ~f:named with
  | Some r -> Some r
  | None ->
    (match named "origin" with
     | Some r -> Some r
     | None ->
       (match known () with
        | Some r -> Some r
        | None -> List.hd parsed))
;;

let repo t ~git =
  let open Or_error.Let_syntax in
  let%bind info =
    host_result
      (git Git_forge.Request.Info)
      ~of_json:[%of_jsonaf: Git_forge.Info.t]
  in
  match pick_remote t info with
  | None ->
    Or_error.errorf
      "no GitHub or Forgejo remote in %s (git remote -v lists %s): add one \
       with git remote add origin URL"
      info.root
      (match info.remotes with
       | [] -> "none"
       | remotes ->
         String.concat
           ~sep:", "
           (List.map remotes ~f:(fun (r : Git_forge.Remote.t) ->
              r.name ^ " " ^ String.concat ~sep:" " r.urls)))
  | Some (remote, parsed) ->
    let%map instance = instance t parsed.host in
    ( { Forge_repo.instance
      ; owner = parsed.owner
      ; name = parsed.name
      ; remote = remote.name
      }
    , info )
;;

module Pulls = struct
  type t =
    { repo : Forge_repo.t
    ; prs : Pull_request.t list
    }
  [@@deriving sexp_of]

  let to_json { repo; prs } =
    `Object
      [ "repo", Forge_repo.to_json repo
      ; "prs", `Array (List.map prs ~f:Pull_request.to_json)
      ]
  ;;
end

module Pull = struct
  type t =
    { repo : Forge_repo.t
    ; pr : Pull_request.t
    ; checks : Pr_checks.t Or_error.t option
    ; diff : (string * bool) option
    }
  [@@deriving sexp_of]

  let to_json { repo; pr; checks; diff } =
    let diff, truncated = Option.value diff ~default:("", false) in
    `Object
      ([ "repo", Forge_repo.to_json repo; "pr", Pull_request.to_json pr ]
       @ (match checks with
          | None -> []
          | Some c ->
            [ ( "checks"
              , match c with
                | Ok c -> Pr_checks.to_json c
                | Error _ -> `Null )
            ])
       @ [ "diff", `String diff
         ; ("truncated", if truncated then `True else `False)
         ])
  ;;
end

module Defaults = struct
  type t =
    { repo : Forge_repo.t
    ; head : string
    ; base : string
    ; title : string
    ; body : string
    ; commits : string list
    ; pushed : bool
    }
  [@@deriving sexp_of]

  let to_json { repo; head; base; title; body; commits; pushed } =
    `Object
      [ "repo", Forge_repo.to_json repo
      ; "head", `String head
      ; "base", `String base
      ; "title", `String title
      ; "body", `String body
      ; "commits", `Array (List.map commits ~f:(fun s -> `String s))
      ; ("pushed", if pushed then `True else `False)
      ]
  ;;
end

let with_api t ~git ~f =
  let open Or_error.Let_syntax in
  let%bind repo, info = repo t ~git in
  let%bind api = api t repo.instance in
  f api repo info
;;

let list_pulls t ~git ~state =
  let%bind.Or_error state =
    match state with
    | "open" | "closed" | "all" -> Ok state
    | other -> Or_error.errorf "unknown state %S: open, closed or all" other
  in
  with_api t ~git ~f:(fun api repo _ ->
    Or_error.map (Forge_api.list_pulls api repo ~state) ~f:(fun prs ->
      { Pulls.repo; prs }))
;;

let view_pull t ~git ?(diff = true) number =
  with_api t ~git ~f:(fun api repo _ ->
    let open Or_error.Let_syntax in
    let%bind pr = Forge_api.get_pull api repo number in
    let%map diff =
      if diff
      then Forge_api.pull_diff api repo number >>| Option.some
      else Ok None
    in
    let checks =
      if String.is_empty pr.head_sha
      then None
      else Some (Forge_api.checks api repo ~sha:pr.head_sha)
    in
    { Pull.repo; pr; checks; diff })
;;

let humanize_branch branch =
  let last =
    match String.rsplit2 branch ~on:'/' with
    | Some (_, last) -> last
    | None -> branch
  in
  let words =
    String.map last ~f:(function
      | '-' | '_' -> ' '
      | c -> c)
  in
  String.capitalize words
;;

let detached (info : Git_forge.Info.t) =
  Or_error.errorf
    "%s has no current branch (a detached HEAD): git switch -c NAME makes one"
    info.root
;;

let defaults_of api (repo : Forge_repo.t) (info : Git_forge.Info.t) ~git ~base =
  let open Or_error.Let_syntax in
  let%bind head =
    match info.branch with
    | Some b -> Ok b
    | None -> detached info
  in
  let%bind base =
    match base with
    | Some b -> Ok b
    | None -> Forge_api.default_branch api repo
  in
  let%map log =
    host_result
      (git
         (Git_forge.Request.Log { remote = repo.remote; base; branch = head }))
      ~of_json:[%of_jsonaf: Git_forge.Log.t]
  in
  head, base, log
;;

let pull_defaults t ~git =
  with_api t ~git ~f:(fun api repo info ->
    let open Or_error.Let_syntax in
    let%map head, base, log = defaults_of api repo info ~git ~base:None in
    let title, body =
      match log.commits with
      | [ (subject, body) ] -> subject, body
      | [] -> humanize_branch head, ""
      | commits ->
        ( humanize_branch head
        , String.concat
            ~sep:"\n"
            (List.map commits ~f:(fun (subject, _) -> "- " ^ subject)) )
    in
    { Defaults.repo
    ; head
    ; base
    ; title
    ; body
    ; commits = List.map log.commits ~f:fst
    ; pushed = log.pushed
    })
;;

let create_pull t ~git ~title ~body ~base ~head ~draft ~push =
  if String.is_empty (String.strip title)
  then Or_error.error_string "a pull request needs a title"
  else
    with_api t ~git ~f:(fun api repo info ->
      let open Or_error.Let_syntax in
      let%bind head =
        match head, info.branch with
        | Some h, _ -> Ok h
        | None, Some b -> Ok b
        | None, None -> detached info
      in
      let%bind base =
        match base with
        | Some b -> Ok b
        | None -> Forge_api.default_branch api repo
      in
      let%bind () =
        if push
        then
          Or_error.ignore_m
            (host_result
               (git
                  (Git_forge.Request.Push
                     { remote = repo.remote; branch = head }))
               ~of_json:Fn.id)
        else if [%equal: string option] info.branch (Some head)
        then (
          let%bind log =
            host_result
              (git
                 (Git_forge.Request.Log
                    { remote = repo.remote; base; branch = head }))
              ~of_json:[%of_jsonaf: Git_forge.Log.t]
          in
          if log.pushed
          then Ok ()
          else
            Or_error.errorf
              "%s does not have %s with all its commits: push it first (the \
               push option, or git push -u %s %s)"
              repo.remote
              head
              repo.remote
              head)
        else Ok ()
      in
      let%map pr =
        Forge_api.create_pull
          api
          repo
          ~title:(String.strip title)
          ~body
          ~head
          ~base
          ~draft
      in
      { Pull.repo; pr; checks = None; diff = None })
;;

let checkout_pull t ~git ~worktrees_dir number =
  let%bind.Or_error repo, _ = repo t ~git in
  host_result
    (git
       (Git_forge.Request.Checkout
          { remote = repo.remote; number; worktrees_dir }))
    ~of_json:[%of_jsonaf: Git_forge.Checkout.t]
;;
