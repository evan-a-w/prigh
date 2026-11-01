open! Core
open! Import
module J = Forge_json

module Issue = struct
  type t =
    { number : int
    ; title : string
    ; state : string
    ; state_reason : string option
    ; author : string
    ; labels : string list
    ; assignees : string list
    ; comments : int
    ; url : string
    ; body : string
    ; created_at : string
    ; updated_at : string
    ; is_pull : bool
    }
  [@@deriving sexp_of]

  let of_json json =
    match J.int json [ "number" ] with
    | None -> Or_error.error_string "issue: missing number"
    | Some number ->
      Ok
        { number
        ; title = J.str json [ "title" ]
        ; state = J.str json [ "state" ]
        ; state_reason = J.str_opt json [ "state_reason" ]
        ; author = J.str json [ "user"; "login" ]
        ; labels = J.strings json [ "labels" ] ~field:"name"
        ; assignees = J.strings json [ "assignees" ] ~field:"login"
        ; comments = Option.value (J.int json [ "comments" ]) ~default:0
        ; url = J.str json [ "html_url" ]
        ; body = J.str json [ "body" ]
        ; created_at = J.str json [ "created_at" ]
        ; updated_at = J.str json [ "updated_at" ]
        ; is_pull =
            (match J.path json [ "pull_request" ] with
             | Some (`Object _) -> true
             | _ -> false)
        }
  ;;

  let to_json t =
    `Object
      ([ "number", J.number t.number
       ; "title", `String t.title
       ; "state", `String t.state
       ]
       @ Option.value_map t.state_reason ~default:[] ~f:(fun r ->
         [ "state_reason", `String r ])
       @ [ "author", `String t.author
         ; "labels", J.strings_json t.labels
         ; "assignees", J.strings_json t.assignees
         ; "comments", J.number t.comments
         ; "url", `String t.url
         ; "body", `String t.body
         ; "created_at", `String t.created_at
         ; "updated_at", `String t.updated_at
         ; ("pull_request", if t.is_pull then `True else `False)
         ])
  ;;
end

module Comment = struct
  type t =
    { id : int
    ; author : string
    ; body : string
    ; created_at : string
    ; url : string
    }
  [@@deriving sexp_of]

  let of_json json =
    match J.int json [ "id" ] with
    | None -> Or_error.error_string "comment: missing id"
    | Some id ->
      Ok
        { id
        ; author = J.str json [ "user"; "login" ]
        ; body = J.str json [ "body" ]
        ; created_at = J.str json [ "created_at" ]
        ; url = J.str json [ "html_url" ]
        }
  ;;

  let to_json t =
    `Object
      [ "id", J.number t.id
      ; "author", `String t.author
      ; "body", `String t.body
      ; "created_at", `String t.created_at
      ; "url", `String t.url
      ]
  ;;
end

module Filter = struct
  type t =
    { state : string
    ; labels : string list
    ; assignee : string option
    ; author : string option
    ; search : string option
    ; limit : int
    }

  let default =
    { state = "open"
    ; labels = []
    ; assignee = None
    ; author = None
    ; search = None
    ; limit = 30
    }
  ;;
end

let issues_url repo = Forge_repo.api_url repo ^ "/issues"
let issue_url repo n = sprintf "%s/%d" (issues_url repo) n
let name (repo : Forge_repo.t) = sprintf "%s/%s" repo.owner repo.name
let with_query = Forge_api.with_query
let parse_all items = Or_error.all (List.map items ~f:Issue.of_json)

let list (api : Forge_api.t) repo (f : Filter.t) =
  let open Or_error.Let_syntax in
  let%bind () =
    match f.state with
    | "open" | "closed" | "all" -> Ok ()
    | other -> Or_error.errorf "unknown state %S: open, closed or all" other
  in
  let what = sprintf "the issues of %s" (name repo) in
  let not_found = Forge_api.repo_hint repo in
  let opt k v = Option.value_map v ~default:[] ~f:(fun v -> [ k, v ]) in
  let labels =
    match f.labels with
    | [] -> []
    | l -> [ "labels", String.concat ~sep:"," l ]
  in
  let%map issues =
    match api.instance.kind, f.search with
    | Github, Some search ->
      let q =
        String.concat
          ~sep:" "
          ([ "repo:" ^ name repo; "is:issue" ]
           @ (match f.state with
              | "all" -> []
              | s -> [ "state:" ^ s ])
           @ List.map f.labels ~f:(fun l -> sprintf "label:\"%s\"" l)
           @ Option.value_map f.assignee ~default:[] ~f:(fun a ->
             [ "assignee:" ^ a ])
           @ Option.value_map f.author ~default:[] ~f:(fun a ->
             [ "author:" ^ a ])
           @ [ search ])
      in
      Forge_api.get_list
        api
        ~limit:f.limit
        ~items:(Json.member "items")
        ~what
        ~not_found
        (with_query
           (Forge_instance.api_url api.instance ^ "/search/issues")
           [ "q", q ])
      >>= parse_all
    | Github, None ->
      (* GitHub's issues include its pull requests: they do not count
         towards [limit], so pages are fetched until it is reached. *)
      Forge_api.get_list
        api
        ~keep:(fun json -> Option.is_none (Json.member "pull_request" json))
        ~limit:f.limit
        ~what
        ~not_found
        (with_query
           (issues_url repo)
           ([ "state", f.state; "sort", "updated"; "direction", "desc" ]
            @ labels
            @ opt "assignee" f.assignee
            @ opt "creator" f.author))
      >>= parse_all
    | Forgejo, search ->
      Forge_api.get_list
        api
        ~limit:f.limit
        ~what
        ~not_found
        (with_query
           (issues_url repo)
           ([ "state", f.state; "type", "issues" ]
            @ labels
            @ opt "assigned_by" f.assignee
            @ opt "created_by" f.author
            @ opt "q" search))
      >>= parse_all
  in
  List.filter issues ~f:(fun i -> not i.is_pull)
;;

let get api repo n =
  Or_error.bind
    (Forge_api.get_json
       api
       ~what:(sprintf "issue #%d of %s" n (name repo))
       (issue_url repo n))
    ~f:Issue.of_json
;;

let comments ?(limit = 100) api repo n =
  Or_error.bind
    (Forge_api.get_list
       api
       ~limit
       ~what:(sprintf "the comments of #%d" n)
       (issue_url repo n ^ "/comments"))
    ~f:(fun items -> Or_error.all (List.map items ~f:Comment.of_json))
;;

(* Forgejo takes label ids (older versions only ids), so names are looked
   up among the repository's labels and its organization's. *)
let label_ids (api : Forge_api.t) repo names =
  let open Or_error.Let_syntax in
  let fetch url =
    Forge_api.get_list api ~limit:1000 ~what:"the labels" url
    >>| List.filter_map ~f:(fun l ->
      match J.str_opt l [ "name" ], J.int l [ "id" ] with
      | Some name, Some id -> Some (name, id)
      | _ -> None)
  in
  let%bind repo_labels = fetch (Forge_repo.api_url repo ^ "/labels") in
  let find labels =
    List.partition_map names ~f:(fun n ->
      match
        List.find labels ~f:(fun (name, _) -> String.Caseless.equal name n)
      with
      | Some (_, id) -> First id
      | None -> Second n)
  in
  let%bind ids, missing, known =
    match find repo_labels with
    | ids, [] -> Ok (ids, [], repo_labels)
    | _ ->
      let org =
        match
          fetch
            (sprintf
               "%s/orgs/%s/labels"
               (Forge_instance.api_url api.instance)
               (Uri.pct_encode repo.owner))
        with
        | Ok l -> l
        | Error _ -> []
      in
      let known = repo_labels @ org in
      let ids, missing = find known in
      Ok (ids, missing, known)
  in
  match missing with
  | [] -> Ok ids
  | missing ->
    Or_error.errorf
      "%s has no label %s (it has %s)"
      (name repo)
      (String.concat ~sep:", " missing)
      (match known with
       | [] -> "none"
       | l -> String.concat ~sep:", " (List.map l ~f:fst))
;;

let labels_json (api : Forge_api.t) repo names =
  match api.instance.kind with
  | Github -> Ok (J.strings_json names)
  | Forgejo ->
    Or_error.map (label_ids api repo names) ~f:(fun ids ->
      `Array (List.map ids ~f:J.number))
;;

let create (api : Forge_api.t) repo ~title ~body ~labels ~assignees =
  let open Or_error.Let_syntax in
  let%bind labels =
    match labels with
    | [] -> Ok []
    | l -> labels_json api repo l >>| fun j -> [ "labels", j ]
  in
  let request =
    `Object
      ([ "title", `String title; "body", `String body ]
       @ labels
       @
       match assignees with
       | [] -> []
       | a -> [ "assignees", J.strings_json a ])
  in
  Forge_api.send_json
    api
    ~body:request
    ~what:(sprintf "the new issue on %s" (name repo))
    ~not_found:(Forge_api.repo_hint repo)
    Post
    (issues_url repo)
  >>= Issue.of_json
;;

let comment api repo n ~body =
  Or_error.bind
    (Forge_api.send_json
       api
       ~body:(`Object [ "body", `String body ])
       ~what:(sprintf "a comment on #%d" n)
       Post
       (issue_url repo n ^ "/comments"))
    ~f:Comment.of_json
;;

let patch api repo n fields =
  Or_error.bind
    (Forge_api.send_json
       api
       ~body:(`Object fields)
       ~what:(sprintf "#%d of %s" n (name repo))
       Patch
       (issue_url repo n))
    ~f:Issue.of_json
;;

let edit api repo n ~title ~body =
  let fields =
    Option.value_map title ~default:[] ~f:(fun t -> [ "title", `String t ])
    @ Option.value_map body ~default:[] ~f:(fun b -> [ "body", `String b ])
  in
  match fields with
  | [] -> Or_error.error_string "nothing to change: give a title or a body"
  | fields -> patch api repo n fields
;;

let set_state (api : Forge_api.t) repo n ~state ~reason =
  let%bind.Or_error () =
    match state, reason with
    | ("open" | "closed"), None -> Ok ()
    | "closed", Some ("completed" | "not_planned") -> Ok ()
    | "closed", Some r ->
      Or_error.errorf "unknown reason %S: completed or not_planned" r
    | _ -> Or_error.errorf "unknown state %S" state
  in
  patch
    api
    repo
    n
    ([ "state", `String state ]
     @
     match api.instance.kind, reason with
     | Github, Some r -> [ "state_reason", `String r ]
     | _ -> [])
;;

let add_labels (api : Forge_api.t) repo n labels =
  let open Or_error.Let_syntax in
  let%bind json = labels_json api repo labels in
  let%bind (_ : Json.t) =
    Forge_api.send_json
      api
      ~body:(`Object [ "labels", json ])
      ~what:(sprintf "the labels of #%d" n)
      Post
      (issue_url repo n ^ "/labels")
  in
  get api repo n
;;

let remove_labels (api : Forge_api.t) repo n labels =
  let open Or_error.Let_syntax in
  let%bind keys =
    match api.instance.kind with
    | Github -> Ok (List.map labels ~f:Forge_api.encode_segment)
    | Forgejo -> label_ids api repo labels >>| List.map ~f:Int.to_string
  in
  let%bind () =
    List.fold keys ~init:(Ok ()) ~f:(fun acc key ->
      let%bind () = acc in
      Forge_api.send
        api
        ~what:(sprintf "a label of #%d" n)
        ~not_found:": the issue does not have that label"
        Delete
        (sprintf "%s/labels/%s" (issue_url repo n) key)
      >>| ignore)
  in
  get api repo n
;;

let assign (api : Forge_api.t) repo n ~add ~remove =
  let open Or_error.Let_syntax in
  match api.instance.kind with
  | Github ->
    let call meth logins =
      match logins with
      | [] -> Ok ()
      | logins ->
        Forge_api.send
          api
          ~body:(`Object [ "assignees", J.strings_json logins ])
          ~what:(sprintf "the assignees of #%d" n)
          meth
          (issue_url repo n ^ "/assignees")
        >>| ignore
    in
    let%bind () = call Post add in
    let%bind () = call Delete remove in
    get api repo n
  | Forgejo ->
    let%bind issue = get api repo n in
    let assignees =
      List.filter
        (List.dedup_and_sort ~compare:String.compare (issue.assignees @ add))
        ~f:(fun a -> not (List.mem remove a ~equal:String.Caseless.equal))
    in
    patch api repo n [ "assignees", J.strings_json assignees ]
;;
