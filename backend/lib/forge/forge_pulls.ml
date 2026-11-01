open! Core
open! Import
module J = Forge_json

let pull_url repo n = sprintf "%s/pulls/%d" (Forge_repo.api_url repo) n

let of_json (api : Forge_api.t) json =
  Pull_request.of_json api.instance.kind json
;;

let patch (api : Forge_api.t) repo n fields =
  Or_error.bind
    (Forge_api.send_json
       api
       ~body:(`Object fields)
       ~what:(sprintf "pull request #%d" n)
       Patch
       (pull_url repo n))
    ~f:(of_json api)
;;

let edit api repo n ~title ~body ~base =
  let field name =
    Option.value_map ~default:[] ~f:(fun v -> [ name, `String v ])
  in
  match field "title" title @ field "body" body @ field "base" base with
  | [] -> Or_error.error_string "nothing to change: give a title, body or base"
  | fields -> patch api repo n fields
;;

let set_state api repo n ~state =
  match state with
  | "open" | "closed" -> patch api repo n [ "state", `String state ]
  | other -> Or_error.errorf "unknown state %S" other
;;

let strip_wip title =
  let t = String.lstrip title in
  List.find_map [ "WIP:"; "[WIP]" ] ~f:(fun prefix ->
    if String.Caseless.is_prefix t ~prefix
    then Some (String.lstrip (String.drop_prefix t (String.length prefix)))
    else None)
  |> Option.value ~default:title
;;

let set_draft (api : Forge_api.t) repo n ~draft =
  let open Or_error.Let_syntax in
  match api.instance.kind with
  | Forgejo ->
    let%bind pr = Forge_api.get_pull api repo n in
    let title =
      if draft
      then Pull_request.forgejo_draft_title pr.title
      else strip_wip pr.title
    in
    if String.equal title pr.title
    then Ok pr
    else patch api repo n [ "title", `String title ]
  | Github ->
    let what = sprintf "pull request #%d" n in
    let%bind json = Forge_api.get_json api ~what (pull_url repo n) in
    let%bind pr = of_json api json in
    if Bool.equal pr.draft draft
    then Ok pr
    else (
      let%bind id =
        match J.str_opt json [ "node_id" ] with
        | Some id -> Ok id
        | None -> Or_error.errorf "%s: the reply has no node_id" what
      in
      let mutation =
        if draft
        then "convertPullRequestToDraft"
        else "markPullRequestReadyForReview"
      in
      let%map data =
        Forge_api.graphql
          api
          ~what:(sprintf "%s for #%d" mutation n)
          ~query:
            (sprintf
               "mutation($id: ID!) { %s(input: {pullRequestId: $id}) { \
                pullRequest { isDraft } } }"
               mutation)
          ~variables:(`Object [ "id", `String id ])
      in
      (* The mutation's reply says what the pull request is now. *)
      let draft =
        match J.path data [ mutation; "pullRequest"; "isDraft" ] with
        | Some `True -> true
        | Some `False -> false
        | _ -> draft
      in
      { pr with draft })
;;

module Merge_method = struct
  type t =
    | Merge
    | Squash
    | Rebase
  [@@deriving sexp_of]

  let of_string = function
    | "merge" -> Ok Merge
    | "squash" -> Ok Squash
    | "rebase" -> Ok Rebase
    | other ->
      Or_error.errorf "unknown merge method %S: merge, squash or rebase" other
  ;;

  let to_string = function
    | Merge -> "merge"
    | Squash -> "squash"
    | Rebase -> "rebase"
  ;;
end

module Branch_deletion = struct
  type t =
    | Not_asked
    | Deleted
    | Kept of string
  [@@deriving sexp_of, equal]
end

let merge
      (api : Forge_api.t)
      repo
      (pr : Pull_request.t)
      ~method_
      ~title
      ~message
      ~delete_branch
  =
  let open Or_error.Let_syntax in
  let n = pr.number in
  let what = sprintf "merging #%d" n in
  let%bind () =
    match pr.state with
    | Open -> Ok ()
    | state ->
      Or_error.errorf
        "#%d is %s, not open"
        n
        (Pull_request.State.to_string state)
  in
  let opt name =
    Option.value_map ~default:[] ~f:(fun v -> [ name, `String v ])
  in
  (* Only a branch of the base repository itself, never its default branch
     nor the pull request's base (a fork's head is often called [main]). *)
  let%bind (deletion : Branch_deletion.t) =
    if not delete_branch
    then Ok Branch_deletion.Not_asked
    else if not pr.same_repo
    then
      Ok
        (Branch_deletion.Kept
           "it is in another repository (a fork, or one that was deleted), not \
            this one")
    else (
      let%bind default = Forge_api.default_branch api repo in
      if String.equal pr.head default || String.equal pr.head pr.base
      then
        Or_error.errorf
          "refusing to delete %s: it is %s; merge without delete_branch"
          pr.head
          (if String.equal pr.head default
           then "the repository's default branch"
           else "the pull request's base branch")
      else Ok Branch_deletion.Deleted)
  in
  let delete = Branch_deletion.equal deletion Deleted in
  let refused (response : Forge_http.Response.t) =
    let detail =
      Option.value (Forge_api.message_of_body response.body) ~default:""
    in
    Or_error.errorf
      "%s refused to merge #%d (HTTP %d%s): it may have conflicts, failing \
       required checks or missing approvals (pull_request view shows its \
       checks), or its head moved"
      api.instance.host
      n
      response.status
      (if String.is_empty detail then "" else ": " ^ detail)
  in
  let call meth url body =
    match%bind
      api.http
        ~cancel:api.cancel
        { meth
        ; url
        ; headers = Forge_api.headers ~json_body:true api
        ; body = Some (Json.to_string body)
        }
    with
    | r when r.status / 100 = 2 -> Ok ()
    | r when List.mem [ 405; 409; 422 ] r.status ~equal:Int.equal -> refused r
    | r -> Forge_api.http_error api ~what r
  in
  match api.instance.kind with
  | Github ->
    let%bind () =
      call
        Put
        (pull_url repo n ^ "/merge")
        (`Object
            ([ "merge_method", `String (Merge_method.to_string method_) ]
             @ opt "commit_title" title
             @ opt "commit_message" message
             @ opt
                 "sha"
                 (Option.some_if
                    (not (String.is_empty pr.head_sha))
                    pr.head_sha)))
    in
    if delete
    then (
      match
        Forge_api.send
          api
          ~what:(sprintf "deleting the branch %s" pr.head)
          Delete
          (sprintf
             "%s/git/refs/heads/%s"
             (Forge_repo.api_url repo)
             (Forge_api.encode_ref pr.head))
      with
      | Ok _ -> Ok deletion
      | Error e ->
        Ok
          (Branch_deletion.Kept ("deleting it failed: " ^ Error.to_string_hum e)))
    else Ok deletion
  | Forgejo ->
    let%map () =
      call
        Post
        (pull_url repo n ^ "/merge")
        (`Object
            ([ "Do", `String (Merge_method.to_string method_)
             ; ("delete_branch_after_merge", if delete then `True else `False)
             ]
             @ opt "MergeTitleField" title
             @ opt "MergeMessageField" message
             @ opt
                 "head_commit_id"
                 (Option.some_if
                    (not (String.is_empty pr.head_sha))
                    pr.head_sha)))
    in
    (* Forgejo deletes it itself, after the merge. *)
    deletion
;;

module Review = struct
  type t =
    { id : int
    ; author : string
    ; state : string
    ; body : string
    ; submitted_at : string
    }
  [@@deriving sexp_of]

  let normalize = function
    | "REQUEST_CHANGES" -> "CHANGES_REQUESTED"
    | "COMMENT" -> "COMMENTED"
    | s -> s
  ;;

  let of_json json =
    match J.int json [ "id" ] with
    | None -> Or_error.error_string "review: missing id"
    | Some id ->
      Ok
        { id
        ; author = J.str json [ "user"; "login" ]
        ; state = normalize (J.str json [ "state" ])
        ; body = J.str json [ "body" ]
        ; submitted_at = J.str json [ "submitted_at" ]
        }
  ;;

  let to_json t =
    `Object
      [ "id", J.number t.id
      ; "author", `String t.author
      ; "state", `String t.state
      ; "body", `String t.body
      ; "submitted_at", `String t.submitted_at
      ]
  ;;
end

module Line_comment = struct
  type t =
    { id : int
    ; review_id : int option
    ; author : string
    ; path : string
    ; line : int option
    ; body : string
    ; in_reply_to : int option
    ; created_at : string
    ; url : string
    }
  [@@deriving sexp_of]

  let of_json ?review_id json =
    match J.int json [ "id" ] with
    | None -> Or_error.error_string "review comment: missing id"
    | Some id ->
      let first names = List.find_map names ~f:(fun n -> J.int json [ n ]) in
      Ok
        { id
        ; review_id =
            (match review_id with
             | Some _ -> review_id
             | None -> first [ "pull_request_review_id" ])
        ; author = J.str json [ "user"; "login" ]
        ; path = J.str json [ "path" ]
        ; line =
            (* GitHub's [position]s count lines of the diff, not the file;
               Forgejo's (whose comments come per review) are file lines. *)
            first
              (match review_id with
               | Some _ ->
                 [ "line"; "original_line"; "position"; "original_position" ]
               | None -> [ "line"; "original_line" ])
        ; body = J.str json [ "body" ]
        ; in_reply_to = J.int json [ "in_reply_to_id" ]
        ; created_at = J.str json [ "created_at" ]
        ; url = J.str json [ "html_url" ]
        }
  ;;

  let to_json t =
    let opt name =
      Option.value_map ~default:[] ~f:(fun v -> [ name, J.number v ])
    in
    `Object
      ([ "id", J.number t.id ]
       @ opt "review_id" t.review_id
       @ [ "author", `String t.author; "path", `String t.path ]
       @ opt "line" t.line
       @ [ "body", `String t.body ]
       @ opt "in_reply_to" t.in_reply_to
       @ [ "created_at", `String t.created_at; "url", `String t.url ])
  ;;
end

let all f items = Or_error.all (List.map items ~f)

let reviews api repo n =
  Or_error.bind
    (Forge_api.get_list
       api
       ~what:(sprintf "the reviews of #%d" n)
       (pull_url repo n ^ "/reviews"))
    ~f:(all Review.of_json)
;;

let line_comments ?reviews:known (api : Forge_api.t) repo n =
  let open Or_error.Let_syntax in
  match api.instance.kind with
  | Github ->
    Forge_api.get_list
      api
      ~limit:300
      ~what:(sprintf "the review comments of #%d" n)
      (pull_url repo n ^ "/comments")
    >>= all (fun j -> Line_comment.of_json j)
  | Forgejo ->
    let%bind reviews =
      match known with
      | Some reviews -> Ok reviews
      | None -> reviews api repo n
    in
    let%map per_review =
      all
        (fun (r : Review.t) ->
           Forge_api.get_list
             api
             ~what:(sprintf "the comments of review %d" r.id)
             (sprintf "%s/reviews/%d/comments" (pull_url repo n) r.id)
           >>= all (Line_comment.of_json ~review_id:r.id))
        reviews
    in
    List.concat per_review
;;

module Event = struct
  type t =
    | Approve
    | Request_changes
    | Comment
  [@@deriving sexp_of]

  let of_string = function
    | "approve" -> Ok Approve
    | "request_changes" -> Ok Request_changes
    | "comment" -> Ok Comment
    | other ->
      Or_error.errorf
        "unknown review event %S: approve, request_changes or comment"
        other
  ;;
end

module New_comment = struct
  type t =
    { path : string
    ; line : int
    ; body : string
    }

  let of_json json =
    match
      ( J.str_opt json [ "path" ]
      , J.int json [ "line" ]
      , J.str_opt json [ "body" ] )
    with
    | Some path, Some line, Some body -> Ok { path; line; body }
    | _ -> Or_error.error_string "a review comment is {path, line, body}"
  ;;
end

let review (api : Forge_api.t) repo (pr : Pull_request.t) ~event ~body ~comments
  =
  let open Or_error.Let_syntax in
  let kind = api.instance.kind in
  let event =
    match kind, (event : Event.t) with
    | Github, Approve -> "APPROVE"
    | Forgejo, Approve -> "APPROVED"
    | _, Request_changes -> "REQUEST_CHANGES"
    | _, Comment -> "COMMENT"
  in
  let comment (c : New_comment.t) =
    match kind with
    | Github ->
      `Object
        [ "path", `String c.path
        ; "line", J.number c.line
        ; "side", `String "RIGHT"
        ; "body", `String c.body
        ]
    | Forgejo ->
      `Object
        [ "path", `String c.path
        ; "new_position", J.number c.line
        ; "body", `String c.body
        ]
  in
  let request =
    `Object
      ([ "event", `String event; "body", `String body ]
       @ (if String.is_empty pr.head_sha
          then []
          else [ "commit_id", `String pr.head_sha ])
       @
       match comments with
       | [] -> []
       | c -> [ "comments", `Array (List.map c ~f:comment) ])
  in
  Forge_api.send_json
    api
    ~body:request
    ~what:(sprintf "a review of #%d" pr.number)
    Post
    (pull_url repo pr.number ^ "/reviews")
  >>= Review.of_json
;;

let reply (api : Forge_api.t) repo n ~comment_id ~body =
  match api.instance.kind with
  | Forgejo ->
    Or_error.errorf
      "%s (Forgejo) cannot reply in a review thread: comment on the pull \
       request instead, quoting the comment"
      api.instance.host
  | Github ->
    Or_error.bind
      (Forge_api.send_json
         api
         ~body:(`Object [ "body", `String body ])
         ~what:(sprintf "a reply to review comment %d" comment_id)
         ~not_found:": pull_request comments lists the review comments"
         Post
         (sprintf "%s/comments/%d/replies" (pull_url repo n) comment_id))
      ~f:(fun j -> Line_comment.of_json j)
;;

let request_reviewers api repo n reviewers =
  Forge_api.send
    api
    ~body:(`Object [ "reviewers", J.strings_json reviewers ])
    ~what:(sprintf "the reviewers of #%d" n)
    Post
    (pull_url repo n ^ "/requested_reviewers")
  |> Or_error.map ~f:ignore
;;

let closes_re =
  Re.Perl.compile_pat
    ~opts:[ `Caseless ]
    {|\b(close[sd]?|fix(e[sd])?|resolve[sd]?)\s+#(\d+)\b|}
;;

let with_closes body issues =
  let closed =
    Re.all closes_re body
    |> List.filter_map ~f:(fun g -> Int.of_string_opt (Re.Group.get g 3))
  in
  match
    List.filter (List.dedup_and_sort issues ~compare:Int.compare) ~f:(fun n ->
      not (List.mem closed n ~equal:Int.equal))
  with
  | [] -> body
  | missing ->
    let lines =
      String.concat ~sep:"\n" (List.map missing ~f:(sprintf "Closes #%d"))
    in
    if String.is_empty (String.strip body)
    then lines
    else String.rstrip body ^ "\n\n" ^ lines
;;
