open! Core
open! Import

let number args =
  Tool_forge.number
    ~what:"the pull request's number (action=list shows them)"
    args
;;

let short (pr : Pull_request.t) verb =
  sprintf
    "%s #%d %s (%s%s)\n%s"
    verb
    pr.number
    pr.title
    (Pull_request.State.to_string pr.state)
    (if pr.draft then ", draft" else "")
    pr.url
;;

let comments_text
      ~(n : int)
      (conversation : Forge_issues.Comment.t list)
      (reviews : Forge_pulls.Review.t list)
      (lines : Forge_pulls.Line_comment.t list)
  =
  let section title = function
    | [] -> []
    | items -> "" :: (title ^ ":") :: items
  in
  let conversation =
    List.map conversation ~f:(fun c ->
      sprintf
        "--- %s, %s (comment %d)\n%s"
        c.author
        c.created_at
        c.id
        (String.strip c.body))
  in
  let reviews =
    List.filter_map reviews ~f:(fun r ->
      if String.equal r.state "PENDING"
      then None
      else
        Some
          (sprintf
             "--- %s %s, %s (review %d)%s"
             r.author
             r.state
             r.submitted_at
             r.id
             (if String.is_empty (String.strip r.body)
              then ""
              else "\n" ^ String.strip r.body)))
  in
  let lines =
    List.map lines ~f:(fun c ->
      sprintf
        "--- %s on %s%s, %s (comment_id %d%s)\n%s"
        c.author
        c.path
        (Option.value_map c.line ~default:"" ~f:(sprintf ":%d"))
        c.created_at
        c.id
        (Option.value_map
           c.in_reply_to
           ~default:""
           ~f:(sprintf ", replying to %d"))
        (String.strip c.body))
  in
  match conversation, reviews, lines with
  | [], [], [] -> sprintf "#%d has no comments or reviews" n
  | _ ->
    String.concat
      ~sep:"\n"
      (sprintf "comments on #%d" n
       :: (section "conversation" conversation
           @ section "reviews" reviews
           @ section "line comments" lines))
;;

let run ~forges ~git ~args action =
  let open Or_error.Let_syntax in
  let with_repo f = Forges.with_repo forges ~git ~f in
  let body_opt () =
    Option.filter (Tool_args.string_opt args "body") ~f:(fun b ->
      not (String.is_empty (String.strip b)))
  in
  let comment_first api repo n =
    match body_opt () with
    | None -> Ok ()
    | Some body -> Forge_issues.comment api repo n ~body >>| ignore
  in
  match action with
  | "comments" ->
    let n = number args in
    Some
      (with_repo (fun api repo ->
         let%bind conversation = Forge_issues.comments api repo n in
         let%bind reviews = Forge_pulls.reviews api repo n in
         let%map lines = Forge_pulls.line_comments ~reviews api repo n in
         comments_text ~n conversation reviews lines))
  | "comment" ->
    let n = number args in
    let body = Tool_forge.required_string args "body" ~why:"the comment" in
    Some
      (with_repo (fun api repo ->
         Forge_issues.comment api repo n ~body
         >>| fun c -> sprintf "commented on #%d (comment %d)\n%s" n c.id c.url))
  | "edit" ->
    let n = number args in
    let closes = Tool_forge.int_list args "closes" in
    Some
      (with_repo (fun api repo ->
         let%bind body =
           match Tool_args.string_opt args "body", closes with
           | body, [] -> Ok body
           | Some body, closes ->
             Ok (Some (Forge_pulls.with_closes body closes))
           | None, closes ->
             let%map pr = Forge_api.get_pull api repo n in
             let body = Forge_pulls.with_closes pr.body closes in
             Option.some_if (not (String.equal body pr.body)) body
         in
         Forge_pulls.edit
           api
           repo
           n
           ~title:(Tool_args.string_opt args "title")
           ~body
           ~base:(Tool_args.string_opt args "base")
         >>| fun pr -> short pr "edited"))
  | "close" | "reopen" ->
    let n = number args in
    let state = if String.equal action "close" then "closed" else "open" in
    Some
      (with_repo (fun api repo ->
         let%bind () = comment_first api repo n in
         Forge_pulls.set_state api repo n ~state
         >>| fun pr ->
         short pr (if String.equal action "close" then "closed" else "reopened")))
  | "ready" | "draft" ->
    let n = number args in
    let draft = String.equal action "draft" in
    Some
      (with_repo (fun api repo ->
         Forge_pulls.set_draft api repo n ~draft
         >>| fun pr ->
         short pr (if draft then "made a draft:" else "ready for review:")))
  | "merge" ->
    let n = number args in
    Some
      (let%bind method_ =
         Forge_pulls.Merge_method.of_string
           (Option.value (Tool_args.string_opt args "method") ~default:"merge")
       in
       let delete_branch =
         Option.value (Tool_args.bool_opt args "delete_branch") ~default:false
       in
       with_repo (fun api repo ->
         let%bind pr = Forge_api.get_pull api repo n in
         let%bind () =
           Forge_pulls.merge
             api
             repo
             pr
             ~method_
             ~title:(Tool_args.string_opt args "title")
             ~message:(body_opt ())
             ~delete_branch
         in
         return
           (sprintf
              "merged #%d %s (%s) into %s%s\n%s"
              pr.number
              pr.title
              (Option.value
                 (Tool_args.string_opt args "method")
                 ~default:"merge")
              pr.base
              (if delete_branch then sprintf ", deleted %s" pr.head else "")
              pr.url)))
  | "review" ->
    let n = number args in
    Some
      (let%bind event =
         match Tool_args.string_opt args "event" with
         | Some e -> Forge_pulls.Event.of_string e
         | None ->
           Or_error.error_string
             "missing \"event\": approve, request_changes or comment"
       in
       let%bind comments =
         Or_error.all
           (List.map
              (Option.value (Tool_args.list_opt args "comments") ~default:[])
              ~f:Forge_pulls.New_comment.of_json)
       in
       let body = Option.value (Tool_args.string_opt args "body") ~default:"" in
       with_repo (fun api repo ->
         let%bind pr = Forge_api.get_pull api repo n in
         Forge_pulls.review api repo pr ~event ~body ~comments
         >>| fun r ->
         sprintf
           "reviewed #%d %s: %s%s (review %d)\n%s"
           pr.number
           pr.title
           r.state
           (match comments with
            | [] -> ""
            | c ->
              sprintf
                ", %d line comment%s"
                (List.length c)
                (if List.length c = 1 then "" else "s"))
           r.id
           pr.url))
  | "reply" ->
    let n = number args in
    let body = Tool_forge.required_string args "body" ~why:"the reply" in
    Some
      (match Tool_args.int_opt args "comment_id" with
       | None ->
         Or_error.error_string
           "missing \"comment_id\": the line comment (action=comments lists \
            them)"
       | Some comment_id ->
         with_repo (fun api repo ->
           Forge_pulls.reply api repo n ~comment_id ~body
           >>| fun c ->
           sprintf
             "replied to comment %d on #%d (comment %d)\n%s"
             comment_id
             n
             c.id
             c.url))
  | "request_reviewers" ->
    let n = number args in
    Some
      (match Tool_forge.strings args "reviewers" with
       | [] -> Or_error.error_string "missing \"reviewers\": the logins"
       | reviewers ->
         with_repo (fun api repo ->
           Forge_pulls.request_reviewers api repo n reviewers
           >>| fun () ->
           sprintf
             "asked %s to review #%d"
             (String.concat ~sep:", " reviewers)
             n))
  | _ -> None
;;
