open! Core
open! Import

let name = "issue"

let changes action =
  not (List.mem [ "list"; "view" ] action ~equal:String.equal)
;;

let actions =
  "list, view, create, comment, edit, close, reopen, label, unlabel, assign or \
   unassign"
;;

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
      "Issues of the working directory's repository on its git host (GitHub or \
       Forgejo), through the user's sign-in to that host in prigh: no gh CLI \
       needed. list shows them (state open, closed or all; labels, assignee, \
       author and search narrow it). view shows one (number) with its \
       comments. create opens one (title, body, labels, assignees). comment \
       adds body as a comment (also on a pull request, by its number). edit \
       changes title and/or body. close (reason completed or not_planned, \
       GitHub only) and reopen; a body with either is posted as a comment \
       first. label/unlabel add or remove labels; assign/unassign add or \
       remove assignees (logins). To close issues when a pull request merges, \
       give pull_request's closes."
  ; parameters =
      Tool_args.schema
        ~required:[ "action" ]
        [ "action", `String, actions
        ; "number", `Integer, "every action but list and create: the issue"
        ; "state", `String, "list: open (default), closed or all"
        ; ( "labels"
          , `Array (`Object [ "type", `String "string" ])
          , "list: only with all of these; create, label, unlabel: the labels" )
        ; ( "assignees"
          , `Array (`Object [ "type", `String "string" ])
          , "create, assign, unassign: logins" )
        ; "assignee", `String, "list: only those assigned to this login"
        ; "author", `String, "list: only those opened by this login"
        ; "search", `String, "list: words to search for"
        ; "limit", `Integer, "list: at most this many (default 30)"
        ; "title", `String, "create, edit: the title"
        ; ( "body"
          , `String
          , "create, edit: the description (Markdown); comment, close, reopen: \
             the comment" )
        ; "reason", `String, "close: completed (default) or not_planned"
        ; ( "cwd"
          , `String
          , "A directory in the repository (default: this agent's working \
             directory)" )
        ]
  }
;;

let line (i : Forge_issues.Issue.t) =
  let extras =
    List.filter_opt
      [ (match i.labels with
         | [] -> None
         | l -> Some ("[" ^ String.concat ~sep:", " l ^ "]"))
      ; (match i.assignees with
         | [] -> None
         | a -> Some ("→ " ^ String.concat ~sep:", " a))
      ]
  in
  sprintf
    "#%d %s%s (%s, by %s, %d comment%s, updated %s)"
    i.number
    i.title
    (match extras with
     | [] -> ""
     | e -> " " ^ String.concat ~sep:" " e)
    i.state
    i.author
    i.comments
    (if i.comments = 1 then "" else "s")
    (String.prefix i.updated_at 10)
;;

let list_text repo ~(filter : Forge_issues.Filter.t) issues =
  let which =
    if String.equal filter.state "all" then "" else filter.state ^ " "
  in
  match issues with
  | [] -> sprintf "no %sissues in %s" which (Tool_forge.repo_name repo)
  | issues ->
    sprintf
      "%d %sissue%s in %s%s:\n%s"
      (List.length issues)
      which
      (if List.length issues = 1 then "" else "s")
      (Tool_forge.repo_name repo)
      (if List.length issues >= filter.limit
       then sprintf " (the first %d: limit shows more)" filter.limit
       else "")
      (String.concat ~sep:"\n" (List.map issues ~f:line))
;;

let comment_text (c : Forge_issues.Comment.t) =
  sprintf
    "--- %s, %s (comment %d)\n%s"
    c.author
    c.created_at
    c.id
    (String.strip c.body)
;;

let view_text (i : Forge_issues.Issue.t) comments =
  String.concat
    ~sep:"\n"
    ([ sprintf "#%d %s" i.number i.title
     ; sprintf
         "%s%s, opened by %s on %s%s"
         (if i.is_pull then "pull request, " else "")
         (match i.state_reason with
          | Some r when String.equal i.state "closed" -> sprintf "closed (%s)" r
          | _ -> i.state)
         i.author
         (String.prefix i.created_at 10)
         (if i.is_pull
          then " (pull_request view shows its branches and checks)"
          else "")
     ]
     @ (match i.labels with
        | [] -> []
        | l -> [ "labels: " ^ String.concat ~sep:", " l ])
     @ (match i.assignees with
        | [] -> []
        | a -> [ "assignees: " ^ String.concat ~sep:", " a ])
     @ [ i.url
       ; ""
       ; (if String.is_empty (String.strip i.body)
          then "(no description)"
          else Tool_forge.clip_lines 200 (String.strip i.body))
       ]
     @
     match comments with
     | [] -> []
     | comments -> "" :: List.map comments ~f:comment_text)
;;

let summary verb (i : Forge_issues.Issue.t) =
  sprintf "%s #%d %s (%s)\n%s" verb i.number i.title i.state i.url
;;

let create ~forges () =
  let run (context : Tool.Context.t) args =
    let cwd = Tool_forge.cwd context args in
    let git = Tool_forge.git context ~cwd in
    let action = Tool_args.string args "action" in
    let number () =
      Tool_forge.number ~what:"the issue's number (action=list shows them)" args
    in
    let body_opt () =
      Option.filter (Tool_args.string_opt args "body") ~f:(fun b ->
        not (String.is_empty (String.strip b)))
    in
    let with_repo f = Forges.with_repo forges ~git ~f in
    let open Or_error.Let_syntax in
    let comment_first api repo n =
      match body_opt () with
      | None -> Ok ()
      | Some body -> Forge_issues.comment api repo n ~body >>| ignore
    in
    let result =
      match action with
      | "list" ->
        let filter =
          { Forge_issues.Filter.state =
              Option.value (Tool_args.string_opt args "state") ~default:"open"
          ; labels = Tool_forge.strings args "labels"
          ; assignee = Tool_args.string_opt args "assignee"
          ; author = Tool_args.string_opt args "author"
          ; search = Tool_args.string_opt args "search"
          ; limit =
              Option.value_map
                (Tool_args.int_opt args "limit")
                ~default:30
                ~f:(Int.max 1)
          }
        in
        with_repo (fun api repo ->
          Forge_issues.list api repo filter >>| list_text repo ~filter)
      | "view" ->
        let n = number () in
        with_repo (fun api repo ->
          let%bind issue = Forge_issues.get api repo n in
          let%map comments =
            if issue.comments = 0
            then Ok []
            else Forge_issues.comments api repo n
          in
          view_text issue comments)
      | "create" ->
        let title =
          Tool_forge.required_string args "title" ~why:"the new issue's title"
        in
        with_repo (fun api repo ->
          Forge_issues.create
            api
            repo
            ~title
            ~body:(Option.value (Tool_args.string_opt args "body") ~default:"")
            ~labels:(Tool_forge.strings args "labels")
            ~assignees:(Tool_forge.strings args "assignees")
          >>| summary "opened")
      | "comment" ->
        let n = number () in
        let body = Tool_forge.required_string args "body" ~why:"the comment" in
        with_repo (fun api repo ->
          Forge_issues.comment api repo n ~body
          >>| fun c -> sprintf "commented on #%d (comment %d)\n%s" n c.id c.url)
      | "edit" ->
        let n = number () in
        with_repo (fun api repo ->
          Forge_issues.edit
            api
            repo
            n
            ~title:(Tool_args.string_opt args "title")
            ~body:(Tool_args.string_opt args "body")
          >>| summary "edited")
      | "close" | "reopen" ->
        let n = number () in
        let state = if String.equal action "close" then "closed" else "open" in
        let reason =
          if String.equal action "close"
          then Tool_args.string_opt args "reason"
          else None
        in
        with_repo (fun api repo ->
          let%bind () = comment_first api repo n in
          Forge_issues.set_state api repo n ~state ~reason
          >>| summary
                (if String.equal action "close" then "closed" else "reopened"))
      | "label" | "unlabel" ->
        let n = number () in
        let labels =
          match Tool_forge.strings args "labels" with
          | [] ->
            raise (Tool_args.Invalid "missing \"labels\": the label names")
          | l -> l
        in
        with_repo (fun api repo ->
          (if String.equal action "label"
           then Forge_issues.add_labels api repo n labels
           else Forge_issues.remove_labels api repo n labels)
          >>| fun i ->
          sprintf
            "#%d %s: labels %s"
            i.number
            i.title
            (match i.labels with
             | [] -> "none"
             | l -> String.concat ~sep:", " l))
      | "assign" | "unassign" ->
        let n = number () in
        let logins =
          match Tool_forge.strings args "assignees" with
          | [] -> raise (Tool_args.Invalid "missing \"assignees\": the logins")
          | l -> l
        in
        let add, remove =
          if String.equal action "assign" then logins, [] else [], logins
        in
        with_repo (fun api repo ->
          Forge_issues.assign api repo n ~add ~remove
          >>| fun i ->
          sprintf
            "#%d %s: assignees %s"
            i.number
            i.title
            (match i.assignees with
             | [] -> "none"
             | l -> String.concat ~sep:", " l))
      | other ->
        raise
          (Tool_args.Invalid (sprintf "unknown action %S: use %s" other actions))
    in
    match result with
    | Ok text -> Tool.Result.ok text
    | Error e ->
      Tool.Result.error
        (sprintf
           "issue %s: %s"
           action
           (Tool_forge.for_the_model (Error.to_string_hum e)))
  in
  { Tool.spec; run }
;;
