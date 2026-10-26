open! Core
open! Import

let dim = Style.dim Style.plain

let state_style : P.Pull_request.State.t -> Style.t = function
  | Open -> Style.fg Green
  | Merged -> Style.fg Magenta
  | Closed -> Style.fg Red
;;

let meta ?(state = true) (pr : P.Pull_request.t) =
  String.concat
    ~sep:" · "
    ([ pr.author; sprintf "%s → %s" pr.head pr.base ]
     @ (if pr.draft then [ "draft" ] else [])
     @
     match pr.state with
     | (Closed | Merged) when state ->
       [ P.Pull_request.State.to_string pr.state ]
     | Open | Closed | Merged -> [])
;;

let item (pr : P.Pull_request.t) =
  Picker.Item.create
    ~id:(Int.to_string pr.number)
    ~detail:(meta pr)
    ~search:(sprintf "#%d %s %s %s" pr.number pr.title pr.author pr.head)
    ~dimmed:(not (P.Pull_request.State.equal pr.state Open))
    (sprintf "#%d %s" pr.number pr.title)
;;

let picker_title (list : P.Pr_list.t) ~state =
  sprintf
    "Pull requests in %s (%s)"
    (P.Forge_repo.slug list.repo)
    (Option.value state ~default:"open")
;;

let check_mark = function
  | "success" -> "✓", Style.fg Green
  | "failure" -> "✗", Style.fg Red
  | "pending" -> "●", Style.fg Yellow
  | _ -> "–", Style.fg Gray
;;

let checks (checks : P.Pr_checks.t option) : Content.t =
  match checks with
  | None -> [ Content.Line.of_string ~style:dim "checks: could not be read" ]
  | Some { state = "none"; _ } | Some { items = []; _ } ->
    [ Content.Line.of_string ~style:dim "checks: none" ]
  | Some { state; items } ->
    let mark, style = check_mark state in
    let width =
      List.fold items ~init:0 ~f:(fun acc (i : P.Pr_checks.Item.t) ->
        Int.max acc (Text_width.string i.name))
    in
    [ { Content.Span.text = "checks "; style = Style.bold Style.plain }
    ; { text = mark ^ " " ^ state; style }
    ]
    :: List.map items ~f:(fun (i : P.Pr_checks.Item.t) ->
      let mark, style = check_mark i.state in
      ([ { text = "  " ^ mark ^ " "; style }
       ; { text = Text_width.pad_right i.name ~width; style = Style.plain }
       ; { text = "  " ^ i.state; style = dim }
       ]
       : Content.Line.t))
;;

let render (d : P.Pr_detail.t) : Content.t =
  let pr = d.pr in
  let header : Content.t =
    [ [ { text = sprintf "#%d %s" pr.number pr.title
        ; style = Style.bold Style.plain
        }
      ]
    ; [ { text = P.Pull_request.State.to_string pr.state
        ; style = state_style pr.state
        }
      ; { text = " · " ^ meta ~state:false pr; style = Style.plain }
      ]
    ; [ { text = pr.url; style = Style.link dim pr.url } ]
    ]
  in
  let body =
    if String.is_empty (String.strip pr.body)
    then [ Content.Line.of_string ~style:dim "(no description)" ]
    else Markdown.render pr.body
  in
  let diff =
    let git_diff = P.Pr_detail.git_diff d in
    if String.is_empty git_diff.diff
    then [ Content.Line.of_string ~style:dim "no changes" ]
    else
      Diff_files.render { git_diff with truncated = false }
      @
      if d.truncated
      then
        [ Content.Line.of_string
            ~style:(Style.fg Yellow)
            ("the diff is cut short: the rest is at " ^ pr.url)
        ]
      else []
  in
  header
  @ [ [] ]
  @ body
  @ [ [] ]
  @ checks d.checks
  @ [ [] ]
  @ diff
  @ [ []
    ; Content.Line.of_string
        ~style:dim
        (sprintf
           "/pr checkout %d checks it out · /pr lists the others"
           pr.number)
    ]
;;
