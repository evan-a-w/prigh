open! Core
open! Import

module Status = struct
  type t =
    | Approved
    | Changed
    | Unapproved
  [@@deriving sexp_of, equal]

  let to_string = function
    | Approved -> "approved"
    | Changed -> "changed since approved"
    | Unapproved -> "not approved"
  ;;
end

let status (review : P.Review.t) (file : Diff_files.File.t) : Status.t =
  match P.Review.approval review file.path with
  | Some { hash; _ } when String.equal hash file.hash -> Approved
  | Some _ -> Changed
  | None -> Unapproved
;;

let approved review files =
  List.count files ~f:(fun f -> Status.equal (status review f) Approved)
;;

let since_counts (s : P.Review_since.File.t) =
  if not s.changed
  then None
  else (
    match s.note with
    | Some _ -> Some "changed"
    | None -> Some (sprintf "+%d −%d" s.added s.removed))
;;

let since_label (s : P.Review_since.File.t) =
  Option.map (since_counts s) ~f:(fun counts ->
    sprintf "%s since %s" counts s.snapshot.action)
;;

let describe status (since : P.Review_since.File.t option) =
  match status, since with
  | Status.Changed, Some ({ snapshot = { action = "approved"; _ }; _ } as s) ->
    (match since_counts s with
     | Some counts -> sprintf "changed since approved (%s)" counts
     | None -> Status.to_string status)
  | _, Some s ->
    (match since_label s with
     | Some label -> sprintf "%s, %s" (Status.to_string status) label
     | None -> Status.to_string status)
  | _, None -> Status.to_string status
;;

let gray = Style.fg Gray
let dim = Style.dim Style.plain

let render ?(since = []) (d : P.Git_diff.t) (review : P.Review.t) : Content.t =
  let files = Diff_files.parse d.diff in
  let head : Content.Line.t =
    [ { text = "review "; style = Style.bold Style.plain }
    ; { text = Diff_files.where d; style = Style.plain }
    ; { text = " against " ^ Diff_files.against d; style = dim }
    ; { text =
          sprintf
            "  %d of %d file%s approved"
            (approved review files)
            (List.length files)
            (if List.length files = 1 then "" else "s")
      ; style = gray
      }
    ]
  in
  let comment (c : P.Review_comment.t) : Content.t =
    let range =
      match c.range.columns with
      | None -> P.Review_comment.Range.label c.range
      | Some _ ->
        let quote =
          match String.split_lines (P.Review_comment.selected c) with
          | [] -> ""
          | [ line ] -> line
          | line :: _ -> line ^ "…"
        in
        sprintf
          "%s “%s”"
          (P.Review_comment.Range.label c.range)
          (Text_width.truncate quote ~width:40)
    in
    let state =
      match c.status with
      | Resolved -> "resolved"
      | Open -> if P.Review_comment.unsent c then "not sent yet" else "open"
    in
    Content.Line.of_string ~style:dim (sprintf "    %s %s, %s" c.id range state)
    :: List.map c.thread ~f:(fun (p : P.Review_comment.Post.t) ->
      let who =
        match p.author with
        | User -> "you"
        | Agent -> "prigh"
      in
      let proposal =
        match p.proposal with
        | None -> ""
        | Some { state = Pending; _ } -> " [proposes a change]"
        | Some { state = Accepted; _ } -> " [change accepted]"
        | Some { state = Rejected; _ } -> " [change rejected]"
        | Some { state = Failed _; _ } -> " [change failed]"
      in
      Content.Line.of_string
        (sprintf
           "      %s: %s%s"
           who
           (String.concat ~sep:" " (String.split_lines (String.strip p.text)))
           proposal))
  in
  let file (f : Diff_files.File.t) : Content.t =
    let status = status review f in
    let mark, style =
      match status with
      | Approved -> "✓", Style.fg Green
      | Changed -> "●", Style.fg Yellow
      | Unapproved -> "○", gray
    in
    ([ { Content.Span.text = "  " ^ mark ^ " "; style }
     ; { text = f.path; style = Style.bold Style.plain }
     ; { text =
           sprintf
             "  +%d −%d  %s"
             f.added
             f.removed
             (describe
                status
                (List.find since ~f:(fun (s : P.Review_since.File.t) ->
                   String.equal s.path f.path)))
       ; style = dim
       }
     ]
     : Content.Line.t)
    :: List.concat_map (P.Review.on review f.path) ~f:comment
  in
  let elsewhere =
    List.filter review.comments ~f:(fun c ->
      not (List.exists files ~f:(fun f -> String.equal f.path c.path)))
  in
  (head :: List.concat_map files ~f:file)
  @ (match elsewhere with
     | [] -> []
     | comments ->
       Content.Line.of_string ~style:gray "  on files no longer changed:"
       :: List.concat_map comments ~f:(fun c ->
         Content.Line.of_string ("  " ^ c.path) :: comment c))
  @ [ Content.Line.of_string
        ~style:gray
        "approve files and comment on code you select in prigh-web's review \
         page; /review HEAD has prigh review the changes itself"
    ]
;;
