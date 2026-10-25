open! Core
open! Import
module C = Review_comment

module Change = struct
  type t =
    | Comment of Review_comment.t
    | Delete of { id : string }
    | Approve of
        { path : string
        ; hash : string
        }
    | Unapprove of { path : string }
  [@@deriving sexp, jsonaf, equal]
end

type t =
  { comments : Review_comment.t list
  ; approvals : string String.Map.t
  ; next_id : int
  }
[@@deriving sexp_of, equal]

let empty = { comments = []; approvals = String.Map.empty; next_id = 1 }

let number id =
  Option.bind (String.chop_prefix id ~prefix:"c") ~f:Int.of_string_opt
;;

let apply t (change : Change.t) =
  match change with
  | Comment c ->
    let replaced = ref false in
    let comments =
      List.map t.comments ~f:(fun old ->
        if String.equal old.id c.id
        then (
          replaced := true;
          c)
        else old)
    in
    let next_id =
      match number c.id with
      | Some n -> Int.max t.next_id (n + 1)
      | None -> t.next_id
    in
    { t with
      comments = (if !replaced then comments else comments @ [ c ])
    ; next_id
    }
  | Delete { id } ->
    { t with
      comments = List.filter t.comments ~f:(fun c -> not (String.equal c.id id))
    }
  | Approve { path; hash } ->
    { t with approvals = Map.set t.approvals ~key:path ~data:hash }
  | Unapprove { path } -> { t with approvals = Map.remove t.approvals path }
;;

let of_changes = List.fold ~init:empty ~f:apply

let find t id =
  match List.find t.comments ~f:(fun c -> String.equal c.id id) with
  | Some c -> Ok c
  | None ->
    Or_error.errorf
      "no review comment %S (the comments: %s)"
      id
      (match t.comments with
       | [] -> "none"
       | comments ->
         String.concat ~sep:", " (List.map comments ~f:(fun c -> c.C.id)))
;;

let post ~author ~text ?proposal ~at () =
  { C.Post.author; text; at; proposal; delivered = C.Author.equal author Agent }
;;

let non_empty ~what text =
  let text = String.strip text in
  if String.is_empty text
  then Or_error.errorf "the %s is empty: write something first" what
  else Ok text
;;

let changed c = Ok (c, [ Change.Comment c ])

let add t ~root ~path ~(range : C.Range.t) ~anchor ~text ~at =
  let open Or_error.Let_syntax in
  let%bind text = non_empty ~what:"comment" text in
  let%bind () =
    if range.first < 1 || range.last < range.first
    then
      Or_error.errorf
        "lines %d-%d are not a range of lines: the first is 1 or more and the \
         last no smaller"
        range.first
        range.last
    else Ok ()
  in
  let%bind () =
    if String.is_empty (String.strip path)
    then Or_error.error_string "a comment needs the file's path"
    else Ok ()
  in
  changed
    { C.id = sprintf "c%d" t.next_id
    ; root
    ; path
    ; range
    ; anchor
    ; status = Open
    ; thread = [ post ~author:User ~text ~at () ]
    }
;;

let reply t ~id ~author ~text ?(edits = []) ?(resolve = false) ~at () =
  let open Or_error.Let_syntax in
  let%bind c = find t id in
  let%bind text = non_empty ~what:"reply" text in
  let%bind () =
    match author, edits with
    | C.Author.User, _ :: _ ->
      Or_error.error_string "only the agent's replies propose edits"
    | _ -> Ok ()
  in
  let%bind () =
    if List.exists edits ~f:(fun e -> String.is_empty e.C.Edit.old_text)
    then
      Or_error.error_string
        "an edit's old_text is empty: give the exact text to replace, as for \
         the edit tool"
    else Ok ()
  in
  let proposal =
    match edits with
    | [] -> None
    | edits -> Some { C.Proposal.edits; state = Pending }
  in
  let status : C.Status.t =
    match author with
    | User -> Open
    | Agent -> if resolve then Resolved else c.status
  in
  changed
    { c with
      thread = c.thread @ [ post ~author ~text ?proposal ~at () ]
    ; status
    }
;;

let set_status t ~id status =
  Or_error.bind (find t id) ~f:(fun c -> changed { c with status })
;;

let delete t ~id =
  Or_error.map (find t id) ~f:(fun _ -> [ Change.Delete { id } ])
;;

let pending_proposal t ~id ~post =
  let open Or_error.Let_syntax in
  let%bind c = find t id in
  match List.nth c.thread post with
  | Some { proposal = Some ({ state = Pending; _ } as p); _ } -> Ok (c, p)
  | Some { proposal = Some { state; _ }; _ } ->
    Or_error.errorf
      "that change was %s already"
      (C.Proposal.State.to_string state)
  | Some { proposal = None; _ } | None ->
    Or_error.errorf "%s has no proposed change at post %d" id post
;;

let proposal t ~id ~post =
  Or_error.map (pending_proposal t ~id ~post) ~f:(fun (c, p) -> c, p.edits)
;;

let decide t ~id ~post:index (state : C.Proposal.State.t) ~at =
  let open Or_error.Let_syntax in
  let%bind c, p = pending_proposal t ~id ~post:index in
  let note =
    match state with
    | Accepted -> Some "Accepted your proposed change: it is applied."
    | Rejected -> Some "Rejected your proposed change: it is not applied."
    | Failed e ->
      Some
        (sprintf
           "Accepted your proposed change, but it no longer applies (%s): the \
            file changed since."
           e)
    | Pending -> None
  in
  let thread =
    List.mapi c.thread ~f:(fun i (post : C.Post.t) ->
      if i = index then { post with proposal = Some { p with state } } else post)
    @ Option.to_list
        (Option.map note ~f:(fun text -> post ~author:User ~text ~at ()))
  in
  changed { c with thread }
;;

let instructions =
  "The user left review comments on the changes in the working tree (line \
   numbers are as when they commented: the file may have changed since). \
   Answer every comment with reply_to_comment: say what you did, or why not. \
   Make the changes they ask for; where the user should decide, propose the \
   change with the reply's edits instead of making it (it is applied when they \
   accept it). Resolve a comment (resolve: true) when it needs nothing more. \
   Posts marked new are what you have not seen yet."
;;

let submit t =
  match List.filter t.comments ~f:C.unsent with
  | [] ->
    Or_error.error_string
      "no new comments to send: select lines of a changed file to comment on \
       them"
  | comments ->
    let text =
      String.concat
        ~sep:"\n\n"
        (instructions :: List.map comments ~f:C.to_prompt)
    in
    let changes =
      List.map comments ~f:(fun c ->
        Change.Comment
          { c with
            thread =
              List.map c.thread ~f:(fun p -> { p with C.Post.delivered = true })
          })
    in
    Ok (text, List.length comments, changes)
;;

let approve ~path ~hash = [ Change.Approve { path; hash } ]

let unapprove t ~path =
  if Map.mem t.approvals path then [ Change.Unapprove { path } ] else []
;;
