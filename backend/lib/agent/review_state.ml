open! Core
open! Import
module C = Review_comment

module File_key = struct
  module T = struct
    type t =
      { root : string option
      ; path : string
      }
    [@@deriving sexp, compare, equal]
  end

  include T
  include Comparable.Make (T)
end

module Change = struct
  type t =
    | Comment of Review_comment.t
    | Delete of { id : string }
    | Approve of
        { root : string option [@sexp.option] [@jsonaf.option]
        ; path : string
        ; hash : string
        }
    | Unapprove of
        { root : string option [@sexp.option] [@jsonaf.option]
        ; path : string
        }
    | Snapshot of
        { snapshot : Review_snapshot.t
        ; text : string option [@sexp.option] [@jsonaf.option]
        }
  [@@deriving sexp, jsonaf, equal]
end

type t =
  { comments : Review_comment.t list
  ; approvals : string File_key.Map.t
  ; next_id : int
  ; snapshots : Review_snapshot.t list File_key.Map.t
  ; blobs : string String.Map.t
  }
[@@deriving sexp_of, equal]

let empty =
  { comments = []
  ; approvals = File_key.Map.empty
  ; next_id = 1
  ; snapshots = File_key.Map.empty
  ; blobs = String.Map.empty
  }
;;

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
  | Approve { root; path; hash } ->
    { t with approvals = Map.set t.approvals ~key:{ root; path } ~data:hash }
  | Unapprove { root; path } ->
    { t with approvals = Map.remove t.approvals { root; path } }
  | Snapshot { snapshot; text } ->
    let blobs =
      match snapshot.content, text with
      | Text { md5 }, Some text -> Map.set t.blobs ~key:md5 ~data:text
      | (Text _ | Absent | Binary _ | Too_large _), _ -> t.blobs
    in
    { t with
      snapshots =
        Map.add_multi
          t.snapshots
          ~key:{ root = Some snapshot.root; path = snapshot.path }
          ~data:snapshot
    ; blobs
    }
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
  let%bind () = C.Range.validate range ~anchor in
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
  "The user left review comments on the changes in a worktree (each comment's \
   file is its absolute path, in the worktree it names: edit that file, even \
   when it is not under your directory; line numbers are as when they \
   commented: the file may have changed since). Answer every comment with \
   reply_to_comment: say what you did, or why not. Make the changes they ask \
   for; where the user should decide, propose the change with the reply's \
   edits instead of making it (it is applied when they accept it). Resolve a \
   comment (resolve: true) when it needs nothing more. Posts marked new are \
   what you have not seen yet. A comment on text the user selected marks it \
   ⟦like this⟧ in its code and quotes it in <selected>; one with side=\"old\" \
   is on lines the change removed, numbered as in the file before it."
;;

let submit ?root t =
  match
    List.filter t.comments ~f:(fun c ->
      C.unsent c && Option.for_all root ~f:(String.equal c.root))
  with
  | [] ->
    Or_error.error_string
      "no new comments to send: select text in a changed file to comment on it"
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

let approve ?root ~path ~hash () = [ Change.Approve { root; path; hash } ]

let unapprove ?root t ~path =
  if Map.mem t.approvals { root; path }
  then [ Change.Unapprove { root; path } ]
  else []
;;

let snapshot t (snapshot : Review_snapshot.t) ~text =
  let text =
    match snapshot.content with
    | Text { md5 } when not (Map.mem t.blobs md5) -> text
    | Text _ | Absent | Binary _ | Too_large _ -> None
  in
  [ Change.Snapshot { snapshot; text } ]
;;

let snapshots t ~root ~path =
  List.rev
    (Option.value (Map.find t.snapshots { root = Some root; path }) ~default:[])
;;

let blob t ~md5 = Map.find t.blobs md5

module Since = struct
  type t =
    { path : string
    ; index : int
    ; snapshot : Review_snapshot.t
    ; current : Review_snapshot.Content.t
    ; changed : bool
    ; diff : Text_diff.t
    ; note : string option
    }
  [@@deriving sexp_of, equal]
end

let since t ~root ~path ?index ~current () =
  let history = snapshots t ~root ~path in
  let count = List.length history in
  let chosen =
    match index with
    | None -> Option.map (List.last history) ~f:(fun s -> count - 1, s)
    | Some i -> Option.map (List.nth history i) ~f:(fun s -> i, s)
  in
  match chosen, index with
  | None, Some i when count > 0 ->
    Or_error.errorf
      "%s has no review action %d to compare with: it has %d (0 to %d)"
      path
      i
      count
      (count - 1)
  | None, _ ->
    Or_error.errorf
      "%s has no review action to compare with: approve it or comment on it \
       first"
      path
  | Some (index, (snapshot : Review_snapshot.t)), _ ->
    let current_content, current_text = current in
    let text (content : Review_snapshot.Content.t) ~known
      : [ `Text of string | `Binary | `Too_large ] option
      =
      match content with
      | Text { md5 } -> Option.map (known md5) ~f:(fun t -> `Text t)
      | Absent -> Some (`Text "")
      | Binary _ -> Some `Binary
      | Too_large _ -> Some `Too_large
    in
    let old = text snapshot.content ~known:(fun md5 -> blob t ~md5) in
    let new_ = text current_content ~known:(fun _ -> current_text) in
    let changed =
      not (Review_snapshot.Content.equal snapshot.content current_content)
    in
    let nothing = { Text_diff.diff = ""; added = 0; removed = 0 } in
    let note, diff =
      match old, new_ with
      | _ when not changed -> None, nothing
      | Some (`Text old), Some (`Text new_) ->
        None, Text_diff.unified ~path ~old ~new_
      | None, _ -> Some "its text then was not kept", nothing
      | _, None -> Some "it cannot be read now", nothing
      | Some `Too_large, _ | _, Some `Too_large ->
        Some "larger than 1 MiB, so not compared", nothing
      | Some `Binary, _ | _, Some `Binary ->
        Some "a binary file, so not compared", nothing
    in
    Ok
      { Since.path
      ; index
      ; snapshot
      ; current = current_content
      ; changed
      ; diff
      ; note
      }
;;
