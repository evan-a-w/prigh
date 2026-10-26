open! Core
open! Import
open Html

let review action = App.Action.Review action

let who (m : App.Model.t) (s : Review.Snapshot.t) =
  match s.by, Option.bind m.hello ~f:(fun h -> h.user) with
  | None, _ -> "you"
  | Some by, Some me when String.equal by me -> "you"
  | Some by, _ -> by
;;

let action_label (m : App.Model.t) (s : Review.Snapshot.t) =
  sprintf
    "%s %s%s"
    (who m s)
    s.action
    (Option.value_map s.at ~default:"" ~f:(fun at ->
       " · " ^ Message_time.short (App.Model.message_time m) at))
;;

(* The action compared with: the one picked, else the last. *)
let compared (t : Code_review.t) path =
  let history = Code_review.history t path in
  let index =
    match Map.find t.picked path with
    | Some (i, _) -> i
    | None -> List.length history - 1
  in
  Option.map (List.nth history index) ~f:(fun s -> index, s)
;;

let segment (m : App.Model.t) path ~inject =
  let t = m.review in
  Option.map (compared t path) ~f:(fun (_, s) ->
    ( "Since " ^ action_label m s
    , "What changed in the file since then: its text then to now (l)"
    , Code_review.Layout.equal (Code_review.layout_of t path) Since
    , inject (review (Set_view { path; layout = Since })) ))
;;

let menu (m : App.Model.t) path ~inject =
  let t = m.review in
  let history = Code_review.history t path in
  match compared t path with
  | Some (current, _)
    when List.length history > 1
         && Code_review.Layout.equal (Code_review.layout_of t path) Since ->
    Node.select
      ~attrs:
        [ Attr.class_ "review-select review-since-menu"
        ; Attr.create "aria-label" "Compare with an earlier action"
        ; Attr.title "Compare with an earlier action on this file"
        ; Attr.on_change (fun _ value ->
            match Int.of_string_opt value with
            | Some index -> inject (review (Pick_since { path; index }))
            | None -> Effect.Ignore)
        ]
      (List.rev
         (List.mapi history ~f:(fun i s ->
            Node.option
              ~attrs:
                [ Attr.value (Int.to_string i)
                ; Attr.bool_property "selected" (i = current)
                ]
              [ Node.text (action_label m s) ])))
  | Some _ | None -> Node.none
;;

let note (m : App.Model.t) path =
  let t = m.review in
  match compared t path, Code_review.since t path with
  | None, _ -> Node.none
  | Some (_, s), None ->
    div
      ~cls:"review-note review-since-note"
      [ lamp ()
      ; Node.text (sprintf "Reading what changed since %s…" (action_label m s))
      ]
  | Some (_, s), Some since ->
    let label = action_label m s in
    (match since with
     | { changed = false; _ } ->
       div
         ~cls:"review-empty review-since-same"
         [ Node.text (sprintf "Nothing changed since %s." label) ]
     | { note = Some note; _ } ->
       div
         ~cls:"review-empty review-since-same"
         [ Node.text
             (sprintf "Changed since %s, but %s: no diff to show." label note)
         ]
     | { added; removed; _ } ->
       div
         ~cls:"review-note review-since-note"
         [ Node.text (sprintf "Since %s: " label)
         ; span ~cls:"review-since-counts" (sprintf "+%d −%d" added removed)
         ; (if removed > 0
            then
              span
                ~cls:"review-since-hint"
                "(removed lines are the file as it was then)"
            else Node.none)
         ])
;;
