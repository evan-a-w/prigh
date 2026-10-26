open! Core
open! Import

(* The comment acted on: a post of the user's made with the action (its
   snapshot is taken in the same call, moments after). *)
let comment comments (s : Review.Snapshot.t) =
  Option.bind s.at ~f:(fun at ->
    List.filter_map comments ~f:(fun (c : Review_comment.t) ->
      List.filter_map c.thread ~f:(fun (p : Review_comment.Post.t) ->
        match p.author, p.at with
        | User, Some posted ->
          let gap = Time_ns.Span.abs (Time_ns.diff at posted) in
          Option.some_if Time_ns.Span.(gap <= of_int_sec 5) gap
        | _ -> None)
      |> List.min_elt ~compare:Time_ns.Span.compare
      |> Option.map ~f:(fun gap -> gap, c))
    |> List.min_elt ~compare:(fun (a, _) (b, _) -> Time_ns.Span.compare a b)
    |> Option.map ~f:snd)
;;

let describe comments (s : Review.Snapshot.t) =
  let on what =
    match comment comments s with
    | Some c -> sprintf "%s on %s" what (Review_comment.Range.label c.range)
    | None -> what
  in
  match s.action with
  | "commented" -> on "Comment"
  | "replied" -> on "Reply"
  | "accepted" -> on "Accepted change"
  | "rejected" -> on "Rejected change"
  | "approved" -> "Approved"
  | "unapproved" -> "Took the approval back"
  | "edited" -> "Edited"
  | other -> String.capitalize other
;;

let phrase comments ~me ~time (s : Review.Snapshot.t) =
  let mine =
    match s.by, me with
    | None, _ -> true
    | Some by, Some me -> String.equal by me
    | Some _, None -> false
  in
  let whose = if mine then "your" else Option.value s.by ~default:"" ^ "'s" in
  let who = if mine then "you" else Option.value s.by ~default:"" in
  let where =
    match comment comments s with
    | Some c -> " on " ^ Review_comment.Range.label c.range
    | None -> ""
  in
  let what =
    match s.action with
    | "commented" -> sprintf "%s comment%s" whose where
    | "replied" -> sprintf "%s reply%s" whose where
    | "accepted" -> sprintf "%s accepted the change%s" who where
    | "rejected" -> sprintf "%s rejected the change%s" who where
    | "approved" -> sprintf "%s approval" whose
    | "unapproved" -> sprintf "%s took the approval back" who
    | "edited" -> sprintf "%s edit" whose
    | other -> sprintf "%s %s" who other
  in
  what
  ^ Option.value_map s.at ~default:"" ~f:(fun at ->
    " · " ^ Message_time.short time at)
;;

let short (s : Review.Snapshot.t) =
  match s.action with
  | "commented" -> "comment"
  | "replied" -> "reply"
  | "accepted" -> "accepted change"
  | "rejected" -> "rejected change"
  | "approved" -> "approval"
  | "unapproved" -> "unapproval"
  | "edited" -> "edit"
  | other -> other
;;

let at ~time (s : Review.Snapshot.t) =
  Option.value_map s.at ~default:"" ~f:(fun at ->
    " · " ^ Message_time.short time at)
;;

let by ~me (s : Review.Snapshot.t) =
  match s.by, me with
  | None, _ -> ""
  | Some by, Some me when String.equal by me -> ""
  | Some by, _ -> " · by " ^ by
;;

let label comments ~me ~time s = describe comments s ^ at ~time s ^ by ~me s
let brief ~time s = short s ^ at ~time s

let picker (t : Code_review.t) ~path ~me ~time =
  let comments = Review.on t.review path in
  let history = Code_review.history t path in
  let picked = Option.map (Map.find t.picked path) ~f:fst in
  let item ~group ~detail ~marked index label =
    { Review_picker.Item.choice = Since { path; index }
    ; group
    ; label
    ; detail
    ; marked
    }
  in
  let latest =
    match List.last history with
    | None -> []
    | Some s ->
      [ item
          ~group:"Follow"
          ~detail:("follows your newest action: " ^ label comments ~me ~time s)
          ~marked:(Option.is_none picked)
          None
          "Latest"
      ]
  in
  let earlier =
    List.mapi history ~f:(fun i s -> i, s)
    |> List.drop_last
    |> Option.value ~default:[]
    |> List.rev
    |> List.map ~f:(fun (i, s) ->
      item
        ~group:"Earlier actions"
        ~detail:""
        ~marked:(Option.exists picked ~f:(Int.equal i))
        (Some i)
        (label comments ~me ~time s))
  in
  Review_picker.since ~path (latest @ earlier)
;;
