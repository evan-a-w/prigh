open! Core
open! Import
open Html

let review action = App.Action.Review action

(* The action compared with: the one picked, else the latest. *)
let compared (t : Code_review.t) path =
  let history = Code_review.history t path in
  match Map.find t.picked path with
  | Some (i, _) -> Option.map (List.nth history i) ~f:(fun s -> `Picked s)
  | None -> Option.map (List.last history) ~f:(fun s -> `Latest s)
;;

let label (m : App.Model.t) path s =
  Review_action.label
    (Review.on m.review.review path)
    ~me:(Option.bind m.hello ~f:(fun h -> h.user))
    ~time:(App.Model.message_time m)
    s
;;

let open_menu (m : App.Model.t) path =
  match m.dialog with
  | Some (Review_pick p) ->
    (match Review_picker.which p with
     | Since p' when String.equal p' path -> Some p
     | Since _ | Target | Base -> None)
  | _ -> None
;;

let segment (m : App.Model.t) path ~place ~inject =
  let t = m.review in
  Option.map (compared t path) ~f:(fun compared ->
    let on = Code_review.Layout.equal (Code_review.layout_of t path) Since in
    let time = App.Model.message_time m in
    let text =
      match compared with
      | `Latest s ->
        [ Node.text "Since latest"
        ; span ~cls:"review-since-what" ("(" ^ Review_action.brief ~time s ^ ")")
        ]
      | `Picked s ->
        [ Node.text "Since "
        ; span
            ~cls:"review-since-what picked"
            (String.uncapitalize (label m path s))
        ]
    in
    let title =
      match compared with
      | `Latest s ->
        sprintf
          "What changed since your latest action on the file, whichever it is \
           as you act: now %s (l)"
          (label m path s)
      | `Picked s -> sprintf "What changed since %s (l)" (label m path s)
    in
    let menu = open_menu m path in
    let page =
      match place with
      | `Page -> true
      | `Files -> false
    in
    let id = if page then "review-since-button" else "files-since-button" in
    Node.div
      ~attrs:[ classes [ "review-since" ] [ "on", on ] ]
      [ Node.button
          ~attrs:
            [ classes [ "segment"; "review-since-segment" ] [ "on", on ]
            ; Attr.type_ "button"
            ; Attr.create "aria-pressed" (Bool.to_string on)
            ; Attr.title title
            ; Attr.on_click (fun _ ->
                inject
                  (match compared with
                   | `Latest _ -> review (Follow_latest path)
                   | `Picked _ -> review (Set_view { path; layout = Since })))
            ]
          text
      ; Node.button
          ~attrs:
            [ classes
                [ "segment"; "review-since-more" ]
                [ "open", Option.is_some menu ]
            ; Attr.id id
            ; Attr.type_ "button"
            ; Attr.create "aria-haspopup" "dialog"
            ; Attr.create "aria-expanded" (Bool.to_string (Option.is_some menu))
            ; Attr.create "aria-label" "Compare with another action"
            ; Attr.title
                "Compare with another action on this file: Latest follows your \
                 newest (Shift+L)"
            ; Attr.on_click (fun _ ->
                inject (App.Action.Review_pick (Since path)))
            ]
          [ icon ~cls:"chevron" Chevron ]
      ; (match menu with
         | Some p when Bool.equal page t.open_ ->
           Review_picker_view.view p ~inject
         | Some _ | None -> Node.none)
      ])
;;

let note (m : App.Model.t) path =
  let t = m.review in
  match compared t path, Code_review.since t path with
  | None, _ -> Node.none
  | Some (`Latest s | `Picked s), since ->
    let label =
      Review_action.phrase
        (Review.on m.review.review path)
        ~me:(Option.bind m.hello ~f:(fun h -> h.user))
        ~time:(App.Model.message_time m)
        s
    in
    (match since with
     | None ->
       div
         ~cls:"review-note review-since-note"
         [ lamp (); Node.text (sprintf "Reading what changed since %s…" label) ]
     | Some { changed = false; _ } ->
       div
         ~cls:"review-note review-since-note review-since-same"
         [ Node.text (sprintf "Nothing changed since %s" label)
         ; span ~cls:"review-since-hint" "· the changes as they are, below"
         ]
     | Some { note = Some note; _ } ->
       div
         ~cls:"review-note review-since-note review-since-same"
         [ Node.text
             (sprintf "Changed since %s, but %s: no diff to show" label note)
         ; span ~cls:"review-since-hint" "· the changes, below"
         ]
     | Some { added; removed; _ } ->
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
