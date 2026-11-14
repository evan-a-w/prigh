open! Core
open! Import
module Known = Session_link.Known

let current = ref (None, Known.empty)

let set sessions =
  match !current with
  | Some last, _ when phys_equal last sessions -> ()
  | _ -> current := Some sessions, Known.create sessions
;;

let known () = snd !current
let stamp () = Known.stamp (known ())
let id_attribute = "data-session-link"

let anchor id children =
  (* A known session's link has its whole id: a prefix can become
     ambiguous. *)
  let id, tooltip =
    match Known.find (known ()) id with
    | Found s ->
      ( s.id
      , sprintf "Session %s: %s" (String.prefix s.id 8) (Session_link.title s)
      )
    | Ambiguous _ | Missing _ -> id, "Session " ^ id
  in
  Node.a
    ~attrs:
      [ Attr.href (Session_link.href id)
      ; Attr.class_ "session-link"
      ; Attr.create id_attribute id
      ; Attr.title tooltip
      ]
    children
;;

let linkify text =
  match Session_link.ids (known ()) text with
  | [] -> [ Node.text text ]
  | spans ->
    let piece pos stop =
      if stop > pos
      then [ Node.text (String.sub text ~pos ~len:(stop - pos)) ]
      else []
    in
    let pos, rev =
      List.fold spans ~init:(0, []) ~f:(fun (pos, rev) (at, len, _) ->
        let id = String.sub text ~pos:at ~len in
        ( at + len
        , (anchor id [ Node.text id ] :: List.rev (piece pos at)) @ rev ))
    in
    List.rev rev @ piece pos (String.length text)
;;

let whole text =
  match Session_link.ids (known ()) (String.strip text) with
  | [ (0, len, _) ] when len = String.length (String.strip text) ->
    Some (anchor (String.strip text) [ Node.code [ Node.text text ] ])
  | _ -> None
;;

let within known f =
  let saved = !current in
  current := None, known;
  Exn.protect ~f ~finally:(fun () -> current := saved)
;;
