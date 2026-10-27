open! Core
open! Import

type t =
  { before : string
  ; index : int
  ; role : Message_point.Role.t
  ; text : string
  ; at : float option
  ; messages : int
  ; tokens : int
  }
[@@deriving sexp_of]

let create messages =
  let outline =
    List.filter_map (Outline.create messages) ~f:(fun (p : Outline.Point.t) ->
      match p.kind with
      | User -> Some (p.index, p.text)
      | Compaction | Error -> None)
    |> Int.Table.of_alist_exn
  in
  List.filter_map
    (Message_point.create messages)
    ~f:(fun (p : Message_point.t) ->
      Option.map p.trim_before ~f:(fun (e : Message_point.Effect.t) ->
        { before = p.message
        ; index = p.index
        ; role = p.role
        ; text = Option.value (Hashtbl.find outline p.index) ~default:p.text
        ; at = p.at
        ; messages = e.messages
        ; tokens = e.tokens
        }))
;;

let to_json points =
  let int n = `Number (Int.to_string n) in
  `Object
    [ ( "points"
      , `Array
          (List.map points ~f:(fun t ->
             `Object
               ([ "before", `String t.before
                ; "index", int t.index
                ; "role", `String (Message_point.Role.name t.role)
                ; "text", `String t.text
                ; "messages", int t.messages
                ; "tokens", int t.tokens
                ]
                @ Option.value_map t.at ~default:[] ~f:(fun at ->
                  [ "at", int (Float.iround_nearest_exn (at *. 1000.)) ])))) )
    ]
;;
