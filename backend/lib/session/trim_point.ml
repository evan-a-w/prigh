open! Core
open! Import

type t =
  { before : string
  ; index : int
  ; text : string
  ; at : float option
  ; messages : int
  ; tokens : int
  }
[@@deriving sexp_of]

let create messages =
  let items = Array.of_list messages in
  let tokens_before = Array.create ~len:(Array.length items + 1) 0 in
  Array.iteri items ~f:(fun i (_, (m : Timed_message.t)) ->
    tokens_before.(i + 1)
    <- tokens_before.(i) + Message.estimate_tokens [ m.message ]);
  List.filter_map (Outline.create messages) ~f:(fun (p : Outline.Point.t) ->
    match p.kind, fst items.(p.index) with
    | User, Some before when p.index > 0 ->
      Some
        { before
        ; index = p.index
        ; text = p.text
        ; at = p.at
        ; messages = p.index
        ; tokens = tokens_before.(p.index)
        }
    | (User | Compaction | Error), _ -> None)
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
                ; "text", `String t.text
                ; "messages", int t.messages
                ; "tokens", int t.tokens
                ]
                @ Option.value_map t.at ~default:[] ~f:(fun at ->
                  [ "at", int (Float.iround_nearest_exn (at *. 1000.)) ])))) )
    ]
;;
