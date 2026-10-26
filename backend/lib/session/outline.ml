open! Core
open! Import

module Kind = struct
  type t =
    | User
    | Compaction
    | Error
  [@@deriving sexp_of]

  let name = function
    | User -> "user"
    | Compaction -> "compaction"
    | Error -> "error"
  ;;
end

module Point = struct
  type t =
    { index : int
    ; kind : Kind.t
    ; text : string
    ; at : float option
    }
  [@@deriving sexp_of]
end

let text_limit = 240

(* At most [text_limit] bytes, cut at a character's first byte. *)
let cut text =
  let text = String.strip text in
  if String.length text <= text_limit
  then text
  else (
    let rec boundary i =
      if i > 0 && Char.to_int text.[i] land 0xC0 = 0x80
      then boundary (i - 1)
      else i
    in
    String.prefix text (boundary text_limit))
;;

let point index ({ message; at } : Timed_message.t) =
  let point kind text = Some { Point.index; kind; text = cut text; at } in
  match message with
  | User u ->
    (match Injected.of_message ~origin:u.origin u.text with
     | None -> point User u.text
     | Some { source = Some Summary; _ } -> point Compaction ""
     | Some
         { source =
             Some
               ((Goal_start _ | Review _ | Init | Review_comments _) as source)
         ; _
         } -> point User (Injected.Source.title source)
     | Some { source = Some _; _ } -> None
     | Some ({ source = None; _ } as origin) ->
       let body = Injected.body origin u.text in
       (* Notes between turns, nothing of the user's. *)
       if String.is_empty (String.strip body) && List.is_empty u.images
       then None
       else point User body)
  | Assistant { stop_reason = Error e; _ } -> point Error e
  | Assistant _ | Tool_result _ -> None
;;

let create messages =
  List.filter_mapi messages ~f:(fun index (_, message) -> point index message)
;;

let to_json points =
  `Object
    [ ( "points"
      , `Array
          (List.map points ~f:(fun { Point.index; kind; text; at } ->
             `Object
               ([ "index", `Number (Int.to_string index)
                ; "kind", `String (Kind.name kind)
                ; "text", `String text
                ]
                @
                match at with
                | None -> []
                | Some at ->
                  [ ( "at"
                    , `Number
                        (Int.to_string (Float.iround_nearest_exn (at *. 1000.)))
                    )
                  ]))) )
    ]
;;
