open! Core

type t =
  { first : Content.Line.t
  ; rest : Content.Line.t
  ; line : Content.Line.t
  }

let gutter_width = 2
let flush line = { first = []; rest = []; line }

let in_gutter (span : Content.Span.t) : Content.Line.t =
  [ { span with text = span.text ^ " " } ]
;;

let pad n : Content.Line.t =
  [ { text = String.make n ' '; style = Style.plain } ]
;;

let mark span line = { first = in_gutter span; rest = pad gutter_width; line }

let bar span line =
  let gutter = in_gutter span in
  { first = gutter; rest = gutter; line }
;;

let indent ?(depth = 1) line =
  let gutter = pad (depth * gutter_width) in
  { first = gutter; rest = gutter; line }
;;

let wrap_one { first; rest; line } ~width =
  let gutter = Content.Line.width first in
  if List.is_empty line
  then
    [ List.filter_map first ~f:(fun span ->
        let text = String.rstrip span.text in
        Option.some_if (not (String.is_empty text)) { span with text })
    ]
  else (
    match Content.Line.wrap line ~width:(Int.max 1 (width - gutter)) with
    | [] -> [ first ]
    | head :: tail -> (first @ head) :: List.map tail ~f:(fun l -> rest @ l))
;;

let wrap ts ~width = List.concat_map ts ~f:(wrap_one ~width)
