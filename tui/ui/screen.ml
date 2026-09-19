open! Core

type t =
  { lines : Content.t
  ; cursor : (int * int) option
  ; width : int
  ; height : int
  }
[@@deriving sexp_of]

let to_plain ?(show_cursor = false) t =
  List.mapi t.lines ~f:(fun row line ->
    let text = Content.Line.to_plain line in
    let text =
      match t.cursor with
      | Some (r, c) when show_cursor && r = row ->
        let padded = Text_width.pad_right text ~width:(c + 1) in
        let before, rest = Text_width.take padded ~width:c in
        let _, after = Text_width.take rest ~width:1 in
        before ^ "▏" ^ after
      | _ -> text
    in
    String.rstrip text)
  |> String.concat ~sep:"\n"
;;
