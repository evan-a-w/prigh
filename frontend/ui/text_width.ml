open! Core

let uchar u =
  match Uucp.Break.tty_width_hint u with
  | -1 -> 1
  | w -> w
;;

let uchars s =
  let rec go i acc =
    if i >= String.length s
    then List.rev acc
    else (
      let d = Stdlib.String.get_utf_8_uchar s i in
      let n = Stdlib.Uchar.utf_decode_length d in
      let u = Stdlib.Uchar.utf_decode_uchar d in
      let piece = String.sub s ~pos:i ~len:n in
      let w = if Stdlib.Uchar.utf_decode_is_valid d then uchar u else 1 in
      go (i + n) ((piece, w) :: acc))
  in
  go 0 []
;;

let string s =
  if String.for_all s ~f:(fun c -> Char.to_int c < 128)
  then String.length s
  else List.sum (module Int) (uchars s) ~f:snd
;;

let take s ~width =
  if String.for_all s ~f:(fun c -> Char.to_int c < 128)
  then
    if String.length s <= width
    then s, ""
    else String.prefix s width, String.drop_prefix s width
  else (
    let buf = Buffer.create (String.length s) in
    let rec go pieces used =
      match pieces with
      | [] -> Buffer.contents buf, ""
      | (piece, w) :: rest ->
        if used + w > width
        then Buffer.contents buf, String.concat (List.map pieces ~f:fst)
        else (
          Buffer.add_string buf piece;
          go rest (used + w))
    in
    go (uchars s) 0)
;;

let truncate ?(ellipsis = "…") s ~width =
  if string s <= width
  then s
  else (
    let ew = string ellipsis in
    let head, _ = take s ~width:(Int.max 0 (width - ew)) in
    head ^ ellipsis)
;;

let pad_right s ~width =
  let w = string s in
  if w >= width then s else s ^ String.make (width - w) ' '
;;
