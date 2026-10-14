open! Core

type t =
  { id : string
  ; after : string
  ; output : string
  }
[@@deriving sexp_of, equal]

let moved = ", so it was moved to the background as job "

let parse text =
  let open Option.Let_syntax in
  match String.split_lines text with
  | [] -> None
  | first :: rest ->
    let%bind header = String.chop_prefix first ~prefix:"still running after " in
    let%bind index = String.substr_index header ~pattern:moved in
    let after = String.prefix header index in
    let id =
      String.drop_prefix header (index + String.length moved)
      |> String.take_while ~f:Char.is_alphanum
    in
    let output =
      if String.is_substring first ~substring:"Output so far"
      then
        List.take_while rest ~f:(fun line ->
          not (String.is_prefix line ~prefix:"Its report ("))
        |> String.concat ~sep:"\n"
      else ""
    in
    Option.some_if (not (String.is_empty id)) { id; after; output }
;;
