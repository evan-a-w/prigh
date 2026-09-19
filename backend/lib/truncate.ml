open! Core
open! Import

type t =
  { text : string
  ; truncated : bool
  ; total_lines : int
  ; total_bytes : int
  }
[@@deriving sexp_of]

let default_max_lines = 2000
let default_max_bytes = 50 * 1024

let count_lines s =
  if String.is_empty s
  then 0
  else
    String.count s ~f:(Char.equal '\n')
    + if Char.equal s.[String.length s - 1] '\n' then 0 else 1
;;

let head ?(max_lines = default_max_lines) ?(max_bytes = default_max_bytes) s =
  let total_lines = count_lines s in
  let total_bytes = String.length s in
  let text =
    if total_lines <= max_lines
    then s
    else (
      let lines = List.take (String.split_lines s) max_lines in
      String.concat_lines lines)
  in
  let text =
    if String.length text <= max_bytes
    then text
    else (
      let cut = String.prefix text max_bytes in
      match String.rsplit2 cut ~on:'\n' with
      | Some (keep, _) -> keep ^ "\n"
      | None -> cut)
  in
  { text
  ; truncated = String.length text < total_bytes
  ; total_lines
  ; total_bytes
  }
;;

let tail ?(max_lines = default_max_lines) ?(max_bytes = default_max_bytes) s =
  let total_lines = count_lines s in
  let total_bytes = String.length s in
  let text =
    if total_lines <= max_lines
    then s
    else (
      let lines = String.split_lines s in
      let dropped = List.drop lines (List.length lines - max_lines) in
      String.concat_lines dropped)
  in
  let text =
    if String.length text <= max_bytes
    then text
    else (
      let cut = String.suffix text max_bytes in
      match String.lsplit2 cut ~on:'\n' with
      | Some (_, keep) -> keep
      | None -> cut)
  in
  { text
  ; truncated = String.length text < total_bytes
  ; total_lines
  ; total_bytes
  }
;;
