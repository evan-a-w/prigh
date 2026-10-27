open! Core
open! Import

type t =
  { id : int
  ; text : string
  ; error : bool
  ; repeats : int
  ; undo : Hidden.t option
  }
[@@deriving sexp_of, equal]

let max_shown = 2

(* About the time it takes to read it: 2.5s for "Archived", 6s at most. *)
let lifetime_ms t =
  if t.error
  then None
  else if Option.is_some t.undo
  then Some 8000
  else
    Some
      (Int.clamp_exn (1500 + (40 * String.length t.text)) ~min:2500 ~max:6000)
;;

let push toasts ~id ?(replaces = []) ?undo ~error text =
  (* Each undoable toast undoes its own. *)
  let same (t : t) =
    String.equal t.text text
    && Bool.equal t.error error
    && Option.is_none t.undo
    && Option.is_none undo
  in
  let repeats =
    List.find toasts ~f:same
    |> Option.value_map ~default:0 ~f:(fun t -> t.repeats + 1)
  in
  let stale (t : t) =
    same t
    || List.exists replaces ~f:(fun prefix -> String.is_prefix t.text ~prefix)
  in
  let toast = { id; text; error; repeats; undo } in
  let toasts = List.filter toasts ~f:(Fn.non stale) @ [ toast ] in
  List.drop toasts (List.length toasts - max_shown), toast
;;

let long = 48

(* Not inside a UTF-8 character. *)
let rec boundary s i =
  if i > 0 && i < String.length s && Char.to_int s.[i] land 0xC0 = 0x80
  then boundary s (i - 1)
  else i
;;

(* The first directory and the last two components, or the ends of it. *)
let elide word =
  let n = String.length word in
  let head =
    match String.rindex (String.prefix word 14) '/' with
    | Some i when i > 0 -> i + 1
    | _ -> boundary word 12
  in
  let tail =
    let from = n - 24 in
    match String.index_from word from '/' with
    | Some i when i > head -> i
    | _ -> boundary word from
  in
  String.prefix word head ^ "…" ^ String.drop_prefix word tail
;;

let short text =
  String.split text ~on:' '
  |> List.map ~f:(fun word ->
    if
      String.length word > long
      && String.mem word '/'
      && not (String.is_substring word ~substring:"://")
    then elide word
    else word)
  |> String.concat ~sep:" "
;;
