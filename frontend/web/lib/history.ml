open! Core

type t =
  { entries : string list
  ; index : int option
  ; saved : string
  }
[@@deriving sexp_of]

let limit = 100
let empty = { entries = []; index = None; saved = "" }
let of_list entries = { empty with entries = List.take entries limit }
let to_list t = t.entries

let add t text =
  let entries =
    match t.entries with
    | last :: _ when String.equal last text -> t.entries
    | entries -> List.take (text :: entries) limit
  in
  { entries; index = None; saved = "" }
;;

let older t ~draft =
  let next = Option.value_map t.index ~default:0 ~f:succ in
  match List.nth t.entries next with
  | None -> None
  | Some text ->
    let saved = if Option.is_none t.index then draft else t.saved in
    Some ({ t with index = Some next; saved }, text)
;;

let newer t =
  match t.index with
  | None -> None
  | Some 0 -> Some ({ t with index = None }, t.saved)
  | Some i ->
    Some ({ t with index = Some (i - 1) }, List.nth_exn t.entries (i - 1))
;;

let browsing t = Option.is_some t.index
