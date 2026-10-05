open! Core

type t =
  { from : string
  ; to_ : string
  ; error : string
  }
[@@deriving sexp_of, equal]

(* The error may contain anything, parentheses and commas included: it ends
   at the last [), so ] before the last [ takes over]. *)
let parse text =
  let open Option.Let_syntax in
  let%bind body = String.chop_prefix text ~prefix:"[prigh: " in
  let%bind stop =
    List.last
      (String.substr_index_all
         body
         ~may_overlap:false
         ~pattern:" takes over this conversation")
  in
  let head = String.prefix body stop in
  let%bind from, rest = String.lsplit2 head ~on:' ' in
  let%bind rest = String.chop_prefix rest ~prefix:"cannot continue (" in
  let separator = "), so " in
  let%bind i =
    List.last
      (String.substr_index_all rest ~may_overlap:false ~pattern:separator)
  in
  let error = String.prefix rest i in
  let to_ = String.drop_prefix rest (i + String.length separator) in
  let%map () =
    Option.some_if ((not (String.is_empty to_)) && not (String.mem to_ ' ')) ()
  in
  { from; to_; error }
;;

let summary t = sprintf "↪ handed over from %s to %s (%s)" t.from t.to_ t.error
