open! Core

type t =
  { from : string
  ; to_ : string
  ; error : string
  }
[@@deriving sexp_of, equal]

let prefix = "[prigh: "
let cannot = " cannot continue ("
let so = "), so "
let takes_over = " takes over this conversation"

(* The error may contain anything, parentheses, commas and the phrases around
   it included: it ends at the last [), so ] before the last [ takes over]. *)
let parse text =
  let open Option.Let_syntax in
  let%bind rest = String.chop_prefix text ~prefix in
  let%bind () = Option.some_if (String.is_suffix rest ~suffix:"]") () in
  let%bind i = String.substr_index rest ~pattern:cannot in
  let from = String.prefix rest i in
  let rest = String.drop_prefix rest (i + String.length cannot) in
  let last pattern text =
    List.last (String.substr_index_all text ~may_overlap:false ~pattern)
  in
  let%bind j = last takes_over rest in
  let before = String.prefix rest j in
  let%bind k = last so before in
  let error = String.prefix before k in
  let to_ = String.drop_prefix before (k + String.length so) in
  let model_key s =
    not (String.is_empty s || String.exists s ~f:Char.is_whitespace)
  in
  let%map () = Option.some_if (model_key from && model_key to_) () in
  { from; to_; error }
;;

let summary t = sprintf "↪ handed over from %s to %s" t.from t.to_
