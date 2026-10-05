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

(* The error may itself contain parentheses and commas, so it ends at the last
   [), so ] before [takes over]. *)
let parse text =
  let open Option.Let_syntax in
  let%bind rest = String.chop_prefix text ~prefix in
  let%bind () = Option.some_if (String.is_suffix rest ~suffix:"]") () in
  let%bind i = String.substr_index rest ~pattern:cannot in
  let from = String.prefix rest i in
  let rest = String.drop_prefix rest (i + String.length cannot) in
  let%bind j = String.substr_index rest ~pattern:takes_over in
  let before = String.prefix rest j in
  let%bind k =
    List.last (String.substr_index_all before ~may_overlap:false ~pattern:so)
  in
  let error = String.prefix before k in
  let to_ = String.drop_prefix before (k + String.length so) in
  let%map () =
    Option.some_if (not (String.is_empty from || String.is_empty to_)) ()
  in
  { from; to_; error }
;;

let summary t = sprintf "↪ handed over from %s to %s" t.from t.to_
