open! Core

type t =
  { at : int
  ; lines : int
  ; bytes : int
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind at = Json.int_field j "at" in
  let%bind lines = Json.int_field j "lines" in
  let%map bytes = Json.int_field j "bytes" in
  { at; lines; bytes }
;;

let split t text =
  let at = Int.clamp_exn t.at ~min:0 ~max:(String.length text) in
  String.prefix text at, String.drop_prefix text at
;;
