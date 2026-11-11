open! Core

type t =
  { text : string
  ; max : int
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind text = Json.string_field j "text" in
  let%map max = Json.int_field j "max" in
  { text; max }
;;

let set_params text = [ "text", Json.str text ]
