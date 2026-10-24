open! Core

type t =
  { input : int
  ; output : int
  ; cache_read : int
  }
[@@deriving sexp_of, equal]

let zero = { input = 0; output = 0; cache_read = 0 }

let of_json j =
  let open Or_error.Let_syntax in
  let%bind input = Json.int_field j "input" in
  let%bind output = Json.int_field j "output" in
  let%map cache_read = Json.int_field j "cache_read" in
  { input; output; cache_read }
;;
