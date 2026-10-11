open! Core

type t =
  { root : string
  ; against : string
  ; diff : string
  ; truncated : bool
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind root = Json.string_field j "root" in
  let%bind against = Json.string_field j "against" in
  let%bind diff = Json.string_field j "diff" in
  let%map truncated = Json.bool_default_field j "truncated" ~default:false in
  { root; against; diff; truncated }
;;
