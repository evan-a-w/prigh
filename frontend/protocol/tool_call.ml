open! Core

type t =
  { id : string
  ; name : string
  ; arguments : string
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind id = Json.string_field j "id" in
  let%bind name = Json.string_field j "name" in
  let%map arguments = Json.string_field j "arguments" in
  { id; name; arguments }
;;
