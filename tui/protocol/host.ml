open! Core

type t =
  { id : string
  ; name : string
  ; cwd : string
  }
[@@deriving sexp_of, equal]

let backend_id = "backend"

let of_json j =
  let open Or_error.Let_syntax in
  let%bind id = Json.string_field j "id" in
  let%bind name = Json.string_field j "name" in
  let%map cwd = Json.string_field j "cwd" in
  { id; name; cwd }
;;
