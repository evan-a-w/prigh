open! Core

type t =
  { path : string
  ; branch : string
  ; created : bool
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind path = Json.string_field j "path" in
  let%bind branch = Json.string_field j "branch" in
  let%map created = Json.bool_default_field j "created" ~default:true in
  { path; branch; created }
;;
