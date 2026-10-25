open! Core

type t =
  { agent : string
  ; task : string
  ; path : string
  ; branch : string
  ; root : string
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind agent = Json.string_field j "agent" in
  let%bind task = Json.string_field j "task" in
  let%bind path = Json.string_field j "path" in
  let%bind branch = Json.string_field j "branch" in
  let%map root = Json.string_field j "root" in
  { agent; task; path; branch; root }
;;
