open! Core

type t =
  { agent : string
  ; task : string
  ; path : string
  ; branch : string
  ; root : string
  ; host : string option
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind agent = Json.string_field j "agent" in
  let%bind task = Json.string_field j "task" in
  let%bind path = Json.string_field j "path" in
  let%bind branch = Json.string_field j "branch" in
  let%bind root = Json.string_field j "root" in
  let%map host = Json.string_opt_field j "host" in
  { agent; task; path; branch; root; host }
;;
