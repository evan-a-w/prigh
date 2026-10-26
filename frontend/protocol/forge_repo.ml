open! Core

type t =
  { host : string
  ; kind : string
  ; owner : string
  ; name : string
  ; remote : string
  ; url : string
  }
[@@deriving sexp_of, equal]

let slug t = t.owner ^ "/" ^ t.name

let of_json j =
  let open Or_error.Let_syntax in
  let%bind host = Json.string_field j "host" in
  let%bind kind = Json.string_field j "kind" in
  let%bind owner = Json.string_field j "owner" in
  let%bind name = Json.string_field j "name" in
  let%bind remote = Json.string_field j "remote" in
  let%map url = Json.string_field j "url" in
  { host; kind; owner; name; remote; url }
;;
