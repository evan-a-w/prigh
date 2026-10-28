open! Core

type t =
  { summary : string
  ; level : string
  ; spans : int
  ; perfetto : Json.t option
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind summary = Json.string_field j "summary" in
  let%bind level = Json.string_field j "level" in
  let%map spans = Json.int_field j "spans" in
  { summary; level; spans; perfetto = Json.field j "perfetto" }
;;
