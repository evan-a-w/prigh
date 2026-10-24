open! Core

type t =
  { name : string
  ; description : string
  ; path : string
  ; model_invocable : bool
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind name = Json.string_field j "name" in
  let%bind description = Json.string_field j "description" in
  let%bind path = Json.string_field j "path" in
  let%map model_invocable = Json.bool_field j "model_invocable" in
  { name; description; path; model_invocable }
;;
