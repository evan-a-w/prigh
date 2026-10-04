open! Core

type t =
  { client_id : string
  ; namespace : string option
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind client_id = Json.string_field j "client_id" in
  let%map namespace = Json.string_opt_field j "namespace" in
  { client_id; namespace }
;;
