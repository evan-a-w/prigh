open! Core

type t =
  { paths_deleted : int
  ; generations_deleted : int
  ; message : string option
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind paths_deleted = Json.int_field j "paths_deleted" in
  let%bind generations_deleted = Json.int_field j "generations_deleted" in
  let%map message = Json.string_opt_field j "message" in
  { paths_deleted; generations_deleted; message }
;;
