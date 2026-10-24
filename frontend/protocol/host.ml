open! Core

type t =
  { id : string
  ; name : string
  ; cwd : string
  ; session_id : string option
  ; session_name : string option
  }
[@@deriving sexp_of, equal]

let backend_id = "backend"

let of_json j =
  let open Or_error.Let_syntax in
  let%bind id = Json.string_field j "id" in
  let%bind name = Json.string_field j "name" in
  let%bind cwd = Json.string_field j "cwd" in
  let%bind session_id = Json.string_opt_field j "session_id" in
  let%map session_name = Json.string_opt_field j "session_name" in
  { id; name; cwd; session_id; session_name }
;;
