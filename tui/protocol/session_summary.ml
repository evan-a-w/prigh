open! Core

type t =
  { id : string
  ; path : string
  ; cwd : string
  ; created_at : string
  ; first_prompt : string option
  ; message_count : int
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind id = Json.string_field j "id" in
  let%bind path = Json.string_field j "path" in
  let%bind cwd = Json.string_field j "cwd" in
  let%bind created_at = Json.string_field j "created_at" in
  let%bind first_prompt = Json.string_opt_field j "first_prompt" in
  let%map message_count = Json.int_field j "message_count" in
  { id; path; cwd; created_at; first_prompt; message_count }
;;
