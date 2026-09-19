open! Core

type t =
  { id : string
  ; path : string
  ; name : string option
  ; cwd : string
  ; created_at : string
  ; updated_at : string option
  ; first_prompt : string option
  ; message_count : int
  ; parent : string option
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind id = Json.string_field j "id" in
  let%bind path = Json.string_field j "path" in
  let%bind name = Json.string_opt_field j "name" in
  let%bind cwd = Json.string_field j "cwd" in
  let%bind created_at = Json.string_field j "created_at" in
  let%bind updated_at = Json.string_opt_field j "updated_at" in
  let%bind first_prompt = Json.string_opt_field j "first_prompt" in
  let%bind message_count = Json.int_field j "message_count" in
  let%map parent = Json.string_opt_field j "parent" in
  { id
  ; path
  ; name
  ; cwd
  ; created_at
  ; updated_at
  ; first_prompt
  ; message_count
  ; parent
  }
;;
