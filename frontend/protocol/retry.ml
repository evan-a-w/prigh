open! Core

type t =
  { attempt : int
  ; max_attempts : int
  ; error : string
  ; delay_ms : int
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind attempt = Json.int_field j "attempt" in
  let%bind max_attempts = Json.int_field j "max_attempts" in
  let%bind error = Json.string_field j "error" in
  let%map delay_ms = Json.int_field j "delay_ms" in
  { attempt; max_attempts; error; delay_ms }
;;
