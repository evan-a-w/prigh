open! Core

type t =
  { id : string
  ; command : string
  ; running : bool
  ; exit : string option
  ; delivered : bool
  ; elapsed : float
  ; bytes : int
  ; last_line : string option
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind id = Json.string_field j "id" in
  let%bind command = Json.string_field j "command" in
  let%bind running = Json.bool_field j "running" in
  let%bind exit = Json.string_opt_field j "exit" in
  let%bind delivered = Json.bool_field j "delivered" in
  let%bind elapsed = Json.float_field j "elapsed" in
  let%bind bytes = Json.int_field j "bytes" in
  let%map last_line = Json.string_opt_field j "last_line" in
  { id; command; running; exit; delivered; elapsed; bytes; last_line }
;;
