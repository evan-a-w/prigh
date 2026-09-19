open! Core

type t =
  { session_id : string
  ; session_path : string
  ; cwd : string
  ; model : Model.t
  ; thinking : string
  ; running : bool
  ; message_count : int
  ; usage : Usage.t
  ; cost_usd : float
  ; context_tokens : int
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind session_id = Json.string_field j "session_id" in
  let%bind session_path = Json.string_field j "session_path" in
  let%bind cwd = Json.string_field j "cwd" in
  let%bind model = Json.object_field j "model" >>= Model.of_json in
  let%bind thinking = Json.string_field j "thinking" in
  let%bind running = Json.bool_field j "running" in
  let%bind message_count = Json.int_field j "message_count" in
  let%bind usage = Json.object_field j "usage" >>= Usage.of_json in
  let%bind cost_usd = Json.float_field j "cost_usd" in
  let%map context_tokens = Json.int_field j "context_tokens" in
  { session_id
  ; session_path
  ; cwd
  ; model
  ; thinking
  ; running
  ; message_count
  ; usage
  ; cost_usd
  ; context_tokens
  }
;;
