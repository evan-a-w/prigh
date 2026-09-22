open! Core

type t =
  { session_id : string
  ; session_path : string
  ; session_name : string option
  ; cwd : string
  ; git_branch : string option
  ; model : Model.t
  ; thinking : string
  ; running : bool
  ; message_count : int
  ; usage : Usage.t
  ; cost_usd : float
  ; context_tokens : int
  ; active_host : string (** [Host.id]; "backend" or a client id *)
  ; hosts : Host.t list
  (** backend first, then connected tool-capable clients *)
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind session_id = Json.string_field j "session_id" in
  let%bind session_path = Json.string_field j "session_path" in
  let%bind session_name = Json.string_opt_field j "session_name" in
  let%bind cwd = Json.string_field j "cwd" in
  let%bind git_branch = Json.string_opt_field j "git_branch" in
  let%bind model = Json.object_field j "model" >>= Model.of_json in
  let%bind thinking = Json.string_field j "thinking" in
  let%bind running = Json.bool_field j "running" in
  let%bind message_count = Json.int_field j "message_count" in
  let%bind usage = Json.object_field j "usage" >>= Usage.of_json in
  let%bind cost_usd = Json.float_field j "cost_usd" in
  let%bind context_tokens = Json.int_field j "context_tokens" in
  let%bind active_host =
    match Json.string_field j "active_host" with
    | Ok h -> Ok h
    | Error _ -> Ok "backend"
  in
  let%map hosts =
    match Json.list_field j "hosts" ~f:Host.of_json with
    | Ok hosts -> Ok hosts
    | Error _ -> Ok []
  in
  { session_id
  ; session_path
  ; session_name
  ; cwd
  ; git_branch
  ; model
  ; thinking
  ; running
  ; message_count
  ; usage
  ; cost_usd
  ; context_tokens
  ; active_host
  ; hosts
  }
;;
