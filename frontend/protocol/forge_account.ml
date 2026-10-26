open! Core

type t =
  { host : string
  ; kind : string
  ; label : string
  ; user : string option
  ; method_ : string option
  ; expires_ms : Int64.t option
  ; oauth : bool
  ; oauth_note : string option
  }
[@@deriving sexp_of, equal]

let signed_in t = Option.is_some t.method_

let login_arg t =
  match t.kind, t.host with
  | "github", "github.com" -> "github"
  | kind, host -> kind ^ " " ^ host
;;

let of_json j =
  let open Or_error.Let_syntax in
  let%bind host = Json.string_field j "host" in
  let%bind kind = Json.string_field j "kind" in
  let%bind label = Json.string_field j "label" in
  let%bind user = Json.string_opt_field j "user" in
  let%bind method_ = Json.string_opt_field j "method" in
  let%bind expires_ms =
    match Json.field j "expires_ms" with
    | None -> Ok None
    | Some _ -> Json.int64_field j "expires_ms" >>| Option.some
  in
  let%bind oauth = Json.bool_default_field j "oauth" ~default:false in
  let%map oauth_note = Json.string_opt_field j "oauth_note" in
  { host; kind; label; user; method_; expires_ms; oauth; oauth_note }
;;

let list_of_json j = Json.list_field j "accounts" ~f:of_json
