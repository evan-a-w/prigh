open! Core

module Method = struct
  type t =
    { method_ : string
    ; label : string
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind method_ = Json.string_field j "method" in
    let%map label = Json.string_field j "label" in
    { method_; label }
  ;;
end

module Configured = struct
  type t =
    { method_ : string
    ; source : string
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind method_ = Json.string_field j "method" in
    let%map source = Json.string_field j "source" in
    { method_; source }
  ;;
end

type t =
  { provider : string
  ; name : string
  ; methods : Method.t list
  ; configured : Configured.t option
  ; expires_ms : int option
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind provider = Json.string_field j "provider" in
  let%bind name = Json.string_field j "name" in
  let%bind methods = Json.list_field j "methods" ~f:Method.of_json in
  let%bind configured =
    match Json.field j "configured" with
    | None -> Ok None
    | Some c -> Configured.of_json c >>| Option.some
  in
  let%map expires_ms =
    match Json.field j "expires_ms" with
    | None -> Ok None
    | Some _ -> Json.int_field j "expires_ms" >>| Option.some
  in
  { provider; name; methods; configured; expires_ms }
;;
