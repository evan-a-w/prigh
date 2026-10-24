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

module Custom = struct
  type t =
    { base_url : string
    ; api : string
    ; api_label : string
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind base_url = Json.string_field j "base_url" in
    let%bind api = Json.string_field j "api" in
    let%map api_label = Json.string_field j "api_label" in
    { base_url; api; api_label }
  ;;
end

type t =
  { provider : string
  ; name : string
  ; methods : Method.t list
  ; configured : Configured.t option
  ; expires_ms : Int64.t option
  ; custom : Custom.t option
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
  let%bind expires_ms =
    match Json.field j "expires_ms" with
    | None -> Ok None
    | Some _ -> Json.int64_field j "expires_ms" >>| Option.some
  in
  let%map custom =
    match Json.field j "custom" with
    | None -> Ok None
    | Some c -> Custom.of_json c >>| Option.some
  in
  { provider; name; methods; configured; expires_ms; custom }
;;
