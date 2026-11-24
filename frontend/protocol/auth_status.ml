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

module Grant = struct
  type t =
    { user : string
    ; since_ms : Int64.t
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind user = Json.string_field j "user" in
    let%map since_ms = Json.int64_field j "since_ms" in
    { user; since_ms }
  ;;
end

module Account = struct
  type t =
    { ref_ : string
    ; id : string
    ; label : string
    ; method_ : string
    ; source : string
    ; expires_ms : Int64.t option
    ; default : bool
    ; fallback : bool
    ; shareable : bool
    ; shared_with : Grant.t list
    ; shared_by : string option
    ; hidden : bool
    ; usage : Login_usage.t option
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind ref_ = Json.string_field j "ref" in
    let%bind id = Json.string_field j "id" in
    let%bind label = Json.string_field j "label" in
    let%bind method_ = Json.string_field j "method" in
    let%bind source = Json.string_field j "source" in
    let%bind expires_ms =
      match Json.field j "expires_ms" with
      | None -> Ok None
      | Some _ -> Json.int64_field j "expires_ms" >>| Option.some
    in
    let%bind default = Json.bool_default_field j "default" ~default:false in
    let%bind fallback = Json.bool_default_field j "fallback" ~default:false in
    let%bind shareable = Json.bool_default_field j "shareable" ~default:false in
    let%bind shared_with =
      Json.optional_list_field j "shared_with" ~f:Grant.of_json
    in
    let%bind shared_by = Json.string_opt_field j "shared_by" in
    let%bind hidden = Json.bool_default_field j "hidden" ~default:false in
    let%map usage =
      match Json.field j "usage" with
      | None | Some `Null -> Ok None
      | Some u -> Login_usage.of_json u >>| Option.some
    in
    { ref_
    ; id
    ; label
    ; method_
    ; source
    ; expires_ms
    ; default
    ; fallback
    ; shareable
    ; shared_with
    ; shared_by
    ; hidden
    ; usage
    }
  ;;
end

type t =
  { provider : string
  ; name : string
  ; methods : Method.t list
  ; configured : Configured.t option
  ; expires_ms : Int64.t option
  ; custom : Custom.t option
  ; accounts : Account.t list
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
  let%bind custom =
    match Json.field j "custom" with
    | None -> Ok None
    | Some c -> Custom.of_json c >>| Option.some
  in
  let%map accounts = Json.optional_list_field j "accounts" ~f:Account.of_json in
  { provider; name; methods; configured; expires_ms; custom; accounts }
;;
