open! Core

module Window = struct
  type t =
    { id : string
    ; label : string
    ; used_percent : float
    ; resets_at : Time_ns.t option
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind id = Json.string_field j "id" in
    let%bind label = Json.string_field j "label" in
    let%bind used_percent = Json.float_field j "used_percent" in
    let%map resets_at = Json.time_ms_opt_field j "resets_at_ms" in
    { id; label; used_percent; resets_at }
  ;;
end

module Status = struct
  type t =
    | Not_signed_in
    | Api_key
    | Limits of
        { plan : string option
        ; windows : Window.t list
        }
    | Failed of string
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    match%bind Json.string_field j "status" with
    | "not_signed_in" -> Ok Not_signed_in
    | "api_key" -> Ok Api_key
    | "limits" ->
      let%bind plan = Json.string_opt_field j "plan" in
      let%map windows =
        Json.optional_list_field j "windows" ~f:Window.of_json
      in
      Limits { plan; windows }
    | "error" -> Json.string_field j "error" >>| fun e -> Failed e
    | other ->
      Ok (Failed (sprintf "Unknown usage status %S: update prigh" other))
  ;;
end

module Account_usage = struct
  type t =
    { ref_ : string
    ; id : string
    ; label : string
    ; default : bool
    ; shared_by : string option
    ; status : Status.t
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind ref_ = Json.string_field j "ref" in
    let%bind id = Json.string_field j "id" in
    let%bind label = Json.string_field j "label" in
    let%bind default = Json.bool_default_field j "default" ~default:false in
    let%bind shared_by = Json.string_opt_field j "shared_by" in
    let%map status = Status.of_json j in
    { ref_; id; label; default; shared_by; status }
  ;;
end

module Provider_usage = struct
  type t =
    { provider : string
    ; name : string
    ; status : Status.t
    ; accounts : Account_usage.t list
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind provider = Json.string_field j "provider" in
    let%bind name = Json.string_field j "name" in
    let%bind status = Status.of_json j in
    let%map accounts =
      Json.optional_list_field j "accounts" ~f:Account_usage.of_json
    in
    { provider; name; status; accounts }
  ;;
end

type t =
  { fetched_at : Time_ns.t
  ; providers : Provider_usage.t list
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind fetched_at =
    match%bind Json.time_ms_opt_field j "fetched_at_ms" with
    | Some t -> Ok t
    | None -> Or_error.error_string "missing field \"fetched_at_ms\""
  in
  let%map providers = Json.list_field j "providers" ~f:Provider_usage.of_json in
  { fetched_at; providers }
;;
