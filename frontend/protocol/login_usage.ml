open! Core

module Totals = struct
  type t =
    { requests : int
    ; input : int
    ; output : int
    ; cache_read : int
    ; cost_usd : float
    }
  [@@deriving sexp_of, equal]

  let zero = { requests = 0; input = 0; output = 0; cache_read = 0; cost_usd = 0. }

  let of_json j =
    let open Or_error.Let_syntax in
    let int name =
      match Json.field j name with
      | None -> Ok 0
      | Some _ -> Json.int_field j name
    in
    let%bind requests = int "requests" in
    let%bind input = int "input" in
    let%bind output = int "output" in
    let%bind cache_read = int "cache_read" in
    let%map cost_usd =
      match Json.field j "cost_usd" with
      | None -> Ok 0.
      | Some _ -> Json.float_field j "cost_usd"
    in
    { requests; input; output; cache_read; cost_usd }
  ;;
end

type t =
  { today : Totals.t
  ; last_7_days : Totals.t
  ; last_30_days : Totals.t
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let period name =
    match Json.field j name with
    | None -> Ok Totals.zero
    | Some p -> Totals.of_json p
  in
  let%bind today = period "today" in
  let%bind last_7_days = period "last_7_days" in
  let%map last_30_days = period "last_30_days" in
  { today; last_7_days; last_30_days }
;;

module User = struct
  type nonrec t =
    { user : string
    ; you : bool
    ; usage : t
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind user = Json.string_field j "user" in
    let%bind you = Json.bool_default_field j "you" ~default:false in
    let%map usage = of_json j in
    { user; you; usage }
  ;;

  let list_of_json = function
    | `Array items -> Or_error.all (List.map items ~f:of_json)
    | _ -> Or_error.error_string "account_usage: expected an array of users"
  ;;
end
