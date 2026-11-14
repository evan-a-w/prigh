open! Core

type t =
  { message_count : int
  ; turns : int
  ; tool_calls : (string * int) list
  ; usage : Usage.t
  ; cost_usd : float
  ; context_percent : float
  ; model_changes : int
  ; compactions : int
  ; masks : int
  ; duration_seconds : float
  ; approvals_judged : int
  ; approvals_cost_usd : float
  }
[@@deriving sexp_of, equal]

let tool_calls j =
  match Json.field j "tool_calls" with
  | Some (`Object fields) ->
    Or_error.all
      (List.map fields ~f:(fun (name, value) ->
         Or_error.map
           (Json.int_field (Json.obj [ name, value ]) name)
           ~f:(fun count -> name, count)))
  | Some other ->
    Or_error.errorf
      "field \"tool_calls\": expected object, got %s"
      (Json.to_string other)
  | None -> Or_error.error_string "missing field \"tool_calls\""
;;

let of_json j =
  let open Or_error.Let_syntax in
  let%bind message_count = Json.int_field j "message_count" in
  let%bind turns = Json.int_field j "turns" in
  let%bind tool_calls = tool_calls j in
  let%bind usage = Json.object_field j "usage" >>= Usage.of_json in
  let%bind cost_usd = Json.float_field j "cost_usd" in
  let%bind context_percent = Json.float_field j "context_percent" in
  let%bind model_changes = Json.int_field j "model_changes" in
  let%bind compactions = Json.int_field j "compactions" in
  let%bind masks =
    match Json.field j "masks" with
    | None -> Ok 0
    | Some _ -> Json.int_field j "masks"
  in
  let%bind duration_seconds = Json.float_field j "duration_seconds" in
  let%bind approvals_judged =
    match Json.field j "approvals_judged" with
    | None -> Ok 0
    | Some _ -> Json.int_field j "approvals_judged"
  in
  let%map approvals_cost_usd =
    match Json.field j "approvals_cost_usd" with
    | None -> Ok 0.
    | Some _ -> Json.float_field j "approvals_cost_usd"
  in
  { message_count
  ; turns
  ; tool_calls
  ; usage
  ; cost_usd
  ; context_percent
  ; model_changes
  ; compactions
  ; masks
  ; duration_seconds
  ; approvals_judged
  ; approvals_cost_usd
  }
;;
