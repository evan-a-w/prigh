open! Core

module Confirm = struct
  type t =
    { call_id : string
    ; agent_id : string option
    ; name : string
    ; summary : string
    ; rules : string list
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind call_id = Json.string_field j "call_id" in
    let%bind agent_id = Json.string_opt_field j "agent_id" in
    let%bind name = Json.string_field j "name" in
    let%bind summary = Json.string_field j "summary" in
    let%map rules =
      Json.optional_list_field j "rules" ~f:Json.to_string_or_error
    in
    { call_id; agent_id; name; summary; rules }
  ;;
end

module Ask = struct
  type t =
    { call_id : string
    ; questions : Question.t list
    ; deadline : Time_ns.t option
      (** when the agent stops waiting and carries on by itself *)
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind call_id = Json.string_field j "call_id" in
    let%bind questions = Json.list_field j "questions" ~f:Question.of_json in
    let%map deadline = Json.time_ms_opt_field j "deadline_ms" in
    { call_id; questions; deadline }
  ;;
end

type t =
  { steer_texts : string list
  ; follow_up_texts : string list
  ; confirms : Confirm.t list
  ; questions : Ask.t list
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let strings name =
    Json.optional_list_field j name ~f:Json.to_string_or_error
  in
  let%bind steer_texts = strings "steer_texts" in
  let%bind follow_up_texts = strings "follow_up_texts" in
  let%bind confirms =
    Json.optional_list_field j "confirms" ~f:Confirm.of_json
  in
  let%map questions = Json.optional_list_field j "questions" ~f:Ask.of_json in
  { steer_texts; follow_up_texts; confirms; questions }
;;
