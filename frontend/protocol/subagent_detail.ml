open! Core

type t =
  { id : string
  ; call_id : string
  ; parent : string option
  ; task : string
  ; name : string option
  ; model : string
  ; turns : int
  ; result : Event.Subagent_result.t option
  ; messages : Message.t list
  }
[@@deriving sexp_of]

let of_json json =
  let open Or_error.Let_syntax in
  let%bind summary = Json.object_field json "subagent" in
  let%bind id = Json.string_field summary "id" in
  let%bind call_id = Json.string_field summary "call_id" in
  let%bind parent = Json.string_opt_field summary "parent" in
  let%bind task = Json.string_field summary "task" in
  let%bind name = Json.string_opt_field summary "name" in
  let%bind model = Json.string_field summary "model" in
  let%bind turns = Json.int_field summary "turns" in
  let%bind result =
    match Json.field summary "result" with
    | None -> Ok None
    | Some r -> Or_error.map (Event.Subagent_result.of_json r) ~f:Option.some
  in
  let%map messages = Json.list_field json "messages" ~f:Message.of_json in
  { id; call_id; parent; task; name; model; turns; result; messages }
;;
