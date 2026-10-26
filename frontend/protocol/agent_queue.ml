open! Core

type t =
  { agent_id : string
  ; steer : string list
  ; follow_up : string list
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let texts name = Json.optional_list_field j name ~f:Json.to_string_or_error in
  let%bind agent_id = Json.string_field j "agent_id" in
  let%bind steer = texts "steer_texts" in
  let%map follow_up = texts "follow_up_texts" in
  { agent_id; steer; follow_up }
;;

let find ts agent_id =
  match List.find ts ~f:(fun t -> String.equal t.agent_id agent_id) with
  | Some t -> t
  | None -> { agent_id; steer = []; follow_up = [] }
;;
