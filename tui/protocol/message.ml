open! Core

module Assistant = struct
  type t =
    { content : Content.t list
    ; stop_reason : Stop_reason.t
    ; usage : Usage.t
    ; model : string
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind content = Json.list_field j "content" ~f:Content.of_json in
    let%bind stop_reason =
      Json.object_field j "stop_reason" >>= Stop_reason.of_json
    in
    let%bind usage = Json.object_field j "usage" >>= Usage.of_json in
    let%map model = Json.string_field j "model" in
    { content; stop_reason; usage; model }
  ;;
end

module Tool_result = struct
  type t =
    { tool_call_id : string
    ; tool_name : string
    ; text : string
    ; is_error : bool
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind tool_call_id = Json.string_field j "tool_call_id" in
    let%bind tool_name = Json.string_field j "tool_name" in
    let%bind text = Json.string_field j "text" in
    let%map is_error = Json.bool_field j "is_error" in
    { tool_call_id; tool_name; text; is_error }
  ;;
end

type t =
  | User of string
  | Assistant of Assistant.t
  | Tool_result of Tool_result.t
[@@deriving sexp_of, equal]

let of_json j =
  match%bind.Or_error Json.string_field j "role" with
  | "user" -> Or_error.map (Json.string_field j "text") ~f:(fun t -> User t)
  | "assistant" -> Or_error.map (Assistant.of_json j) ~f:(fun a -> Assistant a)
  | "tool_result" ->
    Or_error.map (Tool_result.of_json j) ~f:(fun r -> Tool_result r)
  | other -> Or_error.errorf "unknown message role %S" other
;;
