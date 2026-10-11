open! Core

module Server_tool = struct
  type t =
    { id : string
    ; name : string
    ; input : string
    ; output : string option
    ; is_error : bool
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind id = Json.string_field j "id" in
    let%bind name = Json.string_field j "name" in
    let%bind input = Json.string_field j "input" in
    let%bind output = Json.string_opt_field j "output" in
    let%map is_error = Json.bool_default_field j "is_error" ~default:false in
    { id; name; input; output; is_error }
  ;;
end

type t =
  | Text of string
  | Thinking of string
  | Tool_call of Tool_call.t
  | Server_tool of Server_tool.t
[@@deriving sexp_of, equal]

let of_json j =
  match%bind.Or_error Json.string_field j "type" with
  | "text" -> Or_error.map (Json.string_field j "text") ~f:(fun t -> Text t)
  | "thinking" ->
    Or_error.map (Json.string_field j "text") ~f:(fun t -> Thinking t)
  | "tool_call" -> Or_error.map (Tool_call.of_json j) ~f:(fun c -> Tool_call c)
  | "server_tool" ->
    Or_error.map (Server_tool.of_json j) ~f:(fun s -> Server_tool s)
  | other -> Or_error.errorf "unknown content type %S" other
;;
