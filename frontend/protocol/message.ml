open! Core

module User = struct
  type t =
    { text : string
    ; images : Image.t list [@sexp.list]
    ; origin : Origin.t option [@sexp.option]
    ; at : Time_ns.Alternate_sexp.t option [@sexp.option]
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind text = Json.string_field j "text" in
    let%bind images = Json.optional_list_field j "images" ~f:Image.of_json in
    let%bind origin =
      match Json.field j "origin" with
      | None -> Ok None
      | Some o -> Origin.of_json o >>| Option.some
    in
    let%map at = Json.time_ms_opt_field j "at" in
    { text; images; origin; at }
  ;;

  let source t = Option.bind t.origin ~f:(fun o -> o.source)

  let body t =
    match t.origin with
    | Some { body = Some body; _ } -> body
    | Some { body = None; _ } | None -> t.text
  ;;
end

module Assistant = struct
  type t =
    { content : Content.t list
    ; stop_reason : Stop_reason.t
    ; usage : Usage.t
    ; model : string
    ; at : Time_ns.Alternate_sexp.t option [@sexp.option]
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind content = Json.list_field j "content" ~f:Content.of_json in
    let%bind stop_reason =
      Json.object_field j "stop_reason" >>= Stop_reason.of_json
    in
    let%bind usage = Json.object_field j "usage" >>= Usage.of_json in
    let%bind model = Json.string_field j "model" in
    let%map at = Json.time_ms_opt_field j "at" in
    { content; stop_reason; usage; model; at }
  ;;
end

module Tool_result = struct
  type t =
    { tool_call_id : string
    ; tool_name : string
    ; text : string
    ; is_error : bool
    ; images : Image.t list [@sexp.list]
    ; at : Time_ns.Alternate_sexp.t option [@sexp.option]
    ; elided : Elided.t option [@sexp.option]
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind tool_call_id = Json.string_field j "tool_call_id" in
    let%bind tool_name = Json.string_field j "tool_name" in
    let%bind text = Json.string_field j "text" in
    let%bind is_error = Json.bool_field j "is_error" in
    let%bind images = Json.optional_list_field j "images" ~f:Image.of_json in
    let%bind at = Json.time_ms_opt_field j "at" in
    let%map elided =
      match Json.field j "elided" with
      | None -> Ok None
      | Some e -> Or_error.map (Elided.of_json e) ~f:Option.some
    in
    { tool_call_id; tool_name; text; is_error; images; at; elided }
  ;;
end

type t =
  | User of User.t
  | Assistant of Assistant.t
  | Tool_result of Tool_result.t
[@@deriving sexp_of, equal]

let of_json j =
  match%bind.Or_error Json.string_field j "role" with
  | "user" -> Or_error.map (User.of_json j) ~f:(fun u -> User u)
  | "assistant" -> Or_error.map (Assistant.of_json j) ~f:(fun a -> Assistant a)
  | "tool_result" ->
    Or_error.map (Tool_result.of_json j) ~f:(fun r -> Tool_result r)
  | other -> Or_error.errorf "unknown message role %S" other
;;
