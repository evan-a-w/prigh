open! Core

module Response = struct
  type t =
    { id : int option
    ; result : (Json.t, string) Result.t
    }
  [@@deriving sexp_of]

  let of_json j =
    let open Or_error.Let_syntax in
    let id =
      match Json.field j "id" with
      | Some (`Number n) -> Int.of_string_opt n
      | _ -> None
    in
    match%bind Json.bool_field j "ok" with
    | true ->
      let result = Option.value (Json.field j "result") ~default:`Null in
      Ok { id; result = Ok result }
    | false ->
      let%map error = Json.string_field j "error" in
      { id; result = Error error }
  ;;
end

type t =
  | Response of Response.t
  | Event of Event.t
[@@deriving sexp_of]

let of_json j =
  match%bind.Or_error Json.string_field j "type" with
  | "response" -> Or_error.map (Response.of_json j) ~f:(fun r -> Response r)
  | "event" -> Or_error.map (Event.of_json j) ~f:(fun e -> Event e)
  | other -> Or_error.errorf "unknown message type %S" other
;;

let of_line line =
  Or_error.bind (Json.parse line) ~f:(fun j ->
    Or_error.tag_arg (of_json j) "while decoding" line String.sexp_of_t)
;;
