open! Core

type t =
  { id : string
  ; path : string
  ; name : string option
  ; description : string option
  ; cwd : string
  ; created_at : string
  ; updated_at : string option
  ; first_prompt : string option
  ; message_count : int
  ; parent : string option
  ; live : bool (** loaded in the backend *)
  ; running : bool
  ; clients : int (** frontends attached to it *)
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind id = Json.string_field j "id" in
  let%bind path = Json.string_field j "path" in
  let%bind name = Json.string_opt_field j "name" in
  let%bind description = Json.string_opt_field j "description" in
  let%bind cwd = Json.string_field j "cwd" in
  let%bind created_at = Json.string_field j "created_at" in
  let%bind updated_at = Json.string_opt_field j "updated_at" in
  let%bind first_prompt = Json.string_opt_field j "first_prompt" in
  let%bind message_count = Json.int_field j "message_count" in
  let%bind parent = Json.string_opt_field j "parent" in
  let opt_bool name =
    Result.ok (Json.bool_field j name) |> Option.value ~default:false
  in
  let%map clients =
    match Json.int_field j "clients" with
    | Ok n -> Ok n
    | Error _ -> Ok 0
  in
  { id
  ; path
  ; name
  ; description
  ; cwd
  ; created_at
  ; updated_at
  ; first_prompt
  ; message_count
  ; parent
  ; live = opt_bool "live"
  ; running = opt_bool "running"
  ; clients
  }
;;

let blurb t =
  match t.description, t.first_prompt with
  | Some text, _ | None, Some text ->
    String.concat ~sep:" " (String.split_lines text)
  | None, None -> "(empty)"
;;
