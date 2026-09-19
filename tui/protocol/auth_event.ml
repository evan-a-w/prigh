open! Core

module Prompt = struct
  type t =
    | Secret of { message : string }
    | Manual_code of
        { message : string
        ; placeholder : string
        }
    | Select of
        { message : string
        ; options : (string * string) list
        }
  [@@deriving sexp_of, equal]

  let message = function
    | Secret { message } | Manual_code { message; _ } | Select { message; _ } ->
      message
  ;;

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind message = Json.string_field j "message" in
    match%bind Json.string_field j "prompt" with
    | "secret" -> Ok (Secret { message })
    | "manual_code" ->
      let%map placeholder = Json.string_field j "placeholder" in
      Manual_code { message; placeholder }
    | "select" ->
      let%map options =
        Json.list_field j "options" ~f:(fun o ->
          let%bind id = Json.string_field o "id" in
          let%map label = Json.string_field o "label" in
          id, label)
      in
      Select { message; options }
    | other -> Or_error.errorf "unknown auth prompt %S" other
  ;;
end

type t =
  | Auth_url of
      { url : string
      ; instructions : string
      }
  | Prompt of
      { id : string
      ; prompt : Prompt.t
      }
  | Prompt_cancelled of { id : string }
  | Progress of string
  | Done of
      { provider : string
      ; method_ : string
      }
  | Failed of
      { provider : string
      ; error : string
      }
  | Logged_out of string
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  match%bind Json.string_field j "kind" with
  | "auth_url" ->
    let%bind url = Json.string_field j "url" in
    let%map instructions = Json.string_field j "instructions" in
    Auth_url { url; instructions }
  | "prompt" ->
    let%bind id = Json.string_field j "id" in
    let%map prompt = Prompt.of_json j in
    Prompt { id; prompt }
  | "prompt_cancelled" ->
    let%map id = Json.string_field j "id" in
    Prompt_cancelled { id }
  | "progress" -> Json.string_field j "message" >>| fun m -> Progress m
  | "done" ->
    let%bind provider = Json.string_field j "provider" in
    let%map method_ = Json.string_field j "method" in
    Done { provider; method_ }
  | "failed" ->
    let%bind provider = Json.string_field j "provider" in
    let%map error = Json.string_field j "error" in
    Failed { provider; error }
  | "logged_out" -> Json.string_field j "provider" >>| fun p -> Logged_out p
  | other -> Or_error.errorf "unknown auth event kind %S" other
;;
