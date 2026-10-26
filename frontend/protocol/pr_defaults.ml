open! Core

type t =
  { repo : Forge_repo.t
  ; head : string
  ; base : string
  ; title : string
  ; body : string
  ; commits : string list
  ; pushed : bool
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind repo = Json.object_field j "repo" >>= Forge_repo.of_json in
  let%bind head = Json.string_field j "head" in
  let%bind base = Json.string_field j "base" in
  let%bind title = Json.string_field j "title" in
  let%bind body = Json.string_field j "body" in
  let%bind commits =
    Json.optional_list_field j "commits" ~f:(function
      | `String s -> Ok s
      | _ -> Or_error.error_string "commits: expected strings")
  in
  let%map pushed = Json.bool_default_field j "pushed" ~default:false in
  { repo; head; base; title; body; commits; pushed }
;;
