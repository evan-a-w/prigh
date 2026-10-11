open! Core

type t =
  { messages : Message.t list
  ; older : int
  ; before : string option
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind messages = Json.list_field j "messages" ~f:Message.of_json in
  let%bind older = Json.int_field j "older" in
  let%map before = Json.string_opt_field j "before" in
  { messages; older; before }
;;
