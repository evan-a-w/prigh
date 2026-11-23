open! Core

type t =
  { engines : string list
  ; adapters : string list [@sexp.list]
  }
[@@deriving sexp_of, equal]

let harness_only = { engines = [ "harness" ]; adapters = [] }
let has_choice t = not (equal t harness_only)

let of_hello_json j =
  let open Or_error.Let_syntax in
  let%bind engines =
    Json.optional_list_field j "engines" ~f:Json.to_string_or_error
  in
  let%map adapters =
    Json.optional_list_field j "adapters" ~f:Json.to_string_or_error
  in
  match engines with
  | [] -> harness_only
  | engines -> { engines; adapters }
;;
