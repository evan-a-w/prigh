open! Core
open! Import

type t =
  | Tool
  | Computer_use of { host : string }
[@@deriving sexp_of, equal]

let to_json_fields = function
  | Tool -> []
  | Computer_use { host } ->
    [ "kind", `String "computer_use"; "host", `String host ]
;;
