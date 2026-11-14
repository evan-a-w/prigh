open! Core
open! Import

type t =
  | Tool
  | Judged of
      { model : string
      ; reason : string
      }
  | Computer_use of { host : string }
[@@deriving sexp_of, equal]

let to_json_fields = function
  | Tool -> []
  | Judged { model; reason } ->
    [ "kind", `String "judged"
    ; "model", `String model
    ; "reason", `String reason
    ]
  | Computer_use { host } ->
    [ "kind", `String "computer_use"; "host", `String host ]
;;
