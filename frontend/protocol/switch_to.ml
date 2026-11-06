open! Core

type t =
  | Session of string
  | New_session
[@@deriving sexp_of, equal]

let params = function
  | None -> []
  | Some (Session path) -> [ "switch_to", Json.str path ]
  | Some New_session -> [ "switch_to_new", Json.bool true ]
;;
