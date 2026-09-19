open! Core

type t =
  { id : int
  ; method_ : string
  ; params : (string * Json.t) list
  }
[@@deriving sexp_of]

let to_json { id; method_; params } =
  Json.obj
    [ "id", Json.int id; "method", Json.str method_; "params", Json.obj params ]
;;

let to_line t = Json.to_string (to_json t)
