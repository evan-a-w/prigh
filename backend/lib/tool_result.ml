open! Core

type t =
  { text : string
  ; is_error : bool
  }
[@@deriving sexp_of]

let ok text = { text; is_error = false }
let error text = { text; is_error = true }
