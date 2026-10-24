open! Core

type t =
  { text : string
  ; is_error : bool
  ; images : Image.t list [@sexp.list]
  }
[@@deriving sexp_of]

let ok ?(images = []) text = { text; is_error = false; images }
let error text = { text; is_error = true; images = [] }
