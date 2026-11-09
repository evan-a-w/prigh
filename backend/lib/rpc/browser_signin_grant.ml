open! Core

type t =
  { proxy_cookie : (string[@sexp.opaque])
  ; token : (string[@sexp.opaque]) option
  ; user : string option
  }
[@@deriving sexp_of]
