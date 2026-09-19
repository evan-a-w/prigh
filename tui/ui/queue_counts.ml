open! Core

type t =
  { steer : int
  ; follow_up : int
  }
[@@deriving sexp_of, equal]

let zero = { steer = 0; follow_up = 0 }
let total t = t.steer + t.follow_up
