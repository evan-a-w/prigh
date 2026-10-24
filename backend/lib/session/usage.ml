open! Core
open! Import

type t =
  { input : int
  ; output : int
  ; cache_read : int
  }
[@@deriving sexp, jsonaf, equal, fields ~getters]

let zero = { input = 0; output = 0; cache_read = 0 }

let add a b =
  { input = a.input + b.input
  ; output = a.output + b.output
  ; cache_read = a.cache_read + b.cache_read
  }
;;

let total_tokens t = t.input + t.output
