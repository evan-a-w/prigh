open! Core

type t = { mutable latest : int } [@@deriving sexp_of]

let create () = { latest = Int.min_value }
let schedule t ~generation = t.latest <- Int.max t.latest generation
let due t ~generation ~connected = generation >= t.latest && not connected
