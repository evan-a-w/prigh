open! Core
open! Import

type t =
  { usage : (Subscription_usage.t, string) Result.t option
  ; loading : bool
  }
[@@deriving sexp_of, equal]

let empty = { usage = None; loading = false }
let loading t = { t with loading = true }
let loaded _ usage = { usage = Some usage; loading = false }
