open! Core

type t =
  { text : string
  ; severity : Severity.t
  ; ticks_left : int
  }
[@@deriving sexp_of, equal]

let lifetime : Severity.t -> int = function
  | Debug | Info -> 80
  | Warn -> 120
  | Error -> 200
;;

let create ?(severity = Severity.Info) text =
  { text; severity; ticks_left = lifetime severity }
;;

let tick t =
  if t.ticks_left <= 1
  then None
  else Some { t with ticks_left = t.ticks_left - 1 }
;;
