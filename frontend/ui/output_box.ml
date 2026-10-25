open! Core

type t =
  { title : string
  ; lines : Content.t
  ; top : int
  }
[@@deriving sexp_of, equal]

let create ~title lines = { title; lines; top = 0 }
let wrapped t ~width = Content.wrap t.lines ~width:(Int.max 1 width)

let clamp t ~total ~rows =
  { t with top = Int.max 0 (Int.min t.top (total - Int.max 1 rows)) }
;;

let scroll t ~by ~width ~rows =
  clamp
    { t with top = t.top + by }
    ~total:(List.length (wrapped t ~width))
    ~rows
;;

let window t ~width ~rows =
  let lines = wrapped t ~width in
  let total = List.length lines in
  let rows = Int.max 1 rows in
  let t = clamp t ~total ~rows in
  let shown = List.take (List.drop lines t.top) rows in
  let footer =
    if total <= rows
    then "Esc closes"
    else
      sprintf
        "%d–%d of %d · ↑↓ PgUp PgDn scroll · Esc closes"
        (t.top + 1)
        (t.top + List.length shown)
        total
  in
  shown, footer
;;
