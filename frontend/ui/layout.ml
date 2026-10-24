open! Core

type t =
  { width : int
  ; height : int
  ; header : bool
  ; sidebar : int
  ; main_top : int
  ; main_rows : int
  ; main_width : int
  ; rule_row : int
  ; editor_rows : int
  ; status_row : int
  }
[@@deriving sexp_of]

let sidebar_threshold = 120
let sidebar_width = 30
let editor_cap ~height = Int.max 1 (Int.min 10 ((height - 4) / 2))

let compute ~width ~height ~sidebar ~editor_rows =
  let width = Int.max 1 width in
  let height = Int.max 3 height in
  let header = height >= 6 in
  let sidebar =
    if sidebar && width >= sidebar_threshold then sidebar_width else 0
  in
  let editor_rows =
    Int.max 1 (Int.min editor_rows (Int.min (editor_cap ~height) (height - 2)))
  in
  let main_top = if header then 1 else 0 in
  let main_rows = Int.max 0 (height - main_top - 2 - editor_rows) in
  let rule_row = main_top + main_rows in
  { width
  ; height
  ; header
  ; sidebar
  ; main_top
  ; main_rows
  ; main_width = Int.max 1 (width - sidebar)
  ; rule_row
  ; editor_rows
  ; status_row = rule_row + 1 + editor_rows
  }
;;
