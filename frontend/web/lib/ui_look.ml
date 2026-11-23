open! Core
open! Import

let button ?(small = false) kind =
  let hook, variant =
    match kind with
    | `Primary -> "ui-btn primary", Loon.Button.Variant.Default
    | `Secondary -> "ui-btn", Outline
    | `Ghost -> "ui-btn ghost", Ghost
  in
  Loon.Button.classes
    ~variant
    ~size:(if small then Xs else Sm)
    ~class_:(if small then hook ^ " small" else hook)
    ()
;;

let input = Loon.Input.classes ~class_:"ui-input" ()
let textarea = Loon.Textarea.classes ~class_:"ui-input" ()

let select =
  Loon.Input.classes ~class_:"ui-input ui-select appearance-none pr-8" ()
;;

let calc_input = Loon.Input.classes ~class_:"font-mono tabular-nums" ()
