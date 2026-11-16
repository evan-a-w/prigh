open! Core

type t =
  { before : string list
  ; after : string list
  ; draft : string
  ; undone : bool
  }
[@@deriving sexp_of]

let record ~before ~after ~draft = { before; after; draft; undone = false }

let current t ~pasted ~draft =
  String.equal draft t.draft
  && [%equal: string list] pasted (if t.undone then t.before else t.after)
;;

let undo t ~pasted ~draft =
  Option.some_if
    ((not t.undone) && current t ~pasted ~draft)
    (t.before, { t with undone = true })
;;

let redo t ~pasted ~draft =
  Option.some_if
    (t.undone && current t ~pasted ~draft)
    (t.after, { t with undone = false })
;;
