open! Core

module Color = struct
  type t =
    | Default
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | Gray
    | White
  [@@deriving sexp_of, equal, compare]
end

type t =
  { fg : Color.t
  ; bold : bool
  ; dim : bool
  ; italic : bool
  ; underline : bool
  ; invert : bool
  ; strike : bool
  ; link : string option
  }
[@@deriving sexp_of, equal, compare]

let plain =
  { fg = Default
  ; bold = false
  ; dim = false
  ; italic = false
  ; underline = false
  ; invert = false
  ; strike = false
  ; link = None
  }
;;

let fg fg = { plain with fg }
let bold t = { t with bold = true }
let dim t = { t with dim = true }
let italic t = { t with italic = true }
let underline t = { t with underline = true }
let invert t = { t with invert = true }
let strike t = { t with strike = true }
let link t url = { t with link = Some url }
