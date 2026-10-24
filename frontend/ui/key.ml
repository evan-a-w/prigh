open! Core

module Code = struct
  type t =
    | Escape
    | Enter
    | Tab
    | Backspace
    | Delete
    | Insert
    | Home
    | End
    | Up
    | Down
    | Left
    | Right
    | Page_up
    | Page_down
    | Function of int
    | Char of string
  [@@deriving sexp_of, equal, compare]

  let to_string = function
    | Escape -> "Esc"
    | Enter -> "Enter"
    | Tab -> "Tab"
    | Backspace -> "Backspace"
    | Delete -> "Delete"
    | Insert -> "Insert"
    | Home -> "Home"
    | End -> "End"
    | Up -> "Up"
    | Down -> "Down"
    | Left -> "Left"
    | Right -> "Right"
    | Page_up -> "PageUp"
    | Page_down -> "PageDown"
    | Function n -> sprintf "F%d" n
    | Char c -> String.uppercase c
  ;;
end

type t =
  { code : Code.t
  ; ctrl : bool
  ; alt : bool
  ; shift : bool
  }
[@@deriving sexp_of, equal, compare]

let plain code = { code; ctrl = false; alt = false; shift = false }
let ctrl c = { (plain (Char (String.of_char c))) with ctrl = true }
let alt code = { (plain code) with alt = true }
let char c = plain (Char (String.of_char c))

let to_string t =
  String.concat
    ~sep:"+"
    (List.filter_opt
       [ Option.some_if t.ctrl "Ctrl"
       ; Option.some_if t.alt "Alt"
       ; Option.some_if t.shift "Shift"
       ; Some (Code.to_string t.code)
       ])
;;
