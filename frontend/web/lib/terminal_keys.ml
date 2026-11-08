open! Core

module Modifier = struct
  type t =
    | Ctrl
    | Alt
  [@@deriving sexp_of, equal]
end

module Key = struct
  type t =
    | Esc
    | Tab
    | Modifier of Modifier.t
    | Left
    | Up
    | Down
    | Right
    | Char of char
    | Home
    | End
    | Page_up
    | Page_down
  [@@deriving sexp_of, equal]

  let all =
    [ Esc
    ; Tab
    ; Modifier Ctrl
    ; Modifier Alt
    ; Left
    ; Up
    ; Down
    ; Right
    ; Char '|'
    ; Char '~'
    ; Char '/'
    ; Char '-'
    ; Home
    ; End
    ; Page_up
    ; Page_down
    ]
  ;;

  let label = function
    | Esc -> "Esc"
    | Tab -> "Tab"
    | Modifier Ctrl -> "Ctrl"
    | Modifier Alt -> "Alt"
    | Left -> "←"
    | Up -> "↑"
    | Down -> "↓"
    | Right -> "→"
    | Char c -> String.of_char c
    | Home -> "Home"
    | End -> "End"
    | Page_up -> "PgUp"
    | Page_down -> "PgDn"
  ;;

  let name = function
    | Esc -> "Escape"
    | Tab -> "Tab"
    | Modifier Ctrl -> "Control: tap for the next key, double-tap to lock"
    | Modifier Alt -> "Alt: tap for the next key, double-tap to lock"
    | Left -> "Left arrow"
    | Up -> "Up arrow"
    | Down -> "Down arrow"
    | Right -> "Right arrow"
    | Char '|' -> "Vertical bar"
    | Char '~' -> "Tilde"
    | Char '/' -> "Slash"
    | Char '-' -> "Hyphen"
    | Char c -> String.of_char c
    | Home -> "Home"
    | End -> "End"
    | Page_up -> "Page up"
    | Page_down -> "Page down"
  ;;

  let repeats = function
    | Left | Up | Down | Right -> true
    | Esc | Tab | Modifier _ | Char _ | Home | End | Page_up | Page_down ->
      false
  ;;
end

module Latch = struct
  type t =
    | Off
    | Once
    | Locked
  [@@deriving sexp_of, equal]
end

type t =
  { ctrl : Latch.t
  ; alt : Latch.t
  ; last_tap : (Modifier.t * float) option
  }
[@@deriving sexp_of]

let empty = { ctrl = Off; alt = Off; last_tap = None }

let latch t : Modifier.t -> Latch.t = function
  | Ctrl -> t.ctrl
  | Alt -> t.alt
;;

let double_tap_ms = 400.

let tap t (modifier : Modifier.t) ~now =
  let double =
    match t.last_tap with
    | Some (m, at) -> Modifier.equal m modifier && Float.(now - at <= double_tap_ms)
    | None -> false
  in
  let next : Latch.t =
    match latch t modifier with
    | Off -> Once
    | Once when double -> Locked
    | Once | Locked -> Off
  in
  let last_tap =
    match next with
    | Once -> Some (modifier, now)
    | Off | Locked -> None
  in
  match modifier with
  | Ctrl -> { t with ctrl = next; last_tap }
  | Alt -> { t with alt = next; last_tap }
;;

let release t =
  let once : Latch.t -> Latch.t = function
    | Once -> Off
    | (Off | Locked) as l -> l
  in
  { ctrl = once t.ctrl; alt = once t.alt; last_tap = None }
;;

let on = function
  | Latch.Off -> false
  | Once | Locked -> true
;;

(* What xterm sends for Ctrl and the key that types [c]. *)
let control c =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' -> Char.of_int_exn (Char.to_int c land 0x1f)
  | '@' | ' ' | '2' | '`' -> '\x00'
  | '[' | '3' -> '\x1b'
  | '\\' | '|' | '4' -> '\x1c'
  | ']' | '5' -> '\x1d'
  | '^' | '~' | '6' -> '\x1e'
  | '_' | '-' | '/' | '7' -> '\x1f'
  | '?' | '8' -> '\x7f'
  | c -> c
;;

let type_ t data =
  if String.is_empty data || not (on t.ctrl || on t.alt)
  then t, data
  else (
    let data =
      if on t.ctrl
      then (
        let first = control data.[0] in
        String.of_char first ^ String.drop_prefix data 1)
      else data
    in
    release t, if on t.alt then "\x1b" ^ data else data)
;;

let press t (key : Key.t) ~app_cursor ~now =
  (* xterm's modifier parameter: 1, +1 Shift, +2 Alt, +4 Ctrl. *)
  let modifiers = 1 + (if on t.alt then 2 else 0) + if on t.ctrl then 4 else 0 in
  let cursor letter =
    if modifiers > 1
    then sprintf "\x1b[1;%d%c" modifiers letter
    else if app_cursor
    then sprintf "\x1bO%c" letter
    else sprintf "\x1b[%c" letter
  in
  let tilde n =
    if modifiers > 1
    then sprintf "\x1b[%d;%d~" n modifiers
    else sprintf "\x1b[%d~" n
  in
  let alt s = if on t.alt then "\x1b" ^ s else s in
  match key with
  | Modifier m -> tap t m ~now, ""
  | Char c -> type_ t (String.of_char c)
  | Esc -> release t, alt "\x1b"
  | Tab -> release t, alt "\t"
  | Up -> release t, cursor 'A'
  | Down -> release t, cursor 'B'
  | Right -> release t, cursor 'C'
  | Left -> release t, cursor 'D'
  | Home -> release t, cursor 'H'
  | End -> release t, cursor 'F'
  | Page_up -> release t, tilde 5
  | Page_down -> release t, tilde 6
;;
