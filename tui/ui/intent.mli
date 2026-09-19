open! Core

(** What the user meant, independent of which key produced it. Each mode
    interprets intents its own way (Enter sends a prompt, accepts a picker row,
    or answers a login prompt). *)
type t =
  | Insert of string
  | Submit
  | Newline
  | Backspace
  | Delete
  | Left
  | Right
  | Up
  | Down
  | Home
  | End
  | Page_up
  | Page_down
  | Complete
  | Cancel
  | Interrupt
  | Force_quit
  | Kill_to_end
  | Kill_line
  | Kill_word
  | Clear_screen
  | Toggle_tool_output
[@@deriving sexp_of, equal, compare]
