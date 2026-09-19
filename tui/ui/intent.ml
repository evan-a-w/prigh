open! Core

type t =
  | Insert of string
  | Paste of string
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
  | Word_left
  | Word_right
  | Delete_word_forward
  | Page_up
  | Page_down
  | Complete
  | Cancel
  | Interrupt
  | Force_quit
  | Kill_to_end
  | Kill_to_start
  | Kill_word
  | Yank
  | Yank_pop
  | Undo
  | Cycle_verbosity
  | Next_agent
  | Focus_agent of int
  | Queue_follow_up
  | Dequeue
  | Copy_last
  | Suspend
  | Path_complete
  | Edit_externally
  | Model_picker
[@@deriving sexp_of, equal, compare]
