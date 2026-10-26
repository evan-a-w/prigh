open! Core

type t =
  | Insert of string
  | Paste of string
  | Submit
  | Submit_as_typed
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
  | Scroll_up
  | Scroll_down
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
  | Next_model
  | Prev_model
  | Next_thinking
  | Cycle_mode
  | Next_agent
  | Focus_agent of int
  | Queue_follow_up
  | Dequeue
  | Copy_last
  | Suspend
  | Path_complete
  | Edit_externally
  | Model_picker
  | Snippet_picker
  | Picker_toggle_filter
  | Search
  | Prev_user_message
  | Next_user_message
  | Jump_to_bottom
  | Toggle_archived
  | Review_highlighted
  | Toggle_sidebar
[@@deriving sexp_of, equal, compare]
