open! Core

(** What the user meant, independent of which key produced it. Each mode
    interprets intents its own way (Enter sends a prompt, accepts a picker row,
    or answers a login prompt). *)
type t =
  | Insert of string
  | Paste of string (** a bracketed paste of a whole block at once *)
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
  | Scroll_up (** a few transcript lines: the mouse wheel *)
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
  | Next_model (** Ctrl+P: cycle forward through the scoped models *)
  | Prev_model (** Alt+P: cycle backward *)
  | Next_thinking (** Ctrl+T: cycle the thinking level *)
  | Next_agent
  | Focus_agent of int
  | Queue_follow_up
  | Dequeue
  | Copy_last
  | Suspend
  | Path_complete
  | Edit_externally
  | Model_picker
  | Picker_toggle_filter (** Ctrl+N in the sessions picker *)
  | Search (** Ctrl+F: search the transcript *)
  | Prev_user_message (** Ctrl+Up: jump to the previous user message *)
  | Next_user_message (** Ctrl+Down: jump to the next user message *)
[@@deriving sexp_of, equal, compare]
