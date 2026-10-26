open! Core
open! Import

(** A session's or worktree's context menu ({!Context_menu}): a menu
    surface floating at the pointer or under the row ([Command.Place_menu]
    keeps it in the window), its items with their icons and the command
    that does the same; ↑↓ move, Enter chooses, Esc or a click outside
    closes it. *)
val view
  :  App.Model.t
  -> Context_menu.t
  -> Picker.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t
