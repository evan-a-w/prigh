open! Core

(** The page's side of the sidebar's context menus ({!Prigh_web.Context_menu}). *)

(** Once rendered: the menu at its anchor (the pointer, or under an
    element), moved back into the window when it would leave it, and
    shown. *)
val place : Prigh_web.Context_menu.Anchor.t -> unit

(** A long press (a touch held still for half a second) on a session or
    worktree row, or a message with a menu, is a right-click there (Safari
    has none of its own), with [long_press_field] set on the event; the
    click that follows the press is swallowed. *)
val install : unit -> unit

val long_press_field : string
