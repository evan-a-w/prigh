open! Core

(** The page's side of the sidebar's context menus ({!Prigh_web.Context_menu}). *)

(** Once rendered: the menu at its anchor (the pointer, or under an
    element), moved back into the window when it would leave it, and
    shown. [id]: the menu's (by default the dialog's). *)
val place : ?id:string -> Prigh_web.Context_menu.Anchor.t -> unit

(** A long press (a touch held still for about half a second) on a session
    or worktree row, a review's file, or a message with a menu, is a
    right-click there, with [long_press_field] set on the event: from a
    timer (Safari fires no [contextmenu] for a touch), or from the browser's
    own long press, which is cancelled (Chrome on Android would select the
    word under the finger and show its copy bar). The row shows [pressing]
    while held; the phone buzzes when the menu opens; the click that
    follows the press is swallowed. Not in a message made selectable by
    {!select_message}, whose long press is the browser's again. *)
val install : unit -> unit

val long_press_field : string

(** The text of the message of this entry selected, and left selectable to
    a long press until the next menu opens ([App.Command.Select_message]). *)
val select_message : int -> unit
