open! Core

(** The page's side of {!Prigh_web.Field_menu}: in the desktop app's browser
    window ({!Desktop_page.in_browser}) no right-click shows the browser's
    menu; one on a text field (a [<textarea>], or an [<input>] one types
    text in) or in the terminal opens the field's menu instead, and the
    field is kept for {!edit}. Elsewhere prigh's own menus still open; and
    outside the browser shell nothing changes. *)
val install : schedule:(Prigh_web.App.Action.t -> unit) -> unit

(** [Command.Edit_field]: the field (or the terminal) focused again, with its
    selection, and the item done to it. Determined: what went wrong, for a toast. *)
val edit
  :  Prigh_web.Field_menu.Item.t option
  -> string option Async_kernel.Deferred.t
