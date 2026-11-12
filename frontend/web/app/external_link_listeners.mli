open! Core

(** The page's side of {!Prigh_web.External_link}: in the desktop app's
    browser window ({!Desktop_page.in_browser}), a click (any button the
    browser opens links with, with or without Ctrl, ⌘ or Shift) on a link to
    another site that no handler of prigh's has taken is handed to the app's
    {!Prigh_web.External_link.binding}, which opens it in the default
    browser, and the browser's own opening of it cancelled. Without the
    binding, or outside the browser shell, nothing changes. *)
val install : unit -> unit
