open! Core

(** The page's side of {!Prigh_web.Session_link_click}: a click on a link
    to a session (any button the browser opens links with) is taken from
    the browser and scheduled as [Follow_session_link], so the page
    switches to the session in place, or opens it in a new window as "Open
    in new window" does (the desktop app's window too). Installed before
    {!External_link_listeners}, which then leaves the click alone. *)
val install : schedule:(Prigh_web.App.Action.t -> unit) -> unit
