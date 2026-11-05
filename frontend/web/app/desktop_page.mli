open! Core

(** The page's side of [prigh desktop] (see {!Prigh_web.Desktop_link}). *)

(** The run's key: taken from the URL's fragment (which it then clears) into
    the tab's sessionStorage, or found there after a reload. [None]: not
    under the desktop app. *)
val key : unit -> string option

(** Notes for the tab that the page is in an installed browser's app window
    ([shell=browser] in the fragment, see
    {!Prigh_web.Desktop_link.browser_shell_of_hash}). Before {!key} and
    {!take_hint}, which clear the fragment. *)
val take_shell : unit -> unit

(** Where the page goes to show [url] (the backend's own site), keeping the
    browser's app window's mark. *)
val follow : string -> string

(** On the backend's own site opened by the desktop app ([#app=desktop]):
    notes it for the tab, prefers the tool host the fragment names, and
    clears the fragment. *)
val take_hint : unit -> unit

(** Whether the page runs under the desktop app. *)
val active : unit -> bool

(** Asks the sidecar where it stands (it waits for a check under way). *)
val status
  :  key:string
  -> f:(Prigh_web.Desktop_link.Status.t option -> unit)
  -> unit

val connect
  :  key:string
  -> address:string
  -> user:string option
  -> token:string option
  -> plaintext:bool
  -> f:(Prigh_web.Desktop_link.Connect_reply.t option -> unit)
  -> unit

(** Opens [url] (the page's own, for another session) in a new window: a
    new tab in a browser; under the desktop app, another of its windows, in
    desktop mode too (see {!Prigh_web.Desktop_link.window_fragment}): the
    app's own window opens another app window, a browser's app window a
    popup (no tabs, no address bar) in the same browser. *)
val open_window : string -> unit

(** Stops the desktop app, and shows what happened in the page meanwhile
    (the window closes once the sidecar has exited). On the backend's own
    site it goes to {!Prigh_web.Desktop_link.quit_url}, which the window
    catches; in a browser's app window it closes the window, which ends the
    app (or, if the browser will not close it, says to). *)
val quit : unit -> unit
