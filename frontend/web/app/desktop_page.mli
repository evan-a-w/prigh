open! Core

(** The page's side of [prigh desktop] (see {!Prigh_web.Desktop_link}). *)

(** The run's key: taken from the URL's fragment (which it then clears) into
    the tab's sessionStorage, or found there after a reload. [None]: not
    under the desktop app. *)
val key : unit -> string option

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

(** Stops the desktop app, and shows what happened in the page meanwhile
    (the window closes once the sidecar has exited). On the backend's own
    site it goes to {!Prigh_web.Desktop_link.quit_url}, which the window
    catches. *)
val quit : unit -> unit
