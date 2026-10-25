open! Core

(** The browser's copy of the user's preferences ([Prigh_web.Pref_sync]):
    what the page starts with until the backend's arrive, and all there is
    with a backend that keeps none. In localStorage, [prigh-web.prefs]:
    [{owner, legacy, prefs}], whose [owner] is the backend and user they
    belong to (another account's are not restored, only drawn over by the
    backend's).

    Pages from before kept some under keys of their own
    ([prigh-web.notify], [prigh-web.history], the panels' sizes,
    [prigh-web.terminal-open]): the first load moves them here as [legacy],
    to be uploaded once ([Restore_prefs]'s [legacy]). *)

val key : string

(** [owner]'s preferences, and whether they are legacy. *)
val load
  :  Login.Storage.t
  -> owner:string
  -> Prigh_protocol.Prefs.Change.t list * bool

val save
  :  Login.Storage.t
  -> owner:string
  -> Prigh_protocol.Prefs.Change.t list
  -> unit

(** [prigh-web.theme]: the document's attributes and variables, which
    [index.html] puts back before the first paint. *)
val theme_key : string

val save_theme : Login.Storage.t -> Prigh_web.Theme.t -> unit
