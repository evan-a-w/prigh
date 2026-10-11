open! Core

(** Where the chat was scrolled to, kept in the tab's sessionStorage
    ([prigh-web.scroll]) so that reloading the page (or the browser bringing
    back a discarded tab) returns to the same place in the session. *)

(** The chat's position now, for the session in the page's URL. *)
val save : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> unit

(** The first time the chat is to go to its end after the page loaded: where
    it was in the URL's session before the reload, unless that was its end.
    Later calls give [None]. *)
val take : unit -> float option
