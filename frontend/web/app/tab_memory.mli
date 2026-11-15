open! Core

(** What the tab was doing, kept in its sessionStorage so that reloading the
    page (or the browser bringing back a discarded tab) returns to the same
    place: where the chat was scrolled to ([prigh-web.scroll]) and the
    composer's text ([prigh-web.draft]), each for the session in the page's
    URL. *)

(** The chat's position now. *)
val save_scroll : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> unit

(** The first time the chat is to go to its end after the page loaded: where
    it was in the URL's session before the reload (how far its top was from
    the end of the transcript), unless that was its end. Later calls give
    [None]. *)
val take_scroll : unit -> float option

(** Kept within 300ms (at once when it is empty: sent or cleared), for the
    session the page is in now. *)
val save_draft_soon : string -> unit

(** Writes the draft waiting to be kept, if any: when the page may go (hidden,
    unloaded) or the editor loses focus. *)
val flush_draft : unit -> unit

(** The draft the URL's session had before the page loaded. *)
val saved_draft : unit -> string option

(** The temporary session this tab is in and its key
    ([prigh-web.temporary]): only this tab's sessionStorage has it, so that
    a reload gets back in, and nothing else does; [None] forgets it. *)
val remember_temporary : (string * string) option -> unit

(** The key of [session], if it is the temporary session remembered. *)
val temporary_key_of : string -> string option
