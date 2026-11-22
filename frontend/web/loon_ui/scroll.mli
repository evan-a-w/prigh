(** Scrolling: an element into view, and locking the page's scroll under a
    modal. *)

open! Import

(** Scrolls the first element matching [selector] the least needed to show
    it (after the next render). *)
val into_view : string -> unit Effect.t

(** While an element with this attribute is on the page, the page itself
    does not scroll (Radix's RemoveScroll under a modal dialog). *)
val lock : Attr.t
