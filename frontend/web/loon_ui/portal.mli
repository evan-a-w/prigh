(** Radix's Portal: overlay content shown in the browser's top layer (the
    Popover API, [popover="manual"]), above everything else on the page and
    outside every ancestor's overflow clipping, stacking context and
    containing block, while staying where it is in the DOM (so it inherits
    the theme's variables and [dark] class, and focus order and events are
    unchanged). Where the Popover API is missing, the element is only fixed
    in place. *)

open! Import
open Js_of_ocaml

(** For an element positioned by its own code ({!Popper}): [popover="manual"]
    and styles undoing the browser's for popovers (inset, margin, border,
    padding, background, overflow) and the text styles inherited from where
    it is in the DOM (white space, alignment, weight), as React's portals to
    the body would not inherit them. Call {!show} on it once mounted. *)
val positioned : Attr.t list

(** A layer covering the viewport, for a modal's overlay and content (which
    have [pointer-events: auto]: the layer itself lets the pointer through);
    shown on mount. *)
val fullscreen : Attr.t list

(** Puts the element, and the [popover] elements inside it, in the top layer
    (it must be on the page); nothing if they are already there. *)
val show : Dom_html.element Js.t -> unit

(** Whether the element is in the top layer. *)
val is_shown : Dom_html.element Js.t -> bool
