(** Moving focus within a part of the page (keeping it inside a modal). *)

open! Import

(** The first, or last, focusable element inside the element with id [id]. *)
val first_in : string -> unit Effect.t

val last_in : string -> unit Effect.t

(** An invisible focusable element at one end of the element [id]: focused
    by Tab leaving [id] at one end, it sends focus to the other end; focused
    from outside [id] (Tab from where the focus was left on open), to its
    own end. *)
val guard : id:string -> [ `Start | `End ] -> Node.t

(** Focuses the element when it is added to the page (an overlay's content
    that just opened), without scrolling: its item with [data-highlighted]
    if it has one, else the first element inside matching the selector
    ([`Selector]), else the first focusable element inside
    ([`First_tabbable]), else the element itself; [`Keep] leaves the focus
    where it is (Radix's prevented [onOpenAutoFocus]). When it leaves the page
    with the focus, the focus goes back to the element that had it before
    (Radix's FocusScope). *)
val on_mount : [ `First_tabbable | `Keep | `Self | `Selector of string ] -> Attr.t

(** For the overlays closing in this render: no focus back where it was
    when they leave the page (Radix's onCloseAutoFocus prevented, as a
    non-modal popover does after a click outside). *)
val no_restore : unit Effect.t

(** Focuses the element with id [id], without scrolling, after the next
    render, unless an open dialog not containing it has taken focus meanwhile
    (a dialog a menu item opened). *)
val restore : string -> unit Effect.t

(** Focuses the element whenever [focused] becomes true (an item highlighted
    from the keyboard). *)
val when_ : bool -> Attr.t

(** For a container's [keydown]: Tab on its last tabbable element goes to
    its first, Shift+Tab on its first to its last (Radix's FocusScope with
    [loop], as a non-modal popover's content). *)
val loop_tab : Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t -> unit Effect.t
