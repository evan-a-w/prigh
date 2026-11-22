(** Dismissing an overlay, as Radix's DismissableLayer: Esc, a pointer
    down outside it, and (if asked) focus moving outside it. Overlays stack:
    only the topmost one (the last opened) is dismissed. A [modal] layer
    makes the rest of the page ignore the pointer while it is open (Radix's
    [disableOutsidePointerEvents]): a click outside only dismisses it. *)

open! Import

module Reason : sig
  type t =
    | Escape
    | Pointer_outside
    | Focus_outside
  [@@deriving sexp_of, equal]
end

(** The attribute for the layer's outermost element. [inside] are the ids of
    elements that count as part of the layer (its trigger). *)
val attr
  :  ?inside:string list
  -> ?focus_outside:bool
  -> ?modal:bool
  -> on_dismiss:(Reason.t -> unit Effect.t)
  -> unit
  -> Attr.t

(** Esc in the layer's content: [on_escape], and the key goes no further (to
    the layers beneath). For the content's [on_keydown]; the document's
    listener catches Esc pressed with the focus elsewhere. *)
val escape_keydown
  :  on_escape:unit Effect.t
  -> Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t
  -> unit Effect.t option
