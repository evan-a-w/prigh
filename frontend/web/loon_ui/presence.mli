(** Exit animations, as Radix's Presence: an element with this attribute,
    when removed from the page, is replaced by a copy of itself (inert, ids
    removed) whose [data-state] is ["closed"] (and its children's), so that
    shadcn's [data-[state=closed]:animate-out] classes play; the copy is
    removed when its animation ends. Nothing happens outside a browser, or
    when the element's parent left the page with it. *)

open! Import

val attr : Attr.t
