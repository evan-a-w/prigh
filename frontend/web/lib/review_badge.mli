open! Core
open! Import

(** Loon's Badge as a [span] of text, with prigh's own class [cls] beside
    loon's (the tests and the CSS find it by that): the review page's and
    git's counts, states and chips. *)
val view
  :  ?variant:Loon.Badge.Variant.t
  -> ?attrs:Attr.t list
  -> cls:string
  -> string
  -> Node.t
