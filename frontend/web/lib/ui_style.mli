open! Core
open! Import

(** An inline [style] attribute of [field, value] pairs, set as the
    attribute's text: unlike [Attr.style], which sets the style object's
    properties, it takes custom properties ([--x]), which the components'
    CSS reads. *)
val attr : (string * string) list -> Attr.t
