open! Core

(** Prints virtual DOM as indented HTML or as the text a user sees. *)

val html : ?selector:string -> Virtual_dom.Vdom.Node.t -> unit
val text : ?selector:string -> Virtual_dom.Vdom.Node.t -> unit
