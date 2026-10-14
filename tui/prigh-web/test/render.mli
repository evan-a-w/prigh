open! Core

(** Prints virtual DOM as indented HTML or as the text a user sees. *)

val html : ?selector:string -> Virtual_dom.Vdom.Node.t -> unit
val text : ?selector:string -> Virtual_dom.Vdom.Node.t -> unit

(** [Html.mark]'s icon as the glyph it stands for (✓ ✕ – ■ ...), for the
    text a user sees. *)
val mark_glyph : Virtual_dom_test_helpers.Node_helpers.t -> string option
