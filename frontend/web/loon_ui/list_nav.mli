(** Moving a highlight through a list with the keyboard: arrows (wrapping),
    Home and End, and typeahead (the next item starting with a letter). *)

open! Import
open Js_of_ocaml

(** [items] are the enabled items in order, with their labels. [loop]
    (default [true]): the arrows wrap round at the ends. *)
val move
  :  ?loop:bool
  -> equal:('a -> 'a -> bool)
  -> items:('a * string) list
  -> current:'a option
  -> Dom_html.Keyboard_code.t
  -> 'a option

(** The letter or digit a key types, for typeahead. *)
val letter : Dom_html.Keyboard_code.t -> char option

(** The text of nodes, as far as it can be seen: their text nodes, but not
    the text inside elements. *)
val text_of_nodes : Node.t list -> string
