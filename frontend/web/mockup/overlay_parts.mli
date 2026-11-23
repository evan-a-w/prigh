(** Pieces the overlay mockups share. *)

open! Core
open Bonsai_web

(** [text] with the characters matching [query] (a subsequence, as the
    fuzzy filter matches) in bold. *)
val highlight : query:string -> string -> Vdom.Node.t list

(** The row of key hints under a picker: [(keys, what they do)]. *)
val footer_hints : (string list * string) list -> Vdom.Node.t
