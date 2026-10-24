open! Core
open! Import

(** Highlighted code as DOM: a span per token with its [Highlight.Kind.css]
    class, plain text as text. *)

val spans : Highlight.Token.t list -> Node.t list

(** [text] in [lang] (as is without one). Past [limit] bytes (default
    [default_limit]) the rest, from the next line on, is plain text: a
    large file costs a bounded time. *)
val code : ?limit:int -> Highlight.Lang.t option -> string -> Node.t list

val default_limit : int
