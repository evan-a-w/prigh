open! Core
open! Import

(** Markdown as DOM: paragraphs, fenced code and inline code. *)
val render : string -> Node.t
