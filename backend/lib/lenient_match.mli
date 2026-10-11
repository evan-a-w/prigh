open! Core
open! Import

(** Finding text that a model reproduced slightly wrong: with typographic
    punctuation typed as ASCII, trailing whitespace dropped or CRLF line
    endings written as LF. *)

(** The ASCII character a model would type for a typographic one (dashes,
    curly quotes, unicode spaces), by code point. *)
val ascii_equivalent : int -> char option

(** [s] with [ascii_equivalent] applied to every character. *)
val ascii_punctuation : string -> string

(** The byte ranges [(start, stop)] of [content] that match [pattern] when
    both have [ascii_punctuation] applied, CRLF read as LF and trailing
    spaces and tabs removed from every line. A range covers whole
    characters of [content], from the first matched one to the last. *)
val find : string -> pattern:string -> (int * int) list
