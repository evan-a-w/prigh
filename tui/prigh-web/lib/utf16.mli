open! Core

(** The browser counts text in UTF-16 code units (a caret's [selectionStart]),
    OCaml strings in UTF-8 bytes. *)

(** The byte offset in [text] of the [utf16]th code unit (clamped to the
    text). *)
val byte_offset : string -> utf16:int -> int
