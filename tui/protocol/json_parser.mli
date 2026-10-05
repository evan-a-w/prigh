open! Core

(** A JSON parser whose stack use grows with the nesting depth only, not with
    the length of arrays, objects or strings: js_of_ocaml's stack is small,
    and [Jsonaf.parse] overflows it on arrays of a few thousand elements (a
    long session's [get_messages]). Numbers keep their source text, like
    [Jsonaf]. *)
val parse : string -> Jsonaf.t Or_error.t
