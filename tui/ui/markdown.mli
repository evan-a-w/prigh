open! Core

(** Small markdown-to-styled-text renderer: headings, fenced code, bullets,
    inline code and bold. Unwrapped lines. *)
val render : string -> Content.t
