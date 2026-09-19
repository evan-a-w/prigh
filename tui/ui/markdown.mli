open! Core

(** Small markdown-to-styled-text renderer: headings, nested lists, block
    quotes, rules, tables, fenced code and inline spans. Lines are not wrapped;
    the caller wraps. [width] is used for tables and rules. *)
val render : ?width:int -> string -> Content.t
