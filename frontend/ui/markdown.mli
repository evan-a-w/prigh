open! Core

(** Small markdown-to-styled-text renderer: headings, nested lists, block
    quotes, rules, tables, fenced code and inline spans. Lines are not wrapped;
    the caller wraps. [width] is used for tables and rules.

    A link to a session ([\[title\](?session=ID)], {!Session_link}) is its
    title underlined in magenta and its short id, dim; the ids of [sessions]
    in text and inline code are underlined in magenta too. *)
val render : ?sessions:Session_link.Known.t -> ?width:int -> string -> Content.t
