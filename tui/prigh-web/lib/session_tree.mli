open! Core
open! Import

(** A session's entries ([get_entries]) as pickers: its user messages for
    [/fork] and [/rewind], and the whole tree, abandoned branches included,
    for [/tree]. *)

(** The head and the entries. *)
val of_json : Json.t -> (string option * Entry.t list) Or_error.t

(** The user messages, oldest first, numbered; the last is marked. *)
val user_items : Entry.t list -> Picker.Item.t list

(** A user message's text, by entry id. *)
val user_text : Entry.t list -> string -> string option

(** Messages in depth-first order ([>] user, [·] assistant, [⚙] tool result),
    each branch indented under its branch point; those on the path to [head]
    are marked. *)
val tree_items : Entry.t list -> head:string option -> Picker.Item.t list

(** The first line of a message, for labels. *)
val first_line : Message.t -> string
