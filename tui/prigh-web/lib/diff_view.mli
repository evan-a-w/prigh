open! Core
open! Import

(** Diffs as tables of removed/added/context lines; past [max_rows] the rest
    folds behind a "N more lines" toggle. *)

(** A unified diff (what [edit] returns), with old and new line numbers. *)
val unified : ?max_rows:int -> string -> Node.t

(** An [edit] call's [(old_text, new_text)] pairs, diffed line by line. *)
val edits : ?max_rows:int -> (string * string) list -> Node.t

(** Whether [text] is a unified diff. *)
val is_unified : string -> bool

(** Lines [(added, removed)]. *)
val unified_counts : string -> int * int

val edits_counts : (string * string) list -> int * int
