open! Core
open! Import

(** Diffs as tables of removed/added/context lines. Past 8 rows the rest
    folds behind a "Show all N lines" toggle of the [fold] with that key,
    unless it is expanded. *)

(** A unified diff (what [edit] returns), with old and new line numbers,
    folded to [max_rows] (default 8). *)
val unified : ?max_rows:int -> fold:Fold.t * string -> string -> Node.t

(** Diff lines without line numbers: an [apply_patch] input's hunks. *)
val hunks : fold:Fold.t * string -> string list -> Node.t

(** An [edit] call's [(old_text, new_text)] pairs, diffed line by line. *)
val edits : fold:Fold.t * string -> (string * string) list -> Node.t

(** Whether [text] is a unified diff. *)
val is_unified : string -> bool

(** Lines [(added, removed)]. *)
val unified_counts : string -> int * int

val edits_counts : (string * string) list -> int * int
