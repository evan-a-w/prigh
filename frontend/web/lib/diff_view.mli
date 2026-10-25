open! Core
open! Import

(** Diffs as tables of removed/added/context lines. Past 8 rows the rest
    folds behind a "Show all N lines" toggle of the [fold] with that key,
    unless it is expanded. With [lang], the lines' code is highlighted. *)

module Row : sig
  type t =
    | Hunk of string
    | Line of
        { kind : [ `Same | `Added | `Removed ]
        ; old_line : int option
        ; new_line : int option
        ; text : string
        }
end

(** [@@ -12,7 +12,8 @@]'s old and new starts. *)
val hunk_starts : string -> (int * int) option

(** A unified diff's rows ([---]/[+++] lines dropped). *)
val unified_rows : string -> Row.t list

(** Each row's tokens ([None] for a hunk), the old and the new side each
    carrying its own state. *)
val highlighted
  :  Highlight.Lang.t
  -> Row.t list
  -> Highlight.Token.t list option list

(** A unified diff (what [edit] returns), with old and new line numbers,
    folded to [max_rows] (default 8). *)
val unified
  :  ?max_rows:int
  -> ?lang:Highlight.Lang.t
  -> fold:Fold.t * string
  -> string
  -> Node.t

(** Diff lines without line numbers: an [apply_patch] input's hunks. *)
val hunks
  :  ?lang:Highlight.Lang.t
  -> fold:Fold.t * string
  -> string list
  -> Node.t

(** An [edit] call's [(old_text, new_text)] pairs, diffed line by line. *)
val edits
  :  ?lang:Highlight.Lang.t
  -> fold:Fold.t * string
  -> (string * string) list
  -> Node.t

(** Whether [text] is a unified diff. *)
val is_unified : string -> bool

(** Lines [(added, removed)]. *)
val unified_counts : string -> int * int

val edits_counts : (string * string) list -> int * int
