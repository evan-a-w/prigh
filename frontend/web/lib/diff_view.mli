open! Core
open! Import

(** Diffs as tables of removed/added/context lines. Past 8 rows the rest
    folds behind a "Show all N lines" toggle of the [fold] with that key,
    unless it is expanded. With [lang], the lines' code is highlighted. *)

(** Unchanged lines folded away between changes (see [Context_fold]): [id]
    names it while lines are shown from either end, [first] is the new
    number of its first line, [before] and [after] say whether rows are
    shown before and after it. *)
module Gap : sig
  type t =
    { id : int
    ; first : int
    ; hidden : int
    ; before : bool
    ; after : bool
    }
  [@@deriving sexp_of, equal]
end

module Row : sig
  type t =
    | Hunk of string
    | Gap of Gap.t
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

(** How far a wrapped line's continuation is indented, in columns: its own
    indent plus 2 (tabs to the next multiple of [tab_width]; an indent with
    tabs is rounded down to one, so that the continuation's tab stops line
    up, without the 2). *)
val hang : string -> int

val tab_width : int

(** The [td.text] cell's attributes for a line: its [--hang] when it is not
    the CSS's default (2ch). *)
val text_attrs : string -> Attr.t list

(** Each row's changed words ([Word_diff]): for a run of removed lines then
    added ones, the first removed line is paired with the first added, the
    second with the second, and so on; other rows have none. *)
val word_changes : Row.t list -> Word_diff.Range.t list list

(** [tokens] split at the byte offsets [cuts], each piece with its offset. *)
val pieces
  :  Highlight.Token.t list
  -> cuts:int list
  -> (int * Highlight.Token.t) list

val in_change : Word_diff.Range.t list -> int -> bool

(** The tokens' spans, the [changed] parts each in a [span.wd]. *)
val changed_spans
  :  Highlight.Token.t list
  -> changed:Word_diff.Range.t list
  -> Node.t list

(** "12 lines hidden". *)
val gap_text : Gap.t -> string

(** A hunk header's nodes: its line ranges, then the code it is in
    ([@@ -12,7 +12,8 @@ let f x =]). *)
val hunk_header : string -> Node.t list

(** A unified diff (what [edit] returns), one line number per row (the new
    one, or the old one of a removed line), folded to [max_rows] (default
    8). *)
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
