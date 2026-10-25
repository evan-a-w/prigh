open! Core
open! Import

(** Text selected in a review's code (the diff's rows or the whole file),
    as the page reports it: its two ends, each a row ([data-side] and
    [data-line] on the [tr]) and how far into the row's text, made into
    the [Review_comment.Range.t] a comment is anchored to and its lines'
    text. Pure: the page's glue (in [app/]) reads the DOM. *)

module Point : sig
  type t =
    { side : Review_comment.Side.t
    ; line : int
    ; offset : int
      (** UTF-16 code units into the line's text, as the browser counts;
          past its end means the end *)
    }
  [@@deriving sexp_of, equal]
end

(** The side and number a row is known by: removed lines by their old
    number, the others by their new one; hunk headers and unnumbered lines
    have none. *)
val key : Diff_view.Row.t -> (Review_comment.Side.t * int) option

(** The selection between [start] and [end_] (in either order) among [rows]
    (as shown), with its lines' text. A selection starting at the end of a
    line starts on the next one; one ending at the start of a line ends on
    the line before. On removed lines only, it is on the old text (as far as
    they run on); otherwise its removed lines at either end are left out, so
    that it is on the file as it is. Whole lines have no columns. [lines]:
    the file's text now, for the lines between hunks. [None] when nothing is
    left, or a row is not among [rows]. *)
val resolve
  :  Diff_view.Row.t list
  -> lines:string array option
  -> start:Point.t
  -> end_:Point.t
  -> (Review_comment.Range.t * string) option

(** The lines from [from] to [to_] (in either order; their offsets do not
    matter), whole. *)
val whole_lines
  :  Diff_view.Row.t list
  -> lines:string array option
  -> from:Point.t
  -> to_:Point.t
  -> (Review_comment.Range.t * string) option

(** The bytes of [row]'s text that [range] covers (all of it for whole
    lines), if any. *)
val covered : Review_comment.Range.t -> Diff_view.Row.t -> (int * int) option
