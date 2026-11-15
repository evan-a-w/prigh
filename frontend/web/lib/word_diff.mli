open! Core
open! Import

(** The words that changed between a removed line and the added line that
    replaced it, for highlighting within the lines: the parts of each
    line's text (byte ranges [start, end_), in order, not touching) that
    the other does not have. Words are runs of letters, digits, [_] and
    non-ASCII bytes, runs of operator characters ([>=], [|>], [::]) and runs
    of whitespace; other characters (brackets, quotes, commas) are each a
    word of their own. *)

module Range : sig
  type t =
    { start : int
    ; end_ : int
    }
  [@@deriving sexp_of, equal]
end

(** [None] when the lines are too different for word changes to help (less
    than [min_kept] of their non-blank text is common: the whole line
    changed), when they are too long to compare quickly, or when they are
    the same. *)
val ranges : old:string -> new_:string -> (Range.t list * Range.t list) option

(** The fraction of non-blank text two lines must share. *)
val min_kept : float
