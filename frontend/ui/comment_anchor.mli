open! Core
open! Import

(** Where a review comment's text is now: the file may have changed since
    the user commented, so its [anchor] (the lines' text then) is looked
    for, nearest to where it was; failing that, a selection's text alone
    (the rest of its lines may have changed). *)

module Located : sig
  type t =
    | At of P.Review_comment.Range.t (** on the comment's side *)
    | Outdated (** the text is gone *)
  [@@deriving sexp_of, equal]
end

(** [line n]: the [n]th line (1-based) of the comment's side now, of
    [count]; [None] where that side's text is not known (the old side's
    lines the change kept). *)
val locate
  :  P.Review_comment.t
  -> line:(int -> string option)
  -> count:int
  -> Located.t
