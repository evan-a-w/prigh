open! Core
open! Import

(** Where a review comment's lines are now: the file may have changed since
    the user commented, so its [anchor] (the lines' text then) is looked
    for, nearest to where it was. *)

module Located : sig
  type t =
    | At of
        { first : int
        ; last : int
        } (** 1-based, inclusive *)
    | Outdated (** the lines are gone *)
  [@@deriving sexp_of, equal]
end

(** [lines]: the file's lines now, the first at index 0. *)
val locate : P.Review_comment.t -> lines:string array -> Located.t
