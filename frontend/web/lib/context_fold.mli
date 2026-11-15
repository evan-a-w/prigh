open! Core
open! Import

(** The changes with a little context, on a phone: the whole file's rows
    (the hunks' changes inline, as [Code_review]'s whole-file layout has
    them) with the unchanged lines more than [context] lines from a change
    folded into gaps ([Diff_view.Row.Gap]), each of which shows more of its
    lines from either end, or all of them, as asked. Runs of unchanged lines
    shorter than [min_gap] are not folded: the gap's row would take as much
    room. *)

module How : sig
  type t =
    | Down (** the next [step] lines under the rows above it *)
    | Up (** the [step] lines over the rows below it *)
    | All
  [@@deriving sexp_of, equal]
end

(** What the user has shown of each gap. *)
type t [@@deriving sexp_of, equal]

val empty : t
val context : int
val step : int
val min_gap : int
val fold : t -> Diff_view.Row.t list -> Diff_view.Row.t list
val reveal : t -> Diff_view.Gap.t -> How.t -> t
