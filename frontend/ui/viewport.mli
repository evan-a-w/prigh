open! Core

(** Where the transcript viewport sits. [Follow] pins it to the bottom,
    [Anchored] keeps the line at [top] fixed and counts what arrived beneath
    it: lines in [new_lines], finished messages and tool results in
    [new_items] (see [Transcript.finished]). *)
type t =
  | Follow
  | Anchored of
      { top : int
      ; new_lines : int
      ; new_items : int
      }
[@@deriving sexp_of, equal]

(** Anchored at [top], nothing new yet. *)
val anchored : int -> t
