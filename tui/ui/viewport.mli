open! Core

(** Where the transcript viewport sits. [Follow] pins it to the bottom,
    [Anchored] keeps the line at [top] fixed and counts appended lines in
    [new_lines]. *)
type t =
  | Follow
  | Anchored of
      { top : int
      ; new_lines : int
      }
[@@deriving sexp_of, equal]
