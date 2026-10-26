open! Core
open! Import

(** A unified diff of two texts, as [git diff] writes one file's section
    ([diff --git], [---], [+++], then [@@] hunks with 3 lines of context),
    for the review's view of what changed in a file since an action on it.
    A last line without a newline is followed by git's [\ No newline at end
    of file]. *)

type t =
  { diff : string (** [""] when the texts are equal *)
  ; added : int
  ; removed : int
  }
[@@deriving sexp_of, equal]

val unified : path:string -> old:string -> new_:string -> t
