open! Core
open! Import

(** A diff cut down to fit a model's prompt: whole when it is within
    [max_bytes], else every file keeps its header and a share of its hunks
    (an even split of the budget), and the files past the budget are listed
    with their line counts only. What was left out is said in the text. *)
val digest : max_bytes:int -> string -> string
