open! Core

(** Ctrl+Z right after a long paste became a chip, or after a chip was
    removed: the composer's pasted texts as they were. Only while the draft
    and the chips are still as that left them, so the textarea keeps its own
    undo for typed text. *)

type t [@@deriving sexp_of]

(** The chips went from [before] to [after], the draft [draft]. *)
val record : before:string list -> after:string list -> draft:string -> t

(** The chips to undo or redo to, when [pasted] and [draft] are as the last
    step left them; with the step to come after. *)
val undo : t -> pasted:string list -> draft:string -> (string list * t) option

val redo : t -> pasted:string list -> draft:string -> (string list * t) option
