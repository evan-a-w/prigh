open! Core

(** Sets or clears the [IEXTEN] local flag (which makes the line discipline eat
    ^O and ^V) and returns its previous value. A no-op returning [true] when
    [fd] is not a tty. *)
val set_iexten : Core_unix.File_descr.t -> bool -> bool
