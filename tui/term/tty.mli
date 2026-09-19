open! Core

(** Clears the [IEXTEN] local flag so the line discipline stops eating ^O and
    ^V. A no-op when [fd] is not a tty. *)
val clear_iexten : Core_unix.File_descr.t -> unit
