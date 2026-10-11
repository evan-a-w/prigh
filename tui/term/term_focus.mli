open! Core

(** Focus reporting ([CSI ? 1004 h]): the terminal sends [CSI I] when it gains
    the focus and [CSI O] when it loses it. Notty drops both, so the
    terminal's input is filtered before it reaches Notty. *)

val enable : string
val disable : string

(** The input without the focus reports, and those reports in order ([true]:
    focused). A report split across two reads is not recognised; terminals
    write each in one piece. *)
val split : string -> string * bool list
