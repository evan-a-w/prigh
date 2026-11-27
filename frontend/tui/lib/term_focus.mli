open! Core

(** Focus reporting ([CSI ? 1004 h]): the terminal sends [CSI I] when it gains
    the focus and [CSI O] when it loses it. Notty drops both, so the
    terminal's input is filtered before it reaches Notty. *)

val enable : string
val disable : string

(** The input without the focus reports, and those reports in order ([true]:
    focused). *)
val split : string -> string * bool list

(** Stateful filter for streaming reads: buffers a trailing incomplete CSI
    focus prefix (["\027"] or ["\027["]) so reports split across two reads are
    still recognised. *)
type t

val create : unit -> t
val feed : t -> string -> string * bool list
val has_pending : t -> bool

(** Emits and clears any buffered escape prefix (e.g. a standalone Esc key
    after a short timeout, or on EOF). *)
val flush : t -> string
