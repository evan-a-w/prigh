open! Core

(** A transient acknowledgement ("view: verbose", "copied 12 chars", a usage
    error): shown in the composer's rule, or in a box above it when too long,
    until the next key press or until it expires. Never part of the
    transcript. *)

type t =
  { text : string
  ; severity : Severity.t
  ; ticks_left : int (** [Tick]s (100ms) until it expires *)
  }
[@@deriving sexp_of, equal]

(** Errors last longer than warnings, warnings longer than the rest. *)
val create : ?severity:Severity.t -> string -> t

(** One tick older; [None] once expired. *)
val tick : t -> t option
