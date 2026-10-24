open! Core
open! Import

(** A message with the time it was recorded (seconds since the epoch), for
    transcripts shown to people. [at] is [None] for messages written before
    sessions recorded times and for the compaction summary. *)
type t =
  { message : Message.t
  ; at : float option
  }
[@@deriving sexp_of]
