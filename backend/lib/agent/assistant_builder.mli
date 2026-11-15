open! Core

(** Accumulates streamed [Assistant_event.t]s into a [Message.Assistant.t].
    Consecutive deltas of the same kind are merged into one content block. *)

type t

val create : model:string -> t
val apply : t -> Assistant_event.t -> unit

(** [apply], returning the index in the content of the block the event changed or added
    (that of the last block when it changed none: a delta for a tool call
    that never started). *)
val apply_block : t -> Assistant_event.t -> int

val snapshot : t -> Message.Assistant.t

val finish
  :  t
  -> stop_reason:Stop_reason.t
  -> usage:Usage.t
  -> Message.Assistant.t
