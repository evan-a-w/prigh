open! Core

(** Accumulates streamed [Assistant_event.t]s into a [Message.Assistant.t].
    Consecutive deltas of the same kind are merged into one content block. *)

type t

val create : model:string -> t
val apply : t -> Assistant_event.t -> unit
val snapshot : t -> Message.Assistant.t

val finish
  :  t
  -> stop_reason:Stop_reason.t
  -> usage:Usage.t
  -> Message.Assistant.t
