open! Core
open! Import

(** Reading a session's trace ({!Trace_file}): where the time went. *)

(** The text summary of [prigh trace] and [/trace]: the main agent's wall
    time split into model / tools / waiting on the user / retries /
    compaction / other, time to first token by model, per-tool totals, and
    the [slowest] (default 10) spans with their path in the tree. [bad] is
    the number of lines that could not be read. *)
val summary : ?slowest:int -> ?bad:int -> Trace.Span.t list -> string

(** The spans as an indented tree, with durations and attributes ([attrs]
    filters which, default all), each child under its parent by start. *)
val outline : ?attrs:(string -> bool) -> Trace.Span.t list -> string

(** The Chrome trace event format, for Perfetto ([ui.perfetto.dev]) or
    [chrome://tracing]: one thread per track (an agent or a background job),
    with spans that overlap without nesting (parallel tool calls) on extra
    threads of that track. *)
val perfetto : Trace.Span.t list -> Json.t

module Report : sig
  type t =
    { summary : string (** {!summary}, with what to do next *)
    ; level : Trace.Level.t (** the level being recorded *)
    ; spans : Trace.Span.t list
    }
end
