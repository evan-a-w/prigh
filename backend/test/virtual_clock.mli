open! Core
open! Prigh

(** Virtual time for tests: fibers [sleep] for exact spans, and time jumps
    to the next waker once every fiber is waiting, so durations (and the
    overlap of parallel work) do not depend on the machine. *)

type t

val create : unit -> t
val now : t -> float
val sleep : t -> float -> unit

(** Monotonic: [now]; wall clock: [now] after 2027-01-15 08:00 UTC. *)
val trace_clock : t -> Trace.Clock.t

(** Runs [f] while moving time. Time moves on even while a fiber waits for
    real IO (a process, a file), so tests wait for those before relying on
    virtual time again. *)
val run : t -> (unit -> 'a) -> 'a
