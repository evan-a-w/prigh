open! Core
open! Import

(** Where the time went between the user's message and an external
    harness's first output: the phases of a run's start (the harness's
    process, its handshake, the checkpoint, the prompt), timed and also
    traced ({!Trace}).

    The timings are fiber-local: {!with_} makes [t] current for [f] and the
    fibers it forks, and {!phase} anywhere under it (the runner, the
    adapter) adds to it; outside, {!phase} only traces. *)

module Phase : sig
  type t =
    { name : string
    ; seconds : float
    ; alongside : bool (** ran beside the others, not after them *)
    ; children : t list
    }
  [@@deriving sexp_of]
end

type t

val create : unit -> t
val with_ : t -> (unit -> 'a) -> 'a
val current : unit -> t option

(** Runs [f] as the phase [name] of the current timings (nested in the
    phase running it), and in a {!Trace} span. [alongside]: [f] runs in a
    fiber beside the phases (nothing nests in it). *)
val phase : ?alongside:bool -> string -> (unit -> 'a) -> 'a

(** Records a phase of [seconds] that ended now, at the top level. *)
val record : t -> string -> seconds:float -> unit

(** Seconds since [t] was created. *)
val elapsed : t -> float

(** In order, those [alongside] last. *)
val phases : t -> Phase.t list

(** The phases, in order: [name 1.2 s (child 0.4 s, ...)]. *)
val to_string_hum : Phase.t list -> string

(** The notice for a slow start. *)
val notice : harness:string -> total:float -> Phase.t list -> string
