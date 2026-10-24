open! Core

(** A cross-process lock compatible with node's [proper-lockfile], which pi
    uses for its [auth.json]: the lock is the directory [<file>.lock], taken
    with an atomic [mkdir]; a holder keeps its mtime fresh, and a lock whose
    mtime is older than [stale] is considered abandoned and removed. Sharing
    one credential file with pi is only safe because both sides honour this
    protocol. *)

(** [with_lock ~file ~f] runs [f] holding [<file>.lock]. Must run under an
    Eio scheduler (waiting and touching sleep in fibers). Fibers of this
    process must serialise themselves (see [Auth_store]); this only handles
    other processes. Raises after [timeout] of the lock being held by a live
    holder. The holder touches the lock every [touch_every] so that other
    processes with a shorter [stale] (pi's synchronous path uses 10s) never
    break a live lock. *)
val with_lock
  :  ?stale:Time_ns.Span.t (** default 30s *)
  -> ?timeout:Time_ns.Span.t (** default 30s *)
  -> ?touch_every:Time_ns.Span.t (** default 3s *)
  -> file:string
  -> f:(unit -> 'a)
  -> unit
  -> 'a

val lock_path : file:string -> string
