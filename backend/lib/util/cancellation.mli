open! Core

(** A cancellation token shared between the fiber doing work and whoever may
    want to stop it. *)

type t

val create : unit -> t
val never : t
val cancel : t -> unit
val is_cancelled : t -> bool

(** Blocks the calling fiber until cancelled. *)
val await : t -> unit

(** Runs [f]; returns [None] if [t] is cancelled first, in which case [f]'s
    fiber is cancelled. *)
val protect : t -> f:(unit -> 'a) -> 'a option
