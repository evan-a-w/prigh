open! Core
open! Import

(** What a user sends an agent while it works: steers, delivered at its next
    step boundary (after the current tool results), and follow-ups, delivered
    once it would otherwise stop. The main agent ({!Agent}) and every
    background subagent ({!Subagent_inbox}) keep one. *)

module Kind : sig
  type t =
    | Steer
    | Follow_up
  [@@deriving sexp_of, equal]

  (** [steer], [follow_up]: the RPC's [queue]/[mode] values. *)
  val to_string : t -> string

  val of_string : string -> t Or_error.t
end

type 'a t

val create : unit -> 'a t
val push : 'a t -> Kind.t -> 'a -> unit
val is_empty : 'a t -> bool

(** Oldest first. *)
val items : 'a t -> Kind.t -> 'a list

(** Takes every steer, oldest first. *)
val take_steers : 'a t -> 'a list

(** Takes the oldest follow-up. *)
val take_follow_up : 'a t -> 'a option

(** Takes everything: steers, then follow-ups. *)
val take_all : 'a t -> 'a list

(** Moves the steers to the front of the follow-ups, for steers that arrived
    after the last step boundary of a run that ended: they were sent before
    any follow-up still queued. Whether anything moved. *)
val steers_first : 'a t -> bool

(** Takes the most recently queued message: follow-ups before steers. *)
val pop_last : 'a t -> 'a option

(** Takes the [index]th (from 0) message of a queue if it is still there and
    [matches] (the caller's view may be stale). *)
val remove
  :  'a t
  -> Kind.t
  -> index:int
  -> matches:('a -> bool)
  -> 'a Or_error.t
