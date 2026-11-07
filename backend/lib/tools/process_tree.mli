open! Core
open! Import

(** Killing a command with everything it started, including what left its
    process group ([timeout], [setsid], job control). *)

(** Every process's parent, from [/proc] or else [ps]; [None] when neither
    can tell. *)
val parents : env:Env.t -> (Pid.t * Pid.t) list option

(** The descendants of [root] in [parents] (not [root] itself). *)
val descendants : (Pid.t * Pid.t) list -> root:Pid.t -> Pid.t list

(** SIGKILLs the process group [root] leads and every descendant of [root].
    They are stopped first, so none forks or escapes while they are looked
    for; those that left the tree before (daemons reparented to init) are
    out of reach. *)
val kill : env:Env.t -> root:Pid.t -> unit
