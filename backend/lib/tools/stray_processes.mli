open! Core
open! Import

(** Processes a [bash] call started that outlive it ([cmd &], [nohup],
    [setsid], [disown]): nothing tracks them as jobs or reports their exit.
    Each call's command gets a unique {!variable} in its environment, which
    its descendants inherit even after leaving its process group or session;
    those still running once it has finished are found by it. Only where
    [/proc] shows other processes' environments (Linux); elsewhere none are
    found. *)

val variable : string

(** A call's value for {!variable}, and when it was made: only processes
    started since then can carry it, so only their environments are read. *)
module Marker : sig
  type t

  val create : unit -> t

  (** The value to give {!variable}. *)
  val value : t -> string
end

module Process_info : sig
  type t =
    { pid : Pid.t
    ; command : string
    }
end

(** The running processes of this user whose environment has [marker]. *)
val find : Marker.t -> Process_info.t list

(** What to tell the model about [processes], if any. *)
val note : Process_info.t list -> string option

(** Whether [environ] (NUL-separated [NAME=value] entries, as in
    [/proc/PID/environ]) has the entry [entry]. *)
val has_entry : string -> entry:string -> bool

(** The start time, in clock ticks since boot, of the process whose
    [/proc/PID/stat] is [stat]. *)
val start_ticks : string -> int option
