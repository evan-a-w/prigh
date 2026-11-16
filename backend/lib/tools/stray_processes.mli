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

(** A fresh value for {!variable}. *)
val marker : unit -> string

module Process_info : sig
  type t =
    { pid : Pid.t
    ; command : string
    }
end

(** The running processes of this user whose environment has [marker]. *)
val find : marker:string -> Process_info.t list

(** What to tell the model about [processes], if any. *)
val note : Process_info.t list -> string option
