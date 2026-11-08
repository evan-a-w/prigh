open! Core
open! Import

(** [memory]: the user's notes across sessions ({!Memory}): [save] (text,
    optional title), [recall] (optional query; without one, lists them),
    [forget] (id or title). Runs in the backend on the world's memory; only
    the main agent has it. [save] and [forget] ask in ask mode. Its
    description tells the model to use it very rarely: only when the user
    asks to remember or refers to what they said before. *)

val name : string
val spec : Tool_spec.t

(** Whether [action] changes the memory (approvals). *)
val changes : string -> bool

(** [now] (default: the clock) dates saved notes. While memory is off
    ({!Memory.enabled}) every call is refused. *)
val create : ?now:(unit -> Time_float.t) -> memory:Memory.t -> unit -> Tool.t
