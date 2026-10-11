open! Core
open! Import

(** The queued steers and follow-ups above the composer while they wait:
    each with its text, when it reaches the agent, and buttons to take it
    back into the editor or remove it. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
