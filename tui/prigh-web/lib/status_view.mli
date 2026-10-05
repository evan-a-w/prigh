open! Core
open! Import

(** The status line under the composer: running, context, tokens, cost, queued
    messages (restorable), background subagents and jobs, the tool host and
    the user. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
