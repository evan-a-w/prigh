open! Core
open! Import

(** The settings page's New sessions: what a new session starts with (model,
    thinking, mode, tool host, directory) and the limits every session
    without its own follows (fallback models, budget), each with its control
    and a way back to the built-in. *)
val groups
  :  App.Model.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t list
