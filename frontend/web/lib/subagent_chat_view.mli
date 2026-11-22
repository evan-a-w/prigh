open! Core
open! Import

(** A subagent's transcript in the chat's place ([Model.subagent_chat]),
    read-only: a bar with the way back to the session (Esc) and how the
    subagent is doing, then its transcript as the chat shows one, then its
    report or how it failed. *)
val view
  :  App.Model.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> string
  -> Node.t

(** In the composer's place: it is read-only, and how to get back. *)
val footer : inject:(App.Action.t -> unit Effect.t) -> Node.t
