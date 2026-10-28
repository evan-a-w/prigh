open! Core
open! Import

(** The calling agent's conversation, as a tool call sees it: what its loop
    sends the provider (model, thinking, system prompt) and its messages up
    to and including the assistant message that made the call, whose tool
    calls have no results yet. [subagent]'s [fork] starts from it. *)

type t =
  { model : Model.t
  ; thinking : Thinking.t
  ; system : string option
  ; messages : Message.t list
  }
