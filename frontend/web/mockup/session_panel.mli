(** The session card: a small card in the main area's top right (open by
    default, toggled from the top bar; a bottom sheet on phones) with the
    session's host and checkout, its git actions, and its subagents and the
    project's worktrees as folds. A subagent links to its own chat. *)

open! Core
open Bonsai_web

val view
  :  ?class_:string
  -> subagents_open:bool
  -> worktrees_open:bool
  -> toggle_subagents:unit Effect.t
  -> toggle_worktrees:unit Effect.t
  -> unit
  -> Vdom.Node.t
