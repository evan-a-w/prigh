(** The session panel: a column docked at the chat's right (open by
    default, toggled from the top bar) with the session's host and checkout,
    its git actions, its subagents and the project's worktrees. *)

open! Core
open Bonsai_web

val view : unit -> Vdom.Node.t
