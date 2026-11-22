open! Core
open! Import

(** A session's worktrees, a row each: its branch, what is in it (changes,
    sync, a subagent's or a pull request's, the one the session works in)
    and why it is there; a click (or a right-click) opens its menu. For the
    session card. *)
val rows
  :  App.Model.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Session_summary.t
  -> Node.t list
