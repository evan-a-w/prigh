open! Core
open! Import

(** The worktrees that matter to a session ({!Prigh_ui.Session_worktrees}),
    with what the repositories' listings say of them: for the session card,
    and the commands that act on them. *)
val of_session
  :  App.Model.t
  -> Session_summary.t
  -> Prigh_ui.Session_worktrees.Node.t list
